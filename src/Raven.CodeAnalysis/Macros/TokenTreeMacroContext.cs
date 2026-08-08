using System;
using System.Collections.Immutable;
using System.Threading;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Macros;

public class TokenTreeMacroContext
{
    private static readonly DiagnosticDescriptor s_expectedSingleMemberDeclaration = DiagnosticDescriptor.Create(
        "RAVM022",
        "Expected one member declaration",
        "",
        "",
        "Expected exactly one Raven member declaration.",
        "compiler",
        DiagnosticSeverity.Error,
        true);

    private readonly IMacroTokenStreamProvider? _tokenStreamProvider;
    private readonly ImmutableArray<MacroKeyword> _keywords;
    private readonly ImmutableArray<MacroFileDependency>.Builder _fileDependencies =
        ImmutableArray.CreateBuilder<MacroFileDependency>();

    public TokenTreeMacroContext(
        Compilation compilation,
        SemanticModel semanticModel,
        FreestandingMacroExpressionSyntax syntax,
        CancellationToken cancellationToken = default)
        : this(
            compilation,
            semanticModel,
            syntax,
            tokenStreamProvider: null,
            keywords: ImmutableArray<MacroKeyword>.Empty,
            cancellationToken)
    {
    }

    internal TokenTreeMacroContext(
        Compilation compilation,
        SemanticModel semanticModel,
        FreestandingMacroExpressionSyntax syntax,
        ITokenTreeExpressionMacro macro,
        CancellationToken cancellationToken = default)
        : this(
            compilation,
            semanticModel,
            syntax,
            macro as IMacroTokenStreamProvider,
            macro is IMacroKeywordProvider keywordProvider
                ? keywordProvider.Keywords
                : ImmutableArray<MacroKeyword>.Empty,
            cancellationToken)
    {
        ArgumentNullException.ThrowIfNull(macro);
    }

    private TokenTreeMacroContext(
        Compilation compilation,
        SemanticModel semanticModel,
        FreestandingMacroExpressionSyntax syntax,
        IMacroTokenStreamProvider? tokenStreamProvider,
        ImmutableArray<MacroKeyword> keywords,
        CancellationToken cancellationToken)
    {
        Compilation = compilation ?? throw new ArgumentNullException(nameof(compilation));
        SemanticModel = semanticModel ?? throw new ArgumentNullException(nameof(semanticModel));
        Syntax = syntax ?? throw new ArgumentNullException(nameof(syntax));
        TokenTree = syntax.TokenTree ?? throw new ArgumentException(
            "A token-tree macro context requires a token-tree invocation.",
            nameof(syntax));
        Arguments = CreateArguments(syntax.ArgumentList, semanticModel);
        _tokenStreamProvider = tokenStreamProvider;
        _keywords = keywords.IsDefault ? ImmutableArray<MacroKeyword>.Empty : keywords;
        CancellationToken = cancellationToken;
    }

    public Compilation Compilation { get; }

    public SemanticModel SemanticModel { get; }

    public FreestandingMacroExpressionSyntax Syntax { get; }

    public MacroTokenTreeSyntax TokenTree { get; }

    public ArgumentListSyntax ArgumentList => Syntax.ArgumentList;

    public ImmutableArray<MacroArgument> Arguments { get; }

    public CancellationToken CancellationToken { get; }

    public TextSpan BodySpan => TextSpan.FromBounds(
        TokenTree.OpenBraceToken.Span.End,
        TokenTree.CloseBraceToken.IsMissing
            ? TokenTree.BodyToken.Span.End
            : TokenTree.CloseBraceToken.SpanStart);

    public string GetBodyText()
        => TokenTree.OpenBraceToken.TrailingTrivia + TokenTree.BodyToken.Text;

    public IMacroTokenStream CreateTokenStream()
    {
        var context = new MacroTokenStreamContext(
            GetBodyText(),
            BodySpan,
            Syntax.SyntaxTree?.Options ?? new ParseOptions(),
            CancellationToken);

        if (_tokenStreamProvider is null)
            return new RavenMacroTokenStream(context, _keywords);

        return _tokenStreamProvider.CreateTokenStream(context)
            ?? throw new InvalidOperationException("The macro token-stream provider returned null.");
    }

    public MacroFragmentRegion CreateFragmentRegion(
        MacroFragmentKind kind,
        TextSpan bodyRelativeSpan)
    {
        if (!Enum.IsDefined(kind))
            throw new ArgumentOutOfRangeException(nameof(kind));

        if (bodyRelativeSpan.Start < 0 || bodyRelativeSpan.End > BodySpan.Length)
            throw new ArgumentOutOfRangeException(nameof(bodyRelativeSpan));

        return new MacroFragmentRegion(
            kind,
            bodyRelativeSpan,
            new TextSpan(
                BodySpan.Start + bodyRelativeSpan.Start,
                bodyRelativeSpan.Length));
    }

    internal MacroTokenInfo CreateTokenInfo(
        SyntaxToken token,
        string? kindName,
        MacroTokenClassification classification = MacroTokenClassification.Default)
    {
        if (!Enum.IsDefined(classification))
            throw new ArgumentOutOfRangeException(nameof(classification));

        var bodyRelativeSpan = token.Span;
        if (bodyRelativeSpan.Start < 0 || bodyRelativeSpan.End > BodySpan.Length)
            throw new ArgumentOutOfRangeException(nameof(token));

        return new MacroTokenInfo(
            token,
            bodyRelativeSpan,
            new TextSpan(
                BodySpan.Start + bodyRelativeSpan.Start,
                bodyRelativeSpan.Length),
            kindName,
            classification);
    }

    public ExpressionSyntax ParseExpression()
        => ParseExpressionResult().Syntax;

    public ExpressionSyntax ParseExpression(TextSpan bodyRelativeSpan)
        => ParseExpressionResult(bodyRelativeSpan).Syntax;

    public MacroSyntaxParseResult<ExpressionSyntax> ParseExpressionResult()
        => ParseExpressionResult(new TextSpan(0, BodySpan.Length));

    public MacroSyntaxParseResult<ExpressionSyntax> ParseExpressionResult(TextSpan bodyRelativeSpan)
        => ParseSyntaxResult<ExpressionSyntax>(
            GetBodyText(),
            bodyRelativeSpan,
            static () => new ExpressionSyntax.Missing());

    internal MacroSyntaxParseResult<ExpressionSyntax> ParseExpressionResult(string bodyText)
    {
        ArgumentNullException.ThrowIfNull(bodyText);
        if (bodyText.Length != BodySpan.Length)
            throw new ArgumentException("Replacement body text must preserve the original body length.", nameof(bodyText));

        return ParseSyntaxResult<ExpressionSyntax>(
            bodyText,
            new TextSpan(0, bodyText.Length),
            static () => new ExpressionSyntax.Missing());
    }

    public StatementSyntax ParseStatement()
        => ParseStatementResult().Syntax;

    public StatementSyntax ParseStatement(TextSpan bodyRelativeSpan)
        => ParseStatementResult(bodyRelativeSpan).Syntax;

    public MacroSyntaxParseResult<StatementSyntax> ParseStatementResult()
        => ParseStatementResult(new TextSpan(0, BodySpan.Length));

    public MacroSyntaxParseResult<StatementSyntax> ParseStatementResult(TextSpan bodyRelativeSpan)
        => ParseSyntaxResult<StatementSyntax>(
            GetBodyText(),
            bodyRelativeSpan,
            static () => SyntaxFactory.ExpressionStatement(new ExpressionSyntax.Missing()));

    public TypeSyntax ParseType()
        => ParseTypeResult().Syntax;

    public TypeSyntax ParseType(TextSpan bodyRelativeSpan)
        => ParseTypeResult(bodyRelativeSpan).Syntax;

    public MacroSyntaxParseResult<TypeSyntax> ParseTypeResult()
        => ParseTypeResult(new TextSpan(0, BodySpan.Length));

    public MacroSyntaxParseResult<TypeSyntax> ParseTypeResult(TextSpan bodyRelativeSpan)
        => ParseSyntaxResult<TypeSyntax>(
            GetBodyText(),
            bodyRelativeSpan,
            static () => SyntaxFactory.ParseType(string.Empty));

    public PatternSyntax ParsePattern()
        => ParsePatternResult().Syntax;

    public PatternSyntax ParsePattern(TextSpan bodyRelativeSpan)
        => ParsePatternResult(bodyRelativeSpan).Syntax;

    public MacroSyntaxParseResult<PatternSyntax> ParsePatternResult()
        => ParsePatternResult(new TextSpan(0, BodySpan.Length));

    public MacroSyntaxParseResult<PatternSyntax> ParsePatternResult(TextSpan bodyRelativeSpan)
        => ParseSyntaxResult<PatternSyntax>(
            GetBodyText(),
            bodyRelativeSpan,
            static () => SyntaxFactory.ParsePattern(string.Empty));

    public CompilationUnitSyntax ParseCompilationUnit()
        => ParseCompilationUnitResult().Syntax;

    public CompilationUnitSyntax ParseCompilationUnit(TextSpan bodyRelativeSpan)
        => ParseCompilationUnitResult(bodyRelativeSpan).Syntax;

    public MacroSyntaxParseResult<CompilationUnitSyntax> ParseCompilationUnitResult()
        => ParseCompilationUnitResult(new TextSpan(0, BodySpan.Length));

    public MacroSyntaxParseResult<CompilationUnitSyntax> ParseCompilationUnitResult(TextSpan bodyRelativeSpan)
        => ParseSyntaxResult<CompilationUnitSyntax>(
            GetBodyText(),
            bodyRelativeSpan,
            static () => SyntaxFactory.ParseCompilationUnit(string.Empty));

    public MemberDeclarationSyntax ParseMemberDeclaration()
        => ParseMemberDeclarationResult().Syntax;

    public MemberDeclarationSyntax ParseMemberDeclaration(TextSpan bodyRelativeSpan)
        => ParseMemberDeclarationResult(bodyRelativeSpan).Syntax;

    public MacroSyntaxParseResult<MemberDeclarationSyntax> ParseMemberDeclarationResult()
        => ParseMemberDeclarationResult(new TextSpan(0, BodySpan.Length));

    public MacroSyntaxParseResult<MemberDeclarationSyntax> ParseMemberDeclarationResult(
        TextSpan bodyRelativeSpan)
    {
        var compilationUnitResult = ParseCompilationUnitResult(bodyRelativeSpan);
        var compilationUnit = compilationUnitResult.Syntax;
        var members = compilationUnit.Members;
        var isSingleMemberDeclaration =
            compilationUnit.Imports.Count == 0 &&
            compilationUnit.Aliases.Count == 0 &&
            compilationUnit.AttributeLists.Count == 0 &&
            members.Count == 1 &&
            members[0] is not GlobalStatementSyntax;

        if (isSingleMemberDeclaration)
        {
            return new MacroSyntaxParseResult<MemberDeclarationSyntax>(
                members[0],
                compilationUnitResult.Diagnostics);
        }

        var recoveredMember = members.FirstOrDefault(static member => member is not GlobalStatementSyntax)
            ?? SyntaxFactory.IncompleteMemberDeclaration(
                SyntaxList<AttributeListSyntax>.Empty,
                SyntaxTokenList.Empty,
                SyntaxFactory.MissingToken(SyntaxKind.None));
        var diagnosticLocation = GetSingleMemberDiagnosticLocation(
            compilationUnit,
            bodyRelativeSpan);
        var diagnostics = compilationUnitResult.Diagnostics.Add(Diagnostic.Create(
            s_expectedSingleMemberDeclaration,
            diagnosticLocation));

        return new MacroSyntaxParseResult<MemberDeclarationSyntax>(recoveredMember, diagnostics);
    }

    private MacroSyntaxParseResult<TSyntax> ParseSyntaxResult<TSyntax>(
        string bodyText,
        TextSpan bodyRelativeSpan,
        Func<TSyntax> createMissingSyntax)
        where TSyntax : SyntaxNode
    {
        if (bodyRelativeSpan.Start < 0 || bodyRelativeSpan.End > BodySpan.Length)
            throw new ArgumentOutOfRangeException(nameof(bodyRelativeSpan));

        var fragmentText = bodyText.Substring(bodyRelativeSpan.Start, bodyRelativeSpan.Length);
        var absoluteStart = BodySpan.Start + bodyRelativeSpan.Start;
        var sourceText = SourceText.From(new string(' ', absoluteStart) + fragmentText);
        var parser = new Syntax.InternalSyntax.Parser.LanguageParser(
            Syntax.SyntaxTree?.FilePath,
            Syntax.SyntaxTree?.Options ?? new ParseOptions());
        var parseResult = parser.ParseSyntaxWithDiagnostics(
            typeof(TSyntax),
            sourceText,
            absoluteStart,
            consumeFullText: true);
        var syntax = parseResult?.Root.CreateRed(parent: null, position: absoluteStart) as TSyntax
            ?? createMissingSyntax();
        var diagnostics = parseResult?.Diagnostics
            .Select(diagnostic => Diagnostic.Create(
                diagnostic.Descriptor,
                Syntax.SyntaxTree?.GetLocation(diagnostic.Span) ?? Location.None,
                diagnostic.Args))
            .ToImmutableArray()
            ?? ImmutableArray<Diagnostic>.Empty;

        return new MacroSyntaxParseResult<TSyntax>(syntax, diagnostics);
    }

    private Location GetSingleMemberDiagnosticLocation(
        CompilationUnitSyntax compilationUnit,
        TextSpan bodyRelativeSpan)
    {
        SyntaxNode? responsibleSyntax = compilationUnit.Imports.FirstOrDefault();
        responsibleSyntax ??= compilationUnit.Aliases.FirstOrDefault();
        responsibleSyntax ??= compilationUnit.AttributeLists.FirstOrDefault();

        if (responsibleSyntax is null && compilationUnit.Members.Count > 1)
            responsibleSyntax = compilationUnit.Members[1];

        responsibleSyntax ??= compilationUnit.Members.FirstOrDefault();
        if (responsibleSyntax is not null)
            return Syntax.SyntaxTree?.GetLocation(responsibleSyntax.Span) ?? Location.None;

        var sourceSpan = new TextSpan(
            BodySpan.Start + bodyRelativeSpan.Start,
            bodyRelativeSpan.Length);
        return Syntax.SyntaxTree?.GetLocation(sourceSpan) ?? Location.None;
    }

    internal MacroTokenClassification GetKeywordClassification(SyntaxToken token)
    {
        foreach (var keyword in _keywords)
        {
            if (keyword.RawKind != token.RawKind ||
                !string.Equals(keyword.Text, token.ValueText, StringComparison.Ordinal))
            {
                continue;
            }

            return keyword.Classification switch
            {
                MacroKeywordClassification.Keyword => MacroTokenClassification.Keyword,
                MacroKeywordClassification.ReservedWord => MacroTokenClassification.ReservedWord,
                _ => MacroTokenClassification.Default,
            };
        }

        return MacroTokenClassification.Default;
    }

    public MacroExpansionDiagnostic CreateDiagnostic(
        string message,
        DiagnosticSeverity severity = DiagnosticSeverity.Error,
        SyntaxNode? syntax = null,
        string? code = null)
        => new(severity, message, syntax?.GetLocation() ?? TokenTree.GetLocation(), code);

    public MacroExpansionDiagnostic CreateBodyDiagnostic(
        TextSpan bodyRelativeSpan,
        string message,
        DiagnosticSeverity severity = DiagnosticSeverity.Error,
        string? code = null)
    {
        if (bodyRelativeSpan.Start < 0 || bodyRelativeSpan.End > BodySpan.Length)
            throw new ArgumentOutOfRangeException(nameof(bodyRelativeSpan));

        var sourceSpan = new TextSpan(
            BodySpan.Start + bodyRelativeSpan.Start,
            bodyRelativeSpan.Length);
        var location = Syntax.SyntaxTree?.GetLocation(sourceSpan) ?? Location.None;
        return new MacroExpansionDiagnostic(severity, message, location, code);
    }

    internal MacroFileReadResult ReadFile(string path)
        => MacroFileReader.Read(Syntax, path, _fileDependencies);

    internal ImmutableArray<MacroFileDependency> GetFileDependencies()
        => _fileDependencies.ToImmutable();

    internal void AddFileDependencies(IEnumerable<MacroFileDependency> dependencies)
        => _fileDependencies.AddRange(dependencies);

    private static ImmutableArray<MacroArgument> CreateArguments(
        ArgumentListSyntax argumentList,
        SemanticModel semanticModel)
    {
        var builder = ImmutableArray.CreateBuilder<MacroArgument>(argumentList.Arguments.Count);
        foreach (var argument in argumentList.Arguments)
            builder.Add(new MacroArgument(argument, semanticModel));

        return builder.MoveToImmutable();
    }
}

public sealed class TokenTreeMacroContext<TParameters> : TokenTreeMacroContext
    where TParameters : class
{
    public TokenTreeMacroContext(
        Compilation compilation,
        SemanticModel semanticModel,
        FreestandingMacroExpressionSyntax syntax,
        TParameters parameters,
        CancellationToken cancellationToken = default)
        : base(compilation, semanticModel, syntax, cancellationToken)
    {
        Parameters = parameters ?? throw new ArgumentNullException(nameof(parameters));
    }

    internal TokenTreeMacroContext(
        Compilation compilation,
        SemanticModel semanticModel,
        FreestandingMacroExpressionSyntax syntax,
        ITokenTreeExpressionMacro macro,
        TParameters parameters,
        CancellationToken cancellationToken = default)
        : base(compilation, semanticModel, syntax, macro, cancellationToken)
    {
        Parameters = parameters ?? throw new ArgumentNullException(nameof(parameters));
    }

    public TParameters Parameters { get; }
}
