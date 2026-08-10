using System;
using System.Collections.Immutable;
using System.Threading;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Macros;

public class TokenTreeMacroContext : MacroContext
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
        : base(syntax ?? throw new ArgumentNullException(nameof(syntax)))
    {
        Compilation = compilation ?? throw new ArgumentNullException(nameof(compilation));
        SemanticModel = semanticModel ?? throw new ArgumentNullException(nameof(semanticModel));
        Syntax = syntax;
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

    public MacroTokenStream CreateTokenStream()
    {
        var context = new MacroTokenStreamContext(
            GetBodyText(),
            BodySpan,
            Syntax.SyntaxTree?.Options ?? new ParseOptions(),
            CancellationToken);

        var inner = _tokenStreamProvider is null
            ? new RavenMacroTokenStream(context, _keywords)
            : _tokenStreamProvider.CreateTokenStream(context)
                ?? throw new InvalidOperationException("The macro token-stream provider returned null.");
        return new MacroTokenStream(this, inner);
    }

    /// <summary>
    /// Creates an ordinary Raven fragment with no macro-introduced locals.
    /// </summary>
    public MacroFragmentRegion CreateFragmentRegion(
        MacroFragmentKind kind,
        TextSpan bodyRelativeSpan)
        => CreateFragmentRegion(kind, bodyRelativeSpan, ImmutableArray<MacroFragmentLocal>.Empty, targetType: null);

    /// <summary>
    /// Creates an ordinary Raven expression fragment with an expected target type.
    /// </summary>
    public MacroFragmentRegion CreateExpressionFragmentRegion(
        TextSpan bodyRelativeSpan,
        ITypeSymbol targetType)
    {
        ArgumentNullException.ThrowIfNull(targetType);
        return CreateFragmentRegion(
            MacroFragmentKind.Expression,
            bodyRelativeSpan,
            ImmutableArray<MacroFragmentLocal>.Empty,
            targetType);
    }

    /// <summary>
    /// Creates an ordinary Raven fragment and the macro-introduced locals visible inside it.
    /// </summary>
    public MacroFragmentRegion CreateFragmentRegion(
        MacroFragmentKind kind,
        TextSpan bodyRelativeSpan,
        ImmutableArray<MacroFragmentLocal> locals)
        => CreateFragmentRegion(kind, bodyRelativeSpan, locals, targetType: null);

    /// <summary>
    /// Creates an ordinary Raven fragment with macro-introduced locals and an
    /// optional expression target type.
    /// </summary>
    public MacroFragmentRegion CreateFragmentRegion(
        MacroFragmentKind kind,
        TextSpan bodyRelativeSpan,
        ImmutableArray<MacroFragmentLocal> locals,
        ITypeSymbol? targetType)
    {
        if (!Enum.IsDefined(kind))
            throw new ArgumentOutOfRangeException(nameof(kind));

        if (bodyRelativeSpan.Start < 0 || bodyRelativeSpan.End > BodySpan.Length)
            throw new ArgumentOutOfRangeException(nameof(bodyRelativeSpan));
        if (locals.IsDefault)
            throw new ArgumentException("Macro fragment locals must be initialized.", nameof(locals));
        if (locals.Any(static local => local is null))
            throw new ArgumentException("Macro fragment locals cannot contain null.", nameof(locals));
        if (locals.Select(static local => local.Name).Distinct(StringComparer.Ordinal).Count() != locals.Length)
            throw new ArgumentException("Macro fragment local names must be unique.", nameof(locals));
        if (targetType is not null && kind != MacroFragmentKind.Expression)
            throw new ArgumentException("Only expression fragments can have a target type.", nameof(targetType));

        return new MacroFragmentRegion(
            kind,
            bodyRelativeSpan,
            new TextSpan(
                BodySpan.Start + bodyRelativeSpan.Start,
                bodyRelativeSpan.Length),
            locals,
            targetType);
    }

    /// <summary>
    /// Creates a fragment local whose type is inferred from an authored sequence expression.
    /// </summary>
    public MacroFragmentLocal CreateSequenceElementLocal(
        string name,
        TextSpan sourceExpressionSpan)
        => CreateSequenceElementLocalCore(name, sourceExpressionSpan, declarationSpan: null);

    /// <summary>
    /// Creates a fragment local whose type is inferred from an authored sequence expression
    /// and whose declaration is identified by a body-relative DSL span.
    /// </summary>
    public MacroFragmentLocal CreateSequenceElementLocal(
        string name,
        TextSpan sourceExpressionSpan,
        TextSpan declarationSpan)
        => CreateSequenceElementLocalCore(name, sourceExpressionSpan, declarationSpan);

    private MacroFragmentLocal CreateSequenceElementLocalCore(
        string name,
        TextSpan sourceExpressionSpan,
        TextSpan? declarationSpan)
    {
        if (sourceExpressionSpan.Start < 0 || sourceExpressionSpan.End > BodySpan.Length)
            throw new ArgumentOutOfRangeException(nameof(sourceExpressionSpan));

        var sourceExpression = ParseExpression(sourceExpressionSpan);
        var sourceType = SemanticModel.GetMacroFragmentExpressionType(Syntax, sourceExpression);
        var elementType = sourceType is null
            ? null
            : SequenceTypeUtilities.TryGetElementType(Compilation, sourceType);
        return declarationSpan is { } declaredAt
            ? CreateFragmentLocal(name, elementType ?? Compilation.ErrorTypeSymbol, declaredAt)
            : CreateFragmentLocal(name, elementType ?? Compilation.ErrorTypeSymbol);
    }

    /// <summary>
    /// Creates an explicitly typed local that a macro can attach to selected fragments.
    /// </summary>
    public MacroFragmentLocal CreateFragmentLocal(string name, ITypeSymbol type)
        => CreateFragmentLocalCore(name, type, declarationSpan: null);

    /// <summary>
    /// Creates an explicitly typed local declared at a body-relative span in the macro DSL.
    /// </summary>
    public MacroFragmentLocal CreateFragmentLocal(
        string name,
        ITypeSymbol type,
        TextSpan declarationSpan)
        => CreateFragmentLocalCore(name, type, declarationSpan);

    private MacroFragmentLocal CreateFragmentLocalCore(
        string name,
        ITypeSymbol type,
        TextSpan? declarationSpan)
    {
        if (string.IsNullOrWhiteSpace(name))
            throw new ArgumentException("Macro fragment local names cannot be empty.", nameof(name));
        ArgumentNullException.ThrowIfNull(type);
        if (declarationSpan is { } span && (span.Start < 0 || span.End > BodySpan.Length))
            throw new ArgumentOutOfRangeException(nameof(declarationSpan));

        return new MacroFragmentLocal(
            name,
            type,
            declarationSpan,
            declarationSpan is { } declaredAt
                ? new TextSpan(BodySpan.Start + declaredAt.Start, declaredAt.Length)
                : null);
    }

    /// <summary>
    /// Creates editor metadata for a token read from this macro body's token stream.
    /// </summary>
    public MacroTokenInfo CreateTokenInfo(
        SyntaxToken token,
        string? kindName = null,
        MacroTokenClassification classification = MacroTokenClassification.Default,
        ISymbol? symbol = null)
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
            classification,
            symbol);
    }

    public ExpressionSyntax ParseExpression()
        => ParseExpressionResult().Syntax;

    public ExpressionSyntax ParseExpression(TextSpan bodyRelativeSpan)
        => ParseExpressionResult(bodyRelativeSpan).Syntax;

    /// <summary>
    /// Associates generated Raven syntax with an authored span in this macro's
    /// token-tree body for debugging.
    /// </summary>
    public TSyntax WithOrigin<TSyntax>(TSyntax syntax, TextSpan bodyRelativeSpan)
        where TSyntax : SyntaxNode
    {
        ArgumentNullException.ThrowIfNull(syntax);
        ValidateBodyRelativeSpan(bodyRelativeSpan);
        return MacroSyntaxOrigin.AttachBodyOrigin(
            syntax,
            Syntax.SyntaxTree,
            BodySpan,
            bodyRelativeSpan);
    }

    /// <summary>
    /// Associates spans in generated Raven syntax with authored spans in this
    /// macro's token-tree body. Each mapping is one-to-one so offsets within a
    /// mapped syntax node retain their authored positions.
    /// </summary>
    public TSyntax WithOrigins<TSyntax>(
        TSyntax syntax,
        ImmutableArray<MacroExpansionSourceMap> sourceMaps)
        where TSyntax : SyntaxNode
    {
        ArgumentNullException.ThrowIfNull(syntax);
        if (sourceMaps.IsDefault)
            throw new ArgumentException("Macro source maps must be initialized.", nameof(sourceMaps));

        foreach (var sourceMap in sourceMaps)
        {
            if (sourceMap.ExpandedSpan.Start < syntax.FullSpan.Start ||
                sourceMap.ExpandedSpan.End > syntax.FullSpan.End ||
                sourceMap.ExpandedSpan.Length != sourceMap.BodyRelativeSpan.Length)
            {
                throw new ArgumentOutOfRangeException(nameof(sourceMaps));
            }
            ValidateBodyRelativeSpan(sourceMap.BodyRelativeSpan);
        }

        return MacroSyntaxOrigin.AttachMappedOrigins(
            syntax,
            Syntax.SyntaxTree,
            BodySpan,
            sourceMaps);
    }

    public MacroSyntaxParseResult<ExpressionSyntax> ParseExpressionResult()
        => ParseExpressionResult(new TextSpan(0, BodySpan.Length));

    public MacroSyntaxParseResult<ExpressionSyntax> ParseExpressionResult(TextSpan bodyRelativeSpan)
        => ParseSyntaxResult<ExpressionSyntax>(
            GetBodyText(),
            bodyRelativeSpan,
            consumeFullText: true,
            static () => new ExpressionSyntax.Missing());

    /// <summary>
    /// Gets type information for a parsed Raven expression in the scope of
    /// this macro invocation.
    /// </summary>
    public TypeInfo GetTypeInfo(ExpressionSyntax expression)
    {
        ArgumentNullException.ThrowIfNull(expression);
        return SemanticModel.GetMacroFragmentTypeInfo(Syntax, expression);
    }

    /// <summary>
    /// Gets symbol information for a parsed Raven expression in the scope of
    /// this macro invocation.
    /// </summary>
    public SymbolInfo GetSymbolInfo(ExpressionSyntax expression)
    {
        ArgumentNullException.ThrowIfNull(expression);
        return SemanticModel.GetMacroFragmentSymbolInfo(Syntax, expression);
    }

    /// <summary>
    /// Parses one Raven expression at the token stream's current position and
    /// advances the stream through the parsed expression.
    /// </summary>
    internal MacroSyntaxParseResult<ExpressionSyntax> ParseExpression(
        IMacroTokenStream stream)
        => ParseSyntaxFromStream<ExpressionSyntax>(
            stream,
            static () => new ExpressionSyntax.Missing());

    internal MacroSyntaxParseResult<ExpressionSyntax> ParseExpressionResult(string bodyText)
    {
        ArgumentNullException.ThrowIfNull(bodyText);
        if (bodyText.Length != BodySpan.Length)
            throw new ArgumentException("Replacement body text must preserve the original body length.", nameof(bodyText));

        return ParseSyntaxResult<ExpressionSyntax>(
            bodyText,
            new TextSpan(0, bodyText.Length),
            consumeFullText: true,
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
            consumeFullText: true,
            static () => SyntaxFactory.ExpressionStatement(new ExpressionSyntax.Missing()));

    /// <summary>
    /// Parses one Raven statement at the token stream's current position and
    /// advances the stream through the parsed statement.
    /// </summary>
    internal MacroSyntaxParseResult<StatementSyntax> ParseStatement(
        IMacroTokenStream stream)
        => ParseSyntaxFromStream<StatementSyntax>(
            stream,
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
            consumeFullText: true,
            static () => SyntaxFactory.ParseType(string.Empty));

    /// <summary>
    /// Parses one Raven type at the token stream's current position and
    /// advances the stream through the parsed type.
    /// </summary>
    internal MacroSyntaxParseResult<TypeSyntax> ParseType(
        IMacroTokenStream stream)
        => ParseSyntaxFromStream<TypeSyntax>(
            stream,
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
            consumeFullText: true,
            static () => SyntaxFactory.ParsePattern(string.Empty));

    /// <summary>
    /// Parses one Raven pattern at the token stream's current position and
    /// advances the stream through the parsed pattern.
    /// </summary>
    internal MacroSyntaxParseResult<PatternSyntax> ParsePattern(
        IMacroTokenStream stream)
        => ParseSyntaxFromStream<PatternSyntax>(
            stream,
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
            consumeFullText: true,
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
            var member = members[0];
            var memberSpan = GetBodyRelativeSpan(member);
            return new MacroSyntaxParseResult<MemberDeclarationSyntax>(
                member,
                memberSpan,
                memberSpan.End,
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
        var recoveredSpan = GetBodyRelativeSpan(recoveredMember);

        return new MacroSyntaxParseResult<MemberDeclarationSyntax>(
            recoveredMember,
            recoveredSpan,
            recoveredSpan.End,
            diagnostics);
    }

    /// <summary>
    /// Parses one Raven member declaration at the token stream's current
    /// position and advances through the declaration.
    /// </summary>
    internal MacroSyntaxParseResult<MemberDeclarationSyntax> ParseMemberDeclaration(
        IMacroTokenStream stream)
        => ParseSyntaxFromStream<MemberDeclarationSyntax>(
            stream,
            static () => SyntaxFactory.IncompleteMemberDeclaration(
                SyntaxList<AttributeListSyntax>.Empty,
                SyntaxTokenList.Empty,
                SyntaxFactory.MissingToken(SyntaxKind.None)));

    private MacroSyntaxParseResult<TSyntax> ParseSyntaxResult<TSyntax>(
        string bodyText,
        TextSpan bodyRelativeSpan,
        bool consumeFullText,
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
            consumeFullText);
        var syntax = parseResult?.Root.CreateRed(parent: null, position: absoluteStart) as TSyntax
            ?? createMissingSyntax();
        syntax = MacroSyntaxOrigin.AttachParsedOrigin(syntax, Syntax.SyntaxTree);
        var diagnostics = parseResult?.Diagnostics
            .Select(diagnostic => Diagnostic.Create(
                diagnostic.Descriptor,
                Syntax.SyntaxTree?.GetLocation(diagnostic.Span) ?? Location.None,
                diagnostic.Args))
            .ToImmutableArray()
            ?? ImmutableArray<Diagnostic>.Empty;

        return new MacroSyntaxParseResult<TSyntax>(
            syntax,
            parseResult is null
                ? new TextSpan(bodyRelativeSpan.Start, 0)
                : GetBodyRelativeSpan(syntax),
            parseResult is null
                ? bodyRelativeSpan.Start
                : Math.Clamp(
                    (parseResult.Value.Diagnostics.Any(static diagnostic => diagnostic.Descriptor.DefaultSeverity == DiagnosticSeverity.Error)
                        ? parseResult.Value.ConsumedPosition
                        : syntax.Span.End) - BodySpan.Start,
                    bodyRelativeSpan.Start,
                    bodyRelativeSpan.End),
            diagnostics);
    }

    private MacroSyntaxParseResult<TSyntax> ParseSyntaxFromStream<TSyntax>(
        IMacroTokenStream stream,
        Func<TSyntax> createMissingSyntax)
        where TSyntax : SyntaxNode
    {
        ArgumentNullException.ThrowIfNull(stream);

        var start = stream.IsEndOfFile
            ? BodySpan.Length
            : stream.PeekToken().SpanStart;
        var result = ParseSyntaxResult<TSyntax>(
            GetBodyText(),
            GetRemainingBodySpan(start),
            consumeFullText: false,
            createMissingSyntax);
        AdvanceStreamThrough(stream, result.ConsumedBodyRelativeEnd);
        return result;
    }

    private static void AdvanceStreamThrough(IMacroTokenStream stream, int bodyRelativeEnd)
    {
        while (!stream.IsEndOfFile && stream.PeekToken().SpanStart < bodyRelativeEnd)
            stream.ReadToken();
    }

    private TextSpan GetRemainingBodySpan(int bodyRelativeStart)
    {
        if (bodyRelativeStart < 0 || bodyRelativeStart > BodySpan.Length)
            throw new ArgumentOutOfRangeException(nameof(bodyRelativeStart));

        return new TextSpan(bodyRelativeStart, BodySpan.Length - bodyRelativeStart);
    }

    private TextSpan GetBodyRelativeSpan(SyntaxNode syntax)
    {
        var start = Math.Clamp(syntax.Span.Start - BodySpan.Start, 0, BodySpan.Length);
        var end = Math.Clamp(syntax.Span.End - BodySpan.Start, start, BodySpan.Length);
        return TextSpan.FromBounds(start, end);
    }

    private void ValidateBodyRelativeSpan(TextSpan bodyRelativeSpan)
    {
        if (bodyRelativeSpan.Start < 0 || bodyRelativeSpan.End > BodySpan.Length)
            throw new ArgumentOutOfRangeException(nameof(bodyRelativeSpan));
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

    public override MacroExpansionDiagnostic CreateDiagnostic(
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
