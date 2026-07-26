using System;
using System.Collections.Immutable;
using System.Threading;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Macros;

public sealed class TokenTreeMacroContext
{
    private readonly IMacroTokenStreamProvider? _tokenStreamProvider;
    private readonly ImmutableArray<MacroKeyword> _keywords;

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
        _tokenStreamProvider = tokenStreamProvider;
        _keywords = keywords.IsDefault ? ImmutableArray<MacroKeyword>.Empty : keywords;
        CancellationToken = cancellationToken;
    }

    public Compilation Compilation { get; }

    public SemanticModel SemanticModel { get; }

    public FreestandingMacroExpressionSyntax Syntax { get; }

    public MacroTokenTreeSyntax TokenTree { get; }

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

    public ExpressionSyntax ParseExpression()
        => ParseExpressionResult().Syntax;

    public ExpressionSyntax ParseExpression(TextSpan bodyRelativeSpan)
        => ParseExpressionResult(bodyRelativeSpan).Syntax;

    public MacroSyntaxParseResult<ExpressionSyntax> ParseExpressionResult()
        => ParseExpressionResult(new TextSpan(0, BodySpan.Length));

    public MacroSyntaxParseResult<ExpressionSyntax> ParseExpressionResult(TextSpan bodyRelativeSpan)
    {
        if (bodyRelativeSpan.Start < 0 || bodyRelativeSpan.End > BodySpan.Length)
            throw new ArgumentOutOfRangeException(nameof(bodyRelativeSpan));

        var bodyText = GetBodyText();
        var fragmentText = bodyText.Substring(bodyRelativeSpan.Start, bodyRelativeSpan.Length);
        var absoluteStart = BodySpan.Start + bodyRelativeSpan.Start;
        var sourceText = SourceText.From(new string(' ', absoluteStart) + fragmentText);
        var parser = new Syntax.InternalSyntax.Parser.LanguageParser(
            Syntax.SyntaxTree?.FilePath,
            Syntax.SyntaxTree?.Options ?? new ParseOptions());
        var parseResult = parser.ParseSyntaxWithDiagnostics(
            typeof(ExpressionSyntax),
            sourceText,
            absoluteStart,
            consumeFullText: true);
        var expression = parseResult?.Root.CreateRed() as ExpressionSyntax
            ?? new ExpressionSyntax.Missing();
        var diagnostics = parseResult?.Diagnostics
            .Select(diagnostic => Diagnostic.Create(
                diagnostic.Descriptor,
                Syntax.SyntaxTree?.GetLocation(diagnostic.Span) ?? Location.None,
                diagnostic.Args))
            .ToImmutableArray()
            ?? ImmutableArray<Diagnostic>.Empty;

        return new MacroSyntaxParseResult<ExpressionSyntax>(expression, diagnostics);
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
}
