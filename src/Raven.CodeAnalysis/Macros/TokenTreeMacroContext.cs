using System;
using System.Threading;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Macros;

public sealed class TokenTreeMacroContext
{
    public TokenTreeMacroContext(
        Compilation compilation,
        SemanticModel semanticModel,
        FreestandingMacroExpressionSyntax syntax,
        CancellationToken cancellationToken = default)
    {
        Compilation = compilation ?? throw new ArgumentNullException(nameof(compilation));
        SemanticModel = semanticModel ?? throw new ArgumentNullException(nameof(semanticModel));
        Syntax = syntax ?? throw new ArgumentNullException(nameof(syntax));
        TokenTree = syntax.TokenTree ?? throw new ArgumentException(
            "A token-tree macro context requires a token-tree invocation.",
            nameof(syntax));
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

    public ExpressionSyntax ParseExpression()
        => ParseExpression(new TextSpan(0, BodySpan.Length));

    public ExpressionSyntax ParseExpression(TextSpan bodyRelativeSpan)
    {
        if (bodyRelativeSpan.Start < 0 || bodyRelativeSpan.End > BodySpan.Length)
            throw new ArgumentOutOfRangeException(nameof(bodyRelativeSpan));

        var bodyText = GetBodyText();
        var fragmentText = bodyText.Substring(bodyRelativeSpan.Start, bodyRelativeSpan.Length);
        var absoluteStart = BodySpan.Start + bodyRelativeSpan.Start;
        var sourceText = SourceText.From(new string(' ', absoluteStart) + fragmentText);

        return SyntaxFactory.ParseExpression(
            sourceText,
            Syntax.SyntaxTree?.Options,
            absoluteStart);
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
