namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class NameColonSyntax : SyntaxNode
{
    public NameColonSyntax(
        IdentifierNameSyntax name,
        //ExpressionSyntax expression,
        SyntaxToken colonToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(SyntaxKind.NameColon, [
            name,
            //expression ?? throw new ArgumentException(nameof(expression))
            colonToken,
        ],
        diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static NameColonSyntax NameColon(
        IdentifierNameSyntax name,
        //ExpressionSyntax expression,
        SyntaxToken colonToken,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(name, colonToken, diagnostics);
}