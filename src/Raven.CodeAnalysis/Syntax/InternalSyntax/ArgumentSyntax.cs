namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class ArgumentSyntax : SyntaxNode
{
    public ArgumentSyntax(
        ExpressionSyntax expression,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(SyntaxKind.Argument, [
            null,
            expression ?? throw new ArgumentNullException(nameof(expression))
    ],
    diagnostics)
    {
    }

    public ArgumentSyntax(
        NameColonSyntax? nameColon,
        ExpressionSyntax expression,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(SyntaxKind.Argument, [
            nameColon!,
            expression ?? throw new ArgumentNullException(nameof(expression))
        ],
        diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static ArgumentSyntax Argument(
        ExpressionSyntax expression,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(expression, diagnostics);

    public static ArgumentSyntax Argument(
        NameColonSyntax? nameColon,
        ExpressionSyntax expression,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        => new(nameColon, expression, diagnostics);
}