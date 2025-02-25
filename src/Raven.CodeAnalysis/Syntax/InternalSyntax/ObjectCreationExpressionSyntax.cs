namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class ObjectCreationExpressionSyntax : ExpressionSyntax
{
    public ObjectCreationExpressionSyntax(
        SyntaxToken newKeyword,
        TypeSyntax type,
        ArgumentListSyntax argumentList,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(
              SyntaxKind.ObjectCreationExpression,
              [
                      newKeyword ?? throw new ArgumentNullException(nameof(newKeyword)),
                      type,
                      argumentList ?? throw new ArgumentNullException(nameof(argumentList))
              ],
              diagnostics)
    {
    }
}

internal static partial class SyntaxFactory
{
    public static ObjectCreationExpressionSyntax ObjectCreationExpression(
        SyntaxToken newKeyword,
        TypeSyntax type,
        ArgumentListSyntax argumentList,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
      => new(newKeyword, type, argumentList, diagnostics);
}