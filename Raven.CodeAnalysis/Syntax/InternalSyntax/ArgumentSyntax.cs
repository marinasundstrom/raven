namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal partial class ArgumentSyntax : SyntaxNode
{
    public ArgumentSyntax(
        ExpressionSyntax expression)
        : base(SyntaxKind.Argument, [
            expression,
        ])
    {
    }
}