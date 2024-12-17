namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

public partial class ArgumentSyntax : SyntaxNode
{
    public ArgumentSyntax(
        ExpressionSyntax expression)
        : base(SyntaxKind.Argument, [
            expression,
        ])
    {
    }
}