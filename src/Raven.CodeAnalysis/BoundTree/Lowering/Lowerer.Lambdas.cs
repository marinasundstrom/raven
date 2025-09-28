using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer
{
    public override BoundNode? VisitLambdaExpression(BoundLambdaExpression node)
    {
        return LambdaLowerer.Rewrite(node, _containingSymbol);
    }
}
