using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer
{
    public override BoundNode? VisitFunctionExpression(BoundFunctionExpression node)
    {
        return FunctionExpressionLowerer.Rewrite(node, _containingSymbol);
    }
}
