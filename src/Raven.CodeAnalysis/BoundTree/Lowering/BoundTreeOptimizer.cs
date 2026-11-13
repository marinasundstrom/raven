namespace Raven.CodeAnalysis;

/// <summary>
/// Runs the ordered Release optimization pipeline after semantic lowering.
/// </summary>
internal static class BoundTreeOptimizer
{
    public static T Optimize<T>(Symbol containingSymbol, T node)
        where T : BoundNode
    {
        if (containingSymbol.ContainingAssembly is not BoundTreeOptimizationProvider provider ||
            !provider.IsReleaseOptimizationEnabled)
        {
            return node;
        }

        var optimized = PatternOptimizer.Rewrite(node);
        optimized = BooleanExpressionOptimizer.Rewrite(provider.OperatorTypes, optimized);
        optimized = ControlFlowOptimizer.Rewrite(optimized);
        return (T)optimized;
    }
}
