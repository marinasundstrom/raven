using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

/// <summary>
/// Runs the ordered Release optimization pipeline after semantic lowering.
/// </summary>
internal static class BoundTreeOptimizer
{
    public static T Optimize<T>(ISymbol containingSymbol, T node)
        where T : BoundNode
    {
        if (containingSymbol.ContainingAssembly is not SourceAssemblySymbol sourceAssembly ||
            sourceAssembly.Compilation.Options.OptimizationLevel != OptimizationLevel.Release)
        {
            return node;
        }

        return (T)PatternOptimizer.Rewrite(node);
    }
}
