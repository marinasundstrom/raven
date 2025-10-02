using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer
{
    private static BoundBlockStatement RewriteIteratorsIfNeeded(ISymbol containingSymbol, BoundBlockStatement block)
    {
        if (containingSymbol is not SourceMethodSymbol method)
            return block;

        if (!IteratorLowerer.ShouldRewrite(method, block))
            return block;

        return IteratorLowerer.Rewrite(method, block);
    }
}
