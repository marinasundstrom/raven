using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer
{
    private static BoundBlockStatement RewriteIteratorsIfNeeded(ISymbol containingSymbol, BoundBlockStatement block)
    {
        return containingSymbol switch
        {
            SourceMethodSymbol method when IteratorLowerer.ShouldRewrite(method, block) => IteratorLowerer.Rewrite(method, block),
            SourceLambdaSymbol lambda when IteratorLowerer.ShouldRewrite(lambda, block) => IteratorLowerer.Rewrite(lambda, block),
            _ => block,
        };
    }
}
