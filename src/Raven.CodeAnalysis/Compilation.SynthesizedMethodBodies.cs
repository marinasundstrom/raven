using System.Collections.Concurrent;

namespace Raven.CodeAnalysis;

public partial class Compilation
{
    private readonly ConcurrentDictionary<IMethodSymbol, BoundBlockStatement> _synthesizedMethodBodies =
        new(SymbolEqualityComparer.Default);
    private readonly ConcurrentDictionary<IMethodSymbol, BoundBlockStatement> _loweredSynthesizedMethodBodies =
        new(SymbolEqualityComparer.Default);

    internal bool TryGetSynthesizedMethodBody(
        IMethodSymbol method,
        BoundTreeView view,
        out BoundBlockStatement? body)
    {
        body = null;

        if (method is null || !method.DeclaringSyntaxReferences.IsDefaultOrEmpty)
            return false;

        var cache = view == BoundTreeView.Lowered
            ? _loweredSynthesizedMethodBodies
            : _synthesizedMethodBodies;

        if (cache.TryGetValue(method, out body))
            return body is not null;

        if (!_synthesizedMethodBodies.TryGetValue(method, out var originalBody))
        {
            if (!SynthesizedMethodBodyFactory.TryCreate(this, method, out originalBody))
                return false;

            _synthesizedMethodBodies[method] = originalBody;
        }

        if (view == BoundTreeView.Original)
        {
            body = originalBody;
            return true;
        }

        body = _loweredSynthesizedMethodBodies.GetOrAdd(method, static (m, original) => Lowerer.LowerBlock(m, original), originalBody);
        return true;
    }
}
