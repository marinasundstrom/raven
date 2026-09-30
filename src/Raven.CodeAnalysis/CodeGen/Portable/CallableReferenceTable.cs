namespace Raven.CodeAnalysis.CodeGen.Portable;

// One table per emission. Compiler symbols are the portable identities; the handle
// type and external-reference resolution belong to the backend. Never key by name:
// overloads, owners and assembly identities must remain distinct.
internal sealed class CallableReferenceTable<THandle>(Func<IMethodSymbol, THandle> import)
    where THandle : class
{
    private readonly Dictionary<IMethodSymbol, THandle> _handles = new(SymbolEqualityComparer.Default);

    // Register definitions before emitting bodies, allowing forward and recursive calls.
    internal void Declare(IMethodSymbol symbol, THandle handle) => _handles.Add(symbol, handle);

    internal THandle Resolve(IMethodSymbol symbol)
    {
        if (_handles.TryGetValue(symbol, out var handle)) return handle;
        // Failed resolution must not poison a later attempt or hide its source diagnostic.
        handle = import(symbol);
        _handles.Add(symbol, handle);
        return handle;
    }
}
