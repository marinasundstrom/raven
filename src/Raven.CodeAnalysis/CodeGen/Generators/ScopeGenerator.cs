using System.Reflection.Emit;

namespace Raven.CodeAnalysis.CodeGen;

class Scope : Generator
{
    private readonly IDictionary<ISymbol, LocalBuilder> _localBuilders = new Dictionary<ISymbol, LocalBuilder>(SymbolEqualityComparer.Default);

    public Scope(Generator parent) : base(parent)
    {

    }

    public override void AddLocal(ILocalSymbol localSymbol, LocalBuilder builder)
    {
        if (!_localBuilders.ContainsKey(localSymbol))
            _localBuilders.Add(localSymbol, builder);
    }

    public override LocalBuilder? GetLocal(ILocalSymbol localSymbol)
    {
        if (_localBuilders.TryGetValue(localSymbol, out var localBuilder))
            return localBuilder;

        return Parent?.GetLocal(localSymbol);
    }
}