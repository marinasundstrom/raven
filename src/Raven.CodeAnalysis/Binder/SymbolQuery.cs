using System.Collections.Generic;
using System.Linq;

namespace Raven.CodeAnalysis;

internal readonly record struct SymbolQuery(
    string Name,
    ITypeSymbol? ContainingType = null,
    int? Arity = null,
    bool? IsStatic = null)
{
    public IEnumerable<ISymbol> Lookup(Binder binder)
    {
        IEnumerable<ISymbol> symbols;
        if (ContainingType is not null)
        {
            symbols = IsStatic == true
                ? ContainingType.GetMembers(Name)
                : ContainingType.ResolveMembers(Name);
        }
        else
        {
            symbols = binder.LookupSymbols(Name);
        }

        var isStatic = IsStatic;
        var arity = Arity;

        if (isStatic.HasValue)
            symbols = symbols.Where(s => s.IsStatic == isStatic.Value);

        if (arity.HasValue)
            symbols = symbols.Where(s => s is IMethodSymbol m && m.Parameters.Length == arity.Value);

        return symbols;
    }

    public IEnumerable<IMethodSymbol> LookupMethods(Binder binder) =>
        Lookup(binder).OfType<IMethodSymbol>();
}
