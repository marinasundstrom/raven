using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

class MethodBinder : Binder
{
    private readonly IMethodSymbol _methodSymbol;

    public MethodBinder(IMethodSymbol methodSymbol, Binder parent) : base(parent)
    {
        _methodSymbol = methodSymbol;
    }

    public MethodBinder(IMethodSymbol methodSymbol, Binder parent, IEnumerable<IParameterSymbol> parameters)
        : this(methodSymbol, parent)
    {
        // Parameters are retrieved from _methodSymbol; no additional storage needed
    }

    public override ISymbol? LookupSymbol(string name)
    {
        var paramSymbol = _methodSymbol.Parameters.FirstOrDefault(p => p.Name == name);
        if (paramSymbol is not null)
            return paramSymbol;

        var parentSymbol = base.LookupSymbol(name);
        if (parentSymbol != null)
            return parentSymbol;

        return Compilation.GlobalNamespace.GetMembers(name).FirstOrDefault();
    }

    public IMethodSymbol GetMethodSymbol() => _methodSymbol;
}