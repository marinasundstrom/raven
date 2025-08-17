
namespace Raven.CodeAnalysis;

class MethodBinder : BlockBinder
{
    private readonly Dictionary<string, IParameterSymbol> _parameters = new();
    private readonly IMethodSymbol _methodSymbol;

    public MethodBinder(IMethodSymbol methodSymbol, Binder parent) : base(methodSymbol, parent)
    {
        _methodSymbol = methodSymbol;
    }

    public MethodBinder(IMethodSymbol methodSymbol, Binder parent, IEnumerable<IParameterSymbol> parameters) : base(methodSymbol, parent)
    {
        _methodSymbol = methodSymbol;

        foreach (var param in parameters)
            _parameters[param.Name] = param;
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