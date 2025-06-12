namespace Raven.CodeAnalysis;

class MethodBinder : Binder
{
    public MethodBinder(Binder parent) : base(parent) { }

    private readonly Dictionary<string, IParameterSymbol> _parameters = new();

    public MethodBinder(Binder parent, IEnumerable<IParameterSymbol> parameters) : base(parent)
    {
        foreach (var param in parameters)
            _parameters[param.Name] = param;
    }

    public override ISymbol? LookupSymbol(string name)
    {
        if (_parameters.TryGetValue(name, out var sym))
            return sym;

        var parentSymbol = base.LookupSymbol(name);
        if (parentSymbol != null)
            return parentSymbol;

        return Compilation.GlobalNamespace.GetMembers(name).FirstOrDefault();
    }
}