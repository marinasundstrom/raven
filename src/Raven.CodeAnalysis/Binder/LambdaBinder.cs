namespace Raven.CodeAnalysis;

class LambdaBinder : BlockBinder
{
    private readonly Dictionary<string, IParameterSymbol> _parameters = new();

    public LambdaBinder(ISymbol containingSymbol, Binder parent) : base(containingSymbol, parent) { }

    public void DeclareParameter(IParameterSymbol param)
    {
        _parameters[param.Name] = param;
    }

    public override ISymbol? LookupSymbol(string name)
    {
        if (_parameters.TryGetValue(name, out var param))
            return param;

        return base.LookupSymbol(name);
    }
}