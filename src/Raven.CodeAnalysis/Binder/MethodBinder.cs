using Raven.CodeAnalysis.Syntax;

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

    public override SymbolInfo BindSymbol(SyntaxNode node)
    {
        if (node is IdentifierNameSyntax identifier)
        {
            if (_parameters.TryGetValue(identifier.Identifier.Text, out var param))
                return new SymbolInfo(param);
        }
        return base.BindSymbol(node);
    }
}
