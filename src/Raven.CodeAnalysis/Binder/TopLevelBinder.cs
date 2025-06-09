

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

class TopLevelBinder : BlockBinder
{
    public TopLevelBinder(Binder parent, SemanticModel semanticModel, IMethodSymbol methodSymbol) : base(methodSymbol, parent)
    {
        SemanticModel = semanticModel;
    }

    public override SemanticModel SemanticModel { get; }

    public IMethodSymbol MainMethod => (IMethodSymbol)ContainingSymbol;

    public void BindGlobalStatement(GlobalStatementSyntax stmt)
    {
        if (stmt.Statement is LocalFunctionStatementSyntax localFunction)
        {
            var localFuncBinder = SemanticModel.GetBinder(localFunction, this);
            if (localFuncBinder is LocalFunctionBinder lfBinder)
            {
                var symbol = lfBinder.GetMethodSymbol();
                DeclareLocalFunction(symbol);
            }
            BindStatement(localFunction);
            return;
        }

        BindStatement(stmt.Statement);
    }

    public override ISymbol? LookupSymbol(string name)
    {
        var paramSymbol = MainMethod.Parameters.FirstOrDefault(p => p.Name == name);
        if (paramSymbol is not null)
            return paramSymbol;

        var parentSymbol = base.LookupSymbol(name);
        if (parentSymbol != null)
            return parentSymbol;

        return Compilation.GlobalNamespace.GetMembers(name).FirstOrDefault();
    }

    public IEnumerable<IParameterSymbol> GetParameters()
    {
        return MainMethod.Parameters;
    }

    public void DeclareLocalFunction(IMethodSymbol symbol)
    {
        _localFunctions[symbol.Name] = symbol;
    }
}
