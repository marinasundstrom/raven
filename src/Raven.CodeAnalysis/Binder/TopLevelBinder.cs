
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

class TopLevelBinder : BlockBinder
{
    public TopLevelBinder(Binder parent, IMethodSymbol methodSymbol) : base(methodSymbol, parent) { }
    
    public IMethodSymbol MainMethod => (IMethodSymbol)ContainingSymbol;

    public void BindGlobalStatement(GlobalStatementSyntax stmt)
    {
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
}
