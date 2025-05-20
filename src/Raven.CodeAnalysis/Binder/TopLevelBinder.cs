
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

class TopLevelBinder : BlockBinder
{
    public TopLevelBinder(Binder parent, IMethodSymbol methodSymbol) : base(methodSymbol, parent) { }

    public void BindGlobalStatement(GlobalStatementSyntax stmt)
    {
        BindStatement(stmt.Statement);
    }
}
