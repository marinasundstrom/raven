

using Raven.CodeAnalysis.Syntax;
using System.Collections.Generic;

namespace Raven.CodeAnalysis;

class TopLevelBinder : BlockBinder
{
    public TopLevelBinder(Binder parent, SemanticModel semanticModel, IMethodSymbol methodSymbol) : base(methodSymbol, parent)
    {
        SemanticModel = semanticModel;
    }

    public override SemanticModel SemanticModel { get; }

    public IMethodSymbol MainMethod => (IMethodSymbol)ContainingSymbol;

    public void BindGlobalStatements(IEnumerable<GlobalStatementSyntax> statements)
    {
        // Declare all functions first so they are available to subsequent statements
        foreach (var stmt in statements)
        {
            if (stmt.Statement is FunctionStatementSyntax localFunc)
            {
                var binder = SemanticModel.GetBinder(localFunc, this);
                if (binder is FunctionBinder lfBinder)
                {
                    var symbol = lfBinder.GetMethodSymbol();
                    if (_functions.TryGetValue(symbol.Name, out var existing) && HaveSameSignature(existing, symbol))
                        _diagnostics.ReportFunctionAlreadyDefined(symbol.Name, localFunc.Identifier.GetLocation());
                    else
                        DeclareFunction(symbol);
                }
            }
        }

        // Bind each statement
        foreach (var stmt in statements)
        {
            if (stmt.Statement is FunctionStatementSyntax function)
                BindStatement(function);
            else
                BindStatement(stmt.Statement);
        }
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

    public void DeclareFunction(IMethodSymbol symbol)
    {
        _functions[symbol.Name] = symbol;
    }
}