

using System.Collections.Generic;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

class TopLevelBinder : BlockBinder
{
    private readonly CompilationUnitSyntax _compilationUnit;
    private readonly SourceMethodSymbol _scriptMethod;

    public TopLevelBinder(
        Binder parent,
        SemanticModel semanticModel,
        SourceMethodSymbol scriptMethod,
        SynthesizedMainMethodSymbol entryPointMethod,
        CompilationUnitSyntax compilationUnit)
        : base(scriptMethod, parent)
    {
        SemanticModel = semanticModel;
        _compilationUnit = compilationUnit;
        _scriptMethod = scriptMethod;
        EntryPointMethod = entryPointMethod;
    }

    public override SemanticModel SemanticModel { get; }

    public SynthesizedMainMethodSymbol EntryPointMethod { get; }

    public SourceMethodSymbol ScriptMethod => _scriptMethod;

    public IMethodSymbol MainMethod => _scriptMethod;

    public void BindGlobalStatements(IEnumerable<GlobalStatementSyntax> statements)
    {
        var globalStatements = new List<GlobalStatementSyntax>();
        foreach (var stmt in statements)
            globalStatements.Add(stmt);

        // Declare all functions first so they are available to subsequent statements
        foreach (var stmt in globalStatements)
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
        var boundStatements = new List<BoundStatement>(globalStatements.Count);

        foreach (var stmt in globalStatements)
        {
            if (stmt.Statement is FunctionStatementSyntax function)
                boundStatements.Add(BindStatement(function));
            else
                boundStatements.Add(BindStatement(stmt.Statement));
        }

        var localsToDispose = ImmutableArray.CreateBuilder<ILocalSymbol>();
        foreach (var stmt in globalStatements)
        {
            if (stmt.Statement is UsingDeclarationStatementSyntax usingDeclaration)
            {
                foreach (var declarator in usingDeclaration.Declaration.Declarators)
                {
                    if (SemanticModel.GetDeclaredSymbol(declarator) is ILocalSymbol local)
                        localsToDispose.Add(local);
                }
            }
        }

        var block = new BoundBlockStatement(boundStatements, localsToDispose.ToImmutable());
        CacheBoundNode(_compilationUnit, block);
    }

    public override ISymbol? LookupSymbol(string name)
    {
        var paramSymbol = _scriptMethod.Parameters.FirstOrDefault(p => p.Name == name);
        if (paramSymbol is not null)
            return paramSymbol;

        var parentSymbol = base.LookupSymbol(name);
        if (parentSymbol != null)
            return parentSymbol;

        var currentNamespace = CurrentNamespace;
        if (currentNamespace is not null)
        {
            var namespaceMember = currentNamespace.GetMembers(name).FirstOrDefault();
            if (namespaceMember is not null)
                return namespaceMember;
        }

        return Compilation.GlobalNamespace.GetMembers(name).FirstOrDefault();
    }

    public IEnumerable<IParameterSymbol> GetParameters()
    {
        return _scriptMethod.Parameters;
    }

    public void DeclareFunction(IMethodSymbol symbol)
    {
        _functions[symbol.Name] = symbol;
    }
}