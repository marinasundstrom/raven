using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

class BinderFactory
{
    private readonly Compilation _compilation;

    public BinderFactory(Compilation compilation)
    {
        _compilation = compilation;
    }

    public Binder? GetBinder(SyntaxNode node, Binder? parentBinder = null)
    {
        // Now safely construct this node's binder with a guaranteed parent
        Binder? newBinder = node switch
        {
            NamespaceDeclarationSyntax ns => CreateNamespaceBinder(ns, parentBinder!),
            MethodDeclarationSyntax => new MethodBinder(parentBinder!),
            BlockSyntax => parentBinder is MethodBinder ? new BlockBinder((parentBinder?.ParentBinder as LocalFunctionBinder)?.GetMethodSymbol()!, parentBinder!) : new LocalScopeBinder(parentBinder!),
            IfExpressionSyntax expr => new LocalScopeBinder(parentBinder!),
            ElseClauseSyntax elseClause => new LocalScopeBinder(parentBinder!),
            WhileExpressionSyntax expr => new LocalScopeBinder(parentBinder!),
            LocalFunctionStatementSyntax localFunc => new LocalFunctionBinder(parentBinder!, localFunc),
            _ => parentBinder
        };

        return newBinder;
    }

    private Binder CreateNamespaceBinder(NamespaceDeclarationSyntax nsSyntax, Binder parentBinder)
    {
        var nsSymbol = _compilation.GlobalNamespace.LookupNamespace(nsSyntax.Name.ToString());
        var nsBinder = new NamespaceBinder(parentBinder, nsSymbol!, _compilation);

        // Register `import` directives
        foreach (var importDirective in nsSyntax.Imports)
        {
            var importedNamespace = _compilation.GlobalNamespace.LookupNamespace(importDirective.NamespaceOrType.ToString());
            if (importedNamespace != null)
                nsBinder.AddUsingDirective(importedNamespace);
        }

        return nsBinder;
    }
}
