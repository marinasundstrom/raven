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
            MethodDeclarationSyntax => parentBinder,
            BlockSyntax => CreateBlockBinder(parentBinder),
            IfExpressionSyntax expr => new LocalScopeBinder(parentBinder!),
            ElseClauseSyntax elseClause => new LocalScopeBinder(parentBinder!),
            WhileExpressionSyntax expr => new LocalScopeBinder(parentBinder!),
            LocalFunctionStatementSyntax localFunc => new LocalFunctionBinder(parentBinder!, localFunc),
            ClassDeclarationSyntax classDecl => new TypeDeclarationBinder(parentBinder!, null), //, GetTypeSymbolFor(classDecl)),
            //FieldDeclarationSyntax => parent, // Fields are handled during symbol declaration
            _ => parentBinder
        };

        return newBinder;
    }

    private Binder? CreateBlockBinder(Binder? parentBinder)
    {
        // A block directly under a method requires a MethodBodyBinder so the method body
        // can bind parameters and local declarations. Nested blocks receive a
        // LocalScopeBinder to track their own local scope.
        return parentBinder is MethodBinder
            ? new MethodBodyBinder(GetMethodSymbol(parentBinder)!, parentBinder!)
            : new LocalScopeBinder(parentBinder!);
    }

    private static IMethodSymbol? GetMethodSymbol(Binder? parentBinder)
    {
        if (parentBinder is MethodBinder methodBinder)
            return methodBinder?.GetMethodSymbol()!;

        if (parentBinder is LocalFunctionBinder localFunctionBinder)
            return localFunctionBinder?.GetMethodSymbol()!;

        return null;
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
