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
            // ClassDeclarationSyntax binders are created and cached by SemanticModel
            ClassDeclarationSyntax => parentBinder,
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
        var nsSymbol = _compilation.GetNamespaceSymbol(nsSyntax.Name.ToString());
        var nsBinder = new NamespaceBinder(parentBinder, nsSymbol!);

        foreach (var importDirective in nsSyntax.Imports)
        {
            var importName = importDirective.Name.ToString();

            if (IsWildcard(importDirective.Name, out var nsName))
            {
                var nsImport = ResolveNamespace(nsSymbol!, nsName.ToString());
                if (nsImport != null)
                    nsBinder.AddUsingDirective(nsImport);
                continue;
            }

            var typeSymbol = ResolveType(nsSymbol!, importName);
            if (typeSymbol != null)
            {
                var alias = importDirective.Alias?.Name.Identifier.Text ?? GetRightmostIdentifier(importDirective.Name);
                nsBinder.AddTypeImport(alias, typeSymbol);
                continue;
            }

            var namespaceSymbol = ResolveNamespace(nsSymbol!, importName);
            if (namespaceSymbol != null)
                nsBinder.AddUsingDirective(namespaceSymbol);
        }

        return nsBinder;

        INamespaceSymbol? ResolveNamespace(INamespaceSymbol current, string name)
        {
            var full = Combine(current, name);
            return _compilation.GetNamespaceSymbol(full) ?? _compilation.GetNamespaceSymbol(name);
        }

        ITypeSymbol? ResolveType(INamespaceSymbol current, string name)
        {
            var full = Combine(current, name);
            return _compilation.GetTypeByMetadataName(full) ?? _compilation.GetTypeByMetadataName(name);
        }

        static string Combine(INamespaceSymbol current, string name)
        {
            var currentName = current.ToString();
            return string.IsNullOrEmpty(currentName) ? name : currentName + "." + name;
        }

        static string GetRightmostIdentifier(NameSyntax nameSyntax)
        {
            return nameSyntax switch
            {
                IdentifierNameSyntax id => id.Identifier.Text,
                QualifiedNameSyntax qn => GetRightmostIdentifier(qn.Right),
                _ => nameSyntax.ToString()
            };
        }

        static bool IsWildcard(NameSyntax nameSyntax, out NameSyntax baseName)
        {
            if (nameSyntax is QualifiedNameSyntax { Right: WildcardNameSyntax, Left: var left })
            {
                baseName = left;
                return true;
            }

            baseName = default!;
            return false;
        }
    }
}
