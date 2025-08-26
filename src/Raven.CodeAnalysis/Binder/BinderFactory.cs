using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

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
            ArrowExpressionClauseSyntax => CreateBlockBinder(parentBinder),
            IfExpressionSyntax expr => new LocalScopeBinder(parentBinder!),
            ElseClauseSyntax elseClause => new LocalScopeBinder(parentBinder!),
            WhileExpressionSyntax expr => new LocalScopeBinder(parentBinder!),
            ForExpressionSyntax expr => new BlockBinder(parentBinder!.ContainingSymbol, parentBinder!),
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

        var namespaceImports = new List<INamespaceOrTypeSymbol>();
        var typeImports = new List<ITypeSymbol>();
        var aliases = new Dictionary<string, IReadOnlyList<IAliasSymbol>>();

        var provisionalImportBinder = new ImportBinder(nsBinder, namespaceImports, typeImports, aliases);

        foreach (var importDirective in nsSyntax.Imports)
        {
            var importName = importDirective.Name.ToString();

            if (IsWildcard(importDirective.Name, out var nsName))
            {
                INamespaceOrTypeSymbol? nsImport =
                    (INamespaceOrTypeSymbol?)ResolveNamespace(nsSymbol!, nsName.ToString())
                    ?? ResolveType(nsSymbol!, nsName.ToString());
                if (nsImport != null)
                    namespaceImports.Add(nsImport);
                continue;
            }

            var namespaceSymbol = ResolveNamespace(nsSymbol!, importName);
            if (namespaceSymbol != null)
            {
                nsBinder.Diagnostics.ReportTypeExpectedWithoutWildcard(importDirective.Name.GetLocation());
                continue;
            }

            ITypeSymbol? typeSymbol = HasTypeArguments(importDirective.Name)
                ? ResolveOpenGenericType(nsSymbol!, importDirective.Name)
                : ResolveType(nsSymbol!, importName);

            if (typeSymbol != null)
                typeImports.Add(typeSymbol);
        }

        foreach (var aliasDirective in nsSyntax.Aliases)
        {
            IReadOnlyList<ISymbol> symbols;
            if (aliasDirective.Target is NameSyntax name)
            {
                symbols = ResolveAlias(nsSymbol!, name);
            }
            else
            {
                var typeSymbol = provisionalImportBinder.ResolveType(aliasDirective.Target);
                symbols = typeSymbol == _compilation.ErrorTypeSymbol
                    ? Array.Empty<ISymbol>()
                    : new ISymbol[] { typeSymbol };
            }

            if (symbols.Count > 0)
            {
                var aliasSymbols = symbols
                    .Select(s => AliasSymbolFactory.Create(aliasDirective.Identifier.Text, s))
                    .ToArray();
                aliases[aliasDirective.Identifier.Text] = aliasSymbols;
            }
        }

        var importBinder = new ImportBinder(nsBinder, namespaceImports, typeImports, aliases);

        foreach (var diagnostic in nsBinder.Diagnostics.AsEnumerable())
            importBinder.Diagnostics.Report(diagnostic);

        return importBinder;

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

        IReadOnlyList<ISymbol> ResolveAlias(INamespaceSymbol current, NameSyntax name)
        {
            var nsSymbol = ResolveNamespace(current, name.ToString());
            if (nsSymbol != null)
                return [nsSymbol];

            ITypeSymbol? typeSymbol = HasTypeArguments(name)
                ? ResolveGenericType(current, name)
                : ResolveType(current, name.ToString());
            if (typeSymbol != null)
                return [typeSymbol];

            if (name is QualifiedNameSyntax qn)
            {
                var memberName = GetRightmostIdentifier(name);
                var left = qn.Left;

                ITypeSymbol? containingType = HasTypeArguments(left)
                    ? ResolveGenericType(current, left)
                    : ResolveType(current, left.ToString());

                if (containingType != null)
                {
                    var members = containingType.GetMembers(memberName)
                        .Where(m => m.IsStatic)
                        .ToArray();
                    if (members.Length > 0)
                        return members;
                }
            }

            return Array.Empty<ISymbol>();
        }

        static bool HasTypeArguments(NameSyntax nameSyntax)
            => nameSyntax.DescendantNodes().OfType<GenericNameSyntax>().Any();

        ITypeSymbol? ResolveGenericType(INamespaceSymbol current, NameSyntax name)
        {
            if (name is GenericNameSyntax g)
            {
                var baseName = g.Identifier.Text + "`" + g.TypeArgumentList.Arguments.Count;
                var full = Combine(current, baseName);
                var unconstructed = (INamedTypeSymbol?)_compilation.GetTypeByMetadataName(full)
                    ?? (INamedTypeSymbol?)_compilation.GetTypeByMetadataName(baseName);
                if (unconstructed is null)
                    return null;

                var args = g.TypeArgumentList.Arguments
                    .Select(a => provisionalImportBinder.ResolveType(a.Type))
                    .ToArray();
                return _compilation.ConstructGenericType(unconstructed, args);
            }

            if (name is QualifiedNameSyntax { Right: GenericNameSyntax gen })
            {
                var leftName = ((QualifiedNameSyntax)name).Left.ToString();
                var baseName = leftName + "." + gen.Identifier.Text + "`" + gen.TypeArgumentList.Arguments.Count;
                var full = Combine(current, baseName);
                var unconstructed = (INamedTypeSymbol?)_compilation.GetTypeByMetadataName(full)
                    ?? (INamedTypeSymbol?)_compilation.GetTypeByMetadataName(baseName);
                if (unconstructed is null)
                    return null;

                var args = gen.TypeArgumentList.Arguments
                    .Select(a => provisionalImportBinder.ResolveType(a.Type))
                    .ToArray();
                return _compilation.ConstructGenericType(unconstructed, args);
            }

            return null;
        }

        ITypeSymbol? ResolveOpenGenericType(INamespaceSymbol current, NameSyntax name)
        {
            if (name is GenericNameSyntax g)
            {
                var baseName = g.Identifier.Text + "`" + g.TypeArgumentList.Arguments.SeparatorCount + 1;
                var full = Combine(current, baseName);
                var unconstructed = (INamedTypeSymbol?)_compilation.GetTypeByMetadataName(full)
                    ?? (INamedTypeSymbol?)_compilation.GetTypeByMetadataName(baseName);
                if (unconstructed is null)
                    return null;

                var args = g.TypeArgumentList.Arguments
                    .Select(a => provisionalImportBinder.ResolveType(a.Type))
                    .ToArray();
                return _compilation.ConstructGenericType(unconstructed, args);
            }

            if (name is QualifiedNameSyntax { Right: GenericNameSyntax gen })
            {
                var leftName = ((QualifiedNameSyntax)name).Left.ToString();
                var baseName = leftName + "." + gen.Identifier.Text + "`" + gen.TypeArgumentList.Arguments.SeparatorCount + 1;
                var full = Combine(current, baseName);
                var unconstructed = (INamedTypeSymbol?)_compilation.GetTypeByMetadataName(full)
                    ?? (INamedTypeSymbol?)_compilation.GetTypeByMetadataName(baseName);
                if (unconstructed is not null)
                    return unconstructed;
            }

            return null;
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
