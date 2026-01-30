using System;
using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal sealed class AttributeBindingContext
{
    private readonly Compilation _compilation;
    private readonly Dictionary<SyntaxTree, Binder> _importBinderCache = new();

    public AttributeBindingContext(Compilation compilation)
    {
        _compilation = compilation ?? throw new ArgumentNullException(nameof(compilation));
    }

    public AttributeBinder CreateAttributeBinder(ISymbol owner, AttributeSyntax attribute)
    {
        if (owner is null)
            throw new ArgumentNullException(nameof(owner));

        if (attribute is null)
            throw new ArgumentNullException(nameof(attribute));

        var parentBinder = GetImportBinder(attribute.SyntaxTree);
        return new AttributeBinder(owner, parentBinder);
    }

    private Binder GetImportBinder(SyntaxTree syntaxTree)
    {
        if (_importBinderCache.TryGetValue(syntaxTree, out var cached))
            return cached;

        if (syntaxTree.GetRoot() is not CompilationUnitSyntax compilationUnit)
        {
            _importBinderCache[syntaxTree] = _compilation.GlobalBinder;
            return _compilation.GlobalBinder;
        }

        var fileScopedNamespace = compilationUnit.Members.OfType<FileScopedNamespaceDeclarationSyntax>().FirstOrDefault();
        INamespaceSymbol targetNamespace = fileScopedNamespace is not null
            ? _compilation.GetOrCreateNamespaceSymbol(fileScopedNamespace.Name.ToString()) ?? _compilation.SourceGlobalNamespace
            : _compilation.SourceGlobalNamespace;

        var namespaceBinder = new NamespaceBinder(_compilation.GlobalBinder, targetNamespace);

        var namespaceImports = new List<INamespaceOrTypeSymbol>();
        var typeImports = new List<ITypeSymbol>();
        var aliases = new Dictionary<string, IReadOnlyList<IAliasSymbol>>();
        var deferredWildcardImports = new List<NameSyntax>();

        var provisionalImportBinder = new ImportBinder(namespaceBinder, namespaceImports, typeImports, aliases);

        foreach (var import in compilationUnit.DescendantNodes().OfType<ImportDirectiveSyntax>())
        {
            var name = import.Name.ToString();

            if (IsWildcard(import.Name, out var baseName))
            {
                INamespaceOrTypeSymbol? resolved =
                    (INamespaceOrTypeSymbol?)ResolveNamespace(targetNamespace, baseName.ToString())
                    ?? ResolveType(targetNamespace, baseName.ToString());

                if (resolved is not null)
                {
                    namespaceImports.Add(resolved);
                }
                else
                {
                    deferredWildcardImports.Add(baseName);
                }

                continue;
            }

            var namespaceSymbol = ResolveNamespace(targetNamespace, name);
            if (namespaceSymbol is not null)
            {
                namespaceBinder.Diagnostics.ReportTypeExpectedWithoutWildcard(import.Name.GetLocation());
                continue;
            }

            ITypeSymbol? typeSymbol = HasTypeArguments(import.Name)
                ? ResolveOpenGenericType(targetNamespace, import.Name)
                : ResolveType(targetNamespace, name);

            if (typeSymbol is not null)
            {
                typeImports.Add(typeSymbol);
            }
            else
            {
                namespaceBinder.Diagnostics.ReportInvalidImportTarget(import.Name.GetLocation());
            }
        }

        var importBinder = new ImportBinder(namespaceBinder, namespaceImports, typeImports, aliases);

        foreach (var baseName in deferredWildcardImports)
        {
            INamespaceOrTypeSymbol? resolved =
                (INamespaceOrTypeSymbol?)ResolveNamespace(targetNamespace, baseName.ToString())
                ?? ResolveType(targetNamespace, baseName.ToString());

            if (resolved is not null)
            {
                namespaceImports.Add(resolved);
            }
            else
            {
                namespaceBinder.Diagnostics.ReportInvalidImportTarget(baseName.GetLocation());
            }
        }

        BindAliases(compilationUnit.Aliases);
        if (fileScopedNamespace is not null)
            BindAliases(fileScopedNamespace.Aliases);

        foreach (var diagnostic in namespaceBinder.Diagnostics.AsEnumerable())
            importBinder.Diagnostics.Report(diagnostic);

        _importBinderCache[syntaxTree] = importBinder;
        return importBinder;

        void BindAliases(SyntaxList<AliasDirectiveSyntax> aliasList)
        {
            foreach (var alias in aliasList)
            {
                IReadOnlyList<ISymbol> symbols;
                if (alias.Target is NameSyntax nameSyntax)
                {
                    symbols = ResolveAlias(targetNamespace, nameSyntax);
                }
                else
                {
                    var typeSymbol = provisionalImportBinder.ResolveType(alias.Target);
                    symbols = typeSymbol == _compilation.ErrorTypeSymbol
                        ? Array.Empty<ISymbol>()
                        : new ISymbol[] { typeSymbol };
                }

                if (symbols.Count > 0)
                {
                    var aliasName = alias.Identifier.ValueText;
                    var aliasSymbols = symbols
                        .Select(s => AliasSymbolFactory.Create(aliasName, s))
                        .ToArray();
                    aliases[aliasName] = aliasSymbols;
                }
                else
                {
                    namespaceBinder.Diagnostics.ReportInvalidAliasType(alias.Target.GetLocation());
                }
            }
        }

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
            if (nsSymbol is not null)
                return [nsSymbol];

            ITypeSymbol? typeSymbol = HasTypeArguments(name)
                ? ResolveGenericType(current, name)
                : ResolveType(current, name.ToString());
            if (typeSymbol is not null)
                return [typeSymbol];

            if (name is QualifiedNameSyntax qn)
            {
                var memberName = GetRightmostIdentifier(name);
                var left = qn.Left;

                ITypeSymbol? containingType = HasTypeArguments(left)
                    ? ResolveGenericType(current, left)
                    : ResolveType(current, left.ToString());

                if (containingType is not null)
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
            if (name is GenericNameSyntax genericName)
            {
                var baseName = $"{genericName.Identifier.ValueText}`{genericName.TypeArgumentList.Arguments.Count}";
                var full = Combine(current, baseName);
                var unconstructed = (INamedTypeSymbol?)_compilation.GetTypeByMetadataName(full)
                    ?? (INamedTypeSymbol?)_compilation.GetTypeByMetadataName(baseName);
                if (unconstructed is null)
                    return null;

                var args = genericName.TypeArgumentList.Arguments
                    .Select(a => provisionalImportBinder.ResolveType(a.Type))
                    .ToArray();
                return _compilation.ConstructGenericType(unconstructed, args);
            }

            if (name is QualifiedNameSyntax { Right: GenericNameSyntax gen })
            {
                var leftName = ((QualifiedNameSyntax)name).Left.ToString();
                var baseName = $"{leftName}.{gen.Identifier.ValueText}`{gen.TypeArgumentList.Arguments.Count}";
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
            if (name is GenericNameSyntax genericName)
            {
                var baseName = $"{genericName.Identifier.ValueText}`{genericName.TypeArgumentList.Arguments.SeparatorCount + 1}";
                var full = Combine(current, baseName);
                var unconstructed = (INamedTypeSymbol?)_compilation.GetTypeByMetadataName(full)
                    ?? (INamedTypeSymbol?)_compilation.GetTypeByMetadataName(baseName);
                if (unconstructed is null)
                    return null;

                var args = genericName.TypeArgumentList.Arguments
                    .Select(a => provisionalImportBinder.ResolveType(a.Type))
                    .ToArray();
                return _compilation.ConstructGenericType(unconstructed, args);
            }

            if (name is QualifiedNameSyntax { Right: GenericNameSyntax gen })
            {
                var leftName = ((QualifiedNameSyntax)name).Left.ToString();
                var baseName = $"{leftName}.{gen.Identifier.ValueText}`{gen.TypeArgumentList.Arguments.SeparatorCount + 1}";
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
                IdentifierNameSyntax id => id.Identifier.ValueText,
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
