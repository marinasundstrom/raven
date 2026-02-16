using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal abstract class TypeDeclarationBinder : Binder
{
    internal readonly record struct NominalTypeShape(
        INamedTypeSymbol? BaseType,
        ImmutableArray<INamedTypeSymbol> Interfaces);

    protected TypeDeclarationBinder(Binder parent, INamedTypeSymbol containingType, SyntaxNode syntax)
        : base(parent)
    {
        ContainingSymbol = containingType;
        Syntax = syntax;
    }

    protected SyntaxNode Syntax { get; }

    public override INamedTypeSymbol ContainingSymbol { get; }

    public override ISymbol? LookupSymbol(string name)
    {
        var symbol = ContainingSymbol.GetMembers(name).FirstOrDefault();
        if (symbol is not null)
            return symbol;

        return base.LookupSymbol(name);
    }

    public override ITypeSymbol? LookupType(string name)
    {
        var typeParameter = ContainingSymbol.TypeParameters.FirstOrDefault(tp => tp.Name == name);
        if (typeParameter is not null)
            return typeParameter;

        return base.LookupType(name);
    }

    public override ISymbol? BindDeclaredSymbol(SyntaxNode node)
    {
        return node == Syntax ? ContainingSymbol : base.BindDeclaredSymbol(node);
    }

    protected override IReadOnlyDictionary<string, ITypeSymbol> GetInScopeTypeParameters()
    {
        var map = new Dictionary<string, ITypeSymbol>(StringComparer.Ordinal);

        if (!ContainingSymbol.TypeParameters.IsDefaultOrEmpty)
        {
            foreach (var tp in ContainingSymbol.TypeParameters)
                map.TryAdd(tp.Name, tp);
        }

        return map;
    }

    internal NominalTypeShape BindNominalTypeShape(
        TypeDeclarationSyntax declaration,
        INamedTypeSymbol? defaultBaseType,
        ImmutableArray<INamedTypeSymbol> defaultInterfaces)
    {
        var baseType = defaultBaseType;
        var interfaces = defaultInterfaces;
        var baseList = GetBaseList(declaration);

        if (baseList is not null)
        {
            var resolvedInterfaces = ImmutableArray.CreateBuilder<INamedTypeSymbol>();
            foreach (var typeSyntax in baseList.Types)
            {
                if (!TryResolveNamedTypeFromTypeSyntax(typeSyntax, out var resolved) || resolved is null)
                    continue;

                if (resolved.TypeKind == TypeKind.Interface)
                    resolvedInterfaces.Add(resolved);
                else
                    baseType = resolved;
            }

            if (resolvedInterfaces.Count > 0)
                interfaces = MergeInterfaceSets(interfaces, resolvedInterfaces.ToImmutable());

            ReportInvalidInheritedBaseType(declaration, baseList, baseType);
        }

        return new NominalTypeShape(baseType, interfaces);
    }

    private void ReportInvalidInheritedBaseType(TypeDeclarationSyntax declaration, BaseListSyntax baseList, INamedTypeSymbol? baseType)
    {
        if (baseType is null)
            return;

        if (baseType.IsStatic)
        {
            Diagnostics.ReportStaticTypeCannotBeInherited(
                baseType.Name,
                baseList.Types[0].GetLocation());
            return;
        }

        if (baseType is SourceNamedTypeSymbol sourceBase && sourceBase.IsSealedHierarchy)
        {
            if (sourceBase.HasExplicitPermits)
            {
                var derivingName = declaration.Identifier.ValueText;
                var isPermitted = false;
                foreach (var permitted in sourceBase.PermittedDirectSubtypes)
                {
                    if (string.Equals(permitted.Name, derivingName, StringComparison.Ordinal))
                    {
                        isPermitted = true;
                        break;
                    }
                }

                if (!isPermitted)
                {
                    Diagnostics.ReportSealedHierarchyInheritanceDeniedNotPermitted(
                        derivingName,
                        baseType.Name,
                        baseList.Types[0].GetLocation());
                }
            }
            else
            {
                var derivingFile = declaration.SyntaxTree?.FilePath;
                var baseFile = sourceBase.SealedHierarchySourceFile;

                if (!string.Equals(derivingFile, baseFile, StringComparison.Ordinal))
                {
                    Diagnostics.ReportSealedHierarchyInheritanceDeniedSameFile(
                        declaration.Identifier.ValueText,
                        baseType.Name,
                        baseList.Types[0].GetLocation());
                }
            }
            return;
        }

        if (baseType.IsClosed)
        {
            Diagnostics.ReportCannotInheritFromClosedType(
                baseType.Name,
                baseList.Types[0].GetLocation());
        }
    }

    private static BaseListSyntax? GetBaseList(TypeDeclarationSyntax declaration)
        => declaration switch
        {
            ClassDeclarationSyntax classDeclaration => classDeclaration.BaseList,
            RecordDeclarationSyntax recordDeclaration => recordDeclaration.BaseList,
            StructDeclarationSyntax structDeclaration => structDeclaration.BaseList,
            _ => null
        };

    private static ImmutableArray<INamedTypeSymbol> MergeInterfaceSets(
        ImmutableArray<INamedTypeSymbol> existing,
        ImmutableArray<INamedTypeSymbol> additional)
    {
        if (existing.IsDefaultOrEmpty)
            return additional;

        var builder = ImmutableArray.CreateBuilder<INamedTypeSymbol>();
        var seen = new HashSet<INamedTypeSymbol>(SymbolEqualityComparer.Default);
        foreach (var type in existing)
            if (seen.Add(type))
                builder.Add(type);

        foreach (var type in additional)
            if (seen.Add(type))
                builder.Add(type);

        return builder.ToImmutable();
    }
}
