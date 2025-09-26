using System.Collections.Generic;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class SourceTypeParameterSymbol : Symbol, ITypeParameterSymbol
{
    private ImmutableArray<ITypeSymbol> _constraintTypes;

    public SourceTypeParameterSymbol(
        string name,
        ISymbol containingSymbol,
        INamedTypeSymbol? containingType,
        INamespaceSymbol? containingNamespace,
        Location[] locations,
        SyntaxReference[] declaringSyntaxReferences,
        int ordinal,
        TypeParameterConstraintKind constraintKind,
        ImmutableArray<SyntaxReference> constraintTypeReferences)
        : base(SymbolKind.TypeParameter, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
        Ordinal = ordinal;
        ConstraintKind = constraintKind;
        ConstraintTypeReferences = constraintTypeReferences;
    }

    public int Ordinal { get; }

    public TypeParameterConstraintKind ConstraintKind { get; }

    internal ImmutableArray<SyntaxReference> ConstraintTypeReferences { get; }

    internal bool HasResolvedConstraintTypes => !_constraintTypes.IsDefault;

    public override string MetadataName => Name;

    public SpecialType SpecialType => SpecialType.None;

    public TypeKind TypeKind => TypeKind.TypeParameter;

    public bool IsNamespace => false;

    public bool IsType => true;

    public bool IsReferenceType => false;

    public bool IsValueType => false;

    public INamedTypeSymbol? BaseType => null;

    public ITypeSymbol? OriginalDefinition => this;

    public ImmutableArray<ISymbol> GetMembers() => ImmutableArray<ISymbol>.Empty;

    public ImmutableArray<ISymbol> GetMembers(string name) => ImmutableArray<ISymbol>.Empty;

    public ITypeSymbol? LookupType(string name) => null;

    public bool IsMemberDefined(string name, out ISymbol? symbol)
    {
        symbol = null;
        return false;
    }

    public ImmutableArray<INamedTypeSymbol> Interfaces => GetConstraintInterfaces(includeInherited: false);

    public ImmutableArray<INamedTypeSymbol> AllInterfaces => GetConstraintInterfaces(includeInherited: true);

    public ImmutableArray<ITypeSymbol> ConstraintTypes =>
        _constraintTypes.IsDefault ? ImmutableArray<ITypeSymbol>.Empty : _constraintTypes;

    internal void SetConstraintTypes(ImmutableArray<ITypeSymbol> constraintTypes)
    {
        _constraintTypes = constraintTypes.IsDefault
            ? ImmutableArray<ITypeSymbol>.Empty
            : constraintTypes;
    }

    public override void Accept(SymbolVisitor visitor)
    {
        visitor.DefaultVisit(this);
    }

    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor)
    {
        return visitor.DefaultVisit(this);
    }

    private ImmutableArray<INamedTypeSymbol> GetConstraintInterfaces(bool includeInherited)
    {
        var builder = ImmutableArray.CreateBuilder<INamedTypeSymbol>();
        var seen = new HashSet<INamedTypeSymbol>(SymbolEqualityComparer.Default);

        foreach (var constraint in ConstraintTypes)
        {
            if (constraint is not INamedTypeSymbol named || named.TypeKind != TypeKind.Interface)
                continue;

            AddInterface(named, includeInherited, builder, seen);
        }

        return builder.ToImmutable();
    }

    private static void AddInterface(
        INamedTypeSymbol interfaceSymbol,
        bool includeInherited,
        ImmutableArray<INamedTypeSymbol>.Builder builder,
        HashSet<INamedTypeSymbol> seen)
    {
        if (!seen.Add(interfaceSymbol))
            return;

        builder.Add(interfaceSymbol);

        if (!includeInherited)
            return;

        foreach (var inherited in interfaceSymbol.AllInterfaces)
            if (seen.Add(inherited))
                builder.Add(inherited);
    }
}
