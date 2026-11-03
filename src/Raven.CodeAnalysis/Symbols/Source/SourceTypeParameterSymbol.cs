using System.Collections.Generic;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class SourceTypeParameterSymbol : SourceSymbol, TypeParameterSymbol
{
    private ImmutableArray<TypeSymbol> _constraintTypes;

    public SourceTypeParameterSymbol(
        string name,
        Symbol containingSymbol,
        NamedTypeSymbol? containingType,
        NamespaceSymbol? containingNamespace,
        Location[] locations,
        SyntaxReference[] declaringSyntaxReferences,
        int ordinal,
        TypeParameterConstraintKind constraintKind,
        ImmutableArray<SyntaxReference> constraintTypeReferences,
        VarianceKind variance)
        : base(SymbolKind.TypeParameter, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
        Ordinal = ordinal;
        ConstraintKind = constraintKind;
        ConstraintTypeReferences = constraintTypeReferences;
        Variance = variance;
    }

    public int Ordinal { get; }

    public TypeParameterOwnerKind OwnerKind => ContainingSymbol switch
    {
        MethodSymbol => TypeParameterOwnerKind.Method,
        _ => TypeParameterOwnerKind.Type,
    };

    public NamedTypeSymbol? DeclaringTypeParameterOwner =>
        OwnerKind == TypeParameterOwnerKind.Type ? (ContainingSymbol as NamedTypeSymbol) : null;

    public MethodSymbol? DeclaringMethodParameterOwner =>
        OwnerKind == TypeParameterOwnerKind.Method ? (ContainingSymbol as MethodSymbol) : null;

    public TypeParameterConstraintKind ConstraintKind { get; }

    internal ImmutableArray<SyntaxReference> ConstraintTypeReferences { get; }

    internal bool HasResolvedConstraintTypes => !_constraintTypes.IsDefault;

    public override string MetadataName => Name;

    public SpecialType SpecialType => SpecialType.None;

    public TypeKind TypeKind => TypeKind.TypeParameter;

    public bool IsNamespace => false;

    public bool IsType => true;

    public bool IsReferenceType => TypeParameterFacts.IsKnownReferenceType(this);

    public bool IsValueType => (ConstraintKind & TypeParameterConstraintKind.ValueType) != 0;

    public NamedTypeSymbol? BaseType => null;

    public TypeSymbol? OriginalDefinition => this;

    public ImmutableArray<Symbol> GetMembers() =>
        GetDeclaringCompilation()?.GetFunctionContractMembers(this) ?? [];

    public ImmutableArray<Symbol> GetMembers(string name) => GetMembers().Where(m => m.Name == name).ToImmutableArray();

    public TypeSymbol? LookupType(string name) => null;

    public bool IsMemberDefined(string name, out Symbol? symbol)
    {
        symbol = GetMembers(name).FirstOrDefault();
        return symbol is not null;
    }

    public ImmutableArray<NamedTypeSymbol> Interfaces => GetConstraintInterfaces(includeInherited: false);

    public ImmutableArray<NamedTypeSymbol> AllInterfaces => GetConstraintInterfaces(includeInherited: true);

    public ImmutableArray<TypeSymbol> ConstraintTypes =>
        _constraintTypes.IsDefault ? ImmutableArray<TypeSymbol>.Empty : _constraintTypes;

    public VarianceKind Variance { get; }

    internal void SetConstraintTypes(ImmutableArray<TypeSymbol> constraintTypes)
    {
        _constraintTypes = constraintTypes.IsDefault
            ? ImmutableArray<TypeSymbol>.Empty
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

    private ImmutableArray<NamedTypeSymbol> GetConstraintInterfaces(bool includeInherited)
    {
        var builder = ImmutableArray.CreateBuilder<NamedTypeSymbol>();
        var seen = new HashSet<NamedTypeSymbol>(SymbolEqualityComparer.Default);

        foreach (var constraint in ConstraintTypes)
        {
            if (constraint is not NamedTypeSymbol named || named.TypeKind != TypeKind.Interface)
                continue;

            AddInterface(named, includeInherited, builder, seen);
        }

        return builder.ToImmutable();
    }

    private static void AddInterface(
        NamedTypeSymbol interfaceSymbol,
        bool includeInherited,
        ImmutableArray<NamedTypeSymbol>.Builder builder,
        HashSet<NamedTypeSymbol> seen)
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
