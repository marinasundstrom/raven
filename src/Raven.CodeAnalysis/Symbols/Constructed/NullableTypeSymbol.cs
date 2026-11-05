using System;
using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class NullableTypeSymbol : SourceSymbol, TypeSymbol, INullableTypeIdentity
{
    public NullableTypeSymbol(
        TypeSymbol underlyingType,
        Symbol? containingSymbol,
        NamedTypeSymbol? containingType,
        NamespaceSymbol? containingNamespace,
        Location[] locations,
        NullableAbiProjection? abiProjection = null)
        : base(SymbolKind.Type, string.Empty, containingSymbol, containingType, containingNamespace, locations, [], addAsMember: false)
    {
        UnderlyingType = underlyingType;
        AbiProjection = abiProjection ?? TypeSymbolNullabilityExtensions.InferNullableAbiProjection(underlyingType);
        BaseType = underlyingType.GetAbsoluteBaseType();
        TypeKind = TypeKind.Nullable;
    }

    public override string Name => UnderlyingType.ToDisplayStringKeywordAware(SymbolDisplayFormat.FullyQualifiedFormat) + "?";

    public TypeSymbol UnderlyingType { get; }

    internal NullableAbiProjection AbiProjection { get; }

    public SpecialType SpecialType => SpecialType.None;

    public bool IsNamespace => false;

    public bool IsType => true;

    public bool IsValueType => UnderlyingType.IsValueType;

    public NamedTypeSymbol? BaseType { get; }

    public TypeKind TypeKind { get; }

    public TypeSymbol? OriginalDefinition { get; }

    public ImmutableArray<NamedTypeSymbol> Interfaces => UnderlyingType.Interfaces;

    public ImmutableArray<NamedTypeSymbol> AllInterfaces => UnderlyingType.AllInterfaces;

    public ImmutableArray<Symbol> GetMembers() => UnderlyingType.GetMembers();

    public ImmutableArray<Symbol> GetMembers(string name) => UnderlyingType.GetMembers(name);

    public TypeSymbol? LookupType(string name)
    {
        var nested = UnderlyingType.LookupType(name);
        if (nested is not null)
            return nested;

        return BaseType?.LookupType(name);
    }

    public override string ToString() => Name;

    public bool IsMemberDefined(string name, out Symbol? symbol)
    {
        if (UnderlyingType.IsMemberDefined(name, out symbol))
            return true;

        if (BaseType is not null && BaseType.IsMemberDefined(name, out symbol))
            return true;

        symbol = null;
        return false;
    }

    public override void Accept(SymbolVisitor visitor)
    {
        visitor.DefaultVisit(this);
    }

    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor)
    {
        return visitor.DefaultVisit(this);
    }

}
