using System;
using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class NullableTypeSymbol : SourceSymbol, ITypeSymbol
{
    public NullableTypeSymbol(ITypeSymbol underlyingType, ISymbol? containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations)
        : base(SymbolKind.Type, string.Empty, containingSymbol, containingType, containingNamespace, locations, [])
    {
        UnderlyingType = underlyingType;
        BaseType = underlyingType.GetAbsoluteBaseType();
        TypeKind = TypeKind.Nullable;
    }

    public override string Name => UnderlyingType.ToDisplayStringKeywordAware(SymbolDisplayFormat.FullyQualifiedFormat) + "?";

    public ITypeSymbol UnderlyingType { get; }

    public SpecialType SpecialType => SpecialType.None;

    public bool IsNamespace => false;

    public bool IsType => true;

    public bool IsValueType => UnderlyingType.IsValueType;

    public INamedTypeSymbol? BaseType { get; }

    public TypeKind TypeKind { get; }

    public ITypeSymbol? OriginalDefinition { get; }

    public ImmutableArray<INamedTypeSymbol> Interfaces => UnderlyingType.Interfaces;

    public ImmutableArray<INamedTypeSymbol> AllInterfaces => UnderlyingType.AllInterfaces;

    public ImmutableArray<ISymbol> GetMembers()
    {
        if (BaseType is not null)
            return BaseType.GetMembers();

        return UnderlyingType.GetMembers();
    }

    public ImmutableArray<ISymbol> GetMembers(string name)
    {
        if (BaseType is not null)
            return BaseType.GetMembers(name);

        return UnderlyingType.GetMembers(name);
    }

    public ITypeSymbol? LookupType(string name)
    {
        var nested = UnderlyingType.LookupType(name);
        if (nested is not null)
            return nested;

        return BaseType?.LookupType(name);
    }

    public override string ToString() => Name;

    public bool IsMemberDefined(string name, out ISymbol? symbol)
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
