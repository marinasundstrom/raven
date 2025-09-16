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

    public ImmutableArray<ISymbol> GetMembers() => BaseType!.GetMembers();

    public ImmutableArray<ISymbol> GetMembers(string name) => BaseType!.GetMembers(name);

    public ITypeSymbol? LookupType(string name)
    {
        throw new NotImplementedException();
    }

    public override string ToString() => Name;

    public bool IsMemberDefined(string name, out ISymbol? symbol)
    {
        throw new NotSupportedException();
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
