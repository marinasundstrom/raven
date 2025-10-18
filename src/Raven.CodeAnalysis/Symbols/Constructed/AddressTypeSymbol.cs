using System;
using System.Collections.Generic;
using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class AddressTypeSymbol : Symbol, IAddressTypeSymbol
{
    public AddressTypeSymbol(ITypeSymbol referencedType)
        : base(
            referencedType ?? throw new ArgumentNullException(nameof(referencedType)),
            containingType: null,
            containingNamespace: null,
            locations: Array.Empty<Location>(),
            declaringSyntaxReferences: Array.Empty<SyntaxReference>())
    {
        ReferencedType = referencedType;
    }

    public ITypeSymbol ReferencedType { get; }

    public override SymbolKind Kind => SymbolKind.Type;

    public override string Name => $"&{ReferencedType.Name}";

    public override string MetadataName => $"&{ReferencedType.MetadataName}";

    public bool IsNamespace => false;

    public bool IsType => true;

    public bool IsValueType => false;

    public INamedTypeSymbol? BaseType => (ReferencedType as INamedTypeSymbol)?.BaseType;

    public TypeKind TypeKind => TypeKind.Address;

    public SpecialType SpecialType => SpecialType.None;

    public ITypeSymbol? OriginalDefinition => ReferencedType;

    public ImmutableArray<INamedTypeSymbol> Interfaces => ReferencedType.Interfaces;

    public ImmutableArray<INamedTypeSymbol> AllInterfaces => ReferencedType.AllInterfaces;

    public ImmutableArray<ISymbol> GetMembers() => ReferencedType.GetMembers();

    public ImmutableArray<ISymbol> GetMembers(string name) => ReferencedType.GetMembers(name);

    public IEnumerable<ISymbol> ResolveMembers(string name) => SymbolExtensions.ResolveMembers(ReferencedType, name);

    public ITypeSymbol? LookupType(string name) => null;

    public bool IsMemberDefined(string name, out ISymbol? symbol)
    {
        symbol = null;
        return false;
    }

    public override Accessibility DeclaredAccessibility => Accessibility.NotApplicable;

    public override bool IsImplicitlyDeclared => true;

    public override bool IsStatic => false;

    public override IAssemblySymbol ContainingAssembly => ReferencedType.ContainingAssembly!;

    public override IModuleSymbol ContainingModule => ReferencedType.ContainingModule!;

    public override void Accept(SymbolVisitor visitor) => visitor.DefaultVisit(this);

    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.DefaultVisit(this);

    public override bool Equals(object? obj) =>
        obj is AddressTypeSymbol other &&
        SymbolEqualityComparer.Default.Equals(ReferencedType, other.ReferencedType);

    public override int GetHashCode() => SymbolEqualityComparer.Default.GetHashCode(ReferencedType);

    public override string ToString() => Name;
}
