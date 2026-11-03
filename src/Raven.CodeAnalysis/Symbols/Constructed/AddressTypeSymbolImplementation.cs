using System;
using System.Collections.Generic;
using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class AddressTypeSymbolImplementation : SymbolBase, AddressTypeSymbol
{
    public AddressTypeSymbolImplementation(TypeSymbol referencedType)
        : base(
            referencedType ?? throw new ArgumentNullException(nameof(referencedType)),
            containingType: null,
            containingNamespace: null,
            locations: Array.Empty<Location>(),
            declaringSyntaxReferences: Array.Empty<SyntaxReference>())
    {
        ReferencedType = referencedType;
    }

    public TypeSymbol ReferencedType { get; }

    public override SymbolKind Kind => SymbolKind.Type;

    public override string Name => $"&{ReferencedType.Name}";

    public override string MetadataName => $"&{ReferencedType.MetadataName}";

    public bool IsNamespace => false;

    public bool IsType => true;

    public bool IsValueType => false;

    public NamedTypeSymbol? BaseType => (ReferencedType as NamedTypeSymbol)?.BaseType;

    public TypeKind TypeKind => TypeKind.Address;

    public SpecialType SpecialType => SpecialType.None;

    public TypeSymbol? OriginalDefinition => ReferencedType;

    public ImmutableArray<NamedTypeSymbol> Interfaces => ReferencedType.Interfaces;

    public ImmutableArray<NamedTypeSymbol> AllInterfaces => ReferencedType.AllInterfaces;

    public ImmutableArray<Symbol> GetMembers() => ReferencedType.GetMembers();

    public ImmutableArray<Symbol> GetMembers(string name) => ReferencedType.GetMembers(name);

    public IEnumerable<Symbol> ResolveMembers(string name) => SymbolExtensions.ResolveMembers(ReferencedType, name);

    public TypeSymbol? LookupType(string name) => null;

    public bool IsMemberDefined(string name, out Symbol? symbol)
    {
        symbol = null;
        return false;
    }

    public override Accessibility DeclaredAccessibility => Accessibility.NotApplicable;

    public override bool IsImplicitlyDeclared => true;

    public override bool IsStatic => false;

    public override AssemblySymbol ContainingAssembly => ReferencedType.ContainingAssembly!;

    public override ModuleSymbol ContainingModule => ReferencedType.ContainingModule!;

    public override void Accept(SymbolVisitor visitor) => visitor.DefaultVisit(this);

    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.DefaultVisit(this);

    public override bool Equals(object? obj) =>
        obj is AddressTypeSymbolImplementation other &&
        SymbolEqualityComparer.Default.Equals(ReferencedType, other.ReferencedType);

    public override int GetHashCode() => SymbolEqualityComparer.Default.GetHashCode(ReferencedType);

    public override string ToString() => Name;
}
