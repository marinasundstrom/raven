using System;
using System.Collections.Generic;
using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class PointerTypeSymbolImplementation : SymbolBase, PointerTypeSymbol
{
    public PointerTypeSymbolImplementation(TypeSymbol pointedAtType)
        : base(
            pointedAtType ?? throw new ArgumentNullException(nameof(pointedAtType)),
            containingType: null,
            containingNamespace: null,
            locations: Array.Empty<Location>(),
            declaringSyntaxReferences: Array.Empty<SyntaxReference>())
    {
        PointedAtType = pointedAtType;
    }

    public TypeSymbol PointedAtType { get; }

    public override SymbolKind Kind => SymbolKind.Type;

    public override string Name => $"{PointedAtType.Name}*";

    public override string MetadataName => $"{PointedAtType.MetadataName}*";

    public bool IsNamespace => false;

    public bool IsType => true;

    public bool IsReferenceType => false;

    public bool IsValueType => false;

    public NamedTypeSymbol? BaseType => (PointedAtType as NamedTypeSymbol)?.BaseType;

    public TypeKind TypeKind => TypeKind.Pointer;

    public SpecialType SpecialType => SpecialType.None;

    public TypeSymbol? OriginalDefinition => PointedAtType;

    public ImmutableArray<NamedTypeSymbol> Interfaces => PointedAtType.Interfaces;

    public ImmutableArray<NamedTypeSymbol> AllInterfaces => PointedAtType.AllInterfaces;

    public ImmutableArray<Symbol> GetMembers() => PointedAtType.GetMembers();

    public ImmutableArray<Symbol> GetMembers(string name) => PointedAtType.GetMembers(name);

    public IEnumerable<Symbol> ResolveMembers(string name) => SymbolExtensions.ResolveMembers(PointedAtType, name);

    public TypeSymbol? LookupType(string name) => null;

    public bool IsMemberDefined(string name, out Symbol? symbol)
    {
        symbol = null;
        return false;
    }

    public override Accessibility DeclaredAccessibility => Accessibility.NotApplicable;

    public override bool IsImplicitlyDeclared => true;

    public override bool IsStatic => false;

    public override AssemblySymbol ContainingAssembly => PointedAtType.ContainingAssembly!;

    public override ModuleSymbol ContainingModule => PointedAtType.ContainingModule!;

    public override void Accept(SymbolVisitor visitor) => visitor.DefaultVisit(this);

    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.DefaultVisit(this);

    public override bool Equals(object? obj) =>
        obj is PointerTypeSymbolImplementation other &&
        SymbolEqualityComparer.Default.Equals(PointedAtType, other.PointedAtType);

    public override int GetHashCode() => SymbolEqualityComparer.Default.GetHashCode(PointedAtType);

    public override string ToString() => Name;
}
