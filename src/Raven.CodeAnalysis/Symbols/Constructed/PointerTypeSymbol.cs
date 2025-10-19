using System;
using System.Collections.Generic;
using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class PointerTypeSymbol : Symbol, IPointerTypeSymbol
{
    public PointerTypeSymbol(ITypeSymbol pointedAtType)
        : base(
            pointedAtType ?? throw new ArgumentNullException(nameof(pointedAtType)),
            containingType: null,
            containingNamespace: null,
            locations: Array.Empty<Location>(),
            declaringSyntaxReferences: Array.Empty<SyntaxReference>())
    {
        PointedAtType = pointedAtType;
    }

    public ITypeSymbol PointedAtType { get; }

    public override SymbolKind Kind => SymbolKind.Type;

    public override string Name => $"{PointedAtType.Name}*";

    public override string MetadataName => $"{PointedAtType.MetadataName}*";

    public bool IsNamespace => false;

    public bool IsType => true;

    public bool IsReferenceType => false;

    public bool IsValueType => false;

    public INamedTypeSymbol? BaseType => (PointedAtType as INamedTypeSymbol)?.BaseType;

    public TypeKind TypeKind => TypeKind.Pointer;

    public SpecialType SpecialType => SpecialType.None;

    public ITypeSymbol? OriginalDefinition => PointedAtType;

    public ImmutableArray<INamedTypeSymbol> Interfaces => PointedAtType.Interfaces;

    public ImmutableArray<INamedTypeSymbol> AllInterfaces => PointedAtType.AllInterfaces;

    public ImmutableArray<ISymbol> GetMembers() => PointedAtType.GetMembers();

    public ImmutableArray<ISymbol> GetMembers(string name) => PointedAtType.GetMembers(name);

    public IEnumerable<ISymbol> ResolveMembers(string name) => SymbolExtensions.ResolveMembers(PointedAtType, name);

    public ITypeSymbol? LookupType(string name) => null;

    public bool IsMemberDefined(string name, out ISymbol? symbol)
    {
        symbol = null;
        return false;
    }

    public override Accessibility DeclaredAccessibility => Accessibility.NotApplicable;

    public override bool IsImplicitlyDeclared => true;

    public override bool IsStatic => false;

    public override IAssemblySymbol ContainingAssembly => PointedAtType.ContainingAssembly!;

    public override IModuleSymbol ContainingModule => PointedAtType.ContainingModule!;

    public override void Accept(SymbolVisitor visitor) => visitor.DefaultVisit(this);

    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.DefaultVisit(this);

    public override bool Equals(object? obj) =>
        obj is PointerTypeSymbol other &&
        SymbolEqualityComparer.Default.Equals(PointedAtType, other.PointedAtType);

    public override int GetHashCode() => SymbolEqualityComparer.Default.GetHashCode(PointedAtType);

    public override string ToString() => Name;
}
