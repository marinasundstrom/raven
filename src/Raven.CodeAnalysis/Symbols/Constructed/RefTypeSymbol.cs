using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class RefTypeSymbol : SymbolBase, TypeSymbol, IByRefTypeInfo, IEquatable<RefTypeSymbol>
{
    public TypeSymbol ElementType { get; }

    public RefTypeSymbol(TypeSymbol elementType)
        : base(
            SymbolKind.Type, "",
            containingSymbol: (elementType ?? throw new ArgumentNullException(nameof(elementType))).ContainingSymbol,
            containingType: elementType.ContainingType,
            containingNamespace: elementType.ContainingNamespace,
            locations: [], declaringSyntaxReferences: [], declaredAccessibility: Accessibility.NotApplicable, addAsMember: false)
    {
        ElementType = elementType;
    }

    public override string Name => $"{ElementType.Name}&";

    public override string MetadataName => ElementType.MetadataName + "&";

    public override SymbolKind Kind => SymbolKind.Type;
    public TypeKind TypeKind => ElementType.TypeKind;
    public SpecialType SpecialType => SpecialType.None;

    public bool IsNamespace => false;
    public bool IsType => true;

    public NamedTypeSymbol? BaseType => (ElementType as NamedTypeSymbol)?.BaseType;
    public TypeSymbol? OriginalDefinition => ElementType;

    public override Accessibility DeclaredAccessibility => Accessibility.NotApplicable;
    public override bool IsImplicitlyDeclared => true;
    public override bool IsStatic => false;

    public ImmutableArray<NamedTypeSymbol> Interfaces => ElementType.Interfaces;

    public ImmutableArray<NamedTypeSymbol> AllInterfaces => ElementType.AllInterfaces;

    public ImmutableArray<Symbol> GetMembers() => ElementType.GetMembers();
    public ImmutableArray<Symbol> GetMembers(string name) => ElementType.GetMembers(name);
    public IEnumerable<Symbol> ResolveMembers(string name) => ElementType.ResolveMembers(name);

    public TypeSymbol? LookupType(string name) =>
        ElementType.GetMembers(name).OfType<TypeSymbol>().FirstOrDefault();

    public bool IsMemberDefined(string name, out Symbol? symbol)
    {
        symbol = ElementType.GetMembers(name).FirstOrDefault();
        return symbol is not null;
    }

    public override string ToString() => Name;

    public bool Equals(RefTypeSymbol? other) =>
        other is not null && SymbolEqualityComparer.Default.Equals(ElementType, other.ElementType);

    public override bool Equals(object? obj) => obj is RefTypeSymbol other && Equals(other);

    public override int GetHashCode() => SymbolEqualityComparer.Default.GetHashCode(ElementType);

    public static bool operator ==(RefTypeSymbol? left, RefTypeSymbol? right)
        => ReferenceEquals(left, right) || left is not null && left.Equals(right);

    public static bool operator !=(RefTypeSymbol? left, RefTypeSymbol? right) => !(left == right);

    public override AssemblySymbol? ContainingAssembly => ContainingNamespace?.ContainingAssembly;

    public override ModuleSymbol? ContainingModule => ContainingNamespace?.ContainingModule;

    public override void Accept(SymbolVisitor visitor) => visitor.DefaultVisit(this);

    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.DefaultVisit(this);
}
