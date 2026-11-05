using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class RefTypeSymbol : SourceSymbol, TypeSymbol, IByRefTypeInfo
{
    public TypeSymbol ElementType { get; }

    public RefTypeSymbol(TypeSymbol elementType)
        : base(
            SymbolKind.Type, "",
            containingSymbol: elementType.ContainingSymbol ?? throw new ArgumentNullException(nameof(elementType.ContainingSymbol)),
            containingType: elementType.ContainingType,
            containingNamespace: elementType.ContainingNamespace,
            locations: [], declaringSyntaxReferences: [], addAsMember: false)
    {
        ElementType = elementType;
    }

    public override string Name => $"{ElementType?.Name}&";

    public override string MetadataName => (ElementType?.MetadataName ?? string.Empty) + "&";

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

    public override bool Equals(object? obj) =>
        obj is RefTypeSymbol other &&
        SymbolEqualityComparer.Default.Equals(ElementType, other.ElementType);

    public override int GetHashCode() => HashCode.Combine(ElementType);

    public override void Accept(SymbolVisitor visitor) => visitor.Visit(this);
    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.Visit(this);
}
