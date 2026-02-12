using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class RefTypeSymbol : SourceSymbol, ITypeSymbol
{
    public ITypeSymbol ElementType { get; }

    public RefTypeSymbol(ITypeSymbol elementType)
        : base(
            SymbolKind.Type, "",
            containingSymbol: elementType.ContainingSymbol ?? throw new ArgumentNullException(nameof(elementType.ContainingSymbol)),
            containingType: elementType.ContainingType,
            containingNamespace: elementType.ContainingNamespace,
            locations: [], declaringSyntaxReferences: [])
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

    public INamedTypeSymbol? BaseType => (ElementType as INamedTypeSymbol)?.BaseType;
    public ITypeSymbol? OriginalDefinition => ElementType;

    public override Accessibility DeclaredAccessibility => Accessibility.NotApplicable;
    public override bool IsImplicitlyDeclared => true;
    public override bool IsStatic => false;

    public ImmutableArray<INamedTypeSymbol> Interfaces => ElementType.Interfaces;

    public ImmutableArray<INamedTypeSymbol> AllInterfaces => ElementType.AllInterfaces;

    public ImmutableArray<ISymbol> GetMembers() => ElementType.GetMembers();
    public ImmutableArray<ISymbol> GetMembers(string name) => ElementType.GetMembers(name);
    public IEnumerable<ISymbol> ResolveMembers(string name) => ElementType.ResolveMembers(name);

    public ITypeSymbol? LookupType(string name) =>
        ElementType.GetMembers(name).OfType<ITypeSymbol>().FirstOrDefault();

    public bool IsMemberDefined(string name, out ISymbol? symbol)
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
