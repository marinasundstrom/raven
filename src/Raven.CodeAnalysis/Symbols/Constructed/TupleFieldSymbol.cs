using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class TupleFieldSymbol : FieldSymbol
{
    private readonly FieldSymbol _underlyingField;
    private readonly NamedTypeSymbol _containingType;
    private readonly TypeSymbol? _projectedType;

    public TupleFieldSymbol(
        string name,
        FieldSymbol underlyingField,
        NamedTypeSymbol containingType,
        Location[] locations,
        TypeSymbol? projectedType = null)
    {
        Name = name;
        _underlyingField = underlyingField;
        _containingType = containingType;
        _projectedType = projectedType;
        Locations = [.. locations];
    }

    public SymbolKind Kind => SymbolKind.Field;
    public string Name { get; }
    public string MetadataName => _underlyingField.MetadataName;
    public TypeSymbol Type => _projectedType ?? _underlyingField.Type;
    public RefKind RefKind => _underlyingField.RefKind;
    public bool IsConst => _underlyingField.IsConst;
    public bool IsRequired => _underlyingField.IsRequired;
    public bool IsReadOnly => _underlyingField.IsReadOnly;
    public bool IsStatic => _underlyingField.IsStatic;
    public Accessibility DeclaredAccessibility => _underlyingField.DeclaredAccessibility;
    public Symbol ContainingSymbol => _containingType;
    public AssemblySymbol? ContainingAssembly => _containingType.ContainingAssembly;
    public ModuleSymbol? ContainingModule => _containingType.ContainingModule;
    public NamedTypeSymbol ContainingType => _containingType;
    public NamespaceSymbol? ContainingNamespace => _containingType.ContainingNamespace;
    public ImmutableArray<Location> Locations { get; }
    public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences => [];
    public bool IsImplicitlyDeclared => false;
    public Symbol UnderlyingSymbol => this;
    public bool IsAlias => false;
    public FieldSymbol UnderlyingField => _underlyingField;

    public object? GetConstantValue() => _underlyingField.GetConstantValue();
    public ImmutableArray<AttributeData> GetAttributes() => _underlyingField.GetAttributes();
    public void Accept(SymbolVisitor visitor) => visitor.VisitField(this);
    public TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitField(this);
    public bool Equals(Symbol? other, SymbolEqualityComparer comparer) => comparer.Equals(this, other);
    public bool Equals(Symbol? other) => SymbolEqualityComparer.Default.Equals(this, other);
}
