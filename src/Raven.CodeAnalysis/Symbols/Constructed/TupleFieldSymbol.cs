using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class TupleFieldSymbol : IFieldSymbol
{
    private readonly IFieldSymbol _underlyingField;
    private readonly INamedTypeSymbol _containingType;
    private readonly ITypeSymbol? _projectedType;

    public TupleFieldSymbol(
        string name,
        IFieldSymbol underlyingField,
        INamedTypeSymbol containingType,
        Location[] locations,
        ITypeSymbol? projectedType = null)
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
    public ITypeSymbol Type => _projectedType ?? _underlyingField.Type;
    public RefKind RefKind => _underlyingField.RefKind;
    public bool IsConst => _underlyingField.IsConst;
    public bool IsRequired => _underlyingField.IsRequired;
    public bool IsReadOnly => _underlyingField.IsReadOnly;
    public bool IsStatic => _underlyingField.IsStatic;
    public Accessibility DeclaredAccessibility => _underlyingField.DeclaredAccessibility;
    public ISymbol ContainingSymbol => _containingType;
    public IAssemblySymbol? ContainingAssembly => _containingType.ContainingAssembly;
    public IModuleSymbol? ContainingModule => _containingType.ContainingModule;
    public INamedTypeSymbol ContainingType => _containingType;
    public INamespaceSymbol? ContainingNamespace => _containingType.ContainingNamespace;
    public ImmutableArray<Location> Locations { get; }
    public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences => [];
    public bool IsImplicitlyDeclared => false;
    public ISymbol UnderlyingSymbol => this;
    public bool IsAlias => false;
    public IFieldSymbol UnderlyingField => _underlyingField;

    public object? GetConstantValue() => _underlyingField.GetConstantValue();
    public ImmutableArray<AttributeData> GetAttributes() => _underlyingField.GetAttributes();
    public void Accept(SymbolVisitor visitor) => visitor.VisitField(this);
    public TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitField(this);
    public bool Equals(ISymbol? other, SymbolEqualityComparer comparer) => comparer.Equals(this, other);
    public bool Equals(ISymbol? other) => SymbolEqualityComparer.Default.Equals(this, other);
}
