namespace Raven.CodeAnalysis.Symbols;

internal sealed class TupleFieldSymbol : PEFieldSymbol, IFieldSymbol
{
    private readonly SubstitutedFieldSymbol _underlyingField;

    public TupleFieldSymbol(string name, SubstitutedFieldSymbol underlyingField, INamedTypeSymbol? containingType,
        Location[] locations)
        : base(null, null!, containingType, locations)
    {
        Name = name;
        _underlyingField = underlyingField;
    }

    public override string Name { get; }

    public override string MetadataName => _underlyingField.MetadataName;

    public override Accessibility DeclaredAccessibility => _underlyingField.DeclaredAccessibility;

    public override bool IsStatic => _underlyingField.IsStatic;
    public override bool IsConst => _underlyingField.IsConst;
    public override bool IsReadOnly => _underlyingField.IsReadOnly;

    public override ITypeSymbol Type => _underlyingField.Type;

    public IFieldSymbol UnderlyingField => _underlyingField;
}
