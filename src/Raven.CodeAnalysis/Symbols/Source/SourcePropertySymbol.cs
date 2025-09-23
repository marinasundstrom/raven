namespace Raven.CodeAnalysis.Symbols;

internal partial class SourcePropertySymbol : SourceSymbol, IPropertySymbol
{
    private readonly bool _isStatic;
    private readonly string _metadataName;
    private SourceFieldSymbol? _backingField;

    public SourcePropertySymbol(
        string name,
        ITypeSymbol propertyType,
        ISymbol containingSymbol,
        INamedTypeSymbol? containingType,
        INamespaceSymbol? containingNamespace,
        Location[] locations,
        SyntaxReference[] declaringSyntaxReferences,
        bool isIndexer = false,
        bool isStatic = false,
        string? metadataName = null)
        : base(SymbolKind.Property, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
        Type = propertyType;
        IsIndexer = isIndexer;
        _isStatic = isStatic;
        _metadataName = metadataName ?? name;
    }

    public ITypeSymbol Type { get; }

    public IMethodSymbol? GetMethod { get; private set; }

    public IMethodSymbol? SetMethod { get; private set; }

    public bool IsIndexer { get; }

    public override bool IsStatic => _isStatic;

    public override string MetadataName => _metadataName;

    public bool IsAutoProperty => _backingField is not null;

    public SourceFieldSymbol? BackingField => _backingField;

    internal void SetAccessors(IMethodSymbol? getMethod, IMethodSymbol? setMethod)
    {
        GetMethod = getMethod;
        SetMethod = setMethod;
    }

    internal void SetBackingField(SourceFieldSymbol backingField)
    {
        _backingField = backingField;
    }
}
