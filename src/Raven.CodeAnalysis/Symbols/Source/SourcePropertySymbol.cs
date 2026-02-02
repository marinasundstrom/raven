using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal partial class SourcePropertySymbol : SourceSymbol, IPropertySymbol
{
    private readonly bool _isStatic;
    private readonly string _metadataName;
    private SourceFieldSymbol? _backingField;
    private bool _declaredInExtension;
    private ITypeSymbol? _extensionReceiverType;
    private ImmutableArray<AttributeData> _lazyAugmentedAttributes;

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
        string? metadataName = null,
        Accessibility declaredAccessibility = Accessibility.NotApplicable)
        : base(SymbolKind.Property, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences, declaredAccessibility)
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

    internal bool IsDeclaredInExtension => _declaredInExtension;

    internal ITypeSymbol? ExtensionReceiverType => _extensionReceiverType;

    internal void SetAccessors(IMethodSymbol? getMethod, IMethodSymbol? setMethod)
    {
        GetMethod = getMethod;
        SetMethod = setMethod;
    }

    internal void SetBackingField(SourceFieldSymbol backingField)
    {
        _backingField = backingField;
        backingField.SetAssociatedProperty(this);
    }

    internal void MarkDeclaredInExtension(ITypeSymbol? receiverType)
    {
        _declaredInExtension = true;
        _extensionReceiverType = receiverType;
    }

    public bool IsRequired
    {
        get; private set;
    }

    public void MarkAsRequired() => IsRequired = true;

    public override ImmutableArray<AttributeData> GetAttributes()
    {
        if (_lazyAugmentedAttributes.IsDefault)
        {
            var baseAttributes = base.GetAttributes();
            var builder = ImmutableArray.CreateBuilder<AttributeData>();

            if (IsRequired)
            {
                var compilerGenerated = CreateRequiredMemberAttribute();
                if (compilerGenerated is not null)
                    builder.Add(compilerGenerated);
            }

            _lazyAugmentedAttributes = builder.Count == 0
                ? baseAttributes
                : baseAttributes.AddRange(builder.ToImmutable());
        }

        return _lazyAugmentedAttributes;
    }
}
