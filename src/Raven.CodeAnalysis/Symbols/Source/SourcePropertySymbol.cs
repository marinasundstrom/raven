using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal partial class SourcePropertySymbol : SourceSymbol, PropertySymbol
{
    internal enum PropertyImplementationKind
    {
        Explicit,
        SynthesizedBackingFieldAccessors,
        FieldOnly
    }

    private readonly bool _isStatic;
    private readonly string _metadataName;
    private SourceFieldSymbol? _backingField;
    private bool _declaredInExtension;
    private TypeSymbol? _extensionReceiverType;
    private bool? _isMutable;
    private PropertyImplementationKind _implementationKind;
    private ImmutableArray<AttributeData> _lazyAugmentedAttributes;
    private bool _isPartialDefinition;
    private bool _isPartialImplementation;
    private ImmutableArray<PropertySymbol> _explicitInterfaceImplementations = ImmutableArray<PropertySymbol>.Empty;

    public SourcePropertySymbol(
        string name,
        TypeSymbol propertyType,
        Symbol containingSymbol,
        NamedTypeSymbol? containingType,
        NamespaceSymbol? containingNamespace,
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

    public TypeSymbol Type { get; }

    public MethodSymbol? GetMethod { get; private set; }

    public MethodSymbol? SetMethod { get; private set; }

    public bool IsMutable => _isMutable ?? SetMethod is { MethodKind: MethodKind.PropertySet };

    public bool IsIndexer { get; }

    public override bool IsStatic => _isStatic;

    public override string MetadataName => _metadataName;

    public bool IsAutoProperty => _implementationKind == PropertyImplementationKind.SynthesizedBackingFieldAccessors;
    internal bool IsPartialMember => _isPartialDefinition || _isPartialImplementation;
    internal bool HasPartialDefinition => _isPartialDefinition;
    internal bool HasPartialImplementation => _isPartialImplementation;
    public ImmutableArray<PropertySymbol> ExplicitInterfaceImplementations => _explicitInterfaceImplementations;

    public SourceFieldSymbol? BackingField => _backingField;
    internal bool EmitAsFieldOnly => _implementationKind == PropertyImplementationKind.FieldOnly;
    internal PropertyImplementationKind ImplementationKind => _implementationKind;

    internal bool IsDeclaredInExtension => _declaredInExtension;

    internal TypeSymbol? ExtensionReceiverType => _extensionReceiverType;

    internal void SetAccessors(MethodSymbol? getMethod, MethodSymbol? setMethod)
    {
        GetMethod = getMethod;
        SetMethod = setMethod;
    }

    internal void SetMutability(bool isMutable)
    {
        _isMutable = isMutable;
    }

    internal void SetBackingField(SourceFieldSymbol backingField)
    {
        _backingField = backingField;
        backingField.SetAssociatedProperty(this);
    }

    internal void MarkSynthesizedBackingFieldAccessors()
    {
        _implementationKind = PropertyImplementationKind.SynthesizedBackingFieldAccessors;
    }

    internal void MarkDeclaredInExtension(TypeSymbol? receiverType)
    {
        _declaredInExtension = true;
        _extensionReceiverType = receiverType;
    }

    internal void MarkEmitAsFieldOnly()
    {
        _implementationKind = PropertyImplementationKind.FieldOnly;
    }

    internal void SetExplicitInterfaceImplementations(ImmutableArray<PropertySymbol> implementations)
    {
        _explicitInterfaceImplementations = implementations;
    }

    internal void MarkAsPartialDefinition()
    {
        _isPartialDefinition = true;
    }

    internal void MarkAsPartialImplementation()
    {
        _isPartialImplementation = true;
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
