namespace Raven.CodeAnalysis.Symbols;

internal sealed class SourceEventSymbol : SourceSymbol, IEventSymbol
{
    private readonly bool _isStatic;
    private readonly string _metadataName;
    private SourceFieldSymbol? _backingField;
    private bool _declaredInExtension;
    private ITypeSymbol? _extensionReceiverType;

    public SourceEventSymbol(
        string name,
        ITypeSymbol eventType,
        ISymbol containingSymbol,
        INamedTypeSymbol? containingType,
        INamespaceSymbol? containingNamespace,
        Location[] locations,
        SyntaxReference[] declaringSyntaxReferences,
        bool isStatic = false,
        string? metadataName = null,
        Accessibility declaredAccessibility = Accessibility.NotApplicable)
        : base(SymbolKind.Event, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences, declaredAccessibility)
    {
        Type = eventType;
        _isStatic = isStatic;
        _metadataName = metadataName ?? name;
    }

    public ITypeSymbol Type { get; }

    public IMethodSymbol? AddMethod { get; private set; }

    public IMethodSymbol? RemoveMethod { get; private set; }

    public override bool IsStatic => _isStatic;

    public override string MetadataName => _metadataName;

    public bool IsAutoEvent => _backingField is not null;

    public SourceFieldSymbol? BackingField => _backingField;

    internal bool IsDeclaredInExtension => _declaredInExtension;

    internal ITypeSymbol? ExtensionReceiverType => _extensionReceiverType;

    internal void SetAccessors(IMethodSymbol? addMethod, IMethodSymbol? removeMethod)
    {
        AddMethod = addMethod;
        RemoveMethod = removeMethod;
    }

    internal void SetBackingField(SourceFieldSymbol backingField)
    {
        _backingField = backingField;
        backingField.SetAssociatedEvent(this);
    }

    internal void MarkDeclaredInExtension(ITypeSymbol? receiverType)
    {
        _declaredInExtension = true;
        _extensionReceiverType = receiverType;
    }
}
