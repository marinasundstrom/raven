namespace Raven.CodeAnalysis.Symbols;

internal sealed class SourceEventSymbol : SourceSymbol, EventSymbol
{
    private readonly bool _isStatic;
    private readonly string _metadataName;
    private SourceFieldSymbol? _backingField;
    private bool _declaredInExtension;
    private TypeSymbol? _extensionReceiverType;
    private bool _isPartialDefinition;
    private bool _isPartialImplementation;

    public SourceEventSymbol(
        string name,
        TypeSymbol eventType,
        Symbol containingSymbol,
        NamedTypeSymbol? containingType,
        NamespaceSymbol? containingNamespace,
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

    public TypeSymbol Type { get; }

    public MethodSymbol? AddMethod { get; private set; }

    public MethodSymbol? RemoveMethod { get; private set; }

    public override bool IsStatic => _isStatic;

    public override string MetadataName => _metadataName;

    public bool IsAutoEvent => _backingField is not null;
    internal bool IsPartialMember => _isPartialDefinition || _isPartialImplementation;
    internal bool HasPartialDefinition => _isPartialDefinition;
    internal bool HasPartialImplementation => _isPartialImplementation;

    public SourceFieldSymbol? BackingField => _backingField;

    internal bool IsDeclaredInExtension => _declaredInExtension;

    internal TypeSymbol? ExtensionReceiverType => _extensionReceiverType;

    internal void SetAccessors(MethodSymbol? addMethod, MethodSymbol? removeMethod)
    {
        AddMethod = addMethod;
        RemoveMethod = removeMethod;
    }

    internal void SetBackingField(SourceFieldSymbol backingField)
    {
        _backingField = backingField;
        backingField.SetAssociatedEvent(this);
    }

    internal void MarkDeclaredInExtension(TypeSymbol? receiverType)
    {
        _declaredInExtension = true;
        _extensionReceiverType = receiverType;
    }

    internal void MarkAsPartialDefinition()
    {
        _isPartialDefinition = true;
    }

    internal void MarkAsPartialImplementation()
    {
        _isPartialImplementation = true;
    }

    public override void Accept(SymbolVisitor visitor)
    {
        visitor.DefaultVisit(this);
    }

    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor)
    {
        return visitor.DefaultVisit(this);
    }
}
