namespace Raven.CodeAnalysis.Symbols;

internal sealed class SourceEventSymbol : SourceSymbol, IEventSymbol
{
    private readonly bool _isStatic;
    private readonly string _metadataName;
    private SourceFieldSymbol? _backingField;
    private bool _declaredInExtension;
    private ITypeSymbol? _extensionReceiverType;
    private bool _isPartialDefinition;
    private bool _isPartialImplementation;

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
    internal bool IsPartialMember => _isPartialDefinition || _isPartialImplementation;
    internal bool HasPartialDefinition => _isPartialDefinition;
    internal bool HasPartialImplementation => _isPartialImplementation;

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

    internal void AddDeclaration(Location location, SyntaxReference reference, bool preferAsPrimary = false)
        => base.AddDeclaration(location, reference, preferAsPrimary);

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
