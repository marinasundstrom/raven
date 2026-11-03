using System.Collections.Immutable;

using Raven.CodeAnalysis.Documentation;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class ConstructedExtensionPropertySymbol : PropertySymbol
{
    private readonly PropertySymbol _definition;
    private readonly MethodSymbol? _getMethod;
    private readonly MethodSymbol? _setMethod;
    private readonly TypeSymbol _type;

    public ConstructedExtensionPropertySymbol(
        PropertySymbol definition,
        MethodSymbol? getMethod,
        MethodSymbol? setMethod)
    {
        _definition = definition;
        _getMethod = getMethod;
        _setMethod = setMethod;
        _type = getMethod?.ReturnType
            ?? (setMethod?.Parameters.Length > 0 ? setMethod.Parameters[^1].Type : null)
            ?? definition.Type;
    }

    public string Name => _definition.Name;
    public TypeSymbol Type => _type;
    public Symbol ContainingSymbol => _definition.ContainingSymbol!;
    public PropertySymbol? OriginalDefinition => _definition.OriginalDefinition ?? _definition;
    public MethodSymbol? GetMethod => _getMethod;
    public MethodSymbol? SetMethod => _setMethod;
    public bool IsExtensionProperty => _definition.IsExtensionProperty;
    public ImmutableArray<ParameterSymbol> Parameters => !IsIndexer ? [] :
        GetMethod is { } getter ? getter.Parameters.RemoveAt(0) : SetMethod!.Parameters.RemoveAt(SetMethod.Parameters.Length - 1).RemoveAt(0);
    public bool IsIndexer => _definition.IsIndexer;
    public bool IsRequired => _definition.IsRequired;
    public SymbolKind Kind => _definition.Kind;
    public string MetadataName => _definition.MetadataName;
    public AssemblySymbol? ContainingAssembly => _definition.ContainingAssembly;
    public ModuleSymbol? ContainingModule => _definition.ContainingModule;
    public NamedTypeSymbol? ContainingType => _definition.ContainingType;
    public NamespaceSymbol? ContainingNamespace => _definition.ContainingNamespace;
    public ImmutableArray<Location> Locations => _definition.Locations;
    public Accessibility DeclaredAccessibility => _definition.DeclaredAccessibility;
    public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences => _definition.DeclaringSyntaxReferences;
    public bool IsImplicitlyDeclared => _definition.IsImplicitlyDeclared;
    public bool IsStatic => _definition.IsStatic;
    public Symbol UnderlyingSymbol => this;
    public bool IsAlias => false;

    public ImmutableArray<PropertySymbol> ExplicitInterfaceImplementations =>
        _definition.ExplicitInterfaceImplementations;

    public ImmutableArray<AttributeData> GetAttributes() => _definition.GetAttributes();
    public DocumentationComment? GetDocumentationComment() => _definition.GetDocumentationComment();

    public void Accept(SymbolVisitor visitor) => visitor.VisitProperty(this);

    public TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitProperty(this);

    public bool Equals(Symbol? other, SymbolEqualityComparer comparer) => comparer.Equals(this, other);

    public bool Equals(Symbol? other) => SymbolEqualityComparer.Default.Equals(this, other);
}
