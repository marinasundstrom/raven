using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class ConstructedExtensionPropertySymbol : IPropertySymbol
{
    private readonly IPropertySymbol _definition;
    private readonly IMethodSymbol? _getMethod;
    private readonly IMethodSymbol? _setMethod;
    private readonly ITypeSymbol _type;

    public ConstructedExtensionPropertySymbol(
        IPropertySymbol definition,
        IMethodSymbol? getMethod,
        IMethodSymbol? setMethod)
    {
        _definition = definition;
        _getMethod = getMethod;
        _setMethod = setMethod;
        _type = getMethod?.ReturnType
            ?? (setMethod?.Parameters.Length > 0 ? setMethod.Parameters[^1].Type : null)
            ?? definition.Type;
    }

    public string Name => _definition.Name;
    public ITypeSymbol Type => _type;
    public ISymbol ContainingSymbol => _definition.ContainingSymbol!;
    public IPropertySymbol? OriginalDefinition => _definition.OriginalDefinition ?? _definition;
    public IMethodSymbol? GetMethod => _getMethod;
    public IMethodSymbol? SetMethod => _setMethod;
    public bool IsIndexer => _definition.IsIndexer;
    public SymbolKind Kind => _definition.Kind;
    public string MetadataName => _definition.MetadataName;
    public IAssemblySymbol? ContainingAssembly => _definition.ContainingAssembly;
    public IModuleSymbol? ContainingModule => _definition.ContainingModule;
    public INamedTypeSymbol? ContainingType => _definition.ContainingType;
    public INamespaceSymbol? ContainingNamespace => _definition.ContainingNamespace;
    public ImmutableArray<Location> Locations => _definition.Locations;
    public Accessibility DeclaredAccessibility => _definition.DeclaredAccessibility;
    public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences => _definition.DeclaringSyntaxReferences;
    public bool IsImplicitlyDeclared => _definition.IsImplicitlyDeclared;
    public bool IsStatic => _definition.IsStatic;
    public ISymbol UnderlyingSymbol => this;
    public bool IsAlias => false;

    public ImmutableArray<IPropertySymbol> ExplicitInterfaceImplementations =>
        _definition.ExplicitInterfaceImplementations;

    public ImmutableArray<AttributeData> GetAttributes() => _definition.GetAttributes();

    public void Accept(SymbolVisitor visitor) => visitor.VisitProperty(this);

    public TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitProperty(this);

    public bool Equals(ISymbol? other, SymbolEqualityComparer comparer) => comparer.Equals(this, other);

    public bool Equals(ISymbol? other) => SymbolEqualityComparer.Default.Equals(this, other);
}
