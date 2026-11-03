using System.Collections.Immutable;

using Raven.CodeAnalysis.Documentation;
namespace Raven.CodeAnalysis.Symbols;

internal sealed class NativeSelfPropertySymbol : PropertySymbol
{
    private readonly PropertySymbol _original;
    private readonly Compilation _compilation;
    private readonly TypeSymbol _implementingType;
    private TypeSymbol? _type;
    private MethodSymbol? _getMethod;
    private MethodSymbol? _setMethod;

    public NativeSelfPropertySymbol(Compilation compilation, TypeSymbol implementingType, PropertySymbol original)
    {
        _original = original;
        _compilation = compilation;
        _implementingType = implementingType;
    }

    public string Name => _original.Name;
    public TypeSymbol Type => _type ??= RuntimeSelfTypes.Substitute(_compilation, _original.Type, _implementingType);
    public Symbol ContainingSymbol => _original.ContainingType!;
    public PropertySymbol? OriginalDefinition => _original.OriginalDefinition ?? _original;
    public MethodSymbol? GetMethod => _original.GetMethod is null
        ? null
        : _getMethod ??= new NativeSelfMethodSymbol(_compilation, _implementingType, _original.GetMethod);
    public MethodSymbol? SetMethod => _original.SetMethod is null
        ? null
        : _setMethod ??= new NativeSelfMethodSymbol(_compilation, _implementingType, _original.SetMethod);
    public bool IsIndexer => _original.IsIndexer;
    public bool IsRequired => _original.IsRequired;
    public SymbolKind Kind => _original.Kind;
    public string MetadataName => _original.MetadataName;
    public AssemblySymbol? ContainingAssembly => _original.ContainingAssembly;
    public ModuleSymbol? ContainingModule => _original.ContainingModule;
    public NamedTypeSymbol? ContainingType => _original.ContainingType!;
    public NamespaceSymbol? ContainingNamespace => _original.ContainingNamespace;
    public ImmutableArray<Location> Locations => _original.Locations;
    public Accessibility DeclaredAccessibility => _original.DeclaredAccessibility;
    public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences => _original.DeclaringSyntaxReferences;
    public bool IsImplicitlyDeclared => _original.IsImplicitlyDeclared;
    public bool IsStatic => _original.IsStatic;
    public Symbol UnderlyingSymbol => this;
    public bool IsAlias => false;
    public ImmutableArray<AttributeData> GetAttributes() => _original.GetAttributes();
    public DocumentationComment? GetDocumentationComment() => _original.GetDocumentationComment();

    public ImmutableArray<PropertySymbol> ExplicitInterfaceImplementations => _original.ExplicitInterfaceImplementations;
    public void Accept(SymbolVisitor visitor) => visitor.VisitProperty(this);
    public TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitProperty(this);
    public bool Equals(Symbol? other, SymbolEqualityComparer comparer) => comparer.Equals(this, other);
    public bool Equals(Symbol? other) => SymbolEqualityComparer.Default.Equals(this, other);
}
