using System.Collections.Immutable;

using Raven.CodeAnalysis.Documentation;
namespace Raven.CodeAnalysis.Symbols;

internal sealed class NativeSelfPropertySymbol : IPropertySymbol
{
    private readonly IPropertySymbol _original;
    private readonly Compilation _compilation;
    private readonly ITypeSymbol _implementingType;
    private ITypeSymbol? _type;
    private IMethodSymbol? _getMethod;
    private IMethodSymbol? _setMethod;

    public NativeSelfPropertySymbol(Compilation compilation, ITypeSymbol implementingType, IPropertySymbol original)
    {
        _original = original;
        _compilation = compilation;
        _implementingType = implementingType;
    }

    public string Name => _original.Name;
    public ITypeSymbol Type => _type ??= RuntimeSelfTypes.Substitute(_compilation, _original.Type, _implementingType);
    public ISymbol ContainingSymbol => _original.ContainingType!;
    public IPropertySymbol? OriginalDefinition => _original.OriginalDefinition ?? _original;
    public IMethodSymbol? GetMethod => _original.GetMethod is null
        ? null
        : _getMethod ??= new NativeSelfMethodSymbol(_compilation, _implementingType, _original.GetMethod);
    public IMethodSymbol? SetMethod => _original.SetMethod is null
        ? null
        : _setMethod ??= new NativeSelfMethodSymbol(_compilation, _implementingType, _original.SetMethod);
    public bool IsIndexer => _original.IsIndexer;
    public bool IsRequired => _original.IsRequired;
    public SymbolKind Kind => _original.Kind;
    public string MetadataName => _original.MetadataName;
    public IAssemblySymbol? ContainingAssembly => _original.ContainingAssembly;
    public IModuleSymbol? ContainingModule => _original.ContainingModule;
    public INamedTypeSymbol? ContainingType => _original.ContainingType!;
    public INamespaceSymbol? ContainingNamespace => _original.ContainingNamespace;
    public ImmutableArray<Location> Locations => _original.Locations;
    public Accessibility DeclaredAccessibility => _original.DeclaredAccessibility;
    public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences => _original.DeclaringSyntaxReferences;
    public bool IsImplicitlyDeclared => _original.IsImplicitlyDeclared;
    public bool IsStatic => _original.IsStatic;
    public ISymbol UnderlyingSymbol => this;
    public bool IsAlias => false;
    public ImmutableArray<AttributeData> GetAttributes() => _original.GetAttributes();
    public DocumentationComment? GetDocumentationComment() => _original.GetDocumentationComment();

    public ImmutableArray<IPropertySymbol> ExplicitInterfaceImplementations => _original.ExplicitInterfaceImplementations;
    public void Accept(SymbolVisitor visitor) => visitor.VisitProperty(this);
    public TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitProperty(this);
    public bool Equals(ISymbol? other, SymbolEqualityComparer comparer) => comparer.Equals(this, other);
    public bool Equals(ISymbol? other) => SymbolEqualityComparer.Default.Equals(this, other);
}
