using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class ConstructedNamedTypeSymbol : ITypeSymbol, INamedTypeSymbol
{
    private readonly PENamedTypeSymbol _originalDefinition;
    public ImmutableArray<ITypeSymbol> TypeArguments { get; }

    public ConstructedNamedTypeSymbol(PENamedTypeSymbol originalDefinition, ImmutableArray<ITypeSymbol> typeArguments)
    {
        ConstructedFrom = originalDefinition;
        _originalDefinition = originalDefinition;
        TypeArguments = typeArguments;
    }

    public string Name => _originalDefinition.Name;
    public string MetadataName => _originalDefinition.Name; // Or include arity info
    public SymbolKind Kind => _originalDefinition.Kind;
    public TypeKind TypeKind => _originalDefinition.TypeKind;
    public SpecialType SpecialType => _originalDefinition.SpecialType;

    public bool IsNamespace => false;
    public bool IsType => true;

    public INamedTypeSymbol? ContainingType => _originalDefinition.ContainingType;
    public INamespaceSymbol? ContainingNamespace => _originalDefinition.ContainingNamespace;
    public ISymbol? ContainingSymbol => _originalDefinition.ContainingSymbol;
    public IAssemblySymbol? ContainingAssembly => _originalDefinition.ContainingAssembly;
    public IModuleSymbol? ContainingModule => _originalDefinition.ContainingModule;

    public Accessibility DeclaredAccessibility => Accessibility.Public;
    public bool IsStatic => false;
    public bool IsImplicitlyDeclared => true;
    public bool CanBeReferencedByName => true;

    public ImmutableArray<Location> Locations => _originalDefinition.Locations;
    public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences => [];

    public int Arity => TypeArguments.Length;

    public ImmutableArray<ITypeSymbol> GetTypeArguments() => TypeArguments;

    public ITypeSymbol? OriginalDefinition => _originalDefinition;
    public INamedTypeSymbol? BaseType => _originalDefinition.BaseType;

    public ImmutableArray<ISymbol> GetMembers() => _originalDefinition.GetMembers();
    public ImmutableArray<ISymbol> GetMembers(string name) => _originalDefinition.GetMembers(name);

    public bool IsMemberDefined(string name, out ISymbol? symbol)
        => _originalDefinition.IsMemberDefined(name, out symbol);

    public ITypeSymbol? LookupType(string name)
        => _originalDefinition.LookupType(name);

    public ImmutableArray<ITypeParameterSymbol> TypeParameters =>
        _originalDefinition.TypeParameters;

    public ITypeSymbol ConstructedFrom { get; }

    public bool IsGenericType => ConstructedFrom is INamedTypeSymbol ns && ns.IsGenericType;

    public bool IsUnboundGenericType => ConstructedFrom is INamedTypeSymbol ns && !ns.IsUnboundGenericType;

    public ImmutableArray<IMethodSymbol> Constructors => _originalDefinition.Constructors;

    public IMethodSymbol? StaticConstructor => _originalDefinition.StaticConstructor;

    public void Accept(SymbolVisitor visitor)
        => visitor.VisitNamedType(this);

    public TResult Accept<TResult>(SymbolVisitor<TResult> visitor)
        => visitor.VisitNamedType(this);

    public bool Equals(ISymbol? other, SymbolEqualityComparer comparer)
        => comparer.Equals(this, other);

    public bool Equals(ISymbol? other)
        => SymbolEqualityComparer.Default.Equals(this, other);

    public ITypeSymbol Construct(ITypeSymbol[] typeArguments)
    {
        throw new NotSupportedException();
    }
}