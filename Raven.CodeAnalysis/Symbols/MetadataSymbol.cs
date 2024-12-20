using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal abstract class MetadataSymbol : ISymbol
{
    protected MetadataSymbol(ISymbol containingSymbol,
        INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace,
        Location[] locations)
    {
        ContainingType = containingType;
        ContainingNamespace = containingNamespace;
        ContainingSymbol = containingSymbol;
        Locations = [.. locations];
    }

    public abstract SymbolKind Kind
    {
        get;
    }

    public abstract string Name
    {
        get;
    }

    public ISymbol? ContainingSymbol
    {
        get;
        private set;
    }

    public INamedTypeSymbol? ContainingType
    {
        get;
        private set;
    }

    public INamespaceSymbol? ContainingNamespace
    {
        get;
        private set;
    }

    public ImmutableArray<Location> Locations
    {
        get;
        private set;
    }

    public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences
    {
        get;
        private set;
    }
    
    public virtual bool IsImplicitlyDeclared => false;

    public bool Equals(ISymbol? other)
    {
        throw new NotImplementedException();
    }
}