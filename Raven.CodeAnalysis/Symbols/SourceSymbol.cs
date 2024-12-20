using System.Collections.Immutable;
using System.Runtime.CompilerServices;

using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Symbols;

internal abstract class SourceSymbol : ISymbol
{
    protected SourceSymbol(SymbolKind kind, string name, ISymbol containingSymbol,
        INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace,
        Location[] locations, SyntaxReference[] declaringSyntaxReferences)
    {
        Kind = kind;
        Name = name;
        ContainingType = containingType;
        ContainingNamespace = containingNamespace;
        ContainingSymbol = containingSymbol;
        Locations = [.. locations];
        DeclaringSyntaxReferences = [.. declaringSyntaxReferences];
    }

    public SymbolKind Kind
    {
        get;
        private set;
    }

    public string Name
    {
        get;
        private set;
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
    
    public virtual  string ToDisplayString()
    {
        return Name;
    }

    public bool Equals(ISymbol? other, SymbolEqualityComparer comparer)
    {
        return comparer.Equals(this, other);
    }

    public bool Equals(ISymbol? other)
    {
        throw new NotImplementedException();
    }
}