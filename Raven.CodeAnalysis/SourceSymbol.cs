using System.Collections.Immutable;
using System.Runtime.CompilerServices;

using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis;

public abstract class SourceSymbol : ISymbol
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

    public bool Equals(ISymbol? other)
    {
        throw new NotImplementedException();
    }
}

public class SourceMethodSymbol : SourceSymbol, IMethodSymbol
{
    public SourceMethodSymbol(string name, ITypeSymbol returnType, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations, SyntaxReference[] declaringSyntaxReferences)
            : base(SymbolKind.Method, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
        ReturnType = returnType;
    }

    public ITypeSymbol ReturnType { get; }
}

public class SourceNamespaceSymbol : SourceSymbol, INamespaceSymbol
{
    public SourceNamespaceSymbol(string name, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations, SyntaxReference[] declaringSyntaxReferences)
        : base(SymbolKind.Namespace, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
    }
}

public class SourceLocalSymbol : SourceSymbol, ILocalSymbol
{
    public SourceLocalSymbol(string name, ITypeSymbol type, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations, SyntaxReference[] declaringSyntaxReferences)
        : base(SymbolKind.Local, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
        Type = type;
    }

    public ITypeSymbol Type { get; }
}