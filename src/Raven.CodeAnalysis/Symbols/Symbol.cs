using System.Collections.Immutable;
using System.Diagnostics;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Symbols;

[DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
internal abstract class Symbol : ISymbol
{
    protected Symbol(
        SymbolKind kind,
        string name,
        ISymbol containingSymbol,
        INamedTypeSymbol? containingType,
        INamespaceSymbol? containingNamespace,
        Location[] locations,
        SyntaxReference[] declaringSyntaxReferences)
        : this(containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
        Kind = kind;
        Name = name;
    }

    protected Symbol(
        ISymbol containingSymbol,
        INamedTypeSymbol? containingType,
        INamespaceSymbol? containingNamespace,
        Location[] locations,
        SyntaxReference[] declaringSyntaxReferences)
    {
        ContainingType = containingType;
        ContainingNamespace = containingNamespace;
        ContainingSymbol = containingSymbol;
        Locations = [.. locations];
        DeclaringSyntaxReferences = [.. declaringSyntaxReferences];

        if (this is ITypeSymbol or INamespaceSymbol)
        {
            if (containingNamespace is NamespaceSymbol ns)
            {
                ns.AddMember(this);
            }
        }

        if (containingType is SourceTypeSymbol t)
        {
            t.AddMember(this);
        }
        else if (containingType is MetadataTypeSymbol t2)
        {
            t2.AddMember(this);
        }
    }


    public virtual SymbolKind Kind
    {
        get;
    }

    public virtual Compilation Compilation
    {
        get;
        protected set;
    }

    public virtual string Name
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

    public virtual Accessibility DeclaredAccessibility { get; }

    public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences
    {
        get;
        private set;
    }

    public virtual bool IsImplicitlyDeclared => false;

    public virtual bool IsStatic => false;

    public bool Equals(ISymbol? other, SymbolEqualityComparer comparer)
    {
        return comparer.Equals(this, other);
    }

    public bool Equals(ISymbol? other)
    {
        if (other is null)
        {
            return false;
        }

        if (ReferenceEquals(this, other))
        {
            return true;
        }

        if (other.GetType() != GetType())
        {
            return false;
        }

        return Equals((ISymbol)other);
    }

    private string GetDebuggerDisplay()
    {
        if (this is INamespaceSymbol ns && ns.IsGlobalNamespace)
        {
            return "<global>";
        }

        return this.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
    }

    public override string ToString()
    {
        return this.ToDisplayString();
    }
}
