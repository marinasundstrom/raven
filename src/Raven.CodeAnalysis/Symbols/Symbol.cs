using System.Collections.Immutable;
using System.Diagnostics;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Symbols;

[DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
internal abstract class Symbol : ISymbol
{
    private readonly Accessibility _declaredAccessibility;

    protected Symbol(
        SymbolKind kind,
        string name,
        ISymbol containingSymbol,
        INamedTypeSymbol? containingType,
        INamespaceSymbol? containingNamespace,
        Location[] locations,
        SyntaxReference[] declaringSyntaxReferences,
        Accessibility declaredAccessibility = Accessibility.NotApplicable)
        : this(containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences, declaredAccessibility)
    {
        Kind = kind;
        Name = name;
    }

    protected Symbol(
        ISymbol containingSymbol,
        INamedTypeSymbol? containingType,
        INamespaceSymbol? containingNamespace,
        Location[] locations,
        SyntaxReference[] declaringSyntaxReferences,
        Accessibility declaredAccessibility = Accessibility.NotApplicable)
    {
        _declaredAccessibility = declaredAccessibility;
        ContainingType = containingType;
        ContainingNamespace = containingNamespace;
        ContainingSymbol = containingSymbol;
        Locations = [.. locations];
        DeclaringSyntaxReferences = [.. declaringSyntaxReferences];

        if (this is IParameterSymbol)
            return;

        if (this is ITypeParameterSymbol)
            return;

        if (this is ITypeSymbol or INamespaceSymbol)
        {
            if (containingNamespace is SourceNamespaceSymbol ns)
            {
                ns.AddMember(this);
            }

            if (containingNamespace is PENamespaceSymbol ns2)
            {
                ns2.AddMember(this);
            }
        }

        if (this is ILocalSymbol or ILabelSymbol)
            return;

        if (containingType is SourceNamedTypeSymbol t)
        {
            t.AddMember(this);
        }
        else if (containingType is PENamedTypeSymbol t2)
        {
            t2.AddMember(this);
        }
    }


    public virtual SymbolKind Kind
    {
        get;
    }

    public virtual string Name
    {
        get;
    }

    public virtual string MetadataName
    {
        get;
    }

    public ISymbol? ContainingSymbol
    {
        get;
        private set;
    }

    public virtual IAssemblySymbol ContainingAssembly
    {
        get;
    }

    public virtual IModuleSymbol ContainingModule
    {
        get;
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
        protected set;
    }

    public virtual Accessibility DeclaredAccessibility => _declaredAccessibility;

    public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences
    {
        get;
        protected set;
    }

    public virtual bool IsImplicitlyDeclared => false;

    public virtual bool IsStatic => false;

    public virtual ISymbol UnderlyingSymbol => this;

    public virtual bool IsAlias => false;

    public virtual ImmutableArray<AttributeData> GetAttributes() => ImmutableArray<AttributeData>.Empty;

    public virtual bool CanBeReferencedByName { get; } = false;

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
        try
        {
            if (this is INamespaceSymbol ns)
            {
                if (ns.IsGlobalNamespace)
                {
                    if (ns is MergedNamespaceSymbol)
                    {
                        return $"{Kind}: <global> (Merged)";
                    }

                    return $"{Kind}: <global>";
                }

                if (ns is MergedNamespaceSymbol)
                {
                    return $"{Kind}: {this.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)} (Merged)";
                }
            }

            if (this is IAssemblySymbol or IModuleSymbol)
                return $"{Kind}: {Name}";

            return $"{Kind}: {this.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)}";
        }
        catch (Exception exc)
        {
            return $"{Kind}: <{exc.GetType().Name}>";
        }
    }

    public override string ToString()
    {
        return this.ToDisplayString();
    }

    public abstract void Accept(CodeAnalysis.SymbolVisitor visitor);

    public abstract TResult Accept<TResult>(CodeAnalysis.SymbolVisitor<TResult> visitor);
}
