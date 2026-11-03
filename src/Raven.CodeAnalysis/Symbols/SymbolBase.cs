using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Reflection;

using Raven.CodeAnalysis.Documentation;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Symbols;

[DebuggerDisplay("{GetDebuggerDisplay(), nq}")]
internal abstract class SymbolBase : Symbol
{
    internal virtual bool IsTerminalRuntimeCall => false;

    private readonly Accessibility _declaredAccessibility;
    private HashSet<string>? _fileScopedDeclarationPaths;

    protected SymbolBase(
        SymbolKind kind,
        string name,
        Symbol? containingSymbol,
        NamedTypeSymbol? containingType,
        NamespaceSymbol? containingNamespace,
        Location[] locations,
        SyntaxReference[] declaringSyntaxReferences,
        Accessibility declaredAccessibility = Accessibility.NotApplicable)
        : this(kind, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences, declaredAccessibility, addAsMember: true)
    {
    }

    protected SymbolBase(
        SymbolKind kind,
        string name,
        Symbol? containingSymbol,
        NamedTypeSymbol? containingType,
        NamespaceSymbol? containingNamespace,
        Location[] locations,
        SyntaxReference[] declaringSyntaxReferences,
        Accessibility declaredAccessibility,
        bool addAsMember)
        : this(containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences, declaredAccessibility, addAsMember: false)
    {
        Kind = kind;
        Name = name;

        if (addAsMember)
            AddAsMember(containingType, containingNamespace);
    }

    protected SymbolBase(
        Symbol? containingSymbol,
        NamedTypeSymbol? containingType,
        NamespaceSymbol? containingNamespace,
        Location[] locations,
        SyntaxReference[] declaringSyntaxReferences,
        Accessibility declaredAccessibility = Accessibility.NotApplicable,
        bool addAsMember = true)
    {
        _declaredAccessibility = declaredAccessibility;
        ContainingType = containingType;
        ContainingNamespace = containingNamespace;
        ContainingSymbol = containingSymbol;
        Locations = [.. locations];
        DeclaringSyntaxReferences = [.. declaringSyntaxReferences];

        if (addAsMember)
            AddAsMember(containingType, containingNamespace);
    }

    internal bool AddAsMember(NamedTypeSymbol? containingType, NamespaceSymbol? containingNamespace)
    {
        if (this is ParameterSymbol)
            return false;

        if (this is TypeParameterSymbol)
            return false;

        if (this is TypeSymbol or NamespaceSymbol)
        {
            if (this.ContainingType is null)
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
        }

        if (this is LocalSymbol or LabelSymbol)
            return false;

        if (containingType is SourceNamedTypeSymbol t)
        {
            t.AddMember(this);
        }
        else if (containingType is PENamedTypeSymbol t2)
        {
            t2.AddMember(this);
        }

        return true;
    }

    public virtual SymbolKind Kind
    {
        get;
    }

    public virtual string Name
    {
        get;
    } = string.Empty;

    public virtual string MetadataName => Name;

    public Symbol? ContainingSymbol
    {
        get;
        private set;
    }

    public virtual AssemblySymbol? ContainingAssembly
    {
        get;
    }

    public virtual ModuleSymbol? ContainingModule
    {
        get;
    }

    public NamedTypeSymbol? ContainingType
    {
        get;
        private set;
    }

    public NamespaceSymbol? ContainingNamespace
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

    internal bool IsFileScoped { get; private set; }

    public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences
    {
        get;
        protected set;
    }

    public virtual bool IsImplicitlyDeclared => false;

    public virtual bool IsStatic => false;

    public virtual Symbol UnderlyingSymbol => this;

    public virtual bool IsAlias => false;

    public virtual ImmutableArray<AttributeData> GetAttributes() => ImmutableArray<AttributeData>.Empty;

    public virtual DocumentationComment? GetDocumentationComment() => null;

    public virtual bool CanBeReferencedByName => this switch
    {
        NamespaceOrTypeSymbol => true,
        MethodSymbol => true,
        MacroDeclarationSymbol => true,
        EventSymbol => true,
        PropertySymbol property => !property.IsIndexer,
        FieldSymbol => true,
        ParameterSymbol => true,
        LocalSymbol => true,
        LabelSymbol => true,
        _ => false
    };

    public bool Equals(Symbol? other, SymbolEqualityComparer comparer)
    {
        return comparer.Equals(this, other);
    }

    public bool Equals(Symbol? other)
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

        return Equals(other, SymbolEqualityComparer.Default);
    }

    internal void MarkFileScoped(string? filePath)
    {
        IsFileScoped = true;
        (_fileScopedDeclarationPaths ??= new HashSet<string>(StringComparer.Ordinal))
            .Add(filePath ?? string.Empty);
    }

    internal bool IsAccessibleFromFile(string? filePath)
    {
        if (!IsFileScoped)
            return true;

        if (filePath is null)
            return false;

        return _fileScopedDeclarationPaths?.Contains(filePath) == true;
    }

    private string GetDebuggerDisplay()
    {
        try
        {
            if (this is NamespaceSymbol ns)
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

            if (this is AssemblySymbol or ModuleSymbol)
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

static class Foo
{
    public static T AddAsMember<T>(this T symbol)
        where T : PENamedTypeSymbol
    {
        symbol.AddAsMember(symbol.ContainingType, symbol.ContainingNamespace);
        return symbol;
    }

    public static T AddAsMember2<T>(this T symbol)
        where T : PETypeParameterSymbol
    {
        symbol.AddAsMember(symbol.ContainingType, symbol.ContainingNamespace);
        return symbol;
    }
}
