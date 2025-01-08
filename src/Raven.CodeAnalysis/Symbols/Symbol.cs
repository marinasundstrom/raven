using System.Collections.Immutable;
using System.Diagnostics;
using System.Reflection;
using System.Text;

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

    public string ToDisplayString(SymbolDisplayFormat format = default!)
    {
        if (format is null)
            format = SymbolDisplayFormat.CSharpErrorMessageFormat;

        var result = new StringBuilder();

        if (this is ILocalSymbol localSymbol)
        {
            if (format.LocalOptions == SymbolDisplayLocalOptions.IncludeType)
            {
                var localType = localSymbol.Type.ToDisplayString(format);
                result.Append($"{localType} ");
            }

            result.Append(localSymbol.Name);
            return result.ToString();
        }

        // Example: Include namespace and type qualification
        if (format.TypeQualificationStyle == SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces)
        {
            // Assume `Namespace` and `ContainingType` are properties of the symbol
            if (ContainingNamespace.ContainingNamespace is not null)
            {
                var ns = GetFullNamespace(this);
                result.Append(ns).Append(".");
            }

            if (ContainingType is not null)
            {
                var type = GetFullType(this);
                result.Append(type).Append(".");
            }
        }

        // Append the symbol's name
        result.Append(Name); // Assume `Name` is a property of the symbol

        if (this is INamedTypeSymbol typeSymbol)
        {
            // Handle generics
            if ((format.GenericsOptions & SymbolDisplayGenericsOptions.IncludeTypeParameters) != 0 &&
                typeSymbol.TypeParameters != null)
            {
                result.Append("<");
                result.Append(string.Join(", ",
                    typeSymbol.TypeParameters)); // Assume `TypeParameters` is a list of generic type names
                result.Append(">");
            }
        }

        if (this is IMethodSymbol methodSymbol)
        {
            // Handle method parameters (if the symbol is a method)
            if (format.DelegateStyle == SymbolDisplayDelegateStyle.NameAndSignature)
            {
                result.Append("(");
                result.Append(string.Join(", ", methodSymbol.Parameters.Select(p => FormatParameter(p, format))));
                result.Append(")");
            }
        }

        // Example for accessibility modifiers
        if ((format.MemberOptions & SymbolDisplayMemberOptions.IncludeAccessibility) != 0)
        {
            if (this.DeclaredAccessibility is not Accessibility.NotApplicable)
            {
                result.Insert(0,
                    this.DeclaredAccessibility.ToString().ToLower() + " "); // Assume `Accessibility` is a property
            }
        }

        // Handle miscellaneous options
        if ((format.MiscellaneousOptions & SymbolDisplayMiscellaneousOptions.EscapeIdentifiers) != 0)
        {
            return EscapeIdentifier(result.ToString());
        }

        return result.ToString();
    }

    // Helper method to format a parameter
    private string FormatParameter(IParameterSymbol parameter, SymbolDisplayFormat format)
    {
        var sb = new StringBuilder();

        if ((format.ParameterOptions & SymbolDisplayParameterOptions.IncludeModifiers) != 0)
        {
            //if (parameter.IsRef) sb.Append("ref ");
            //if (parameter.IsOut) sb.Append("out ");
        }

        if ((format.ParameterOptions & SymbolDisplayParameterOptions.IncludeType) != 0)
        {
            sb.Append(parameter.Type); // Assume `Type` is a property of the parameter
            sb.Append(" ");
        }

        if ((format.ParameterOptions & SymbolDisplayParameterOptions.IncludeName) != 0)
        {
            sb.Append(parameter.Name); // Assume `Name` is a property of the parameter
        }

        return sb.ToString();
    }

    // Helper method to escape identifiers
    private string EscapeIdentifier(string identifier)
    {
        // Replace any special characters or keywords with escaped versions
        return identifier; //identifier.Replace("<", "&lt;").Replace(">", "&gt;");
    }

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

    private string GetFullNamespace(ISymbol symbol)
    {
        var namespaces = new List<string>();
        var currentNamespace = symbol.ContainingNamespace;

        // Traverse all containing namespaces
        while (currentNamespace is not null && !currentNamespace.IsGlobalNamespace)
        {
            namespaces.Insert(0, currentNamespace.Name);
            currentNamespace = currentNamespace.ContainingNamespace;
        }

        // Join namespaces with '.'
        return string.Join(".", namespaces);
    }

    private string GetFullType(ISymbol symbol)
    {
        var types = new List<string>();
        var currentType = symbol.ContainingType;

        // Traverse all containing types
        while (currentType is not null)
        {
            types.Insert(0, currentType.Name);
            currentType = currentType.ContainingType;
        }

        // Join types with '.'
        return string.Join(".", types);
    }

    private string GetDebuggerDisplay() => ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);

    public override string ToString()
    {
        return ToDisplayString();
    }
}