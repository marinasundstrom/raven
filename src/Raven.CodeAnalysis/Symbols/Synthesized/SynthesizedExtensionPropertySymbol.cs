using System;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Symbols;

sealed partial class SynthesizedExtensionPropertySymbol : Symbol, IPropertySymbol
{
    private ITypeSymbol? _type;
    private string? _name;

    public SynthesizedExtensionPropertySymbol(
        INamedTypeSymbol containingType,
        Location[] locations,
        SyntaxReference[] declaringReferences,
        IMethodSymbol? getMethod = null,
        IMethodSymbol? setMethod = null,
        string? name = null)
        : base(containingType, containingType, containingType.ContainingNamespace, locations, declaringReferences)
    {
        GetMethod = getMethod;
        SetMethod = setMethod;
        _name = name;
    }

    public override SymbolKind Kind => SymbolKind.Property;

    public override string Name
    {
        get
        {
            if (_name is not null)
                return _name;

            var method = GetMethod ?? SetMethod;
            if (method is null)
            {
                // If accessors haven't been attached yet, fall back to the explicitly provided name.
                // If that wasn't provided either, return an empty string to avoid null/invalid metadata names.
                return string.Empty;
            }

            _name = GetPropertyNameFromAccessor(method.Name);
            return _name;
        }
    }

    public ITypeSymbol Type
    {
        get
        {
            if (_type is not null)
                return _type;

            if (GetMethod is not null)
            {
                _type = GetMethod.ReturnType;
                return _type;
            }

            // Setter-only: property type is the "value" parameter (last parameter).
            var set = SetMethod;
            if (set is null)
                throw new InvalidOperationException("Synthesized extension property has no accessors attached.");

            if (set.Parameters.IsDefaultOrEmpty)
                throw new InvalidOperationException("Setter-only synthesized property has no parameters to determine the value type.");

            _type = set.Parameters[^1].Type;
            return _type;
        }
    }

    public IMethodSymbol? GetMethod { get; internal set; }
    public IMethodSymbol? SetMethod { get; internal set; }

    public IPropertySymbol OriginalDefinition => this;

    public bool IsRequired => false;

    public override Accessibility DeclaredAccessibility =>
        GetMethod?.DeclaredAccessibility ??
        SetMethod?.DeclaredAccessibility ??
        Accessibility.NotApplicable;

    public override bool IsStatic => false;

    public bool IsIndexer => false;

    public bool IsExtensionProperty => true;

    public ImmutableArray<IPropertySymbol> ExplicitInterfaceImplementations => ImmutableArray<IPropertySymbol>.Empty;

    public override void Accept(SymbolVisitor visitor) => visitor.VisitProperty(this);

    public override TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.VisitProperty(this);

    private static string GetPropertyNameFromAccessor(string accessorName)
    {
        // Handle explicit interface prefix: "IFoo.get_Bar"
        var dot = accessorName.LastIndexOf('.');
        if (dot >= 0 && dot + 1 < accessorName.Length)
            accessorName = accessorName[(dot + 1)..];

        const string getPrefix = "get_";
        const string setPrefix = "set_";

        if (accessorName.StartsWith(getPrefix, StringComparison.Ordinal))
            return accessorName[getPrefix.Length..];

        if (accessorName.StartsWith(setPrefix, StringComparison.Ordinal))
            return accessorName[setPrefix.Length..];

        // Fallback: return full name (or strip last '_' segment if you really want).
        return accessorName;
    }

    internal void SetAccessors(IMethodSymbol? getMethod, IMethodSymbol? setMethod)
    {
        GetMethod = getMethod;
        SetMethod = setMethod;

        // Reset caches so Name/Type reflect the newly attached accessors.
        _name = null;
        _type = null;
    }

    public IPropertySymbol Construct(params ITypeSymbol[] typeArguments)
    {
        var constructed = new SynthesizedExtensionPropertySymbol(ContainingType, [], [], name: _name);
        constructed.SetAccessors(
            GetMethod?.Construct(typeArguments),
            SetMethod?.Construct(typeArguments));
        return constructed;
    }
}
