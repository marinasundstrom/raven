using System;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Documentation;

namespace Raven.CodeAnalysis.Symbols;

sealed partial class SynthesizedExtensionPropertySymbol : SymbolBase, PropertySymbol
{
    private TypeSymbol? _type;
    private readonly TypeSymbol? _declaredType;
    private string? _name;
    private readonly Func<Symbol, DocumentationComment?>? _documentationProvider;

    public SynthesizedExtensionPropertySymbol(
        NamedTypeSymbol containingType,
        Location[] locations,
        SyntaxReference[] declaringReferences,
        MethodSymbol? getMethod = null,
        MethodSymbol? setMethod = null,
        string? name = null,
        Func<Symbol, DocumentationComment?>? documentationProvider = null,
        TypeSymbol? declaredType = null)
        : base(containingType, containingType, containingType.ContainingNamespace, locations, declaringReferences)
    {
        GetMethod = getMethod;
        SetMethod = setMethod;
        _name = name;
        _documentationProvider = documentationProvider;
        _declaredType = declaredType;
    }

    public override DocumentationComment? GetDocumentationComment() =>
        _documentationProvider?.Invoke(this) ?? DocumentationCommentUtilities.GetMergedDocumentationComment(
            DeclaringSyntaxReferences, new ParseOptions());

    public override SymbolKind Kind => SymbolKind.Property;

    public override AssemblySymbol? ContainingAssembly => ContainingType?.ContainingAssembly;

    public override ModuleSymbol? ContainingModule => ContainingType?.ContainingModule;

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

    public TypeSymbol Type
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
            // Source declarations can be queried before accessors are attached,
            // or remain accessor-less during error recovery. Keep their known type.
            if (set is null)
                return _declaredType ?? throw new InvalidOperationException("Synthesized extension property has no accessors attached.");

            if (set.Parameters.IsDefaultOrEmpty)
                throw new InvalidOperationException("Setter-only synthesized property has no parameters to determine the value type.");

            _type = set.Parameters[^1].Type;
            return _type;
        }
    }

    public MethodSymbol? GetMethod { get; internal set; }
    public MethodSymbol? SetMethod { get; internal set; }

    public PropertySymbol OriginalDefinition => this;

    public bool IsRequired => false;

    public override Accessibility DeclaredAccessibility =>
        GetMethod?.DeclaredAccessibility ??
        SetMethod?.DeclaredAccessibility ??
        Accessibility.NotApplicable;

    public override bool IsStatic => false;

    public bool IsIndexer => Name == "Item" && (GetMethod?.Parameters.Length > 1 || SetMethod?.Parameters.Length > 2);

    public ImmutableArray<ParameterSymbol> Parameters => !IsIndexer ? [] :
        GetMethod is { } getter ? getter.Parameters.RemoveAt(0) : SetMethod!.Parameters.RemoveAt(SetMethod.Parameters.Length - 1).RemoveAt(0);

    public bool IsExtensionProperty => true;

    public ImmutableArray<PropertySymbol> ExplicitInterfaceImplementations => ImmutableArray<PropertySymbol>.Empty;

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

    internal void SetAccessors(MethodSymbol? getMethod, MethodSymbol? setMethod)
    {
        GetMethod = getMethod;
        SetMethod = setMethod;

        // Reset caches so Name/Type reflect the newly attached accessors.
        if (getMethod is not null || setMethod is not null)
            _name = null;
        _type = null;
    }

    public PropertySymbol Construct(params TypeSymbol[] typeArguments)
    {
        var constructed = new SynthesizedExtensionPropertySymbol(ContainingType, [], [], name: _name);
        constructed.SetAccessors(
            GetMethod?.Construct(typeArguments),
            SetMethod?.Construct(typeArguments));
        return constructed;
    }
}
