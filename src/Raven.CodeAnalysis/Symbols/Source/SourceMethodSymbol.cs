using System;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Symbols;

internal partial class SourceMethodSymbol : SourceSymbol, IMethodSymbol
{
    private IEnumerable<SourceParameterSymbol> _parameters;
    private ITypeSymbol _returnType;
    private ImmutableArray<ITypeParameterSymbol> _typeParameters = ImmutableArray<ITypeParameterSymbol>.Empty;
    private ImmutableArray<ITypeSymbol> _typeArguments = ImmutableArray<ITypeSymbol>.Empty;
    private bool _isOverride;
    private bool _isVirtual;
    private bool _isSealed;

    public SourceMethodSymbol(
        string name,
        ITypeSymbol returnType,
        ImmutableArray<SourceParameterSymbol> parameters,
        ISymbol containingSymbol,
        INamedTypeSymbol? containingType,
        INamespaceSymbol? containingNamespace,
        Location[] locations,
        SyntaxReference[] declaringSyntaxReferences,
        bool isStatic = true,
        MethodKind methodKind = MethodKind.Ordinary,
        bool isVirtual = false,
        bool isOverride = false,
        bool isSealed = false,
        Accessibility declaredAccessibility = Accessibility.NotApplicable)
            : base(SymbolKind.Method, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences, declaredAccessibility)
    {
        _returnType = returnType;
        _parameters = parameters;

        IsStatic = isStatic;

        MethodKind = methodKind;

        _isOverride = isOverride;
        _isVirtual = isVirtual || isOverride;
        _isSealed = isSealed;
    }

    public ITypeSymbol ReturnType => _returnType;

    public ImmutableArray<IParameterSymbol> Parameters => _parameters.OfType<IParameterSymbol>().ToImmutableArray();

    public bool IsConstructor => MethodKind is MethodKind.Constructor or MethodKind.NamedConstructor;

    public bool IsNamedConstructor => MethodKind is MethodKind.NamedConstructor;

    public override bool IsStatic { get; }

    public MethodKind MethodKind { get; }

    public IMethodSymbol? OriginalDefinition => this;

    public bool IsAbstract { get; }

    public bool IsAsync { get; }

    public bool IsCheckedBuiltin { get; }

    public bool IsDefinition { get; }

    public bool IsExtensionMethod { get; }

    public bool IsExtern { get; }

    public bool IsGenericMethod => !_typeParameters.IsDefaultOrEmpty && _typeParameters.Length > 0;

    public bool IsOverride => _isOverride;

    public bool IsReadOnly { get; }

    public bool IsSealed => _isSealed;

    public bool IsVirtual => _isVirtual;

    public IMethodSymbol? OverriddenMethod { get; private set; }

    public ImmutableArray<IMethodSymbol> ExplicitInterfaceImplementations { get; private set; } = ImmutableArray<IMethodSymbol>.Empty;

    public ImmutableArray<ITypeParameterSymbol> TypeParameters => _typeParameters;

    public ImmutableArray<ITypeSymbol> TypeArguments => _typeArguments;

    public IMethodSymbol? ConstructedFrom => this;

    public void SetParameters(IEnumerable<SourceParameterSymbol> parameters) => _parameters = parameters;

    internal void SetOverriddenMethod(IMethodSymbol overriddenMethod) => OverriddenMethod = overriddenMethod;

    internal void SetExplicitInterfaceImplementations(ImmutableArray<IMethodSymbol> implementations) => ExplicitInterfaceImplementations = implementations;

    public BoundObjectCreationExpression? ConstructorInitializer { get; private set; }

    public bool HasConstructorInitializerSyntax { get; private set; }

    internal void MarkConstructorInitializerSyntax() => HasConstructorInitializerSyntax = true;

    internal void SetConstructorInitializer(BoundObjectCreationExpression? initializer) => ConstructorInitializer = initializer;

    internal void SetTypeParameters(IEnumerable<ITypeParameterSymbol> typeParameters)
    {
        _typeParameters = typeParameters.ToImmutableArray();
        _typeArguments = _typeParameters.IsDefaultOrEmpty
            ? ImmutableArray<ITypeSymbol>.Empty
            : _typeParameters.Select(static tp => (ITypeSymbol)tp).ToImmutableArray();
    }

    internal void SetReturnType(ITypeSymbol returnType) => _returnType = returnType;

    internal void UpdateModifiers(bool isVirtual, bool isOverride, bool isSealed)
    {
        _isOverride = isOverride;
        _isVirtual = isVirtual || isOverride;
        _isSealed = isSealed;
    }

    public IMethodSymbol Construct(params ITypeSymbol[] typeArguments)
    {
        if (typeArguments is null)
            throw new ArgumentNullException(nameof(typeArguments));

        return new ConstructedMethodSymbol(this, typeArguments.ToImmutableArray());
    }
}
