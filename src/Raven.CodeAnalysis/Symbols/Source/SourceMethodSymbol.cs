using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Symbols;

internal partial class SourceMethodSymbol : SourceSymbol, IMethodSymbol
{
    private IEnumerable<SourceParameterSymbol> _parameters;

    public SourceMethodSymbol(string name, ITypeSymbol returnType, ImmutableArray<SourceParameterSymbol> parameters, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations, SyntaxReference[] declaringSyntaxReferences, bool isStatic = true, MethodKind methodKind = MethodKind.Ordinary, bool isVirtual = false, bool isOverride = false)
            : base(SymbolKind.Method, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
        ReturnType = returnType;
        _parameters = parameters;

        IsStatic = isStatic;

        MethodKind = methodKind;

        IsOverride = isOverride;
        IsVirtual = isVirtual || isOverride;
    }

    public ITypeSymbol ReturnType { get; }

    public ImmutableArray<IParameterSymbol> Parameters => _parameters.OfType<IParameterSymbol>().ToImmutableArray();

    public bool IsConstructor => MethodKind is MethodKind.Constructor or MethodKind.NamedConstructor;

    public bool IsNamedConstructor => MethodKind is MethodKind.NamedConstructor;

    public override bool IsStatic { get; }

    public MethodKind MethodKind { get; }

    public IMethodSymbol? OriginalDefinition { get; }

    public bool IsAbstract { get; }

    public bool IsAsync { get; }

    public bool IsCheckedBuiltin { get; }

    public bool IsDefinition { get; }

    public bool IsExtensionMethod { get; }

    public bool IsExtern { get; }

    public bool IsGenericMethod { get; }

    public bool IsOverride { get; }

    public bool IsReadOnly { get; }

    public bool IsSealed { get; }

    public bool IsVirtual { get; }

    public IMethodSymbol? OverriddenMethod { get; private set; }

    public void SetParameters(IEnumerable<SourceParameterSymbol> parameters) => _parameters = parameters;

    internal void SetOverriddenMethod(IMethodSymbol overriddenMethod) => OverriddenMethod = overriddenMethod;
}