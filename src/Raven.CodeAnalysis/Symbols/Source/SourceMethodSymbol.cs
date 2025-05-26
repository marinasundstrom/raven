using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal partial class SourceMethodSymbol : SourceSymbol, IMethodSymbol
{
    private bool _isStatic;
    private IEnumerable<SourceParameterSymbol> _parameters;

    public SourceMethodSymbol(string name, ITypeSymbol returnType, ImmutableArray<SourceParameterSymbol> parameters, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations, SyntaxReference[] declaringSyntaxReferences, bool isStatic = true)
            : base(SymbolKind.Method, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
        ReturnType = returnType;
        _parameters = parameters;

        _isStatic = isStatic;
    }

    public ITypeSymbol ReturnType { get; }

    public ImmutableArray<IParameterSymbol> Parameters => _parameters.OfType<IParameterSymbol>().ToImmutableArray();

    public bool IsConstructor => false;

    public override bool IsStatic => _isStatic;

    public void SetParameters(IEnumerable<SourceParameterSymbol> parameters) => _parameters = parameters;
}