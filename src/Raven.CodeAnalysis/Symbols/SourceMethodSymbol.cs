using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Symbols;

internal class SourceMethodSymbol : SourceSymbol, IMethodSymbol
{
    public SourceMethodSymbol(string name, ITypeSymbol returnType, ImmutableArray<IParameterSymbol> parameters, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations, SyntaxReference[] declaringSyntaxReferences)
            : base(SymbolKind.Method, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
        ReturnType = returnType;
        Parameters = parameters;
    }

    public ITypeSymbol ReturnType { get; }

    public ImmutableArray<IParameterSymbol> Parameters { get; set; } = [];

    public bool IsConstructor => false;
}