using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Symbols;

internal partial class SourceMethodSymbol : SourceSymbol, IMethodSymbol
{
    private IEnumerable<SourceParameterSymbol> _parameters;

    public SourceMethodSymbol(string name, ITypeSymbol returnType, ImmutableArray<SourceParameterSymbol> parameters, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations, SyntaxReference[] declaringSyntaxReferences, bool isStatic = true)
            : base(SymbolKind.Method, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences)
    {
        ReturnType = returnType;
        _parameters = parameters;

        IsStatic = isStatic;

        //var declaringSyntax = declaringSyntaxReferences.First().GetSyntax();

        MethodKind = MethodKind.Ordinary;
    }

    public ITypeSymbol ReturnType { get; }

    public ImmutableArray<IParameterSymbol> Parameters => _parameters.OfType<IParameterSymbol>().ToImmutableArray();

    public bool IsConstructor => false;

    public override bool IsStatic { get; }

    public MethodKind MethodKind { get; }

    public void SetParameters(IEnumerable<SourceParameterSymbol> parameters) => _parameters = parameters;
}