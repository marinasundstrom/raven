using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Macros;

namespace Raven.CodeAnalysis.Symbols;

internal sealed partial class SourceMacroFunctionSymbol : SourceSymbol, IMacroFunctionSymbol
{
    private ITypeSymbol _returnType;
    private ImmutableArray<SourceParameterSymbol> _parameters = ImmutableArray<SourceParameterSymbol>.Empty;
    private ImmutableArray<ITypeParameterSymbol> _typeParameters = ImmutableArray<ITypeParameterSymbol>.Empty;

    public SourceMacroFunctionSymbol(
        string name,
        ITypeSymbol returnType,
        ISymbol containingSymbol,
        INamespaceSymbol containingNamespace,
        Location[] locations,
        SyntaxReference[] declaringSyntaxReferences,
        Accessibility declaredAccessibility)
        : base(
            SymbolKind.MacroFunction,
            name,
            containingSymbol,
            containingType: null,
            containingNamespace,
            locations,
            declaringSyntaxReferences,
            declaredAccessibility)
    {
        _returnType = returnType;
    }

    public override string MetadataName => Name;

    public override bool IsStatic => true;

    public override bool CanBeReferencedByName => true;

    public MacroKind MacroKind => MacroKind.FreestandingExpression;

    public ITypeSymbol ReturnType => _returnType;

    public ImmutableArray<IParameterSymbol> Parameters =>
        _parameters.Cast<IParameterSymbol>().ToImmutableArray();

    public ImmutableArray<ITypeParameterSymbol> TypeParameters => _typeParameters;

    internal void SetReturnType(ITypeSymbol returnType)
    {
        _returnType = returnType;
    }

    internal void SetParameters(ImmutableArray<SourceParameterSymbol> parameters)
    {
        _parameters = parameters;
    }

    internal void SetTypeParameters(ImmutableArray<ITypeParameterSymbol> typeParameters)
    {
        _typeParameters = typeParameters;
    }
}
