using System.Collections.Immutable;
using System.Linq;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class SourceDiscriminatedUnionCaseTypeSymbol : SourceNamedTypeSymbol, IDiscriminatedUnionCaseSymbol
{
    public SourceDiscriminatedUnionCaseTypeSymbol(
        string name,
        int ordinal,
        INamedTypeSymbol baseType,
        ISymbol containingSymbol,
        INamedTypeSymbol? containingType,
        INamespaceSymbol? containingNamespace,
        Location[] locations,
        SyntaxReference[] declaringSyntaxReferences,
        Accessibility declaredAccessibility)
        : base(name, baseType, TypeKind.Struct, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences, isSealed: true, declaredAccessibility: declaredAccessibility)
    {
        Ordinal = ordinal;
    }

    public ImmutableArray<IParameterSymbol> ConstructorParameters { get; private set; } = ImmutableArray<IParameterSymbol>.Empty;

    public int Ordinal { get; }

    public IDiscriminatedUnionSymbol Union => (IDiscriminatedUnionSymbol)ContainingType!;

    internal void SetConstructorParameters(IEnumerable<SourceParameterSymbol> parameters)
    {
        ConstructorParameters = parameters.Cast<IParameterSymbol>().ToImmutableArray();
    }
}
