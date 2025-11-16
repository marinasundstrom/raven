using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class SynthesizedUnionConversionMethodSymbol : SourceMethodSymbol
{
    public SynthesizedUnionConversionMethodSymbol(
        SourceNamedTypeSymbol unionType,
        SourceNamedTypeSymbol caseType,
        SourceMethodSymbol unionConstructor,
        Location[] locations,
        SyntaxReference[] declaringSyntaxReferences,
        int caseIndex)
        : base(
            "op_Implicit",
            unionType,
            ImmutableArray<SourceParameterSymbol>.Empty,
            unionType,
            unionType,
            unionType.ContainingNamespace,
            locations,
            declaringSyntaxReferences,
            isStatic: true,
            methodKind: MethodKind.Conversion,
            declaredAccessibility: Accessibility.Public)
    {
        CaseType = caseType;
        UnionConstructor = unionConstructor;
        CaseIndex = caseIndex;
    }

    public SourceNamedTypeSymbol CaseType { get; }

    public SourceMethodSymbol UnionConstructor { get; }

    public int CaseIndex { get; }

    public override bool IsImplicitlyDeclared => true;
}
