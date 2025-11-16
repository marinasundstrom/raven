using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Symbols;

internal sealed class SynthesizedUnionTryGetMethodSymbol : SourceMethodSymbol
{
    public SynthesizedUnionTryGetMethodSymbol(
        SourceNamedTypeSymbol unionType,
        SourceNamedTypeSymbol caseType,
        SourceFieldSymbol discriminatorField,
        SourceFieldSymbol payloadField,
        ITypeSymbol boolType,
        Location[] locations,
        SyntaxReference[] declaringSyntaxReferences,
        int caseIndex)
        : base(
            $"TryGet{caseType.Name}",
            boolType,
            ImmutableArray<SourceParameterSymbol>.Empty,
            unionType,
            unionType,
            unionType.ContainingNamespace,
            locations,
            declaringSyntaxReferences,
            isStatic: false,
            methodKind: MethodKind.Ordinary,
            declaredAccessibility: Accessibility.Public)
    {
        CaseType = caseType;
        DiscriminatorField = discriminatorField;
        PayloadField = payloadField;
        CaseIndex = caseIndex;
    }

    public SourceNamedTypeSymbol CaseType { get; }

    public SourceFieldSymbol DiscriminatorField { get; }

    public SourceFieldSymbol PayloadField { get; }

    public int CaseIndex { get; }

    public override bool IsImplicitlyDeclared => true;
}
