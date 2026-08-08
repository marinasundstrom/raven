namespace Raven.CodeAnalysis.Symbols;

internal sealed class SynthesizedUnionCompanionTypeSymbol : SourceNamedTypeSymbol
{
    public SynthesizedUnionCompanionTypeSymbol(SourceUnionSymbol union)
        : base(
            union.Name,
            union.ContainingAssembly!.GetTypeByMetadataName("System.Object")!,
            TypeKind.Class,
            union.ContainingNamespace!,
            containingType: null,
            union.ContainingNamespace,
            union.Locations.ToArray(),
            declaringSyntaxReferences: [],
            isSealed: true,
            isAbstract: true,
            isStatic: true,
            declaredAccessibility: union.DeclaredAccessibility,
            addAsMember: false)
    {
        Union = union;
    }

    public SourceUnionSymbol Union { get; }
}
