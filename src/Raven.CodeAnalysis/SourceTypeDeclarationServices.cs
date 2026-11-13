namespace Raven.CodeAnalysis;

// Compilation-owned completion and runtime projections for source named types.
internal interface SourceTypeDeclarationServices : SourceDeclarationServices
{
    void EnsureSourceDeclarationsComplete();
    SpecialType GetSourceTypeSpecialType(NamedTypeSymbol type);
    NamedTypeSymbol? GetArrayShape(NamedTypeSymbol sourceType, ArrayTypeSymbol array);
}
