namespace Raven.CodeAnalysis.Symbols;

/// <summary>
/// Canonical nominal owner for a source macro's generic parameters and
/// designated expansion method.
/// </summary>
internal sealed class SynthesizedMacroDefinitionTypeSymbol : SourceNamedTypeSymbol
{
    public SynthesizedMacroDefinitionTypeSymbol(
        Compilation compilation,
        string macroName,
        INamespaceSymbol containingNamespace,
        Location[] locations,
        SyntaxReference[] declaringSyntaxReferences,
        Accessibility declaredAccessibility)
        : base(
            $"{macroName}Macro",
            compilation.GetSpecialType(SpecialType.System_Object),
            TypeKind.Class,
            containingNamespace,
            containingType: null,
            containingNamespace,
            locations,
            declaringSyntaxReferences,
            isSealed: true,
            declaredAccessibility: declaredAccessibility,
            addAsMember: false)
    {
    }

    public override bool IsImplicitlyDeclared => true;
}
