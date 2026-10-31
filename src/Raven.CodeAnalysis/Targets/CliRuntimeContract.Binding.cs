namespace Raven.CodeAnalysis.Targets;

internal abstract partial class CliRuntimeContract
{
    // Language availability is separate from runtime representation. Defaults
    // preserve .NET; concrete targets opt into exclusive language capabilities.
    internal virtual bool SupportsFunctionTypeMethods => false;
    internal virtual bool SupportsFunctionReturnTypeProjection => false;
    internal virtual bool SupportsIntersectionTypes => false;
    internal virtual bool SupportsCharKeyword => true;
    protected virtual string CharacterMetadataName => "System.Char";

    // The binder owns lookup, failures and diagnostics. Target normalization
    // receives only successful semantic results, never syntax or binder state.
    internal virtual (ITypeSymbol Type, INamedTypeSymbol? Definition) NormalizeResolvedType(
        Compilation compilation, ITypeSymbol type, INamedTypeSymbol? definition)
    {
        if (type.SpecialType == SpecialType.System_Void && Options.RuntimeUnitContract is { MapClrVoidToUnit: true })
            type = compilation.GetSpecialType(SpecialType.System_Unit);
        return (type, definition);
    }

    internal virtual ITypeSymbol GetPointerElementType(Compilation compilation, ITypeSymbol type) => type;

    // Source declaration shells have already been established by the binder.
    internal virtual INamedTypeSymbol? GetSourceMemberLookupType(Compilation compilation, INamedTypeSymbol type) => type;
}
