namespace Raven.CodeAnalysis.Targets;

internal abstract partial class CliRuntimeContract
{
    // Language availability is separate from runtime representation. Defaults
    // preserve .NET; concrete targets opt into exclusive language capabilities.
    internal virtual bool SupportsCovariantMethodReturns(Compilation compilation)
        => compilation.GetTypeByMetadataName("System.Runtime.CompilerServices.RuntimeFeature")?
            .GetMembers("CovariantReturnsOfClasses").Any() == true &&
           compilation.GetTypeByMetadataName("System.Runtime.CompilerServices.PreserveBaseOverridesAttribute")?
               .Constructors.Any(method => !method.IsStatic && method.Parameters.IsEmpty) == true;

    internal virtual bool SupportsFunctionTypeMethods => false;
    internal virtual bool SupportsFunctionReturnTypeProjection => false;
    internal virtual bool SupportsIntersectionTypes => false;
    internal virtual bool SupportsCharKeyword => true;
    protected virtual string CharacterMetadataName => "System.Char";

    // The binder owns lookup, failures and diagnostics. Target normalization
    // receives only successful semantic results, never syntax or binder state.
    internal virtual (TypeSymbol Type, NamedTypeSymbol? Definition) NormalizeResolvedType(
        Compilation compilation, TypeSymbol type, NamedTypeSymbol? definition)
    {
        if (type.SpecialType == SpecialType.System_Void && Options.RuntimeUnitContract is { MapClrVoidToUnit: true })
            type = compilation.GetSpecialType(SpecialType.System_Unit);
        return (type, definition);
    }

    internal virtual NamedTypeSymbol? GetNullableValueMemberLookupType(Compilation compilation, TypeSymbol type, string memberName) => null;

    internal virtual TypeSymbol GetPointerElementType(Compilation compilation, TypeSymbol type) => type;

    // Source declaration shells have already been established by the binder.
    internal virtual NamedTypeSymbol? GetSourceMemberLookupType(Compilation compilation, NamedTypeSymbol type) => type;
}
