namespace Raven.CodeAnalysis;

internal static class RavenRuntimeTypeNames
{
    public const string StructuredDisplayInterface = "Raven.Runtime.CompilerServices.IRavenStructuredDisplay";

    public static INamedTypeSymbol? GetStructuredDisplayInterface(Compilation compilation)
        => compilation.TryGetMetadataReferenceTypeByMetadataName(StructuredDisplayInterface)
            ?? compilation.GetTypeByMetadataName(StructuredDisplayInterface);
}
