namespace Raven.CodeAnalysis;

// Source declaration facts and canonical attribute types remain compilation-owned.
internal interface AttributeValidationServices : CallerInfoValidationServices
{
    NamedTypeSymbol? GetTypeByMetadataName(string metadataName);

    bool IsPrimaryConstructor(Symbol symbol);

    bool IsNamespaceContainer(Symbol symbol);
}
