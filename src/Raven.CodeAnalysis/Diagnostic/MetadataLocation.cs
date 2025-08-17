namespace Raven.CodeAnalysis;

internal class MetadataLocation : Location
{
    internal MetadataLocation(IModuleSymbol moduleSymbol)
    {
        Kind = LocationKind.MetadataFile;
        MetadataModule = moduleSymbol;
    }
}