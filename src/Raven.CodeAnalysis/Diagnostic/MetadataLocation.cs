namespace Raven.CodeAnalysis;

internal class MetadataLocation : Location
{
    private MetadataLocation()
    {
        Kind = LocationKind.MetadataFile;
    }
}