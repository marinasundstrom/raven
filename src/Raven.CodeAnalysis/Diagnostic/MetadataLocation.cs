namespace Raven.CodeAnalysis;

internal class MetadataLocation : Location
{
    internal MetadataLocation()
    {
        Kind = LocationKind.MetadataFile;
    }
}