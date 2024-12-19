namespace Raven.CodeAnalysis;

public class MetadataLocation : Location
{
    private MetadataLocation()
    {
        Kind = LocationKind.MetadataFile;
    }
}