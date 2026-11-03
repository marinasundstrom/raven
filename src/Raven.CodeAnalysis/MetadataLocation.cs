namespace Raven.CodeAnalysis;

internal class MetadataLocation : Location
{
    internal MetadataLocation(ModuleSymbol moduleSymbol)
    {
        Kind = LocationKind.MetadataFile;
        MetadataModule = moduleSymbol;
    }

    public override ModuleSymbol MetadataModule { get; }
}
