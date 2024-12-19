namespace Raven.CodeAnalysis;

internal class NoLocation : Location
{
    internal NoLocation()
    {
        Kind = LocationKind.None;
    }
}