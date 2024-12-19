namespace Raven.CodeAnalysis;

public class NoLocation : Location
{
    internal NoLocation()
    {
        Kind = LocationKind.None;
    }
}