namespace Raven.MetadataFixtures.Pins;

public enum PinEventTypes
{
    Falling,
    Rising,
}

public sealed class PinEventArgs
{
    public PinEventArgs(PinEventTypes changeType)
    {
        ChangeType = changeType;
    }

    public PinEventTypes ChangeType { get; }
}
