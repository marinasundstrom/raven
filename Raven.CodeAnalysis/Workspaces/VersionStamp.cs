namespace Raven.CodeAnalysis;

public readonly struct VersionStamp : IEquatable<VersionStamp>
{
    private readonly DateTime _dateTime;

    public VersionStamp(DateTime dateTime)
    {
        _dateTime = dateTime;
    }

    public static VersionStamp Create()
    {
        return Create(DateTime.Now);
    }

    public static VersionStamp Create(DateTime dateTime)
    {
        return new VersionStamp(dateTime);
    }

    public bool Equals(VersionStamp other)
    {
        return _dateTime == other._dateTime;
    }

    public override bool Equals(object? obj)
    {
        return obj is VersionStamp && Equals((VersionStamp)obj);
    }

    public override int GetHashCode()
    {
        return _dateTime.GetHashCode();
    }
}
