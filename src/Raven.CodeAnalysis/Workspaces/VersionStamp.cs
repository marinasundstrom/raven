namespace Raven.CodeAnalysis;

public readonly struct VersionStamp : IEquatable<VersionStamp>
{
    private readonly Guid _value;

    private VersionStamp(Guid value)
    {
        _value = value;
    }

    public static VersionStamp Create()
    {
        return new VersionStamp(Guid.NewGuid());
    }

    public bool Equals(VersionStamp other) => _value.Equals(other._value);
    public override bool Equals(object? obj) => obj is VersionStamp other && Equals(other);
    public override int GetHashCode() => _value.GetHashCode();
    public override string ToString() => _value.ToString();

    public static bool operator ==(VersionStamp left, VersionStamp right) => left.Equals(right);
    public static bool operator !=(VersionStamp left, VersionStamp right) => !(left == right);
}
