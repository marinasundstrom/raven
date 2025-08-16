namespace Raven.CodeAnalysis;

/// <summary>
/// A cheap, immutable version token similar to Roslyn's <c>VersionStamp</c>.
/// Backed by a 64‑bit tick count (DateTime.UtcNow.Ticks) to avoid Guid allocations
/// yet still provide global uniqueness with ~100‑ns granularity.
/// </summary>
public readonly struct VersionStamp : IEquatable<VersionStamp>, IComparable<VersionStamp>
{
    private readonly DateTime _utcLastModified;

    // A single packed value that drives ordering, hashing and equality. Layout (low‑order bits on the right):
    // |               ticks               |  local  | global |
    // | 64‑(LOCAL+GLOBAL) bits            | 10 bits |  6 b   |
    //
    // ‑ ticks : DateTime.UtcNow.Ticks with the low bits shifted left so they stay the most‑significant part.
    // ‑ local : counter to disambiguate multiple stamps created within the same 100‑ns tick.
    // ‑ global: process‑wide counter to disambiguate between different app‑domains / processes.
    private readonly long _value;

    private readonly int _localIncrement;
    private readonly int _globalIncrement;

    private static int s_globalVersion = 1000;

    #region ctors
    private VersionStamp(DateTime utcLastModified)
        : this(utcLastModified, 0, GetNextGlobalVersion())
    {
    }

    private VersionStamp(DateTime utcLastModified, int localIncrement)
        : this(utcLastModified, localIncrement, GetNextGlobalVersion())
    {
    }

    private VersionStamp(DateTime utcLastModified, int localIncrement, int globalIncrement)
    {
        if (utcLastModified != default && utcLastModified.Kind != DateTimeKind.Utc)
        {
            throw new ArgumentException("Timestamp must be expressed in UTC", nameof(utcLastModified));
        }

        _utcLastModified = utcLastModified;
        _localIncrement = localIncrement;
        _globalIncrement = globalIncrement;

        // ********  Packing layout  ********
        const int localBits = 10; // 0‑1023 updates within the same tick
        const int globalBits = 6;  // 0‑63 unique process‑wide values

        long packedTicks = utcLastModified == default
            ? 0L
            : (utcLastModified.Ticks & ((1L << (64 - localBits - globalBits)) - 1)) << (localBits + globalBits);

        long packedLocal = (localIncrement & ((1L << localBits) - 1)) << globalBits;
        long packedGlobal = globalIncrement & ((1L << globalBits) - 1);

        _value = packedTicks | packedLocal | packedGlobal;
    }
    #endregion

    /// <summary>Creates a new monotonically increasing stamp.</summary>
    public static VersionStamp Create() => new(DateTime.UtcNow);

    private static int GetNextGlobalVersion() => Interlocked.Increment(ref s_globalVersion);

    /// <summary>
    /// Gets a new <see cref="VersionStamp"/> that is guaranteed to be newer than the current instance.
    /// This should only be used for the *same* item to move it to a newer version.
    /// </summary>
    public VersionStamp GetNewerVersion()
    {
        DateTime utcNow = DateTime.UtcNow;
        int nextLocal;
        if (utcNow <= _utcLastModified)
        {
            utcNow = _utcLastModified;
            nextLocal = _localIncrement + 1;
        }
        else
        {
            nextLocal = 0;
        }
        return new VersionStamp(utcNow, nextLocal);
    }

    #region Comparison / equality
    public int CompareTo(VersionStamp other) => _value.CompareTo(other._value);
    public bool Equals(VersionStamp other) => _value == other._value;

    public override bool Equals(object? obj) => obj is VersionStamp vs && Equals(vs);
    public override int GetHashCode() => _value.GetHashCode();

    public static bool operator ==(VersionStamp left, VersionStamp right) => left.Equals(right);
    public static bool operator !=(VersionStamp left, VersionStamp right) => !(left == right);
    public static bool operator >(VersionStamp left, VersionStamp right) => left._value > right._value;
    public static bool operator <(VersionStamp left, VersionStamp right) => left._value < right._value;
    #endregion

    public override string ToString() => _value.ToString();
}
