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
    private readonly ulong _value;

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


        ulong packedTicks = utcLastModified == default
            ? 0UL
            : ((ulong)utcLastModified.Ticks & ((1UL << (64 - localBits - globalBits)) - 1))
                << (localBits + globalBits);


        ulong packedLocal = (ulong)(localIncrement & ((1 << localBits) - 1)) << globalBits;
        ulong packedGlobal = (ulong)(globalIncrement & ((1 << globalBits) - 1));

        _value = packedTicks | packedLocal | packedGlobal;
    }
    #endregion

    /// <summary>Creates a new monotonically increasing stamp.</summary>
    public static VersionStamp Create()
    {
        var now = DateTime.UtcNow;
        now = new DateTime(now.Ticks - (now.Ticks % TimeSpan.TicksPerMillisecond), DateTimeKind.Utc);
        return new VersionStamp(now);
    }

    private static int GetNextGlobalVersion() => Interlocked.Increment(ref s_globalVersion);

    /// <summary>
    /// Gets a new <see cref="VersionStamp"/> that is guaranteed to be newer than the current instance.
    /// This should only be used for the *same* item to move it to a newer version.
    /// </summary>
    public VersionStamp GetNewerVersion()
    {
        const int localBits = 10;
        DateTime utcNow = DateTime.UtcNow;
        utcNow = new DateTime(utcNow.Ticks - (utcNow.Ticks % TimeSpan.TicksPerMillisecond), DateTimeKind.Utc);
        int nextLocal;

        if (utcNow <= _utcLastModified)
        {
            if (_localIncrement < ((1 << localBits) - 1))
            {
                utcNow = _utcLastModified;
                nextLocal = _localIncrement + 1;
            }
            else
            {
                // We've used all 1024 slots in this tick — wait for the next tick.
                do { utcNow = DateTime.UtcNow; } while (utcNow <= _utcLastModified);
                nextLocal = 0;
            }
        }
        else
        {
            nextLocal = 0;
        }

        return new VersionStamp(utcNow, nextLocal, _globalIncrement);
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

    public string ToDebugString()
        => $"0x{_value:X16} (utc={_utcLastModified:O}, local={_localIncrement}, global={_globalIncrement})";
}
