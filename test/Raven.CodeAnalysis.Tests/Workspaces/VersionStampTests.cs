using System.Reflection;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace Raven.CodeAnalysis.Tests;

public class VersionStampTests
{
    private static readonly Regex DebugLocalRx = new(@"local=(\d+)", RegexOptions.Compiled);
    private static readonly Regex DebugGlobalRx = new(@"global=(\d+)", RegexOptions.Compiled);

    private static int GetLocal(VersionStamp s)
    {
        var m = DebugLocalRx.Match(s.ToDebugString());
        return m.Success ? int.Parse(m.Groups[1].Value) : throw new InvalidOperationException("local not found");
    }

    private static int GetGlobal(VersionStamp s)
    {
        var m = DebugGlobalRx.Match(s.ToDebugString());
        return m.Success ? int.Parse(m.Groups[1].Value) : throw new InvalidOperationException("global not found");
    }

    [Fact(Skip = "Cannot reliably observe same-tick increments on all environments")]
    public void GetNewerVersion_InSameTick_IncrementsLocal()
    {
        var s1 = VersionStamp.Create();

        // We try a few times to catch calls within the same system tick (UtcNow often has ~0.5–1ms granularity).
        // As soon as we observe local increasing, we pass.
        var attempts = 0;
        var prev = s1;
        while (attempts++ < 10_000)
        {
            var next = prev.GetNewerVersion();
            int l0 = GetLocal(prev);
            int l1 = GetLocal(next);

            if (next > prev && (l1 == l0 + 1 || (l0 == 1023 && l1 == 0)))
                return; // success

            prev = next;
        }

        throw new Xunit.Sdk.XunitException("Could not observe local increment within the same tick.");
    }

    [Fact]
    public async Task GetNewerVersion_AfterTick_ResetsLocalToZero()
    {
        var s1 = VersionStamp.Create();
        // ensure the clock advances to a later tick
        await Task.Delay(2);

        var s2 = s1.GetNewerVersion();

        Assert.True(s2 > s1);
        Assert.Equal(0, GetLocal(s2));
    }

    [Fact]
    public void GetNewerVersion_ReusesGlobalTag()
    {
        var s1 = VersionStamp.Create();
        var g1 = GetGlobal(s1);

        // Get a few newer versions; global should stay identical
        var s2 = s1.GetNewerVersion();
        var s3 = s2.GetNewerVersion();

        Assert.Equal(g1, GetGlobal(s2));
        Assert.Equal(g1, GetGlobal(s3));
    }

    [Fact]
    public async Task Sequence_IsStrictlyMonotonic()
    {
        var s = VersionStamp.Create();

        for (int i = 0; i < 2000; i++)
        {
            var newer = s.GetNewerVersion();
            Assert.True(newer > s, $"Not strictly increasing at i={i}");
            s = newer;

            // Occasionally sleep so we exercise both same-tick (local++) and next-tick (local=0) paths
            if (i % 100 == 0) await Task.Delay(1);
        }
    }

    [Fact]
    public void LocalOverflow_WaitsForNextTickAndResetsToZero()
    {
        // Use reflection to seed a stamp with local=1023 at the current tick.
        var ctor = typeof(VersionStamp).GetConstructor(
            BindingFlags.NonPublic | BindingFlags.Instance,
            binder: null,
            types: [typeof(DateTime), typeof(int), typeof(int)],
            modifiers: null);

        Assert.NotNull(ctor);

        var t = DateTime.UtcNow;
        // global value can be any small int (masked to 6 bits internally)
        var sMaxLocal = (VersionStamp)ctor!.Invoke([t, 1023, 5]);

        // This should spin until the system tick advances, then return local = 0 on the new tick.
        var sNext = sMaxLocal.GetNewerVersion();

        Assert.True(sNext > sMaxLocal);
        Assert.Equal(0, GetLocal(sNext));

        // And the timestamp must be strictly later (packed ordering already asserts this, but we can sanity-check delay)
        // We can't read the internal DateTime directly, but if local reset happened, we at least know tick advanced.
    }
}
