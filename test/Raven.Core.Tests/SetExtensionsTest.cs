namespace Raven.Core.Tests;

public sealed class SetExtensionsTest : RavenCoreDiagnosticTestBase
{
    [Fact]
    public void GetOrInsertBindsForBothSetImplementations()
    {
        CreateVerifier("""
import System.*
import System.Collections.Generic.*

let hash = HashSet<string>(StringComparer.OrdinalIgnoreCase)
let sorted = SortedSet<string>(StringComparer.OrdinalIgnoreCase)
let first = hash.GetOrInsert("Test")
let second = sorted.GetOrInsert("Test")
""").Verify();
    }

    [Fact]
    public void HashSetReturnsStoredRepresentativeAndSupportsNull()
    {
        var set = new HashSet<string?>(StringComparer.OrdinalIgnoreCase);
        var first = new string("Test".ToCharArray());
        var other = new string("TEST".ToCharArray());
        Assert.Same(first, GetOrInsert(set, first));
        Assert.Same(first, GetOrInsert(set, other));
        Assert.Single(set);
        set.Remove(other);
        Assert.Same(other, GetOrInsert(set, other));
        set.Clear();
        Assert.Null(GetOrInsert(set, null));
        Assert.Null(GetOrInsert(set, null));
        Assert.Single(set);
    }

    [Fact]
    public void SortedSetUsesOrderingEquivalence()
    {
        var set = new SortedSet<string>(Comparer<string>.Create((a, b) => a.Length.CompareTo(b.Length)));
        var first = new string("abc".ToCharArray());
        Assert.Same(first, GetOrInsert(set, first));
        Assert.Same(first, GetOrInsert(set, "xyz"));
        Assert.Single(set);
    }
    private static T GetOrInsert<T>(HashSet<T> set, T value) => Invoke<T>("HashSetExtensions", set, value);
    private static T GetOrInsert<T>(SortedSet<T> set, T value) => Invoke<T>("SortedSetExtensions", set, value);
    private static T Invoke<T>(string name, object set, T value)
    {
        var owner = LoadRavenCoreAssembly().GetType("System.Collections.Generic." + name, true)!;
        return (T)owner.GetMethod("GetOrInsert")!.MakeGenericMethod(typeof(T)).Invoke(null, [set, value])!;
    }

}
