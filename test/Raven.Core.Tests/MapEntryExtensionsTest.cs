namespace Raven.Core.Tests;

public sealed class MapEntryExtensionsTest : RavenCoreDiagnosticTestBase
{
    [Fact]
    public void FluentEntryBindsThroughInterfaceAndConcreteDictionary()
    {
        CreateVerifier("""
import System.*
import System.Collections.Generic.*

func Update(map: IDictionary<string, int>) -> int {
    return map.Entry("test").AndModify(value => value + 1).OrInsert(2)
}

let map = Dictionary<string, int>()
let value = map.Entry("test").OrInsertWith(() => 2)
""").Verify();
    }

    [Fact]
    public void LocatorIsLiveAcrossGrowthRemovalAndReinsertion()
    {
        IDictionary<string, int> map = new Dictionary<string, int>();
        var entry = Entry(map, "test");
        Assert.Empty(map);
        Assert.Equal("test", entry.Key);
        Assert.Equal(2, entry.OrInsert(2));
        Assert.Equal(2, entry.OrInsert(99));
        for (var i = 0; i < 200; i++)
            map[i.ToString()] = i;
        map["test"] = 7;
        Assert.Equal(7, entry.Value);
        map.Remove("test");
        Assert.Throws<KeyNotFoundException>(() => entry.Value);
        Assert.Equal(8, entry.OrInsert(8));
        Assert.Same(entry, entry.Set(9));
        Assert.Equal(9, map["test"]);
    }

    [Fact]
    public void FactoriesAndTransformsRunOnlyForTheMatchingState()
    {
        var map = new Dictionary<string, int>();
        var entry = Entry(map, "test");
        var calls = 0;
        Assert.Same(entry, entry.AndModify(value => { calls++; return value + 1; }));
        Assert.Equal(0, calls);
        Assert.Equal(2, entry.OrInsertWith(() => { calls++; return 2; }));
        Assert.Equal(2, entry.OrInsertWith(() => { calls++; return 9; }));
        Assert.Equal(1, calls);
        Assert.Equal(3, entry.AndModify(value => { calls++; return value + 1; }).OrInsert(100));
        Assert.Equal(2, calls);
    }

    [Fact]
    public void FactoryInsertionWinsAndTransformResultWins()
    {
        var map = new Dictionary<string, int>();
        var entry = Entry(map, "test");
        Assert.Equal(3, entry.OrInsertWith(() => { map["test"] = 3; return 99; }));
        entry.AndModify(value => { entry.Set(100); return value + 1; });
        Assert.Equal(4, entry.Value);
        entry.AndModify(value => { map.Remove("test"); return value + 1; });
        Assert.Equal(5, entry.Value);
        entry.AndModify(value => entry.AndModify(inner => inner + 2).Value + 3);
        Assert.Equal(10, entry.Value);
    }

    [Fact]
    public void CallbackExceptionsPropagateWithoutHelperWriteOrRollback()
    {
        var map = new Dictionary<string, int>();
        var entry = Entry(map, "test");
        var failure = new InvalidOperationException("callback failed");
        Assert.Same(failure, Assert.Throws<InvalidOperationException>(() =>
            entry.OrInsertWith(() => throw failure)));
        Assert.Empty(map);
        Assert.Same(failure, Assert.Throws<InvalidOperationException>(() =>
            entry.OrInsertWith(() => { map["test"] = 12; throw failure; })));
        Assert.Equal(12, entry.Value);
        Assert.Same(failure, Assert.Throws<InvalidOperationException>(() =>
            entry.AndModify(value => { map["test"] = 13; throw failure; })));
        Assert.Equal(13, entry.Value);
        Assert.Equal(14, entry.AndModify(value => value + 1).Value);
    }

    [Fact]
    public void ComparerAndPresentNullArePreserved()
    {
        var map = new Dictionary<string, string?>(StringComparer.OrdinalIgnoreCase) { ["Test"] = null };
        var entry = Entry(map, "TEST");
        Assert.Null(entry.OrInsert("fallback"));
        Assert.Null(entry.OrInsertWith(() => throw new Exception("must not run")));
        Assert.Equal("TEST", entry.Key);
        Assert.Equal("Test", Assert.Single(map).Key);
        Assert.True(entry.HasSome());
        Assert.True(!Entry(map, "missing").HasSome());
    }

    [Fact]
    public void ReferenceMutationSurvivesCallbackException()
    {
        var value = new List<int>();
        var map = new Dictionary<string, List<int>> { ["test"] = value };
        Assert.Throws<InvalidOperationException>(() => Entry(map, "test").AndModify(current =>
        {
            current.Add(42);
            throw new InvalidOperationException();
        }));
        Assert.Same(value, map["test"]);
        Assert.Equal(42, Assert.Single(value));
    }

    [Fact]
    public void WorksWithAnotherDictionaryImplementation()
    {
        IDictionary<string, int> map = new SortedDictionary<string, int>(StringComparer.OrdinalIgnoreCase);
        Assert.Equal(2, Entry(map, "test").OrInsert(2));
        Assert.Equal(3, Entry(map, "TEST").AndModify(value => value + 1).OrInsert(0));
        Assert.Single(map);
    }
    // Raven.Core's existing runtime-reference test convention loads its emitted assembly.
    // Keep delegate types strongly typed while invoking the actual library methods.
    private static EntryHandle<K, V> Entry<K, V>(IDictionary<K, V> map, K key)
    {
        var owner = LoadRavenCoreAssembly().GetType("System.Collections.Generic.MutableMapExtensions", true)!;
        var entry = owner.GetMethod("Entry")!.MakeGenericMethod(typeof(K), typeof(V)).Invoke(null, [map, key])!;
        return new EntryHandle<K, V>(entry);
    }

    private sealed class EntryHandle<K, V>(object entry)
    {
        public K Key => (K)entry.GetType().GetProperty("Key")!.GetValue(entry)!;
        public V Value => (V)InvokeGetter("Value")!;
        public V OrInsert(V value) => (V)Call("OrInsert", value)!;
        public V OrInsertWith(Func<V> factory) => (V)Call("OrInsertWith", factory)!;
        public EntryHandle<K, V> Set(V value) { Assert.Same(entry, Call("Set", value)); return this; }
        public EntryHandle<K, V> AndModify(Func<V, V> transform) { Assert.Same(entry, Call("AndModify", transform)); return this; }
        public bool HasSome()
        {
            var option = Call("Find")!;
            return (bool)option.GetType().GetProperty("HasSome")!.GetValue(option)!;
        }
        private object? InvokeGetter(string name) => Unwrap(() => entry.GetType().GetProperty(name)!.GetValue(entry));
        private object? Call(string name, params object?[] args) => Unwrap(() => entry.GetType().GetMethod(name)!.Invoke(entry, args));
        private static object? Unwrap(Func<object?> invoke)
        {
            try { return invoke(); }
            catch (System.Reflection.TargetInvocationException error) when (error.InnerException is not null)
            {
                System.Runtime.ExceptionServices.ExceptionDispatchInfo.Capture(error.InnerException).Throw();
                throw;
            }
        }
    }

}
