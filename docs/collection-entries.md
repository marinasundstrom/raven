# Mutable collection extensions (development)

Raven.Core supplies ordinary library extensions in `System.Collections.Generic`.
No language syntax, Runtime Contract configuration or compiler emission rule changes.

```raven
import System.*
import System.Collections.Generic.*

let map = Dictionary<string, int>()
let value = map.Entry("test").AndModify(value => value + 1).OrInsert(2)
let set = HashSet<string>(StringComparer.OrdinalIgnoreCase)
let canonical = set.GetOrInsert("Test")
```

`Entry` extends `IDictionary<K,V>` and returns `MapEntry<K,V>`. `GetOrInsert`
extends `HashSet<T>` and `SortedSet<T>` because their TryGetValue methods can return
the stored representative. ISet alone cannot expose that value using its own
comparison policy; no arbitrary-ISet fallback is provided. neoCLR separately targets
its MutableMap/MutableSet contracts, without renaming the concrete collections.

## MapEntry members and lifetime

| Member | Behavior |
| --- | --- |
| `MapEntry(map, key)` / `map.Entry(key)` | Retains the map and supplied key; no lookup, insertion or reservation. Null map throws ArgumentNullException. |
| `Key` | The supplied lookup key, not necessarily the stored equivalent key. |
| `Value` | Reads current state through the indexer; missing-key exceptions propagate. |
| `Find()` | Returns Option<V>; Some(null) is distinct from None. |
| `Set(value)` | Inserts/replaces and returns the same entry. |
| `OrInsert(value)` | Returns the existing value or inserts/returns the eager candidate. |
| `OrInsertWith(factory)` | Runs the synchronous factory once only if absent, then inserts only if still absent. Callback insertion wins. |
| `AndModify(transform)` | Reads once; skips absent keys; otherwise invokes `(V) -> V` once and writes the result. Returns the same entry. |

The entry is a reusable live locator, not a storage reference or an occupied/vacant
snapshot. Growth, removal and reinsertion do not invalidate it. Each chained call
resolves current state independently. Keys obey the backing collection's stability,
null and comparer requirements. Returned values use ordinary copying/reference
sharing; the handle keeps its owner/key alive. Entry allocates a small class and can
perform multiple lookups; there is no single-probe or performance-improvement claim.

Callbacks may reenter. AndModify writes its result even after callback replacement
or removal (recreating the key); nested writes happen first. If a callback throws,
the helper performs no subsequent write and propagates the original exception.
Callback side effects and reference mutations remain. Required callbacks are
non-null. Neither individual operations nor chains are atomic or synchronized,
including over ConcurrentDictionary. Externally synchronize shared operations.

## Set operation

`GetOrInsert(value) -> T` uses the set's equality/ordering policy. It returns the
stored equivalent element without replacing it, or inserts/returns the candidate.
It preserves stored reference identity, follows the set's null policy, and provides
no atomicity or synchronization. The receiver must be non-null.

## Design comparison

The baseline is .NET IDictionary's TryGetValue/indexer and
[CollectionExtensions.TryAdd](https://learn.microsoft.com/en-us/dotnet/api/system.collections.generic.collectionextensions.tryadd?view=net-10.0).
[Rust Entry](https://doc.rust-lang.org/std/collections/hash_map/enum.Entry.html)
inspires conditional composition but relies on exclusive borrowing for its mutable
references. [.NET CollectionsMarshal](https://learn.microsoft.com/en-us/dotnet/api/system.runtime.interopservices.collectionsmarshal.getvaluereforadddefault?view=net-10.0)
requires restrictions while a dictionary storage reference is in use. The live
locator trades storage access/single-lookup potential for retention safety and
compatibility with existing interfaces. [HashSet.TryGetValue](https://learn.microsoft.com/en-us/dotnet/api/system.collections.generic.hashset-1.trygetvalue?view=net-10.0)
is the canonical-value baseline for sets. No change to .NET collection semantics
or borrowing machinery is needed.

Focused validation: `dotnet test test/Raven.Core.Tests/Raven.Core.Tests.csproj
--filter 'FullyQualifiedName~MapEntryExtensionsTest|FullyQualifiedName~SetExtensionsTest|FullyQualifiedName~TryGetExtensionsTest'
/property:WarningLevel=0` on .NET 10. Tests exercise binding, generic interface use,
retention, growth/removal, callback counts/nesting/failures, null values, comparer
policy and canonical set identity. Modern .NET validation does not qualify .NET
Framework or NanoFramework. neoCLR interpreter/JIT/AOT validation is maintained in
that repository. List/GetItem helpers remain deferred by author direction.

The compiled [sample](../samples/collection-entries/main.rvn) also executes on .NET
10 and returns 42, exercising dictionary chaining and case-insensitive set lookup.
