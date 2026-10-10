# Friend assemblies

Development, 2026-10-10. Raven's shared accessibility checker recognizes
`System.Runtime.CompilerServices.InternalsVisibleToAttribute` on source assemblies
and ordinary CLI metadata references. This is general .NET compiler behavior, not
a neoCLR-specific accessibility exception.

Place assembly attributes at compilation-unit scope, before a file-scoped namespace:

```raven
import System.Runtime.CompilerServices.*

[assembly: InternalsVisibleTo("Library.Tests")]
[assembly: InternalsVisibleTo("Library.Companion")]

namespace Library

internal class Implementation {
    internal static func Answer() -> int => 42
}
```

The strings identify assemblies, not modules, namespaces or filenames. A grant lets
the named assembly use internal types and members. It does not make those members
public, expose private members, grant reciprocal access or extend access to the
friend's consumers. `private protected` still requires an appropriate derived type;
`protected internal` accepts either the protected or internal access path. Public
signatures still cannot expose internal types. AttributeUsage continues to enforce
Assembly-only placement and permit repeated grants.

Source and imported grants use the same identity policy. Simple names compare
ordinally ignoring case. Full public keys are compared as bytes; token-only,
version/culture-qualified, malformed and unsupported qualifier forms do not grant
access. Strong-named declaring assemblies require a full-key grant. Imported PE
identities use their full public keys, not a public-key-token display string.
Raven source output has no signing configuration in this slice; key comparison
coverage does not claim a new signing toolchain. Invalid grants currently fail closed
through ordinary access diagnostics; dedicated declaration diagnostics remain future
work. Attribute constructors are never executed to decide access.

The semantic accessibility checker also supplies completion filtering through the
binder. This change adds no syntax, keyword, bound node or operations kind; existing
attribute syntax and semantic symbols apply. No TextMate grammar change or separate
language-server permission policy is needed.

## Resolution cost

Public access and same-assembly internal access keep their existing fast paths.
Cross-assembly internal checks cache both grants and denials by the two immutable
assembly-symbol snapshots. Weak keys avoid retaining compilations; thread-safe lazy
publication computes a pair's decision once. Subsequent checks perform cache lookups
without reading attributes, parsing identity strings or allocating. Different snapshots
with the same assembly name do not share cached access rights. A focused regression
checks concurrent access and 100,000 warmed lookups for each outcome with zero measured
thread allocations. This is a hot-path allocation check, not an end-to-end compiler
throughput comparison. Initial cross-assembly checks necessarily inspect grant metadata.

## Runtime and target boundary

Runtime Contract configuration is unchanged. Ordinary .NET emission retains the
existing assembly custom attribute and emits normal member references; the CLR then
enforces its own friend-assembly checks. Focused tests compile separate Raven
libraries and consumers and execute friend calls on .NET, alongside negative access,
source-symbol, usage and identity checks. Modern .NET validation is not evidence for
.NET Framework or NanoFramework execution.

This shared fix does **not** make native neoCLR friends work yet. Its native assembly
metadata provider does not currently retain assembly-level grants, and native import
admission and runtime access checks still need the matching identity policy. Keep
those as explicit integration gaps, including AOT qualification. Missing native
metadata never authorizes access. See [the bridge record](neoclr-cli-bridge.md).

The intended semantics follow Microsoft's
[InternalsVisibleToAttribute contract](https://learn.microsoft.com/en-us/dotnet/api/system.runtime.compilerservices.internalsvisibletoattribute?view=net-10.0).
Friend access is implementation coupling, not a security isolation boundary.
