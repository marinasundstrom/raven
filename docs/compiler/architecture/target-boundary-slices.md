# Target boundary slice ledger

This ledger tracks execution of the [target boundary plan](target-boundaries-and-bootstrap-plan.md).
Investigation and prototypes start on `codex/target-boundaries`, based on
`neoclr` at `b54d2999c`. General changes still require independent main-based
extraction and validation before production integration.

## Slice 0: plan

Committed as `39f42d680`. Establishes sequencing, ownership, and validation gates.
Documentation only; `git diff --check` passed.

## Slice 1: metadata dependency and coverage inventory

The first extraction is context construction and reference resolution, not the
full provider contract. Current dependencies are:

| Path | Owner today | Constraint on extraction |
| --- | --- | --- |
| Select references and metadata core | `Compilation.Setup` | Explicit-only mode must not acquire host fallback references. |
| Normalize paths, deduplicate identities, construct resolver/context | `Compilation.CreateMetadataLoadContext` | First input wins for identical assembly identities; normalized paths are sorted for deterministic fallback selection. |
| Resolve identity or simple name, load bytes | Nested `StreamBackedPathAssemblyResolver` | Preserve exact-identity priority and existing simple-name fallback; do not introduce stricter admission accidentally. |
| Read assembly identities | `Compilation.ReadAssemblyName*` | Browser/WASI use PE metadata instead of host reflection loading. |
| Share paths with runtime resolution | `s_globalAssemblyPathMap` | Resolver registration is consumed by runtime assembly mapping; isolate this dependency explicitly before removing it. |
| Transfer metadata context between snapshots | `AdoptIncrementalReuseFrom`, `TryReuseMetadataLoadContext` | Import/runtime options and portable reference fingerprints gate reuse; no previous compilation may be retained. |
| Load individual references and construct symbols | `LoadMetadataAssembly`, `GetAssembly`, `ReflectionTypeLoader`, `Symbols/PE` | Still reflection-based after the first extraction; later slices must move this ownership. |
| Resolve target types for emit | Runtime assembly maps and `CodeGen` | Keep metadata-only target objects distinct from host-executable types. |

### Existing coverage and gaps

`MetadataImportOptionsTests` covers explicit references, missing core, missing
Console after host compilation, host-to-isolated reuse rejection, and option
copies. `IncrementalCompilationReuseTests` covers source edits, same-path binary
replacement, and discarded compilation/context retention. These form the first
focused baseline.

Add public-behavior checks for the reverse isolated-to-host transition and
isolated-to-isolated reference changes. Resolver precedence and malformed input
handling need dedicated fixtures before changing those policies. Do not treat
the presence of these test classes as complete target-boundary coverage.

The first extraction should be mechanical and preserve policy. Performance
measurement is deferred until an optimization is proposed; no speed or memory
improvement is claimed for moving ownership. Remaining inventory includes
symbol-family consumers, target lowering, macro host contracts, and main branch
differences.

### Next slice

Extract a .NET-specific context factory/resolver from `Compilation`, retaining
the existing context lifetime and explicit registry callback. This factory is
an internal .NET component, not a target-neutral metadata provider. The final
provider must expose semantic contracts rather than `Assembly`/`Type` objects.
Record focused test results before and after extraction in this ledger.

## Slice 2: import transition coverage

Added three cases in `MetadataImportOptionsTests`: isolated-to-host mode changes
and adding/removing Console between explicit-reference snapshots. Assertions
check public symbol lookup, diagnostics against a cold compilation, and that the
prior snapshot keeps its original results. These are characterization tests;
no compiler bug or behavior change is claimed.

Validation on .NET 11 with SDK `11.0.100-rc.1.26425.128`:

- Baseline: `dotnet test test/Raven.CodeAnalysis.Tests/Raven.CodeAnalysis.Tests.csproj
  --filter 'FullyQualifiedName~MetadataImportOptionsTests|FullyQualifiedName~IncrementalCompilationReuseTests'
  /property:WarningLevel=0` — 80 passed, no failures/skips.
- After additions: the same test project filtered to `MetadataImportOptionsTests`,
  with `--no-restore /property:WarningLevel=0 /property:BuildProjectReferences=false`
  — 13 passed, no failures/skips. Referenced projects were built by the baseline.
- Whitespace formatter and `git diff --check` completed. Formatter reported
  workspace-load warnings; no formatting failure occurred.

No neoCLR runtime, .NET Framework, NanoFramework, or browser execution was tested.

## Slice 3: .NET context factory extraction

Moved context construction, the stream-backed resolver, and portable assembly
identity reading into `Metadata.DotNetMetadataContextFactory`. A static callback
preserves shared path registration during construction without capturing a
compilation. Existing internal identity-reading entry points forward to the
factory. Reference selection, context reuse/lifetime, and imported symbol
ownership remain in `Compilation`.

This is a prototype on the experiment-derived feature branch. No public API,
language semantics, Runtime Contract setting, or neoCLR mapping changes. The
corresponding construction/resolver code exists on main; independent extraction
and validation there are still pending. A source comparison confirmed that the
moved implementation differs only in factory/callback plumbing.

The next ownership slice must address metadata-session loading and lifetime,
including the shared runtime path registry. Before removing that registry or
changing resolution policy, add resolver-precedence and malformed-input fixtures.
Do not infer a platform-neutral symbol model from this .NET-only extraction.

Validation with SDK `11.0.100-rc.1.26425.128`:

- `dotnet build src/Raven.CodeAnalysis/Raven.CodeAnalysis.csproj --no-restore
  --property WarningLevel=0` — net10.0 and net11.0 builds passed.
- `dotnet test test/Raven.CodeAnalysis.Tests/Raven.CodeAnalysis.Tests.csproj
  --no-restore --filter 'FullyQualifiedName~MetadataImportOptionsTests|FullyQualifiedName~IncrementalCompilationReuseTests|FullyQualifiedName~WebAssemblyCompatibilityTests|FullyQualifiedName~CliMetadataCompatibilityTests'
  /property:WarningLevel=0 /property:BuildProjectReferences=false` — 89 passed,
  no failures/skips on net11.0. Tests used the newly built compiler; other
  referenced projects came from the initial baseline.
- Whitespace formatting of both compiler files and `git diff --check` passed.

Portable metadata-reading tests executed on .NET, not in a browser/WASI host.
No full baseline, release/bootstrap qualification, or neoCLR execution is claimed.

## Slice 4: shared .NET metadata session

`DotNetMetadataSession` now owns the context and path/identity loading, including
the existing identity fallback after a failed path load. Compatible compilations
reuse the session under the existing option/fingerprint checks. The session
retains no compilation, symbol, or registration callback. Per-compilation path
caches and runtime path registration remain in `Compilation`; this slice does
not broaden cache sharing or remove the host registry.

Context lifetime remains collection-based. Disposing a session when one snapshot
is discarded would invalidate other snapshots; deterministic shared disposal
requires a separate ownership design. Existing weak-reference tests still
inspect the actual context, rather than merely checking that its wrapper dies.
Strengthened tests also resolve metadata after the previous compilation is
collected and verify old/new member surfaces after same-path binary replacement.

Next: cover resolver precedence and malformed-reference/fallback behavior before
separating target reference admission from host runtime path registration.
Main-based extraction remains pending; no neoCLR-specific policy was introduced.

Validation used SDK `11.0.100-rc.1.26425.128` and the same build/test commands
recorded for slice 3. The 89-test focused baseline passed before changes; all
89 passed again after extraction and stronger assertions, with no failures or
skips on net11.0. The compiler built for net10.0 and net11.0 with no warnings
or errors. Whitespace formatting completed (the test formatter reported
workspace-load warnings), and `git diff --check` passed. This is modern .NET
validation, not neoCLR or browser execution or full bootstrap qualification.
