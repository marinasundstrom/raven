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

## Slice 5: reference resolution compatibility coverage

Added ten cases in `DotNetMetadataResolutionTests`, using disposable temporary
directories and minimal CLI assemblies with distinct public type surfaces:

- Duplicate full identities preserve input precedence before path sorting.
- Exact identity selection takes priority over simple-name fallback.
- Simple-name fallback follows sorted paths, independently of input order.
- Missing, malformed, empty, and invalid-path candidates do not prevent valid
  reference resolution during context construction.
- An assembly loaded in the host is not implicitly available to the resolver.
- Missing/corrupt path loads can use a supplied identity fallback; without one,
  the original missing-file or invalid-image failure is reported.

These characterize current .NET behavior, including permissive fallback. They
do not introduce strict target admission or new compiler diagnostics. The
compiler implementation is unchanged. The import guide now describes the rules
and distinguishes candidate filtering from direct reference loading.

The 89-test baseline passed before changes. All ten new cases passed on net11.0
with SDK `11.0.100-rc.1.26425.128`, using the test project with
`--no-restore --filter 'FullyQualifiedName~DotNetMetadataResolutionTests'
/property:WarningLevel=0 /property:BuildProjectReferences=false`.
Whitespace formatting completed with workspace-load warnings.

The post-change combined filter (slice 4's four classes plus
`DotNetMetadataResolutionTests`) passed all 99 tests with no failures/skips,
using `--no-build --no-restore /property:WarningLevel=0` after rebuilding the
test project. `git diff --check` passed. No compiler production code changed;
the tested execution target was .NET 11, not neoCLR or browser/WASI.

Next: separate reference-set construction from host path registration, preserving
the characterized policies. Exact-identity admission and configuration diagnostics
remain distinct future decisions; this coverage does not authorize changing them.

## Slice 6: immutable reference inputs and explicit host registration

`DotNetMetadataReferenceSet` now materializes normalized, identity-deduplicated,
ordered reference entries using immutable strings and paths. It preserves the
existing second identity-read/admission pass; this is an ownership extraction,
not an I/O optimization. It snapshots selected inputs, not file contents.

Context/session construction consumes that set without registration callbacks.
`Compilation.CreateMetadataSession` explicitly registers its selected paths before
constructing a new session. This preserves an important existing distinction:
the resolver's simple-name map keeps the first sorted candidate, while host
registration ends with the last selected candidate of that name. Reused sessions
do not newly register paths, just as before.

Two new tests cover retained ordered candidates (including duplicate identity
elimination) and reuse of a reference set after the caller mutates its input
list. The original ten precedence/failure cases now use the explicit reference
set boundary. Host policy, import configuration, and public APIs are unchanged.

Next: inventory concrete PE assembly discovery requirements in shared symbol
lookup and establish an imported-symbol contract with a .NET implementation.
Host runtime lookup still uses the shared registry; removing or changing its
policy remains separate work. Main-based extraction remains pending.

Validation with SDK `11.0.100-rc.1.26425.128`:

- The slice 5 combined baseline passed all 99 tests before edits.
- `dotnet build src/Raven.CodeAnalysis/Raven.CodeAnalysis.csproj --no-restore
  --property WarningLevel=0` passed for net10.0 and net11.0 with no warnings/errors.
- The same five-class filter passed all 101 tests after edits on net11.0, using
  `dotnet test test/Raven.CodeAnalysis.Tests/Raven.CodeAnalysis.Tests.csproj
  --no-restore /property:WarningLevel=0 /property:BuildProjectReferences=false`
  and the freshly built compiler. No tests failed or were skipped.
- Whitespace formatting completed (test workspace-load warnings only), and
  `git diff --check` passed.

This is .NET validation, not evidence of neoCLR, browser/WASI, .NET Framework,
or NanoFramework execution. No performance improvement or bootstrap qualification
is claimed.

## Slice 7: imported assembly discovery contract

The shared lookup audit found two concrete PE assembly dependencies: simple-name
type discovery and extension-conversion container discovery. Both now consume
`IImportedAssemblySymbol`, an internal extension of the existing assembly symbol
contract. `PEAssemblySymbol` implements it using the unchanged metadata index.
The boundary carries Raven symbols only; no reflection or cache helper APIs are
added to public compiler or language-service surfaces.

Source-first lookup, provider ordering, arity matching, lazy metadata indexing,
and conversion applicability remain unchanged. Strengthened nested-type coverage
checks the imported contract, and a new source/metadata collision test checks
arity discrimination, repeated queries, and absent arities.

Remaining shared lookup dependencies include `PENamespaceSymbol` in namespace
selection and extension-method discovery. The next slice should address that
namespace contract, including merged namespaces. This is not yet a complete
provider-independent symbol implementation; metadata construction and emitter
reflection use remain .NET-specific. Main-based extraction is still pending.

Validation with SDK `11.0.100-rc.1.26425.128`:

- Baseline `scripts/test-feature-suite.sh extensions`: 140 passed. Separately,
  `CompilationSymbolLookupTests|ConversionOperatorBindingTests` passed 13 tests
  using `dotnet test` with `--no-build --no-restore /property:WarningLevel=0`.
- Compiler build passed for net10.0 and net11.0 with no warnings/errors. The
  targeted build ran the bound/symbol generator after the symbol file changed;
  generated source has no tracked diff and no model definition changed.
- Post-change `dotnet test test/Raven.CodeAnalysis.Tests/Raven.CodeAnalysis.Tests.csproj
  --no-restore /property:WarningLevel=0 /property:BuildProjectReferences=false`
  selected the eight extension-suite classes plus `CompilationSymbolLookupTests`
  and `ConversionOperatorBindingTests`: 154 passed, no failures/skips on net11.0.
  The referenced compiler was rebuilt; Core/Macros came from the baseline build.
- Whitespace formatting completed with test workspace-load warnings, and
  `git diff --check` passed.

No neoCLR runtime, .NET Framework, NanoFramework, or bootstrap qualification is
claimed. This is a semantic lookup change; emission implementation was untouched.

## Slice 8: target direction and namespace discovery capability

Commit `bbad32bba` records ADR-0003: metadata and symbol import belong to the
selected target alongside runtime representation and emission. Raven owns its
semantic model and may redesign controlled APIs without matching Roslyn's API
shapes. Target changes can require reimport/rebinding. Existing discovery
interfaces are extraction steps, not promises of a universal assembly model.

`INamespaceExtensionLookup` describes a discovery capability rather than imported
origin. PE namespaces provide it and merged namespaces compose it. Shared lookup
and merged extension discovery no longer dispatch on concrete PE namespace types.
General namespace/type traversal still accepts namespaces without this capability;
only the extension candidate query requires it. Source traversal remains separate.

Three new cases use a non-PE namespace implementation: direct discovery, discovery
through a nested merge with a source namespace, and ordered aggregation with
duplicate containers. Missing/blank names and duplicate results are checked.
Existing .NET extension tests cover applicability and imported metadata behavior.

Next: design target service ownership and inventory symbol construction/runtime
representation dependencies against ADR-0003. Avoid treating the temporary
assembly/namespace interfaces as a fixed public API. Main extraction and proper
neoCLR implementation remain pending.

Follow-up clarification `201ad7c13` names the runtime/platform contract as the
central abstraction. One or more supported symbol sources supply its type
environment; one or more compatible code generators implement its semantics,
representations, and feature restrictions. Sources need not be metadata files.
Known-platform rules and unsupported-feature diagnostics belong in this design.

Validation with SDK `11.0.100-rc.1.26425.128`:

- Pre-change baseline: 154 lookup/conversion/extension tests plus four
  `MergedNamespaceSymbolTests` passed on net11.0.
- Compiler builds passed for net10.0 and net11.0. The targeted build refreshed
  generated symbol files; no tracked generated-source diff remains.
- After fixing the shared helper's return type and the test provider's visitor
  implementations, the combined suite passed all 161 tests with no failures or
  skips. The filter adds `MergedNamespaceSymbolTests` to slice 7's classes, using
  `dotnet test test/Raven.CodeAnalysis.Tests/Raven.CodeAnalysis.Tests.csproj
  --no-restore /property:WarningLevel=0 /property:BuildProjectReferences=false`.
- Whitespace formatting completed with test workspace-load warnings;
  `git diff --check` passed. No concrete PE dependencies remain in
  `CompilationSymbolLookup` or merged namespace extension discovery.

Execution evidence is .NET 11 only. This slice does not implement selectable
runtime/platform contracts or validate neoCLR execution.

## Slice 9: runtime/platform selection and compatibility design

Added `runtime-platform-contract-design.md`, grounded in current
`CompilationOptions`, `Compilation.Setup`, `Compilation.TargetCore.cs`,
`EmitOptions`, project evaluation, and known neoCLR representation checks.
It defines configuration resolution, source-environment validation, binding,
backend admission, and emission as separate stages under one resolved contract.

The design permits multiple source implementations and code generators, with
explicit compatibility rules. It distinguishes platform feature restrictions,
backend limitations, ordinary missing APIs, and supported lowering/emulation.
It records semantic-versus-emission invalidation and the compiler-host boundary,
including host-executable macros. It does not mandate a universal IR or plugin ABI.

Updated architecture navigation and the Runtime Contracts guide, whose existing
CLI-specific definition is now clearly scoped to the current implementation.
No compiler API or behavior changed. The next implementation slice is the
option/consumer inventory and invalidation classification, followed by internal
resolved .NET contract selection.

Validation: `git diff --check` and local Markdown-link checks passed. No builds or
tests were run for this documentation-only slice. Contract selection and additional
backends remain unimplemented; earlier execution evidence is unchanged.

## Slice 10: semantic reference loader boundary

Scope correction `3165d9d01` limits current delivery to coherent loader,
platform/runtime contract, and codegen trios: .NET first, neoCLR later. No
cross-compilation, arbitrary backend mixing, or native backend work is scheduled.

Reference import now calls `ISemanticDataLoader.LoadReference`.
`DotNetSemanticDataLoader` owns reflection assembly caches, PE assembly/module
symbol construction, recursive dependency loading, and omission of non-managed
inputs. Compilation-reference handling remains shared. Host assembly-path and
runtime registration remain compilation services used by the .NET adapter;
the interface itself exposes only existing reference inputs and semantic symbols.

The loader is created per compilation, while compatible metadata sessions remain
shared. Added a regression checking distinct imported type/assembly objects across
snapshots sharing a context, stable symbol identity within each snapshot, and
continued member lookup from both snapshots.

Selection still constructs the .NET implementation directly during setup. The
loader boundary is consumed, but full replacement is not yet wired: setup/core
selection, reflection type projection, and emission still have .NET dependencies.
Next: move setup and core semantic resolution under the loader/target composition
without exposing reflection objects in the shared contract. Do not add an arbitrary
public loader switch independent of the platform contract and codegen.

Validation with SDK `11.0.100-rc.1.26425.128`: the pre-change filter covering
`MetadataImportOptionsTests`, `IncrementalCompilationReuseTests`,
`CliMetadataCompatibilityTests`, `CompilationSymbolLookupTests`, and
`DotNetMetadataResolutionTests` passed 107 tests. After extraction, the same filter
passed all 108 tests with no failures/skips on net11.0, using
`dotnet test test/Raven.CodeAnalysis.Tests/Raven.CodeAnalysis.Tests.csproj
--no-restore /property:WarningLevel=0 /property:BuildProjectReferences=false`.
The referenced compiler was freshly built for net10.0 and net11.0, with no
warnings/errors. Whitespace formatting completed with test workspace-load warnings;
`git diff --check` passed. No neoCLR execution or full bootstrap qualification is
claimed.
