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

## Slice 11: loader-owned .NET session setup

`DotNetSemanticDataLoader.OpenSession` owns reference-path selection, discovery of
the metadata core defining System.Object, optional host fallback inputs, and fresh
session construction. The .NET loader registers selected shared paths through
compilation host services. `Compilation` retains reference-fingerprint checks and
decides whether a prior session can be reused; imported symbols remain per-snapshot.

Two new tests verify that supplied net10.0/net11.0 reference-core identities win
over previously populated host fallback paths, and that semantic Object lookup
uses the reference surface. Existing explicit-only import, missing-core, metadata
identity, runtime, and snapshot tests remain the behavior boundary.

Special-type names and protocol mappings are deliberately not moved into the
loader: those belong to the platform/runtime contract. Compilation still holds
reflection core handles and selects the .NET loader directly. Full target
replacement is not implemented. Next: define the minimal coherent target composition
and move semantic type/protocol mappings to its contract, alongside the loader
and existing .NET code generator. No arbitrary component mixing is planned.

Validation with SDK `11.0.100-rc.1.26425.128`: baseline passed 108 import/reuse/CLI
metadata/lookup/resolver cases plus 14 core identity/selection cases. The combined
filter passed all 124 tests after extraction, with no failures/skips on net11.0,
using `dotnet test test/Raven.CodeAnalysis.Tests/Raven.CodeAnalysis.Tests.csproj
--no-restore /property:WarningLevel=0 /property:BuildProjectReferences=false`.
Compiler builds passed for net10.0 and net11.0. Whitespace formatting completed
with test workspace-load warnings, and `git diff --check` passed. Test execution
was on .NET 11 with .NET 10/11 reference inputs; no neoCLR execution or full
bootstrap qualification is claimed.

## Slice 12: coherent .NET target composition

`DotNetCompilationTarget` composes loader/session creation, the runtime contract,
and existing CLI codegen. Each compilation owns a target built from its options;
normal emission and the lowered macro-plugin compilation both use that target.
Diagnostics and target-core checks remain ahead of emission. No public target
selection or independently interchangeable component switches are added.

`DotNetRuntimeContract` now owns special-type metadata names, preferred special-type
assembly, and tuple-family mapping. Shared compilation code retains symbol lookup,
Unit, and per-snapshot caches. Existing experimental task and tuple mappings are
preserved rather than interpreted as a functioning neoCLR target. Other protocols,
feature policies, reflection cores, and runtime type projection remain follow-up
work. The composition is deliberately concrete while these .NET dependencies
remain; a platform-neutral provider interface would currently overpromise.

Added public symbol-behavior coverage with net10.0/net11.0 reference assemblies:
Object, enumeration protocols, generic Task, tuple and async builder special types
resolve from the imported contract, with stable identity even when a source type
collides with the enumeration interface name. Existing runtime, tuple metadata,
core-selection diagnostics and reusable macro-plugin coverage exercise the composed
emission entry points.

Next: continue removing reflection core/type-projection dependencies from shared
compilation services before introducing a replaceable target boundary. General
refactoring still needs independent main-based validation; this experiment-derived
branch and its neoCLR-specific mappings are not candidates for wholesale merging.

Validation with SDK `11.0.100-rc.1.26425.128`: pre-change baseline passed 145
import/reuse/CLI metadata/lookup/resolver/core selection/tuple/attached-macro cases.
After extraction, the same filter plus the two new special-type cases and
`MacroLibrary_EmitsReusableCompilerPluginFromSingleSourceTree` passed 148 tests,
with no failures/skips on net11.0. Compiler builds passed for net10.0 and net11.0.
Tests used `--no-restore /property:WarningLevel=0
/property:BuildProjectReferences=false` after the compiler build. The existing
compiler-driver subprocess fixture used its previously built driver; direct
compiler API and emission tests used the freshly built compiler. Whitespace
formatting completed with test workspace-load warnings; `git diff --check` passed.
Execution was on .NET 11 with .NET 10/11 reference inputs. No neoCLR execution or
full bootstrap qualification is claimed.

## API direction clarification: CompilationOptions

The agreed public options name remains `CompilationOptions`. Documented the roles
of TargetPlatform, Raven LangVersion, Contract and requested Features, immutable
.NET presets, explicit framework references, shared parsing/compilation language
settings, and staged validation. These are planned API members until implemented;
loader and codegen remain separately implemented but publicly selected together.
Next implementation slice: a .NET preset with explicit-only reference inputs and
core discovery from those references, retaining existing host-assisted constructors.
Documentation-only; no compiler behavior changed in this clarification.

## Slice 13: explicit-reference .NET preset

`CompilationOptions.DotNet` supplies fresh defaults with explicit-only reference
loading and automatic core discovery. Parameterless `MetadataImportOptions`
selects this mode; its null CoreAssemblyName is distinct from null import options
(the legacy host-assisted mode). Named-core import retains its previous independent
emission policy. The preset resolves no frameworks or packages and adds no host
reference paths; callers continue supplying references separately.

Discovered-core mode also selects the same core identity for emission. Explicit
target-core names must match the discovered simple name, and explicit emission
identity conflicts produce RAVT003 before writing output. This uses existing
import/core configuration as a transitional implementation; public TargetPlatform,
LangVersion, Contract and Features are still planned, not placeholder APIs.
Missing-core setup continues to throw FileNotFoundException and is documented.

Coverage exercises net10.0/net11.0 binding and emitted core identities, preservation
through option copies, fresh preset defaults, isolation after a host-assisted
compilation with incremental reuse attempted, empty references, and matching or
conflicting explicit target/emission selections. General main integration remains
pending independent validation; no neoCLR policy is promoted by this slice.

Validation with SDK `11.0.100-rc.1.26425.128`: baseline passed 114 tests across
MetadataImportOptionsTests, IncrementalCompilationReuseTests,
DotNetMetadataResolutionTests, MetadataCoreIdentityTests, TargetCoreSelectionTests,
and TargetSpecialTypeTests. The same filter plus DotNetCompilationPresetTests
passed 122 tests, with no failures/skips. Compiler builds passed for net10.0 and
net11.0. Tests ran on .NET 11 using freshly built compiler outputs and
`--no-restore /property:WarningLevel=0 /property:BuildProjectReferences=false`;
the existing driver subprocess test used its previously built driver. Whitespace
formatting completed with test workspace-load warnings; `git diff --check` passed.
.NET 10/11 reference metadata was validated; no .NET Framework, NanoFramework,
neoCLR execution, or full bootstrap qualification is claimed.

Next: validate configuration before opening the symbol environment, translating
known setup failures into compiler diagnostics, while continuing to separate
reflection-dependent compilation services. Language version work must include
parsing and syntax-tree compatibility rather than merely adding an options field.

## Slice 14: core-session initialization diagnostics

The .NET loader distinguishes expected core-session initialization failures from
unrelated exceptions. Explicit-only discovery without a core defining System.Object
fails before attempting host-core fallback. Known I/O, access, invalid-image and
type-load failures from opening a fresh metadata session carry target-initialization
failure information to the compilation boundary.

Compilation-wide, tree-scoped and document-scoped diagnostic collection return
RAVT004 for these failures. Emit rejects them before source declaration binding or
writing either output stream, including calls with supplied diagnostics. A supplied
copy of the same diagnostic is retained without duplication. This fatal diagnostic
cannot be suppressed or downgraded into permission to use incomplete state.
Syntax-only diagnostic collection no longer initializes the target.

Existing setup locking/finally cleanup remains authoritative: failed setup is not
marked complete or eligible for session reuse. Tests cover named/discovered missing
cores, repeated/concurrent diagnostic calls, both output streams, precomputed
diagnostics, severity configuration, syntax diagnostics, cancellation, and successful
initialization of a new snapshot after adding references. The original invalid
snapshot remains invalid.

The boundary intentionally does not catch arbitrary binding/emission exceptions or
all later dependency-loading failures. Direct semantic queries still require a valid
target and may throw on failed setup; no partial semantic environment is promised.
Next: validate target configuration before core loading where possible, and continue
removing reflection dependencies from shared compilation services. General changes
still require independent main-based integration; no experimental branch merge is
implied.

Validation with SDK `11.0.100-rc.1.26425.128`: the pre-change import/reuse/resolver/
core/preset filter passed 122 tests. Final validation added
TargetInitializationDiagnosticTests, DocumentScopedDiagnosticsTests, and
DiagnosticOptionsTests and passed 146 tests with no failures/skips on net11.0.
One initial regression fixture assumed an incomplete class produced a parser
error; it was replaced with the existing missing-method-return-type parser case.
Compiler builds passed for net10.0 and net11.0. Tests used freshly built compiler
outputs with `--no-restore /property:WarningLevel=0
/property:BuildProjectReferences=false`; the existing driver subprocess fixture
used its previously built driver. Whitespace formatting completed with test
workspace-load warnings, and `git diff --check` passed. Execution was on .NET 11;
.NET 10/11 reference inputs were covered. No .NET Framework, NanoFramework,
neoCLR execution, or bootstrap qualification is claimed.

## Slice 15: configuration validation before metadata loading

`DotNetRuntimeContract` owns configuration-only checks for explicit target/import
core consistency, unit contract names/core selection, and required typeof contract
names. Diagnostic collection and emission run these checks before EnsureSetup;
RAVT003 identifies the first contradiction before missing-reference failures can
produce RAVT004. Preflight configuration errors are fatal regardless of suppression
or severity overrides, and output streams remain untouched.

Post-load checks remain responsible for discovered core identity and resolved
unit/typeof symbol shape. Successful option checks do not assert that the target
exists or supports all requested operations. Syntax-only diagnostics still avoid
target initialization; no new direct-semantic-query validation API is introduced.

Coverage uses invalid configurations with no supplied references (explicit-reference
cases would fail loading) to establish diagnostic precedence across
compilation/tree/document APIs and emit
with supplied diagnostics. Consistent configurations with missing references still
reach RAVT004. Existing valid/invalid unit and typeof contract tests exercise
post-load validation and successful emission/runtime behavior.

Next: continue separating target-specific semantic configuration from shared
compilation services, keeping the planned TargetPlatform/Contract API coherent.
Main integration still requires independent validation; neoCLR-specific policies
remain experimental and no wholesale branch merge is proposed.

Validation with SDK `11.0.100-rc.1.26425.128`: baseline passed 59 tests across
MetadataImportOptionsTests, TargetCoreSelectionTests, DotNetCompilationPresetTests,
TargetInitializationDiagnosticTests, RuntimeUnitContractTests, and
RuntimeTypeOfContractTests. Adding TargetConfigurationDiagnosticTests produced
72 passing tests with no failures/skips. Compiler builds passed for net10.0 and
net11.0. Tests ran on .NET 11 with freshly built compiler outputs using
`--no-restore /property:WarningLevel=0 /property:BuildProjectReferences=false`;
the existing driver subprocess fixture used its previously built driver.
Whitespace formatting completed with test workspace-load warnings, and
`git diff --check` passed. No .NET Framework, NanoFramework, neoCLR execution,
or full bootstrap qualification is claimed.

## Slice 16: target-owned resolved contract validation

`DotNetRuntimeContract` now resolves typeof providers and validates resolved unit
and core contracts using semantic symbols. The typeof binding result lives beside
the contract, while compilation retains its internal forwarding method for binders
and codegen. No cross-snapshot resolved-symbol cache is introduced.

`DotNetCompilationTarget` owns reflection core-identity access for validation and
emission-option selection. Compilation translates target errors into RAVT003 and
retains semantic lookup/caches. Assembly-qualified metadata lookup is made internal
for the contract adapter, preserving its existing behavior without adding public
or language-service APIs. Pre-load checks, post-load symbol checks and explicit
emit-identity conflict checks retain their ordering and policies.

Existing source/imported typeof runtime cases and unit/core tests exercise the
extraction. Added inaccessible context-class and Current-getter cases and verify
all malformed-provider cases reject emission without writing output.

Next: continue reducing reflection core/type-projection dependencies in shared
compilation before exposing a replaceable target. Independent main-based validation
remains required for integration; experimental neoCLR mappings stay on this branch.

Validation with SDK `11.0.100-rc.1.26425.128`: baseline passed 57 tests across
TargetCoreSelectionTests, DotNetCompilationPresetTests,
TargetInitializationDiagnosticTests, TargetConfigurationDiagnosticTests,
RuntimeUnitContractTests and RuntimeTypeOfContractTests. The same filter with the
two additional provider cases passed 59 tests, with no failures/skips on net11.0.
The first build exited with status 1 without an error diagnostic; a rerun succeeded
for net10.0 and net11.0 with no warnings/errors. Tests used the rebuilt compiler
with `--no-restore /property:WarningLevel=0 /property:BuildProjectReferences=false`;
the existing driver subprocess fixture used its previously built driver.
Whitespace formatting completed with test workspace-load warnings and
`git diff --check` passed. No .NET Framework, NanoFramework, neoCLR execution or
full bootstrap qualification is claimed.

## Slice 17: .NET host runtime path policy

`DotNetRuntimeAssemblyPathResolver` owns NuGet ref/lib mapping, SDK-pack mapping,
recognized shared-framework package mapping, and version/path candidate ordering.
Compilation host registration requests a candidate through DotNetCompilationTarget,
then retains its existing runtime assembly loading and cache behavior. Semantic
reference-set selection is unchanged and never augmented by these runtime paths.

An internal shared-framework-root parameter makes filesystem policy testable without
relying on installed host frameworks. Production still derives its root lazily from
the host runtime. New tests cover exact NuGet lib preference, existing descending
lib-path fallback, SDK exact-version mapping, both recognized framework packages,
stable/numeric/same-major shared-framework preference, missing candidate files,
package-lib fallback, and absent/non-reference paths. Fixtures test path selection
only and do not pretend to be loadable runtime assemblies.

No fallback policy is redesigned in this extraction. In particular, NuGet lib
fallback remains path-ordered and is not claimed to perform framework compatibility
resolution. Next: separate host assembly loading/caches and reflection projection
from shared compilation services. Main integration remains pending independent
validation; no neoCLR-specific policy is promoted.

Validation with SDK `11.0.100-rc.1.26425.128`: baseline passed 49 tests across
MetadataImportOptionsTests, MetadataCoreIdentityTests, DotNetCompilationPresetTests,
RuntimeTypeOfContractTests and CliMetadataCompatibilityTests. The same filter plus
DotNetRuntimeAssemblyPathResolverTests passed 57 tests with no failures/skips.
Compiler builds passed for net10.0 and net11.0 with no warnings/errors. Tests ran
on .NET 11 using freshly built compiler outputs with
`--no-restore /property:WarningLevel=0 /property:BuildProjectReferences=false`.
Whitespace formatting completed with test workspace-load warnings;
`git diff --check` passed. Temporary-layout tests establish path-selection behavior,
not execution on those synthetic frameworks. No .NET Framework, NanoFramework,
neoCLR execution or full bootstrap qualification is claimed.

## Slice 18: .NET host assembly service

`DotNetHostRuntime` owns per-compilation path/runtime caches and metadata-to-runtime
assembly associations, alongside the existing process-wide path/runtime caches.
It also owns trusted-platform discovery, assembly loading, runtime type lookup and
host emit-core discovery. DotNetCompilationTarget creates one service per compilation;
shared compilation keeps internal delegates with existing setup/argument checks.
The service stores neither a compilation reference nor semantic symbols.

Lookup order, global cache lifetime, core aliases, default AssemblyLoadContext
loading and existing failure fallbacks are preserved. Metadata sessions and symbol
caches retain separate ownership; sharing a metadata session does not share a host
service. New behavior tests check local path registration survives later shared
registration, later services see shared registrations, runtime assemblies are reused,
and metadata core types map to executable host types without replacing metadata
assembly objects. Existing snapshot lifetime and explicit-import tests cover the
integration boundary.

Next: move reflection-to-semantic-symbol projection ownership behind the .NET
loader/target while retaining snapshot identity and setup reentrancy. Reflection
core APIs still exist on Compilation; this slice does not make the target replaceable.
Main integration remains pending independent validation, and experimental neoCLR
policy remains on the experiment-derived branch.

Validation with SDK `11.0.100-rc.1.26425.128`: baseline passed 128 tests across
MetadataImportOptionsTests, IncrementalCompilationReuseTests,
CliMetadataCompatibilityTests, MetadataCoreIdentityTests,
DotNetCompilationPresetTests, RuntimeTypeOfContractTests and
DotNetRuntimeAssemblyPathResolverTests. Adding DotNetHostRuntimeTests produced
131 passing tests with no failures/skips. Compiler builds passed for net10.0 and
net11.0 with no warnings/errors. Tests ran on .NET 11 with freshly built compiler
outputs using `--no-restore /property:WarningLevel=0
/property:BuildProjectReferences=false`. Whitespace formatting completed with
test workspace-load warnings; `git diff --check` passed. No .NET Framework,
NanoFramework, neoCLR execution or full bootstrap qualification is claimed.

## Slice 19: target-owned reflection-to-symbol projector

DotNetCompilationTarget owns one lazy ReflectionTypeLoader per compilation and
passes it explicitly to DotNetSemanticDataLoader for imported modules. Compilation's
existing reflection entry points forward to that same projector. No projection
method is added to the platform-neutral semantic loader interface, and no projection
algorithm or public API is redesigned in this slice.

The projector can be allocated before setup without binding or loading references;
this preserves cold reflection queries and setup reentrancy. Lazy publication avoids
multiple projector instances under concurrent requests. This is not a claim that
all projection operations have acquired new concurrency guarantees. Metadata sessions
remain shareable independently; projection caches and symbols are snapshot-owned.

New tests cover concurrent pre-setup projector requests without references, cold
public reflection queries sharing symbol identity with imported generic members,
and distinct projected generic types/arguments across snapshots sharing a metadata
session. Existing nullability, metadata-reference and snapshot-lifetime cases cover
the surrounding behavior.

Next: reduce reflection core/session handles exposed by shared compilation setup
before selecting a second target implementation. Main integration remains pending
independent validation; no neoCLR-specific integration is promoted.

Validation with SDK `11.0.100-rc.1.26425.128`: baseline passed 134 tests across
MetadataImportOptionsTests, IncrementalCompilationReuseTests,
CliMetadataCompatibilityTests, MetadataCoreIdentityTests,
DotNetCompilationPresetTests, NullableMetadataBindingTests and
MetadataReferenceResolutionTests. Final validation added
ReflectionProjectionOwnershipTests and ReflectionTypeLoaderNestedTypeTests and
passed 140 tests with no failures/skips. The initial cold-query fixture selected
all imported Item properties; it was narrowed to the public indexer to exclude
explicit-interface indexers. Compiler builds passed for net10.0 and net11.0 with
no warnings/errors. Tests ran on .NET 11 with freshly built compiler outputs using
`--no-restore /property:WarningLevel=0 /property:BuildProjectReferences=false`.
Whitespace formatting completed with test workspace-load warnings;
`git diff --check` passed. No .NET Framework, NanoFramework, neoCLR execution or
full bootstrap qualification is claimed.

## Slice 20: target-owned metadata session and core handles

DotNetCompilationTarget owns the metadata session, its prior-snapshot reuse
candidate, and metadata/runtime/emit core handles. Compilation keeps import-option
and portable-reference fingerprint compatibility checks, then asks its target to
initialize a semantic loader. Existing reflection core properties forward without
triggering setup. Target validation and emission use the owning compilation rather
than accepting another compilation as an argument.

Host handles are seeded before fingerprint capture and same-thread setup
reentrancy, preserving initialization order. Reuse adoption retains only the
compilation-independent session, never the earlier target, projector or host
service. Snapshot regression coverage now also verifies metadata core identity
sharing and executable host-core identity, alongside separate symbols/projectors,
reference replacement, option changes and collection lifetime.

Next: narrow remaining shared-layer reflection adapters and identify the minimum
semantic core information needed by a replaceable target. Loading and codegen
still use .NET reflection; no second target or public provider registry is added.
Main integration still needs independent main-based validation, with experimental
neoCLR policies kept separate.

Validation with SDK `11.0.100-rc.1.26425.128`: baseline passed 139 tests across
MetadataImportOptionsTests, IncrementalCompilationReuseTests,
CliMetadataCompatibilityTests, MetadataCoreIdentityTests,
DotNetCompilationPresetTests, TargetCoreSelectionTests,
TargetInitializationDiagnosticTests, TargetConfigurationDiagnosticTests,
ReflectionProjectionOwnershipTests and DotNetHostRuntimeTests. Final validation
added RuntimeTypeOfContractTests and
MacroLibrary_EmitsReusableCompilerPluginFromSingleSourceTree and passed 154 tests
with no failures/skips. Compiler builds passed for net10.0 and net11.0 with no
warnings/errors. Tests ran on .NET 11 using freshly built compiler outputs with
`--no-restore /property:WarningLevel=0 /property:BuildProjectReferences=false`.
Whitespace formatting completed with test workspace-load warnings;
`git diff --check` passed. No .NET Framework, NanoFramework, neoCLR execution or
full bootstrap qualification is claimed.

## Slice 21: explicit .NET loader dependencies

DotNetCompilationTarget supplies reference inputs, metadata import options and its
host service to session setup. It constructs the loader with that session, the
compilation-bound reflection projector and the same host service. The loader now
uses the service directly for runtime registration and metadata paths, and reads
CLI identities through DotNetMetadataContextFactory. Six unused forwarding methods
are removed from Compilation.

This removes the loader's direct Compilation dependency, not its snapshot identity:
the projector still belongs to one compilation and imported symbols must remain
local to that snapshot. Reference ordering, registration ordering, fallback rules
and session reuse remain unchanged. New coverage registers a host-only CLI fixture
and verifies that host-assisted setup can resolve it while explicit-reference
setup cannot, even though the supplied host service knows its path.

Next: narrow reflection-facing semantic helpers and the imported-symbol/codegen
boundary. This remains concrete .NET composition; no generic host-service interface,
second target, or cross-compilation support is introduced. Main integration still
requires independent main-based validation; neoCLR-specific policy stays separate.

Validation with SDK `11.0.100-rc.1.26425.128`: baseline passed 136 tests across
MetadataImportOptionsTests, IncrementalCompilationReuseTests, DotNetHostRuntimeTests,
DotNetMetadataResolutionTests, MetadataReferenceResolutionTests,
DotNetCompilationPresetTests and TargetInitializationDiagnosticTests. Final
validation passed 138 tests with the two new import-mode cases, no failures/skips.
Compiler builds passed for net10.0 and net11.0 with no warnings/errors. Tests ran
on .NET 11 with freshly built compiler outputs using
`--no-restore /property:WarningLevel=0 /property:BuildProjectReferences=false`.
Whitespace formatting completed with test workspace-load warnings;
`git diff --check` passed. No .NET Framework, NanoFramework, neoCLR execution or
full bootstrap qualification is claimed.

## Slice 22: constructed-type reflection resolution in codegen

ConstructedNamedTypeSymbol no longer builds reflection generic types or maps
runtime generic parameters. ConstructedTypeCodeGenResolver in the .NET codegen
layer performs that work using the symbol's existing definition/substitution APIs
and the current CodeGenerator's builders and parameter cache. TypeGenerator and
substituted member resolution call the backend helper directly. No reflection
operation is added to the semantic interfaces, and an unused signature-placeholder
helper is removed from the constructed-type implementation.

The resolution algorithm is preserved, including imported/source definitions,
extension declaration handling, async state-machine parameter mapping and fallback
order. A new runtime regression emits the same compilation twice, constructs an
imported List of source Box<T> values through a generic method, and checks values,
type arguments and the source type's emitted assembly ownership each time.

Substituted method and field implementations still contain reflection resolution;
extracting those is next. This slice does not claim all symbols are independent
of codegen. No new target or public API is introduced. Main integration still
requires independent main-based validation, with neoCLR-specific policies kept
separate.

Validation with SDK `11.0.100-rc.1.26425.128`: baseline passed 23 tests across
GenericReferenceEmissionTests, AsyncGenericContainingTypeTests,
AsyncGenericCaptureTests, ReflectionTypeLoaderNestedTypeTests and
TypeMetadataNameTests. Final validation added the repeated-emission test,
GenericSelfConstructionTests, TargetCoreGenericSignatureTests,
MixedGenericMetadataTests and ImportedGenericMethodContextTests: 44 passed,
no failures/skips. The initial extraction build exposed Raven's TypeInfo name
shadowing System.Reflection.TypeInfo; the return type was fully qualified.
Final compiler builds passed for net10.0 and net11.0 with no warnings/errors.
Tests ran on .NET 11 with freshly built compiler outputs using
`--no-restore /property:WarningLevel=0 /property:BuildProjectReferences=false`.
Whitespace formatting completed with test workspace-load warnings;
`git diff --check` passed. No .NET Framework, NanoFramework, neoCLR execution or
full bootstrap qualification is claimed.

## Slice 23: substituted member resolution in codegen

SubstitutedMemberCodeGenResolver owns reflection constructor, method and field
resolution formerly implemented by SubstitutedMethodSymbol and
SubstitutedFieldSymbol. Callers dispatch directly to the backend resolver.
Substituted fields expose their original semantic definition internally; methods
already expose it. Containing types and substituted parameter/field types remain
semantic inputs. No reflection or CodeGenerator reference remains in the
ConstructedNamedTypeSymbol file, including its substituted-symbol implementations.

Lookup and fallback order, TypeBuilder handling, signature-placeholder checks,
metadata-token matching, async parameter projections and per-CodeGenerator member
caches are preserved. The repeated-emission runtime regression now includes a
public generic source field, reading an imported ValueTuple<T> field, writing and
reading the source field, and constructing another source instance from its value.
It continues to check values, type arguments and emitted assembly ownership.

Next: extract backend resolution from ConstructedMethodSymbol. Other symbols and
compiler adapters remain .NET-specific; this slice does not establish a fully
replaceable target. Independent main-based validation remains required before
integration, and neoCLR-specific policies remain separate.

Validation with SDK `11.0.100-rc.1.26425.128`: baseline passed 36 tests across
GenericReferenceEmissionTests, AsyncGenericContainingTypeTests,
AsyncGenericCaptureTests, GenericSelfConstructionTests,
TargetCoreGenericSignatureTests, MixedGenericMetadataTests and
ImportedGenericMethodContextTests. Final validation included
RuntimeSymbolResolverTests and ConstructedMethodSymbolTests and passed 57 tests,
no failures/skips. Compiler builds passed for net10.0 and net11.0 with no
warnings/errors. Tests ran on .NET 11 with freshly built compiler outputs using
`--no-restore /property:WarningLevel=0 /property:BuildProjectReferences=false`.
Whitespace formatting completed with test workspace-load warnings;
`git diff --check` passed. No .NET Framework, NanoFramework, neoCLR execution or
full bootstrap qualification is claimed.

Final test review strengthened the field-write fixture to replace a different
initial value, making a missing write observable. That focused test was rebuilt
and rerun successfully after the 57-test pass.

## Slice 24: constructed generic method resolution in codegen

ConstructedMethodCodeGenResolver owns the reflection method search, signature
matching, runtime argument projection, async/closure parameter remapping and
fallback policies previously implemented on ConstructedMethodSymbol. The shared
method-codegen dispatcher calls it directly. The resolver is stateless and uses
the current CodeGenerator's builders and runtime caches.

ConstructedMethodSymbol keeps semantic construction and substitution. An internal
TryGetTypeSubstitution lookup exposes an existing semantic mapping without exposing
the dictionary or reconstructing it in the backend. This preserves canonicalized
method parameters and containing-type substitutions. The symbol file no longer
references reflection or codegen. The existing debug environment switch and message
prefixes remain unchanged, with their implementation moved to the backend.

Repeated-emission runtime coverage now calls a source Copy<U> method and imported
Enumerable.Repeat<T>/First<T> methods inside the generic wrapper. It retains the
source/imported generic fields, constructor/member calls and emitted assembly
ownership assertions from the preceding slice.

Next: review the remaining reflection-facing compiler adapters and consolidate
backend entry points before adding target selection. CLI metadata symbols remain
.NET-specific, and this extraction does not establish a second target or complete
bootstrap support. General changes still require independent main-based validation;
experimental neoCLR policies remain separate.

Validation with SDK `11.0.100-rc.1.26425.128`: baseline passed 62 tests across
GenericReferenceEmissionTests, AsyncGenericContainingTypeTests,
AsyncGenericCaptureTests, GenericSelfConstructionTests,
TargetCoreGenericSignatureTests, MixedGenericMetadataTests,
ImportedGenericMethodContextTests, RuntimeSymbolResolverTests,
ConstructedMethodSymbolTests and GenericMethodGroupContextTests. Final validation
added GenericInvocationCodeGenTests and passed 72 tests, no failures/skips.
The first extraction build identified a receiver-name collision and the private
substitution-map dependency; these were corrected before validation. Final
compiler builds passed for net10.0 and net11.0 with no warnings/errors. Tests ran
on .NET 11 with freshly built compiler outputs using
`--no-restore /property:WarningLevel=0 /property:BuildProjectReferences=false`.
Whitespace formatting completed with test workspace-load warnings;
`git diff --check` passed. No .NET Framework, NanoFramework, neoCLR execution or
full bootstrap qualification is claimed.

## Slice 25: field resolution through the backend member service

IRuntimeSymbolResolver and RuntimeSymbolResolver now expose field resolution
alongside constructors and methods. Field emission, async/closure storage and
expression generation use the current emission's resolver. The forwarding
FieldSymbolExtensions helper is removed from the Symbols namespace; no CodeGenerator
or CodeGen namespace reference remains under Symbols. PE reflection metadata
accessors remain for documentation and assembly normalization.

FieldSymbolCodeGenResolver still owns target metadata proxy selection and dispatch
for source, imported, substituted and tuple fields. Tuple unwrapping calls that
same resolver internally, preserving its checks and order. This is consolidation
of .NET backend entry points, not a platform-neutral reflection interface.
Repeated-emission runtime coverage now reads a named generic tuple element before
writing the generic source field, alongside existing imported tuple fields and
source/imported generic method calls.

Next: consolidate specialized type-resolution entry points and their signature,
method-body and custom-attribute policies. Shared compiler reflection adapters and
CLI metadata symbols still need separation before target selection. Main-based
validation remains required for integration; neoCLR-specific policies stay separate.

Validation with SDK `11.0.100-rc.1.26425.128`: baseline and final validation each
passed 87 tests across GenericReferenceEmissionTests,
AsyncGenericContainingTypeTests, AsyncGenericCaptureTests,
TargetCoreGenericSignatureTests, MixedGenericMetadataTests,
RuntimeSymbolResolverTests and tuple-related tests, no failures/skips. Final
validation explicitly excluded Development tests and included the expanded named
tuple regression. Compiler builds passed for net10.0 and net11.0 with no
warnings/errors. Tests ran on .NET 11 with freshly built compiler outputs using
`--no-restore /property:WarningLevel=0 /property:BuildProjectReferences=false`.
Whitespace formatting completed with test workspace-load warnings;
`git diff --check` passed. No .NET Framework, NanoFramework, neoCLR execution or
full bootstrap qualification is claimed.

## Slice 26: explicit type-resolution policy

RuntimeSymbolResolver now forwards usage and Unit-erasure policy to one recursive
type-resolution implementation. Signature convenience helpers call that same
implementation. Specialized method-body and custom-attribute entry points are
removed, with callers routed through the per-emission resolver and their existing
policy choices made explicit.

This fixes two facade bugs: CustomAttribute usage previously fell through to
signature resolution, including target metadata types, and MethodBody usage forced
Unit to void even when treatUnitAsVoid was false. Existing method-body callers
continue requesting void explicitly. Attribute emission requests host-compatible
attribute types. Nested Unit values remain value types even when top-level Unit
is erased to void.

New regressions reproduced both bugs before the implementation change. They check
method-body Unit with either flag, Unit array element preservation, and host versus
target-metadata identity for a constructed imported generic type. The array check
compares its element type rather than equality of reflection array wrappers, which
can be distinct objects for the same emitted Unit type.

Next: reduce remaining shared-compilation reflection adapters and identify the
minimal semantic core information required for a selectable target. This is still
.NET backend composition. Main-based validation remains required before integration,
and experimental neoCLR policy stays separate.

Validation with SDK `11.0.100-rc.1.26425.128`: baseline passed 49 tests across
RuntimeSymbolResolverTests, CustomAttributeEmissionTests, UnitReturnTests,
RuntimeUnitContractTests, AsyncGenericContainingTypeTests and
GenericReferenceEmissionTests. Final validation added RuntimeTypeResolutionTests,
TargetCoreGenericSignatureTests and ImportedGenericMethodContextTests: 64 passed,
no failures/skips, with Development tests excluded. The initial new fixture needed
an explicit empty syntax-tree input; its first executable run then reproduced the
two policy bugs and exposed the overly strict array-wrapper identity assertion.
Final compiler builds passed for net10.0 and net11.0 with no warnings/errors.
Tests ran on .NET 11 with freshly built compiler outputs using
`--no-restore /property:WarningLevel=0 /property:BuildProjectReferences=false`.
Whitespace formatting completed with test workspace-load warnings;
`git diff --check` passed. No .NET Framework, NanoFramework, neoCLR execution or
full bootstrap qualification is claimed.
