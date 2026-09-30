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

## Slice 27: attribute usage from semantic data

AttributeUsageHelper no longer resolves host runtime types or instantiates CLR
attributes to determine allowed targets and multiplicity. It reads semantic
AttributeData and traverses semantic base types, using the nearest usage declaration.
A direct declaration replaces inherited settings; omitted AllowMultiple defaults to
false. A visited set prevents an invalid inheritance cycle from making this walk
unbounded. Existing no-declaration defaults remain unchanged.

This fixes inherited usage for source and reference-only attribute types, whose
base contracts were previously missed when CLR reflection could not supply them.
New diagnostics coverage checks inherited class-only restrictions and repeatability,
a direct declaration resetting repeatability, and a CLI reference-assembly fixture
that is explicitly rejected by Assembly.Load. The latter is then consumed solely
as metadata under CompilationOptions.DotNet with explicit core/reference inputs.

The implementation still recognizes .NET AttributeUsageAttribute and AttributeTargets;
it is not a platform-independent attribute contract mapping. Next: review the
remaining public reflection adapters and isolate host type conversion from shared
semantic services. Main integration still needs independent main-based validation;
neoCLR-specific policies remain separate.

Removing the fallback exposed missing imported named-type attribute projection:
PENamedTypeSymbol inherited the empty Symbol.GetAttributes implementation. It now
uses PEAttributeDataFactory and caches decoded attributes per symbol, matching the
existing PE-member best-effort behavior for unreadable metadata. The reference-only
fixture also verifies constructor/named argument contents and stable attribute
objects across repeated queries. New source tests explicitly query declared
attributes to exercise lazy binding, and the existing JsonDerivedType fixture's
base class is marked open to remove unrelated inheritance errors.

Expanded metadata tests exposed recursive union classification once imported type
attributes became visible: IsUnion called ToDisplayString on attribute types, and
display called IsUnion again. Marker detection now compares metadata names directly.
A dedicated AttributeUsageAttribute display/classification regression covers the
cycle, and validation was broadened to the repository baseline after the crash.

PE named types additionally classify unions using the loader-selected IUnionSymbol
shape, preserving validation of marked-but-unsupported CLI types and avoiding
attribute decoding during routine classification. This restored demand-driven
source declaration counts in the incremental regression. Documentation coverage
contained stale expectations denying union-case pages, although RavenDoc has
explicitly generated them since commit d08ac174ff; assertions now check the case
page and its constructor xref instead. RavenDoc production code is unchanged.

Validation: the pre-change attribute baseline passed 58 tests. Final focused
coverage passed 145 tests on .NET 11 with no failures or skips, covering attribute
usage/binding/semantic APIs, custom-attribute emission, reference-only inheritance,
attributed unions, nullable metadata, and incremental compilation reuse. Compiler
builds passed for net10.0 and net11.0. Whitespace formatting completed (the test
workspace reported load warnings), and `git diff --check` passed.

The expanded baseline run passed 419 tests before stopping on four fixture
failures: two new inheritance fixtures and two stale union-documentation
assertions. Those cases pass in the final focused run; the full baseline was not
rerun to completion. No .NET Framework, NanoFramework, neoCLR execution or full
bootstrap qualification is claimed.

## Slice 28: remove the unused public CLR type adapter

Removed TypeSymbolExtensions.GetClrType after confirming that its only remaining
caller was a test. There is no new adapter or service: .NET emission already owns
the supported conversion path through RuntimeSymbolResolver. This intentionally
breaks the unused public API and removes its independent host/core lookup and
implicit Unit-to-void policies from the shared semantic surface.

Moved constructed-generic conversion coverage into RuntimeTypeResolutionTests
and added vector/multidimensional generic-array coverage for signature and
custom-attribute usage. The assertions verify type shape and the reflection
assembly context of both the generic definition and its argument.

Next: separate common-type inference from backend type-resolution helpers; shared
TypeSymbolNormalization still calls FindCommonDenominator in the codegen helper.
General changes still require independent main-based validation before integration.

Validation: pre-change focused coverage passed 19 tests; post-change coverage
passed 23 tests on .NET 11, with no failures or skips. The filter included
TypeMetadataNameTests, RuntimeTypeResolutionTests, TypeResolutionPrecedenceTests,
and TargetCoreGenericSignatureTests, excluding Development tests. Compiler builds
passed for net10.0 and net11.0 with no warnings/errors. Whitespace formatting
completed with test workspace-load warnings, and `git diff --check` passed.
No full baseline, .NET Framework, NanoFramework, neoCLR execution or bootstrap
qualification was performed in this slice.

## Slice 29: common nominal inference belongs to semantics

Moved FindCommonDenominator and its alias/literal unwrapping into
TypeSymbolNormalization as private FindCommonNominalType. The only caller was
GetBestCommonType; no forwarding API or new service is needed. Removed the unused
GetDepth helper from the backend class. Base/interface preference, interface
enumeration order, and object fallback remain unchanged.

New semantic tests cover source types sharing both a base and an interface,
source types sharing only an interface, unrelated source types, and imported
exception types sharing a base. Each pair is tested in both orders under explicit
CompilationOptions.DotNet. No reflection resolver or emission is required.

Next: review remaining reflection-facing Compilation APIs and identify the next
small boundary that can move into the .NET target. Main integration still needs
independent main-based validation; neoCLR-specific policies remain separate.

Validation: focused baseline passed 72 tests; post-change coverage passed 80 tests
on .NET 11 with no failures or skips. Coverage included common-type inference,
early returns, function-expression inference and missing-return-type diagnostics.
Compiler builds passed for net10.0/net11.0 with no warnings/errors. Whitespace
formatting completed (test workspace-load warnings only); `git diff --check` passed.
No full baseline, neoCLR execution or bootstrap qualification is claimed.

## Slice 30: target-owned emitter result contract

Added ICompilationEmitter and the .NET implementation, composed by the target.
Shared Compilation retains setup, semantic validation and macro preparation; the
emitter validates options through its target, constructs a fresh CodeGenerator
and returns backend-only EmitResult. Shared orchestration preserves the result's
success flag and appends backend diagnostics to semantic diagnostics on both
normal and plugin paths. Target configuration diagnostics now originate in the
.NET target rather than shared Compilation.

Coverage checks semantic-warning preservation on successful and rejected emission,
untouched streams on target-core conflicts, caller stream ownership, and plugin
emission failure propagation. Existing selected-core, metadata emission, setup
diagnostics and successful macro-plugin coverage remains in the focused set.

The emitter is internal and target-owned. EmitOptions remains .NET-shaped, and
the compilation target is still concretely .NET; neither mixed-target composition
nor a full pluggable target API is claimed. Macro preparation now precedes
emission-option validation, so plugin semantic errors may be reported before
conflicting emission options. No artifact is written in either failure case.

Next: continue reducing .NET-facing adapters on Compilation before generalizing
the complete target contract. Do not merge the experimental branch wholesale;
general changes require independent main-based validation.

Validation: focused pre-change coverage passed 45 tests; post-change coverage
passed 48 tests on .NET 11, with no failures/skips. The filter covered
TargetCoreSelectionTests (excluding the compiler-driver subprocess test),
TargetInitializationDiagnosticTests, TargetMetadataEmissionTests,
RuntimeTypeResolutionTests and the two MacroLibrary emission tests. Compiler
builds passed for net10.0 and net11.0 with no warnings/errors. Whitespace
formatting completed with test workspace-load warnings; `git diff --check` passed.
No full baseline, compiler-driver rebuild, .NET Framework, NanoFramework, neoCLR
execution or bootstrap qualification is claimed.

## Slice 31: explicit .NET host lookup ownership

Removed the three internal Compilation.ResolveRuntimeType overloads and routed
all production callers (the reflection loader and .NET codegen) through the
target-owned DotNetHostRuntime. A single explicitly .NET internal accessor keeps
EnsureSetup ordering at the compilation boundary. This reduces shared semantic
method surface without inventing a target-neutral reflection abstraction.

The new regression accesses the service before any symbol lookup, verifies
metadata/host separation, maps both TypeInfo and imported symbols to host types,
and verifies that a derived compilation has a separate service. Existing caches
and host fallback policies are unchanged; full constructor injection and removal
of other reflection-facing Compilation APIs remain future boundary work.

Next: review the remaining public reflection projection/core-handle APIs and
move .NET-only consumers toward the selected target's implementation services.
General changes still require independent main-based validation.

Validation: the focused pre-change baseline passed 19 tests. Post-change coverage
passed 28 tests on .NET 11, adding the new host-access regression and
TargetCoreGenericSignatureTests to the original host service, nested reflection
loader, metadata-name, core-attribute emission and runtime-type-resolution tests.
No failures or skips. Compiler builds passed for net10.0/net11.0 without warnings
or errors. Whitespace formatting completed with test workspace-load warnings;
`git diff --check` passed. Full baseline, .NET Framework, NanoFramework, neoCLR
execution and bootstrap qualification were not run for this slice.

## Integration checkpoint: target boundaries into neoclr

On 2026-09-30 the author requested integration of the current feature branch into
neoclr, with eventual main integration as the new objective. The clean local
neoclr branch fast-forwarded from b54d2999c to 1991973f8, preserving all 36 slice
and planning commits. No conflicts, remote push or main modification occurred.
The target-boundaries feature branch remains available at the integrated commit.

See [neoCLR main readiness](neoclr-main-readiness.md) for the inventory of implicit
behavior switches, explicit target/contract selection, the planned
CompilationOptions.NeoCLR preset and validation gates. The later main integration
requires reconciliation: the branches had 96 main-only and 259 neoclr-only commits
at this checkpoint. The earlier separate-experiment direction is historical;
preparing a reviewed main integration is now authorized, but not completed.

Validation ran against unchanged compiler sources at 1991973f8. The full baseline
rebuilt its dependencies, including Raven.Core and Raven.Macros, then stopped after
11 test batches: 1,029 passed, one failed, no skips on .NET 11. The failure is
ConstrainedSealedHierarchyTests.NestedGenericSealedCases_ImplementInterfaceMethodsAndBindGenericMath:
two RAV0320 diagnostics for T satisfying INumber<T>. A focused rerun of that class
confirmed one pass and one failure. This was not compared against the pre-merge
compiler and is not classified as pre-existing or introduced by these slices.
The broad baseline is incomplete and not green.

Before the fast-forward, focused neoCLR function/fault tests passed nine cases
and ordinary tuple semantic tests passed eight on the same compiler revision.
No actual neoCLR runtime execution, .NET Framework/NanoFramework validation or
full bootstrap qualification was performed. Next: isolate the generic-constraint
failure before claiming merge readiness, then consolidate the documented target
policies and implement explicit preset selection.

## Slice 32: declaration-owned constraint parameter identities

Isolated the integration baseline's sealed-hierarchy failure to lazy constraint
resolution: a storage annotation in an earlier generic function resolved the
source definition's INumber<T> through the caller's binder. The definition cached
the caller's T, causing false RAV0320 errors when its nested cases were checked.
Constraint binding now supplies a map of parameters from the declaration owner
and enclosing owners, with the closest owner winning. Existing name lookup and
constraint diagnostics remain in place; no target-specific exception is added.

The initial focused hierarchy run reproduced one pass and one failure. Temporary
instrumentation confirmed the validation path. An attempted declaration-binder
lookup caused recursive binding and was removed; the final fix uses the existing
type-resolution substitution mechanism without reentering declaration binding.

Three new tests check declaration order, cached constraint-argument identity and
unconstrained-caller rejection. The original nested hierarchy tests also pass.
Post-fix coverage passed 97 tests on .NET 11, covering hierarchy, generic types,
constraint diagnostics, ref-struct constraints and incremental reuse. Compiler
builds passed for net10.0 and net11.0 with no warnings/errors. Whitespace formatting
completed (test workspace-load warnings only), and `git diff --check` passed.

The integration failure is fixed in focused coverage; the full baseline has not
been rerun to completion. Its provenance before the integrated revision remains
unclassified. No main change, neoCLR execution, .NET Framework/NanoFramework or
bootstrap qualification is claimed. Next: rerun integration validation, then
consolidate the target-specific behavior switches documented in the readiness plan.

## Slice 33: infer declaration patterns before generic-arity diagnostics

The broad baseline rerun at 8f25bc973 passed the prior constrained-hierarchy
checkpoint, including its new regressions. It stopped after 29 batches with
2,472 passes, one failure and no skips. The failure was the existing open-generic
IsPatternSemanticTests regression: a matching Box<int> input still produced
RAV0305 for the bare Box declaration pattern. A focused baseline reproduced that
failure and the related PatternSymbolInfoTests failure (16 passed, two failed).

BindDeclarationPatternType called ordinary type-name binding before attempting
its existing input-driven inference, leaving the missing-arguments diagnostic
behind even when inference succeeded. It now infers first and binds normally
only when inference cannot supply the type. Accessibility validation remains.
Two negative tests verify that object and a different generic definition do not
supply the omitted arguments. Existing semantic and symbol-info tests cover the
positive behavior. No target-specific rule, syntax or generated model changed.

Expanded pattern coverage ran 342 tests: 340 passed and two qualified nested-union
exhaustiveness cases failed with RAV2100 for Problem. An A/B run restored the
original production file at 8f25bc973 and ran the overlapping 28 cases: 24 passed,
four failed (both open-generic failures and both exhaustiveness failures). This
confirms the exhaustiveness failures precede this fix. The fixed source was then
restored and rebuilt; the next slice should isolate those failures.

The first new test compile required qualifying SyntaxTree and importing the test
reference helper; this was corrected before the reported test runs. Full baseline
is still incomplete. No main modification, neoCLR runtime execution or full
bootstrap qualification is claimed.

Final focused verification passed all 20 IsPatternSemanticTests and
PatternSymbolInfoTests on .NET 11 with no failures/skips. Compiler builds passed
for net10.0 and net11.0 with no warnings/errors. Whitespace formatting completed
(test workspace-load warnings only), and `git diff --check` passed.

## Slice 34: complete qualified nested-union payload coverage

The targeted baseline reproduced both qualified complete-match failures in the
existing eight-case imported generic-union theory (six passed, two failed).
TryGetMissingCasePayloads computed an empty missing-case set but returned false,
so its caller treated the domain as unanalyzed and reported the whole Problem
case as missing. It now returns success with the empty set. Partial coverage
continues to identify the missing nested case.

Three new cases verify constant-true, constant-false and dynamic arm guards.
All 11 focused cases passed, including existing qualified/inferred names,
complete/incomplete matches and semantic-model/diagnostics query ordering.
Expanded pattern coverage passed all 345 tests on .NET 11 with no skips.
Compiler builds passed for net10.0 and net11.0 with zero warnings/errors.
Whitespace formatting and git diff --check completed. No syntax/model generation
or target policy changed. Full baseline remains incomplete; neoCLR execution,
.NET Framework/NanoFramework and bootstrap validation are not claimed.

The local codex/target-boundaries branch was safely deleted after confirming its
tip (1991973f8) is an ancestor of neoclr. Its commits remain on neoclr; no remote
branch was deleted and main was not modified. The author reaffirmed eventual
integration of neoclr into main. Next: resume broad integration validation and
then the target-policy consolidation in the readiness plan.

## Slice 35: consolidate transitional neoCLR CLI policy on shared main

After integration and feature-branch rebasing, the author synced the branches.
Remote main at fe4a2372b, intersection at 62b5d8f9c and Self at 44e53dbde matched
local tips; main is an ancestor of both feature branches. The retained remote
neoclr tip b54d2999c has no commits absent from main. No remote refs were changed
by this slice.

NeoClrCliCompatibility now owns the existing assembly-name triggers for inhabited
unit function results, tuple family names, imported tuple special types and
terminal namespace Fault calls. DotNetRuntimeContract exposes the representation
choices; PE symbols and shared bound-node facts delegate classification to that
component. The exact triggers and behavior remain unchanged. In particular,
Fault classification still depends on its declaring assembly independently of
the compilation's configured core; this is not explicit target enforcement.

The pre-change focused baseline passed 19 tests. Post-change target/function/flow
and metadata tests passed 25 tests, including unrelated/case-different core names
and value-type versus reference-type tuple imports. Existing tuple semantics and
symbol-display coverage passed another 26 tests. All tests ran on .NET 11 with
zero failures/skips; CLI fixtures use modern .NET reference assemblies. Compiler
builds passed for net10.0 and net11.0 with zero warnings/errors. Whitespace
formatting completed (test workspace-load warnings only), and diff checks passed.
No syntax, generator inputs or language-service behavior changed. No native
neoCLR, .NET Framework or NanoFramework execution is claimed.

Next: explicit target identity and immutable contract selection, carried through
option copies, project configuration and incremental compatibility. Preserve a
coherent loader/contract/emitter composition and migrate controlled callers before
removing compatibility inference.

## Slice 36: explicit .NET platform selection foundation

CompilationOptions.TargetPlatform, WithTargetPlatform and the constructor argument
now identify the coherent platform composition. DotNet is the only supported enum
value; it preserves existing constructor and preset defaults. All 38 existing
option copies forward the selection. Reference-framework resolution and core
identity remain separate inputs; choosing a platform does not rewrite contracts.

Unknown enum values return unsuppressible RAVT005 before reference initialization
and cannot write PE/PDB output even when callers provide diagnostics. Platform
changes reject both metadata/declaration reuse and semantic-state transfer. Tests
cover defaults, copies, invalid-platform diagnostics and workspace recovery back
to .NET with earlier snapshots retaining their own diagnostics.

Pre-change validation passed seven framework tests and 99 configuration, metadata
and incremental tests. Post-change validation passed 120 focused tests, including
six new platform cases and the existing neoCLR CLI compatibility checks. The seven
framework checks also passed again on the final build. Compiler
builds passed for .NET 10/11 with zero warnings/errors. Tests ran on .NET 11.
Whitespace formatting completed with test workspace-load warnings; diff checks
passed. No generated model, syntax or language-service presentation changes were
needed. Native neoCLR/.NET Framework/NanoFramework execution is not claimed.

This intentionally completes only the .NET API foundation: there is no NeoCLR
preset, project-file selector or strict .NET feature matrix yet. Existing implicit
neoCLR CLI rules remain compatible. Next, define the supported neoCLR CLI profile,
wire project configuration and migrate callers before removing those triggers.

## Slice 37: project platform selection and driver errors

MSBuild evaluation now reads RavenTargetPlatform into CompilationOptions.
Absent/blank values retain the existing .NET default; DotNet is accepted
case-insensitively with surrounding whitespace ignored. Project saving writes
the canonical name, and reload preserves platform and existing core/reference
settings. Unsupported names (including NeoCLR until its profile is implemented),
numeric values and combined names produce an InvalidDataException identifying
the property and value. The compiler driver catches project InvalidDataException
and reports a concise error with exit code 1 instead of an unhandled exception.
Compiler API validation continues to use RAVT005 for unsupported enum values.

The external NeoCLR.Raven.props was inspected, not modified. It also selects the
separate Self feature, so importing its entire configuration as a main preset
would overstate current support. This slice finishes project selection first;
next is defining the supported neoCLR CLI profile independently of Self and
migrating consumers with explicit limits. Existing integration props without
the new selector preserve prior behavior.

The pre-change project/platform baseline passed 60 tests. The final suite passed
70 tests on .NET 11, including four evaluated/imported-property round trips, five
invalid-name cases and a driver regression proving the error text, exit code and
preservation of existing output. Compiler/driver builds passed for .NET 10/11 with
zero warnings/errors. Whitespace formatting completed with workspace-load warnings
and git diff --check passed. No syntax/model generation, SDK target changes or
native neoCLR/.NET Framework/NanoFramework execution is claimed.

## Slice 38: explicit experimental neoCLR CLI preset

CompilationOptions.NeoCLR and TargetPlatform.NeoCLR now select the supported
configuration surface of the existing CLI bridge. The profile owns explicit
NeoCLR.CoreProbe import/emission cores and System.Void unit, plus existing
iteration, propagation, typeof, grapheme and async defaults. Source nullable
values and array covariance default off. Native loader/backend, Self, record
mappings and a complete feature-capability matrix are not supplied by this preset.

Project RavenTargetPlatform=NeoCLR starts from the same preset, excludes host
framework references and .NET prelude defaults, and permits explicit field-level
overrides. Inconsistent core/unit settings diagnose as RAVT003 before loading or
output. Missing references still diagnose as RAVT004. Merely changing the enum on
ordinary .NET options does not apply the preset. Existing CLI implementation and
legacy name-based compatibility triggers remain until controlled callers migrate.

The pre-change baseline passed 92 tests. Final focused coverage passed 109 tests
on .NET 11: preset/copy defaults, six rejected configurations, absent references,
project round trips/overrides, driver validation and existing compatibility tests.
Compiler builds passed for .NET 10/11 with zero warnings/errors. Whitespace
formatting completed (test workspace-load warnings) and diff checks passed.
Matching runtime artifacts were not available at the expected local demo path;
this is compiler/configuration evidence, not native neoCLR, Framework or
NanoFramework execution. No generation input or syntax change was needed.

Both repositories document the experimental preset and its limits. External
runtime props and artifacts are unchanged; existing Self-enabled consumers are
not migrated. Next: select a bounded matching-runtime consumer for migration and
validation, then replace compatibility inference and enforce proven capabilities.


## Slice 39: document bridge behavior and native metadata destination

The author clarified the goal: replace the CLI bridge with native neoCLR metadata
supporting the platform's semantics, and document every temporary bridge behavior.
The new bridge inventory records encodings, semantic distinctions, ownership,
restrictions, branch scope and replacement obligations. Repository instructions now
require that documentation for subsequent bridge changes.

An installed Function-types feature bundle was located and its four principal
artifact hashes checked. A temporary consumer compiled with Raven 9a58e1356,
imported, verified and executed. This was exploratory evidence only. Repository
ancestry confirmed native Function support is on neoCLR feature/function-types and
codex/native-self, not neoCLR main e4f6fe41. The author confirmed deferral until the
metadata layer/full compiler support exists. The proposed Function-dependent smoke
fixture and runtime-props migration were withdrawn before committing.

In response to the author's question, the recommended path is to retain compiler-side
plumbing and experimental bridge support on shared main while preserving .NET defaults;
do not conflate that with enabling native Function semantics. Existing function syntax
and .NET delegates remain valid. Define metadata/symbol/backend requirements and
capabilities before promoting deferred features. No production code or runtime props
changed in this slice; native execution observations are not an acceptance gate.

Docs-only final changes; links, source ownership and branch ancestry were checked,
and git diff --check passed. Existing compiler tests/build evidence from slice 38
is unaffected. The external integration docs record the same scope and recommendation.

## Slice 40 — Target-gated Self, structural experiment isolation

Integrate Self on the shared compiler line with explicit NeoCLR selection plus a
marker contract. DotNet rejects configuration before reference loading/output;
semantic queries preserve user-defined Self on DotNet. Extract native Self onto
neoCLR nominal main instead of merging structural Function ancestry. The native
metadata/backend remains future work; marker and importer limitations are recorded
in the bridge inventory and native Self documentation.

Rebuilding the nominal runtime established that inhabited unit-result transport is
also required by its existing Func ABI. An attempted removal was reverted as a
nominal compatibility rule. Structural Function identity/assignability remains on
feature branches; detailed experimental notes are retained there. The rebuild also
exposed explicit-empty typeof overrides being ignored by the new preset; clearing
all three mapping properties now disables that optional contract.

Validation: 107 focused compiler/project tests pass. `scripts/test-ci.sh` passes
315 compiler tests on .NET 11, 73 core tests on .NET 10, and 256 LSP tests on .NET 10
with three existing skips. Toolchain builds cover .NET 10/11. The external nominal
runtime passes 98 focused tests, regenerated library/API fingerprint checks, native
Self cloning with six rejections, and a nominal unit callback printing 42.
This is not .NET Framework/NanoFramework execution evidence or a native metadata
loader/backend implementation.

## Slice 41: loader-owned metadata input revisions and reuse

Move supplied-PE input snapshots and metadata-session admission out of Compilation
and into the .NET loader. The target offers its previous session; the loader
checks import options, resolved core and ordered file stamps before reusing it.
Snapshots retain no compilation, symbols or host services. Per-compilation symbol
projection and shared-session collection lifetime are unchanged. Both .NET and
the neoCLR CLI bridge use this implementation; native source revision rules remain
the responsibility of a future native loader.

The old unordered path map missed duplicate-identity precedence changes. New
regressions cover reversed input order, a previously missing file appearing,
host-assisted-to-explicit import isolation, unchanged-input reuse, and compiler
symbol queries across reordered snapshots. Three loader regressions were observed
failing before the fix. Existing replacement-at-path, discarded-compilation
collection and fresh-symbol tests continue to pass. File revisions still use
size/time stamps; host fallback registration is not independently revision-tracked.
These limitations are documented rather than hidden by a generic source API.

Update the bootstrap plan to the completed shared-main integration strategy and
retain unfinished intersection/structural work on main-based feature branches.
Next: continue narrowing reflection-backed semantic services and separate the
remaining platform contract decisions from CLI transport implementation before
introducing a native metadata source. No public provider registry is introduced.

Validation: baseline 124 tests; final 174 tests on .NET 11 covering metadata,
incremental reuse, core selection, initialization, emission and neoCLR/Self
configuration. Compiler builds pass for .NET 10 and .NET 11 with zero warnings or
errors. Whitespace formatting and diff checks pass (formatter workspace-load
warnings). No syntax/model changes, native neoCLR execution, Framework or
NanoFramework execution, or full bootstrap qualification is claimed.

## Slice 42: selected .NET and neoCLR CLI runtime contracts

Separate DotNetRuntimeContract and NeoClrCliRuntimeContract behind the shared
internal CliRuntimeContract implementation. Target composition selects the
contract once from immutable compilation options. The explicit neoCLR contract
owns profile validation, inhabited nominal callback results, tuple representation
and marker-gated native Self. The .NET contract preserves defaults and legacy
probe-core transport rules, without opting into Self. Compilation asks the
selected contract for Self availability instead of switching on TargetPlatform.

The common base is explicitly CLI-specific: special-type names, typeof handle
shape and core/unit/marker validation remain transport assumptions. Both contracts
still use the existing .NET loader/emitter; this is not a universal native
contract interface or a capability registry. Bridge encoding, public options,
diagnostic precedence, emitter ABI and external runtime artifacts are unchanged.
Update the bridge inventory and architecture/API docs with the owning layers and
native replacement limits. No external runtime code or consumer migration is needed.

Validation: 74-test baseline; 147 final tests on .NET 11 including incremental
reuse, configuration and core selection, typeof, tuple/callback compatibility,
and Self. New cold/diagnostics-first checks verify user-defined Self remains an
ordinary type on neoCLR without an explicit Self mapping. The initial fixture
omitted a required declaration newline; corrected it and the adjacent fixture.
Compiler builds pass on .NET 10/11 with zero warnings/errors. Whitespace formatting
and diff checks pass (formatter workspace-load warnings). No native neoCLR,
.NET Framework/NanoFramework execution or bootstrap qualification is claimed.

Next: narrow shared semantic consumers' dependence on CLI-specific mappings,
keeping loader/backend replacement coherent with the platform contract. Avoid
turning temporary transport restrictions into native feature capability rules.

## Slice 43: contract validation before backend dispatch

Separate resolved platform-contract validation from backend artifact options.
Compilation's shared emission path validates the emitting target before dispatch,
including the lowered macro-plugin compilation and calls with supplied diagnostics.
DotNetCompilationEmitter now owns core-identity option resolution and no longer
retains or calls DotNetCompilationTarget. TargetDiagnostics centralizes the existing
RAVT003/RAVT005 construction without changing identity, severity or precedence.
ICompilationEmitter documents the shared validation precondition and its backend-only
diagnostic responsibility; stateful code generators remain per-emission objects.

New coverage exercises invalid resolved unit/typeof contracts with normal and
supplied-diagnostic paths, preserving warnings, both streams' bytes and positions,
and caller ownership. Existing macro-plugin success/backend-rejection tests cover
the second dispatch path. Setup failures, semantic errors, selected/discovered core
identity and output behavior remain unchanged. No new public backend selector or
cross-compilation is added; EmitOptions and the concrete generator remain CLI-facing.

Validation: 69-test baseline, 96 final tests on .NET 11 covering target configuration,
core selection, initialization, typeof, metadata emission, neoCLR/Self and macro
plugins. Compiler builds pass on .NET 10 and .NET 11 with zero warnings/errors.
Whitespace formatting and diff checks pass (formatter workspace-load warnings).
Bridge ownership documentation is updated; runtime importer inputs and external
artifacts are unchanged, so no runtime consumer migration is required. No native
neoCLR, .NET Framework/NanoFramework execution or bootstrap qualification is claimed.

Next: continue reducing reflection-backed semantic dependencies while keeping
metadata loading, platform policy and emission paired by the target. Backend
replacement must preserve the shared validation gate rather than duplicate it.

## Slice 44: provider-owned namespace-member container discovery

Shared Compilation namespace-member lookup consumes INamespaceMemberContainer
rather than checking PENamedTypeSymbol. The PE provider owns the unchanged legacy
TopLevel/TopLevelAttribute name interpretation. Synthesized namespace containers
and source syntax recognition retain their paths, including avoidance of recursive
attribute binding. Remove the unused semantic attribute classifier while extracting
this boundary. The capability identifies candidates only; filtering, deduplication,
merged lookup and import policy stay in the compiler.

Four in-memory, non-PE provider cases cover direct/merged discovery, positive and
negative container facts, duplicate containers, named/all-member queries, static
filtering and disabled imports. Attribute access throws in that fixture to prove
shared discovery does not resolve provider attributes. A separate CLI regression
shows a custom TopLevel marker can promote a Fault namespace function without
making it terminal; the bridge's exact runtime marker rule remains independent.
The interface is internal and container-shaped, not a native declaration API or a
requirement that native metadata synthesize CLI attributes.

Validation: 51-test baseline; 56 final namespace-member, merged-namespace and
neoCLR Fault tests pass on .NET 11. Compiler builds pass for .NET 10/11 with zero
warnings/errors. Whitespace formatting and diff checks pass (formatter workspace
load warnings). Source/CLI semantics and emitted metadata are unchanged; language
service lookup continues through the same compiler entry points, with no LSP or
grammar changes required. No native neoCLR, Framework/NanoFramework execution or
bootstrap qualification is claimed. External runtime artifacts are unchanged.

Next: continue auditing provider-specific nested-type and namespace traversal,
preserving lazy discovery without exposing reflection or cache APIs to consumers.

## Slice 45: provider-owned nested-type discovery

Replace PE and constructed-PE checks in recursive namespace type traversal with
the internal INestedTypeDiscovery capability. PE symbols retain their type-only
metadata path and lazy nested-type cache. Constructed symbols delegate to the
original definition's discovery capability; other definitions retain the existing
member-substitution fallback. Traversal no longer knows the PE implementation or
the constructed wrapper's representation.

Six regressions cover direct and constructed non-PE providers whose ordinary
member access throws, PE nested declaration identity under open/closed generic
owners, and source fallback preserving nested containing-type substitution. This
separates declaration discovery from normal closed-type member lookup; the public
GetTypeMembers contract is unchanged. No generic construction algorithm, syntax,
bridge encoding or emission policy changed. Current .NET and neoCLR CLI loading
use the PE provider; a native source can implement type-only discovery later.

Validation: 23-test baseline; 29 final nested metadata, global symbol lookup and
merged-namespace tests pass on .NET 11. Compiler builds pass for .NET 10/11 with
zero warnings/errors. Whitespace formatting and diff checks pass (formatter
workspace-load warnings). No generated model inputs or language-service API
changes; external runtime artifacts remain unchanged. No native neoCLR,
.NET Framework/NanoFramework execution or bootstrap qualification is claimed.

Next: audit remaining shared reflection-backed symbol queries and provider
capabilities before defining a replaceable target lifecycle for bootstrap work.

## Slice 46: provider-owned type-level extension discovery

Move PE receiver and extension-presence checks from shared symbol queries into
`IExtensionTypeInfo`. Constructed symbols own receiver substitution and forward
provider presence facts. The PE provider retains existing lazy metadata decoding;
source declaration behavior is preserved. The capability reports discovery facts,
not member applicability or a target's feature support policy.

Six non-PE provider regressions cover direct/constructed types with and without
member-level extensions, no common receiver, and generic receiver substitution.
Their ordinary-member and attribute access throws, ensuring shared discovery does
not interpret provider metadata. Existing extension suites cover source and CLI
lookup behavior. Member-level PE decoding remains a separate follow-up.

Validation: 140-test extension baseline and 146 final tests pass on .NET 11.
Compiler builds pass for .NET 10/11 with zero warnings/errors. Whitespace
formatting and diff checks pass (formatter workspace-load warnings).
No syntax, generated model inputs, public semantic APIs or emitted encodings
change; language service clients retain the same compiler queries. External
runtime artifacts are unchanged. No native neoCLR, .NET Framework/NanoFramework
execution or bootstrap qualification is claimed.

Next: audit member-level extension facts and remaining shared PE signature queries.

## Slice 47: provider-owned member extension receiver resolution

Remove concrete PE checks from shared method/property extension receiver queries.
The internal `IExtensionReceiverResolver` exposes semantic receiver resolution in
the requested member context. The PE type implementation owns marker decoding,
constructed-owner substitution and CLI ordinal-based parameter remapping. Shared
code retains source rules, explicit receiver parameters and accessor precedence.
A future native provider can resolve its own receiver relationships without
inheriting the CLI remapping convention.

Five non-PE regressions exercise declaration/constructed method context, receiver
identity without ordinal reinterpretation, present/missing property receivers and
accessor precedence. Attribute/member enumeration throws in the provider fixture.
Existing extension tests protect the source and CLI paths. PE symbol identity and
fast signature dependencies elsewhere are follow-up work; this does not claim a
fully replaceable native target yet.

Validation: 146-test extension baseline and 151 final tests pass on .NET 11.
Compiler builds pass for .NET 10/11 with zero warnings/errors. Whitespace
formatting and diff checks pass (formatter workspace-load warnings).
No syntax, generated inputs, public API or emitted encoding changes. Language
service consumers retain existing compiler entry points. External runtime
artifacts are unchanged; native neoCLR and .NET Framework/NanoFramework execution
are not claimed.

Next: audit the remaining shared PE identity and signature query dependencies.

## Slice 48: provider-owned shallow method declaration identity

Replace shared lookup's PE token and parameter-count checks with the internal
`IMethodLookupIdentity` capability. Providers supply qualified opaque declaration
keys; shared code retains declaration unwrapping and generic method argument
composition. PE owns module/token extraction and its existing shallow fallback.
Source symbols retain signature-based keys. These are candidate deduplication
keys, not symbol equality or public/persistent metadata identities.

Four regressions cover non-PE duplicate/distinct declarations whose parameter
access throws, constructed generic argument distinction, PE overload stability
before/after signature loading, and source same-arity overload distinction.
The reflection fallback's existing same-count overload limitation remains
explicitly documented, not generalized into the provider contract.

Validation: 87 extension/entry-point baseline tests and eight compilation lookup
baseline tests passed; all 99 final tests pass on .NET 11. Compiler builds pass
for .NET 10/11 with zero warnings/errors. Whitespace formatting and diff checks
pass (formatter workspace-load warnings). No syntax, generated inputs,
public API or emitted encoding changes. Language-service clients retain existing
compiler queries. External runtime artifacts are unchanged; no native neoCLR or
.NET Framework/NanoFramework execution is claimed.

Next: audit remaining PE-dependent shared signature and identity consumers.

## Slice 49: provider-owned lazy parameter facts

Replace PE checks in semantic-model parameter count, required-count and individual
type helpers with `MethodParameterQueries` and the internal `IMethodParameterInfo`
capability. PE owns reflection decoding; shared queries consume semantic facts.
Missing provider facts remain unavailable instead of forcing a full signature.
Symbols without the capability retain the existing public-symbol fallback.

Eight regressions cover required/optional/variadic offsets, invalid offsets,
provider failures without full signature access, individual types, and source/PE
agreement with public parameter symbols. Existing semantic-model caching tests
protect lazy lookup behavior. PE-specific conversion scoring remains future work;
this slice does not claim that the entire semantic model is provider-independent.

Validation: 84 baseline semantic-model caching and optional/params tests passed.
All 92 final tests pass on .NET 11. Compiler builds pass for .NET 10/11 with
zero warnings/errors. Whitespace formatting and diff checks pass (formatter
workspace-load warnings). No syntax, generated inputs, public API or emission
changes. Language-service consumers retain existing compiler APIs. External
runtime artifacts are unchanged; native neoCLR and .NET Framework/NanoFramework
execution are not claimed.

Next: separate fast conversion classification from PE parameter metadata names.
