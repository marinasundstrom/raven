# Target boundaries before bootstrapping

This review follows the October 2026 consolidation onto main. Its purpose is to
make the shared compiler suitable for the [staged bootstrap procedure](bootstrap-procedure.md),
while allowing .NET and neoCLR to evolve independently. .NET remains the default;
ordinary samples and compiler-host workflows do not implicitly select neoCLR.
This work is preparation, not bootstrap-stage qualification.

## Ownership review

| Boundary | Current responsibility and next step |
| --- | --- |
| Shared language | Parsing, name lookup, type relationships, overload resolution and semantic APIs belong to the compiler. Keep shared observable behavior independent of runtime encoding. Deliberate exclusive features, including native Self and intersection types, remain explicit capabilities. Conjunctive generic constraints remain available on .NET. |
| Target composition | Select the runtime contract and metadata import path at compilation construction/setup. Separate this selection from .NET reflection, metadata-session reuse and host services. This is the first refactoring slice. |
| Runtime contracts | `CliRuntimeContract` already centralizes many policies, but also contains platform switches and native semantics despite its CLI name. Subsequently separate feature availability and representation choices from CLI compatibility encodings, using focused semantic tests before moving each policy. |
| Binding | `Binder.Types`, `TypeMemberBinder` and `BlockBinder.MemberAccess` still inspect `TargetPlatform` directly. Review each decision: native Self and function-category features are capabilities; Character naming and array-slice helpers are target mappings. Move mappings behind focused contracts without delegating ordinary name lookup or overload resolution to backends. |
| Lowering and emission | Preserve language evaluation order, conversions and cleanup semantics in shared transformations. Keep target layout, calling conventions and runtime helpers identifiable. The explicit native emission backend remains separate from the default CLI emitter; metadata-reader compatibility paths still need retirement one capability at a time. |
| Host execution | Reflection and macro execution describe the compiler host, not the selected output runtime. A compiler hosted on .NET may target neoCLR. Later bootstrap hosts must not inherit .NET execution requirements merely through semantic import. |
| Incremental compilation | Existing target/contract option checks gate reuse. Only compilation-independent metadata sessions may transfer; loaders, symbols and reflection projectors belong to their own compilation. Preserve this distinction through each extraction. |

Do not use a single target switch to stand for host execution, language feature
availability, reference format and emitted artifact format. In particular,
neoCLR currently supports explicit CLI compatibility inputs as well as native
semantic inputs. Selecting the platform does not alone select the input format.

## First slice: composition and .NET services

`CompilationTarget` owns runtime-contract selection, semantic-loader selection
and target configuration diagnostics. `DotNetCompilationServices` owns the .NET
metadata session and its reuse candidate, lazy reflection projector, host-runtime
handles and CLI emitter. Native semantic import does not open a .NET metadata
session. Both modes retain the existing setup ordering and host compatibility
accessors, including rejection of reflection `CoreAssembly` in native mode.

This extraction does not make the entire compiler host-independent. The shared
composition still exposes existing .NET compatibility services, and runtime
contracts still contain policies that need finer ownership. Moving or removing
those APIs requires identifying their consumers and testing host/target behavior;
renaming them would not establish a portable bootstrap host.

## Subsequent slices and acceptance

1. Extract one binding mapping or capability at a time, with paired target tests
   for shared behavior and target-tagged tests for intentional differences.
2. Separate native semantic policy from CLI transport restrictions in runtime
   contracts. Record each bridge restriction and its replacement in the
   [bridge ledger](../neoclr-cli-bridge.md).
3. Audit native representation/lowering against observable language behavior,
   prioritizing the [semantic gaps recorded during reconciliation](target-reconciliation-2026-10.md#neoclr-semantic-differences-and-gaps-to-revisit).
4. Qualify a frozen compiler and its target libraries under the
   [release and bootstrap gates](../../testing/release-and-bootstrap-gates.md).
   Record host SDK, target references, backend/runtime revisions and artifact
   hashes separately. Neither self-compilation nor successful .NET execution
   qualifies neoCLR execution.

For the first slice, validate default selection, configuration failures, explicit
metadata imports, incremental reuse and backend dispatch through focused existing
tests. Independently exercise native source-core import and interpreter execution.
No language, grammar, target defaults or Runtime Contract configuration changes
are intended.

## First-slice validation — 2026-10-31

Based on merged main `4f7619fd8`, in an isolated worktree, with SDK
`11.0.100-rc.1.26425.128` on macOS ARM64:

- Fresh-worktree `scripts/codex-build.sh` passed before the extraction.
- The focused comparison passed **158 tests before and 158 after**, with no
  failures or skips on net11.0. The selected classes were
  `MetadataImportOptionsTests`, `IncrementalCompilationReuseTests`,
  `TargetPlatformTests`, `TargetInitializationDiagnosticTests`,
  `TargetConfigurationDiagnosticTests`, `TargetCoreSelectionTests`,
  `NeoClrProfileTests` and `EmissionBackendTests`. Run these as OR-separated
  `FullyQualifiedName~<class>` filters in `Raven.CodeAnalysis.Tests`, with
  `/property:WarningLevel=0`. After explicitly rebuilding the compiler, the
  comparison used `/property:BuildProjectReferences=false`. This includes
  observable .NET emission/execution and native configuration checks.
- Targeted `Raven.CodeAnalysis` builds passed for net10.0 and net11.0. The test
  host's compiler DLL hash matched the rebuilt net11.0 compiler; the native
  probe's compiler DLL hash matched the rebuilt net10.0 compiler.
- `NeoClrMetadataProbe --native-source-core` passed using the isolated metadata
  dependency at `6ed729907` (documentation-only changes over source `4ca2b990b`).
  This exercises reference-free source-core authoring, native consumer import,
  completion, native-call markers, array reservation/views, and configuration
  and signature rejection. The emitted `Consumer.dll` exited **42** on the frozen
  neoCLR interpreter; `ReadUninitialized.dll` and `NegativeLength.dll` each
  exited **1**, as expected. The interpreter SHA-256 was
  `4962197d501dc13198a228b208ad20e5aac546905a18993f70e70b5842e751f4`.
- The net11.0 compiler driver was rebuilt against the extracted services; all
  **7** `TargetPlatformTests` passed again with that driver.
- Whitespace formatting and `git diff --check` passed. The formatter reported
  workspace-load warnings.

This is a focused architecture checkpoint. No full release/bootstrapping gate,
.NET Framework/NanoFramework execution, or native JIT/AOT qualification was run.

## Second slice: array range-access policy

`BlockBinder` no longer selects array helpers by `TargetPlatform` or embeds the
native return-shape rule. `CliRuntimeContract.ArraySlices` owns runtime-helper
lookup and common signature matching. Concrete .NET and neoCLR contracts select
the helper name; the native contract validates its ref-like `System.Slice<T>`
result. The shared binder retains range conversion, array-rank validation,
source diagnostics and invocation construction. Public semantic APIs, lazy
binding and default .NET selection are unchanged.

The runtime distinction is observable, not merely a metadata encoding difference:
.NET `GetSubArray` copies elements; neoCLR `GetSlice` borrows array storage. The
neoCLR runtime helper at source revision `4ca2b990b` calls `ViewCreateSlice`.
Preserve that existing behavior during this extraction. Before freezing a
bootstrap language contract, review whether copy versus borrowed-view behavior
should remain an intentional target distinction or require a shared explicit
operation. Qualify mutation visibility, ownership/lifetime and bounds faults on
the native runtime; do not silently change either target to match the other.

Target-tagged binding fixtures test default .NET selection with both helpers
present, native view selection, and native rejection of a non-ref-like result,
wrong view name, wrong element type, array result and missing helper. They query
semantic models using host reference symbols for Index/Range; these fixtures do
not constitute neoCLR runtime configuration or execution qualification. The .NET
runtime range test also mutates the original array after slicing to verify that
the result remains an independent copy.

Validation on 2026-10-31, based on `aa4a8beb4`, SDK
`11.0.100-rc.1.26425.128`, macOS ARM64:

- Before extraction: 11 existing `RangeAndIndexSemanticTests` and
  `RangeAndIndexCodeGenTests` passed, then all 7 new `ArraySliceTargetTests`
  passed against the original binder implementation.
- After extraction: all **18** tests passed with no skips, including the
  strengthened .NET array-copy execution check. The test host's compiler hash
  matched the rebuilt net11.0 compiler.
- `dotnet build src/Raven.CodeAnalysis/Raven.CodeAnalysis.csproj --no-restore
  /property:WarningLevel=0` passed for net10.0 and net11.0.
- The post-change test command used the three class names above as OR-separated
  `FullyQualifiedName~<class>` filters, `/property:WarningLevel=0` and
  `/property:BuildProjectReferences=false` after the explicit compiler build.
- Whitespace formatting and `git diff --check` passed; formatting reported
  workspace-load warnings. No generated model, grammar or editor behavior changed.

This slice qualifies .NET execution and both targets' binding policies. Native
array-range execution and alias/lifetime behavior were not rerun or newly
qualified; no full bootstrap gate was run. Other direct target checks in binding
remain later slices, beginning with feature capabilities and special-type syntax.

## Third slice: feature capabilities and special types

First merged main through `9cd1c94ae` into this branch (`ddaf45636`), preserving
`8e9bb96ec` structural function inspection and the subsequent imported native
module-function work. The only merge conflict was the changelog; both entries
were retained. The main worktree was clean and was not modified.

Target contracts now own availability of function-type methods, ReturnType
projection, intersection types and the char keyword. Intersection availability
is separate from structural-composition representation, preserving .NET's
constraint conjunction syntax. Shared binding still owns lookup, inference,
feature-specific semantic algorithms and diagnostics. No direct TargetPlatform
check remains in Binder; other option-based language policies and representation
choices are not claimed to be fully extracted.

Successful type resolution passes semantic type/definition pairs through target
normalization. Native policy selects Object/unit providers and canonical source
handles; pointer element mapping and source-primitive member lookup also belong
to the target. Failed lookup bypasses normalization, and failed replacements
retain the original early-exit behavior. Source declaration shells are completed
at the same binder boundary as before. Contract methods hold no symbol caches.

The shared predefined-type API and binder use the same char capability. Concrete
character metadata names are supplied by the target, preserving .NET System.Char
and neoCLR System.Character. Existing native Self and structural representation
contracts remain in place. This slice adds no new syntax, public API, configuration
option, native encoding or runtime behavior.

Validation on 2026-10-31, SDK `11.0.100-rc.1.26425.128`, macOS ARM64:

- Baseline after merging main: **236** tests covering native function signatures,
  function constraints (including main's inspection changes), metadata import,
  source primitive/Object/unit ownership, preferred core and special types,
  runtime unit contracts, pointer semantics, character parsing and shared linear
  bodies; then **67** profile/intersection tests. All passed without skips.
- After extraction: **307** passed without failures/skips in one focused run:
  the same 303 tests plus four target-tagged API checks for ordinary .NET
  ReturnType names, native return projection, and agreement of char binding with
  the predefined-type API on each target. The native API fixtures are semantic
  tests using host references, not native runtime configuration qualification.
- Focused classes: NativeStructuralFunctionTests, FunctionKindConstraintTests,
  MetadataImportOptionsTests, SourcePrimitiveMemberTests, PreferredPrimitiveCoreTests,
  SourceObjectRootTests, SourceUnitContractTests, RuntimeUnitContractTests,
  NeoClrUnitContractTests, TargetSpecialTypeTests, PointerTypeSemanticTests,
  CharacterKeywordTests, SharedLinearBodyTests, NeoClrProfileTests,
  IntersectionConstraintTests, IntersectionTypeDiagnosticsTests,
  SourceStructuralIntersectionTests and TargetBindingCapabilityTests. Run as
  OR-separated `FullyQualifiedName~<class>` filters in Raven.CodeAnalysis.Tests,
  with `/property:WarningLevel=0`. After explicit compiler builds, the test build
  used `/property:BuildProjectReferences=false`.
- Compiler builds passed for net10.0 and net11.0. Test-host and native-probe
  compiler hashes matched their respective rebuilt compiler binaries.
- Native adapter/probe builds used an isolated metadata dependency at neoCLR
  `2c86afc6b`; the earlier `4ca2b990b` dependency lacks the imported-function API
  now required by main. `NeoClrMetadataProbe --native-source-core` passed,
  including source ownership, consumer import/emission, completion and negative
  configuration/signature cases. Its consumer exited **42**, and uninitialized
  read/negative-length controls each exited **1** on the frozen interpreter with
  SHA-256 `4962197d501dc13198a228b208ad20e5aac546905a18993f70e70b5842e751f4`.
- Whitespace formatting and `git diff --check` passed; formatting reported
  workspace-load warnings. No direct `TargetPlatform` check remains under Binder.

This qualifies focused modern .NET behavior and the native source-core interpreter
control, not .NET Framework/NanoFramework, native JIT/AOT or a bootstrap release.
The metadata library and runtime behavior were not changed by this refactoring.

## Integration into main — 2026-10-31

Integrated main's `813dad20c` borrowed nominal constrained-call change before
merging the three refactoring slices and port-first bootstrap documentation.
Merge checkpoint `713db0b2a` preserved the lowering implementation unchanged;
only concurrent changelog additions required conflict resolution.

The combined compiler builds for net10.0 and net11.0. The union of the focused
composition, array-range and capability/special-type suites documented above
passed **443 tests**, with no failures/skips on net11.0. This includes current
main's function inspection and portable-body regressions. Test/probe compiler
hashes matched the respective rebuilt compiler binaries.

The native adapter and updated source-core probe passed using an isolated metadata
checkout at neoCLR `7de5ada65`. On the frozen interpreter recorded above,
`Consumer` and `NominalConstrainedCalls` each exited **42**; uninitialized-read and
negative-length controls each exited **1**, as expected. The new nominal dispatch
control covers method/declaring-type parameters, value/reference receivers and
virtual dispatch. This supersedes the older nominal-dispatch gap recorded during
consolidation; the inherited-Self conformance-owner limitation remains explicitly
checked by the updated probe. No native runtime source was changed.

SDK: `11.0.100-rc.1.26425.128`, macOS ARM64. Validation is focused integration
coverage, not a full release/bootstrap gate or native JIT/AOT qualification.
