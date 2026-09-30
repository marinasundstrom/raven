# neoCLR integration and target contract readiness

Status: integrated into main and synced to origin, 2026-09-30. Both retained
feature branches include shared main. Target-boundary work has resumed; native
neoCLR support and the target feature matrix remain experimental work.

## Completed local integration

Main includes the reconciled neoCLR history and tuple-hover fix at b40fe495d.
The full baseline passed 6,015 tests with no failures/skips. Focused emission and
language-server evidence is recorded below; this is not release qualification.
The author subsequently synced the branches. Remote main, intersection and Self
tips were verified against the validated local commits. The old remote neoclr
branch has no commits absent from main; it remains untouched.

The intersection and native Self branches were rebased onto that shared base and
remain separate feature experiments. Intersection passed its generator/build script
and 188 focused checks. Self passed compiler builds for .NET 10/11 and 83 focused
checks on .NET 11. Both preserve their gates and existing limitations. Their
per-feature docs record reconciliation details. Old tips remain recoverable under
refs/codex/rebase-backups/2026-09-30/intersection and native-self.

Further target-boundary work can now proceed on main. Feature work branches inherit
the same shared compiler foundation; target mappings and unsupported-feature
diagnostics can evolve without a permanent main/neoCLR branch split.

## One shared development line

The goal is to develop Raven features, .NET support and neoCLR support together
on main with one consistent compiler architecture. neoclr served as a temporary
integration branch. Target differences belong in contracts and implementations,
not permanently divergent compiler branches.

Reconcile main and address integration regressions before merging. A complete
native neoCLR loader/backend, NeoCLR preset or target feature matrix is not a
prerequisite for merging: unfinished target work can continue on the shared line
with explicit limitations. Preserve ordinary .NET behavior and identify any
remaining experimental behavior that could affect it during reconciliation.

## Integrated checkpoint

The local neoclr branch was fast-forwarded from b54d2999c to 1991973f8, incorporating
all 36 commits from codex/target-boundaries without conflicts or a synthetic merge
commit. The local feature branch was subsequently retired after verifying that
its tip is an ancestor of neoclr; all commits remain available on neoclr. No
remote branch was deleted. Main was not changed. At that checkpoint,
main and neoclr had 96 and 259 unique commits respectively; their integration
requires reconciliation, not a fast-forward. No remote push is implied.

The compiler still uses CLI metadata and CLI emission for the neoCLR experiment.
A native neoCLR metadata loader and emitter are later work. References remain
explicit inputs. Loader, runtime/platform contract and emitter must be selected
as a coherent target; arbitrary cross-target combinations are out of scope.

## Current behavior switches to separate

| Area | Current trigger and owner | Required boundary |
| --- | --- | --- |
| Unit-returning functions | DotNetRuntimeContract delegates the NeoCLR.CoreProbe check to NeoClrCliCompatibility and chooses Func with inhabited unit instead of Action. | Function representation selected by runtime contract; preserve .NET delegate behavior. |
| Tuple construction | DotNetRuntimeContract delegates the NeoCLR.CoreProbe check to NeoClrCliCompatibility and selects System.Tuple instead of System.ValueTuple. | Contract-owned tuple family. |
| Imported tuple recognition | PENamedTypeSymbol delegates to NeoClrCliCompatibility to recognize value-type System.Tuple from NeoCLR.CoreProbe as tuple special types. | Loader uses selected contract and validates shape; an assembly name alone must not select a platform. |
| Terminal Fault calls | BoundNodeFacts delegates to NeoClrCliCompatibility to recognize a particular System.Fault signature and namespace-member marker in NeoCLR.CoreProbe. | Target-owned terminal-operation semantics shared by binding, flow, lowering and emission. |
| Async representation | UseHeapAsyncStateMachines, CaptureAsyncExceptions, PropagateAsyncCancellation and explicit core selection affect task names, builders and lowering. | Validated async capability/representation contract; do not treat any non-default core as neoCLR. |
| Character representation | UseUnicodeScalarChar and UseGraphemeChar affect literal binding, conversions, constants and emission. | One coherent character representation with parser/compiler agreement and required API validation. |
| Array conversions | AllowArrayCovariance affects conversion classification. | Target semantic capability, checked independently of optional source features. |
| Protocol mappings | RuntimeIterationContract, RuntimePropagationContract, RuntimeUnitContract and RuntimeTypeOfContract plus project properties. | Immutable contract configuration with symbol-shape validation and consistent project/API behavior. |
| Incremental reuse | Compilation and incremental state transfer compare selected option fields. | Target and contract changes invalidate affected semantic state; test immutable copies and changed targets. |

This inventory identifies concrete compiler triggers, not a complete supported
feature matrix. There are also project-system mappings and generated runtime
helpers to review before declaring the contracts complete.

## Reconciliation checkpoint — 2026-09-30

The author explicitly deferred further boundary work until neoCLR is merged into
main. The merge candidate combines main 046dc8532 with neoclr 2da2ff5f1. The
merge preview found 29 conflicted files. Most conflicts arose from independently
backported fixes and their later movement into target-owned services.

Resolution retains the target-owned loader/emitter/host service implementations
and their newer diagnostics. Main's nullable-value policy is preserved through
all option copies and incremental transfer; the regression checks it alongside
array, character and async options. Main's record nullability, initializer
analysis and nested-union coverage changes remain included. Duplicate backport
code and tests are not duplicated in the combined implementation.

The pre-reconciliation baseline was deliberately stopped after two completed
batches (143 passes) when the priority changed to merging. It is not a complete
baseline result. The combined candidate passed scripts/codex-build.sh and 150
focused reconciliation tests and 34 emitted-code/metadata regressions on .NET 11.
The full baseline completed with 6,015 passes, zero failures and zero skips
(compiler tests on .NET 11; additional projects on .NET 10/11). A separate
language-server run exposed tuple declaration hover using tuple sugar in place
of the nominal type. Symbol display now honors the existing ExpandedValueTuple
format flag and hover explicitly requests it. The final display tests passed
18/18 on .NET 11, and the full language-server suite passed 256 tests with three
existing skips on .NET 10. The baseline compiler batches tested the reconciled
candidate before this isolated formatting fix; the focused display and LSP runs
validate the fix. Whitespace formatting and diff checks passed.

These are integration checks, not full release/bootstrap qualification, full
runtime-suite coverage or native neoCLR/.NET Framework/NanoFramework execution.
At this checkpoint, boundary redesign was deferred until main integration and
the rebase of codex/intersection-constraints and codex/neoclr-native-self. Those
steps are now complete. Keep those feature branches separate while
their target mappings are designed: source syntax is intended to stay shared,
while .NET and neoCLR can use different representations, lowering and supported
semantics. neoCLR may supply native support unavailable on .NET. Unsupported
feature/target combinations should be diagnosed; updating the branches does not
enable the features generally or finalize these policies.

## Architecture slices on shared main

1. Completed: consolidate existing implicit neoCLR policy in NeoClrCliCompatibility,
   preserving behavior and adding ordinary .NET negative cases. Assembly-name
   inference remains transitional, not explicit target enforcement.
2. In progress: CompilationOptions.TargetPlatform now explicitly identifies the
   supported .NET pipeline and experimental neoCLR CLI bridge. All option copies and incremental compatibility checks
   carry it; unknown values diagnose before setup/emission. Project loading and saving
   now support RavenTargetPlatform=DotNet or NeoCLR with strict name validation.
   Next, migrate callers and remove
   assembly-name and unrelated-option inference. Contract selection remains the
   existing immutable per-protocol options.
3. Implemented: CompilationOptions.NeoCLR supplies experimental CLI profile defaults
   and requires matching explicit core/unit configuration and supplied references.
   Self and record mappings are excluded. Native backend and full feature validation
   remain future work; controlled runtime callers have not yet migrated.
4. Define supported-feature checks from demonstrated runtime/backend capabilities.
   Distinguish disabled source features, unavailable contract APIs and unsupported
   backend operations. Diagnose invalid configuration early and unsupported use
   during binding; emission must reject remaining incompatibilities before writing.
5. Reconcile main changes and classify every experiment-specific change. Preserve
   .NET behavior by default, document intentional public API changes, and retire
   stale compatibility triggers only after consumers migrate.

## Acceptance evidence

- Build and run the broad ordinary .NET baseline, focused semantic/metadata tests,
  and relevant runtime/emission tests on the reconciled candidate.
- Test target switching, options copying, incremental invalidation, invalid mixes,
  missing contract members, diagnostics and no-write-on-rejection behavior.
- Validate the neoCLR profile against matching reference assemblies, importer and
  runtime artifacts in the runtime repository; record exact revisions and commands.
- Distinguish CLI metadata transport checks from actual neoCLR execution. Modern
  .NET success does not qualify .NET Framework or NanoFramework execution.
- Complete the repository bootstrap/release gates before claiming self-hosting or
  release readiness. Feature-scoped passes are not substitutes for those gates.

The first integration baseline stopped after 1,029 passes on a constrained
hierarchy failure. Slice 32 fixed caller type-parameter identity contamination;
97 focused tests passed. The next baseline passed that checkpoint and reached
2,472 passes before an open-generic declaration-pattern failure. Slice 33 fixes
inference ordering for that case and the related symbol-info regression.

Expanded pattern coverage found two qualified nested-union exhaustiveness failures
(RAV2100 for Problem). Both also reproduce with slice 33's production change
reverted. Slice 34 fixes the distinction between complete payload coverage and
unsupported analysis, with guard regressions. Full evidence is recorded in the
slice ledger. That historical baseline was incomplete; the subsequent integration baseline
passed as recorded above. The contract roadmap now continues on main.
