# neoCLR integration and target contract readiness

Status: planned follow-up, 2026-09-30. The author now intends to prepare the
neoCLR branch for eventual integration into main, including the target-boundary
work. This supersedes the earlier project direction to keep neoCLR integration
separate at this stage. It does not certify the experiment or authorize an
unreviewed merge into main.

## Integrated checkpoint

The local neoclr branch was fast-forwarded from b54d2999c to 1991973f8, incorporating
all 36 commits from codex/target-boundaries without conflicts or a synthetic merge
commit. The feature branch is retained. Main was not changed. At that checkpoint,
main and neoclr had 96 and 259 unique commits respectively; their integration
requires reconciliation, not a fast-forward. No remote push is implied.

The compiler still uses CLI metadata and CLI emission for the neoCLR experiment.
A native neoCLR metadata loader and emitter are later work. References remain
explicit inputs. Loader, runtime/platform contract and emitter must be selected
as a coherent target; arbitrary cross-target combinations are out of scope.

## Current behavior switches to separate

| Area | Current trigger and owner | Required boundary |
| --- | --- | --- |
| Unit-returning functions | Compilation.CreateFunctionTypeSymbol checks NeoCLR.CoreProbe and chooses Func with inhabited unit instead of Action. | Function representation selected by runtime contract; preserve .NET delegate behavior. |
| Tuple construction | DotNetRuntimeContract checks NeoCLR.CoreProbe and selects System.Tuple instead of System.ValueTuple. | Contract-owned tuple family. |
| Imported tuple recognition | PENamedTypeSymbol recognizes value-type System.Tuple from NeoCLR.CoreProbe as tuple special types. | Loader uses selected contract and validates shape; an assembly name alone must not select a platform. |
| Terminal Fault calls | BoundNodeFacts recognizes a particular System.Fault signature and namespace-member marker in NeoCLR.CoreProbe. | Target-owned terminal-operation semantics shared by binding, flow, lowering and emission. |
| Async representation | UseHeapAsyncStateMachines, CaptureAsyncExceptions, PropagateAsyncCancellation and explicit core selection affect task names, builders and lowering. | Validated async capability/representation contract; do not treat any non-default core as neoCLR. |
| Character representation | UseUnicodeScalarChar and UseGraphemeChar affect literal binding, conversions, constants and emission. | One coherent character representation with parser/compiler agreement and required API validation. |
| Array conversions | AllowArrayCovariance affects conversion classification. | Target semantic capability, checked independently of optional source features. |
| Protocol mappings | RuntimeIterationContract, RuntimePropagationContract, RuntimeUnitContract and RuntimeTypeOfContract plus project properties. | Immutable contract configuration with symbol-shape validation and consistent project/API behavior. |
| Incremental reuse | Compilation and incremental state transfer compare selected option fields. | Target and contract changes invalidate affected semantic state; test immutable copies and changed targets. |

This inventory identifies concrete compiler triggers, not a complete supported
feature matrix. There are also project-system mappings and generated runtime
helpers to review before declaring the contracts complete.

## Implementation slices

1. Consolidate existing implicit neoCLR policy in a named compatibility component,
   preserving behavior and adding ordinary .NET negative cases. Keep general fixes
   identifiable separately from target policy in commits and tests.
2. Introduce explicit target identity and immutable contract selection. Ensure all
   option copies, project loading and incremental compatibility checks carry it.
   Remove assembly-name and unrelated-option inference of target identity.
3. Add CompilationOptions.NeoCLR backed by the actual supported CLI transport
   profile. Specify required references and current limitations; do not claim a
   native neoCLR backend. Keep CompilationOptions.DotNet explicit-reference-only.
   Migrate controlled callers and project properties to the presets.
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

The integration baseline stopped after 1,029 passing tests and one failure in
ConstrainedSealedHierarchyTests.NestedGenericSealedCases_ImplementInterfaceMethodsAndBindGenericMath
(two RAV0320 diagnostics for INumber<T>); a focused rerun confirmed the failure.
Its origin is not yet classified. Full evidence is recorded in the slice ledger.
Until this failure, the remaining gates and contract migrations are resolved,
main integration remains pending.
