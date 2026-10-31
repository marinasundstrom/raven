# Shared compiler target reconciliation — October 2026

Status: consolidation qualified for main with explicit .NET and neoCLR targets.
This is not a bootstrap release or complete native feature-matrix qualification.

## Direction and acceptance

Author direction, 2026-10-30: converge on one Raven compiler with .NET and neoCLR
as explicit targets. .NET remains the primary/default target, and existing samples
and SDK workflows remain .NET-oriented unless explicitly native. Integration adds
target support without retargeting that corpus. Preserve the behavior tested on main and the behavior
expected by neoCLR. This supersedes the October 11 instruction to maintain
separate compiler lines. Earlier architecture integrations succeeded; subsequent
neoCLR development continued on that foundation while main evolved separately.
The September integration records describe historical checkpoints.

Reconcile main `6a02b0c42` and codex/neoclr `f4fd726ad` on
`codex/reconcile-neoclr-main`. Their common ancestor is `d7040e21d`.
The tip-to-tip difference is 504 files, with 46 conflicts in the initial merge
preview. Cherry-picked and adapted fixes make commit counts unsuitable as a
behavioral inventory. The external neoCLR checkout initially inspected is
`a079cffdffea9d9c69f7eb8c55a7b24981281c72`.

## Existing architecture to preserve

- Shared syntax, semantic APIs, binding and language-preserving lowering remain
  compiler-owned. Language services consume semantic APIs.
- Runtime contracts select target semantics and representation. Native source
  and imported symbol forms must not change default .NET behavior implicitly.
- Semantic metadata references provide native symbols. Native-only setup rejects
  incompatible reference mixtures; the CLI emitter rejects semantic-only inputs.
- `EmitOptions.Backend` selects explicit artifact production. The native adapter
  and its independent metadata-library dependency remain opt-in. Host execution,
  target references and output identity remain separate concerns.
- Incremental state transfer compares target and contract options. Reconciliation
  must retain invalidation when semantic inputs change.

The implementation is not a completed universal backend abstraction.
`DotNetCompilationTarget` currently also composes native-only semantic loading;
its name does not fully describe its responsibility. Native emission still has
reader-backed paths and compatibility modes documented in
[metadata boundaries](../metadata-backend-boundaries.md). Review these as bounded
architectural debt; do not replace established implementations with wrappers and
claim separation is complete. Do not mix a wholesale compiler rewrite or Raven
source port into branch reconciliation.

Target-sensitive test coverage must identify its target explicitly. New
namespace documentation cases have separate .NET/neoCLR names and filterable
traits; the neoCLR workspace failure-contract regression is tagged accordingly.
See [test classification](../../testing/target-test-classification.md). Historical
suite classification remains a post-merge cleanup, without weakening existing gates.

## Refactoring after integration

Merge qualification establishes a shared starting point; architectural cleanup
continues afterward in separately validated changes. Target ownership must remain
identifiable even where implementations diverge. Share language semantics and
stable contracts where appropriate, while allowing independent metadata import,
lowering, representation and emission implementations for .NET and neoCLR.
A universal backend or identical lowering is not an integration prerequisite.

Prioritize the mixed responsibilities in `DotNetCompilationTarget`, explicit
ownership of target policy currently selected inside shared services, and the
native reader-backed emission paths. Refactor one boundary at a time, retaining
.NET regressions and native interpreter controls as independent gates. Do not
turn temporary CLI bridge restrictions into permanent native language rules.

## neoCLR semantic differences and gaps to revisit

Author clarification, 2026-10-31: review differences primarily to identify gaps
in neoCLR, rather than changing the default .NET target to accommodate them.
Shared Raven behavior should agree unless a target difference is deliberate and
documented. A different runtime representation is not by itself a semantic difference.

| Area | Current distinction | Post-merge review |
| --- | --- | --- |
| Structural unions, tuples and function values | Shared language constructs; .NET uses its CLI representations, while neoCLR has native structural forms. | Keep the representations independent. Compare observable conversions, equality, pattern matching, invocation and capture behavior. Treat unexplained behavioral differences as candidate neoCLR gaps, not automatic new language rules. |
| Unit and no-result calls | Both targets distinguish a Raven unit value from a call with no stack result; runtime ownership and metadata encodings differ. | Preserve the language distinction. Audit native generic arguments, returns and interop for lost unit values rather than treating CLI VOID transport as the native rule. |
| Native Self | neoCLR-only conformance contract; .NET lowercase `self` remains an ordinary receiver and a user-defined `Self` is an ordinary type. | Keep the exclusive feature. Fix the native metadata writer's inherited-conformance ownership gap and qualify instance dispatch through generic bounds; do not revise the language's Base-anchored Self rule to match those gaps. |
| Intersection types | Native value/signature intersections are neoCLR-only. Both targets accept conjunctive `A & B` generic constraints. | Keep the agreed feature boundary. Review incomplete native import, emission and runtime coverage as implementation gaps rather than restricting shared constraint syntax. |
| Function-category constraints | `where T: func` is neoCLR-only; ordinary function values are shared. | Preserve the explicit native capability and expand native qualification without making ordinary callbacks native-only. |
| Module/namespace declarations | neoCLR presents logical modules; .NET normally uses namespaces. Lookup/import syntax is shared and both declaration spellings parse. | Retain the target-facing spelling distinction. Revisit only if module ownership, visibility or re-export semantics are deliberately extended; those semantics must not be inferred from the keyword today. |
| Array covariance and nullable value policy | The neoCLR profile disables CLR-style array covariance and nullable-value projection. Native representations and selected runtime contracts require separate validation. | Audit which restrictions are intentional native safety/model choices and which are bridge/backend gaps. Test assignment, mutation and null handling before classifying them; do not silently import or remove .NET rules. |
| Characters and text | .NET `char` denotes its code-unit type; neoCLR uses its grapheme-oriented Character contract and does not admit the .NET `char` keyword as that contract. | Preserve the deliberate text-model distinction; review conversions, indexing and library interop for missing support. Do not describe missing APIs as intentional semantic differences. |
| Failure, cleanup and async | neoCLR runtime contracts cover terminal failure, propagation, disposal and async behavior; profile defaults differ in exception cleanup/capture and cancellation propagation. | High-priority semantic audit: compare cleanup on exit/failure, fault propagation, cancellation and observable evaluation order. Record intentional runtime behavior separately from missing exception or backend support. |

Priority after the merge: (1) correct the reproduced native Self gaps; (2) compare
failure/cleanup/async behavior with explicit native contracts; (3) audit shared
structural and callable behavior using paired target tests. For each discrepancy,
record a minimal source example, results on both targets, the owning compiler or
runtime layer, whether it is intentional or a gap, and the regression that closes it.
The profile-default rows are audit items, not claims that every production native
configuration has identical behavior. Native JIT/AOT parity remains a separate
qualification task. These follow-ups do not require collapsing the two backends
or completing the entire native feature matrix before branch integration.

## Reconciliation decisions to verify

Preserve main's release configuration, corrected macro diagnostic factory calls,
escaped operator display, documentation presentation and regression tests.
Preserve neoCLR's native import/emission, structural symbols, explicit target
contracts, metadata dependency observation and native documentation support.
Review conflicts at the behavior level, including source declaration cache
readiness, indexer setter accessibility, unit ownership and failure lowering.
Do not resolve all conflicts by selecting one branch wholesale.

## Validation ledger

Pre-change full baselines were started independently on both source tips.
Main stopped after 797 passes and one child-process timeout in
`CompilerDependencyResolutionTests.StandaloneCompile_ReferencesCodeAnalysisWithoutMacroLibrary`
(30 seconds, no compiler output). Its isolated rerun passed in 13 seconds;
classify the original result as a load-sensitive timeout. The neoCLR baseline stopped on its stale
`DefaultParameterTests.DefaultValueMustBeConstant_ReportsDiagnostic` expectation
that `1 + 2` is not constant. The reconciled tree retains main's corrected
function-call rejection fixture and separate constant-expression acceptance test.

The reconciled core and driver build for net10.0 and net11.0. The optional native
adapter, metadata library and probe tool build successfully. The reference-free
native source-core probe passes (source native-call markers, array reservation and
views, consumer emission/completion, and invalid configuration/signature rejection).
A focused workspace probe matches cold control-flow and diagnostic results when
adding and removing the explicit terminal-failure contract, including after warming
diagnostics. No stale semantic behavior was reproduced; retain coverage without
a speculative cache change for that failure-contract scenario. Baseline, isolated
runtime and framework-matrix qualification are complete, with the staged compiler
corrections and their follow-up checks distinguished below.

Completed baseline: 76 compiler batches (6,725 passing test executions) and
107 passing executions in the remaining baseline projects, for 6,832 total.
Failed batches were rerun after fixture corrections and the suite resumed without
changing compiler code; all baseline membership was retained. See the
[baseline receipt](validation/target-reconciliation-20261030/baseline.json).

Qualification scope:

1. Generator/build script and full baseline on the reconciled tree.
2. Runtime/emission regression suites, target configuration and incremental
   lifetime tests, and affected language-server/documentation coverage.
3. .NET host/target framework matrix and representative compiler-driver/project
   sample builds and executions using the repository toolchain.
4. Native adapter build against the recorded metadata library, native compiler
   probes, and neoCLR workload execution against matching runtime/core artifacts.
5. Exact commands, SDKs, source identities, results and remaining limitations.

A successful .NET test run does not establish neoCLR execution. A successful
native probe does not establish full .NET compatibility. Existing experimental
limits must remain explicit and must not silently become general language rules.
Bootstrap seed selection follows qualification of the shared compiler; this
reconciliation does not itself freeze a bootstrap version.

### Combined-driver and native execution checks

The same net11.0 driver built with `NeoClrMetadataProject` compiled a net10.0
application that exits 42 and a native project that also exits 42. The native
project explicitly selects its source core, runtime contracts and empty System
seed. Interpreter controls for uninitialized reads and negative array lengths
fault as expected. VS Code `npm run check-types` passes.

The old default `NeoClrMetadataProbe` path fails on its legacy container-reader
assumption (`invalid DOS signature`); it is not a current native gate. The
`--native-source-core` probe and documented empty-seed/Object-root execution
configuration provide the checks above. Native JIT/AOT and production library
coverage are not implied by these interpreter results.

The current production-library signals consumers also compile and execute with
the same driver in the interpreter: `SignalsDemo`, `SignalReflectionDemo` and
`AsyncSignalsDemo` all exit 0 with their expected success messages. Inputs were
copied from the neoCLR checkout to temporary directories to avoid mutating its
active release outputs. This covers structural callbacks, generic library imports,
nullable arguments, subscription changes, method reflection and async callbacks.
It does not extend the native JIT/AOT support claimed by their original fixtures.

### Documentation and language-service qualification

Focused documentation, symbol-display and runtime-failure-contract tests pass
(96 tests). Namespace function/constant IDs now match native metadata ownership,
including global and generic functions, while .NET carrier IDs remain unchanged.
The native documentation fixture and `native-doc-checks.py` pass against the
frozen production core, covering imported native comments and generated pages.
Language-server unit coverage passes (270 passed, 3 skipped), as do five focused
integration checks for metadata inputs, Directory.Build.props changes and
transient project-open recovery.

### Target classification and language documentation

Test traits now identify the reconciliation's target-specific documentation,
failure-contract and intersection coverage. Shared intersection parsing is marked
separately from .NET conjunctive-constraint semantics/runtime emission and native
intersection semantics. The focused post-classification run passes 151 tests.
The baseline corrected stale fixture assumptions: Result documentation now has
four examples, native CLI rejection must not depend on capability-guard order,
and the custom Result conversion fixture still declares its own Error case.
Tests retain behavior assertions: documentation blocks are preserved, invalid
emission leaves output unchanged, and payload conversions use the declared case.

Language and website guidance leaves shared features unmarked, labels exclusive
facilities, and separates semantics from runtime representation. Structural unions
are shared with different .NET/native implementations. Intersection types are
neoCLR-only, while .NET retains `A & B` as ordinary generic-constraint conjunction
syntax. neoCLR uses `module` declarations where .NET uses `namespace`; both
spellings share qualified lookup and imports. Grammar/highlighting recognition does not imply runtime support. This
clarification does not change parser, binding or target capabilities.

The RavenDoc website build (`scripts/build-docs.sh --no-build`) passes, including
its required API-page/source-link checks. Generated union, type-system, grammar
and target-platform articles contain the updated distinctions. The full deployed
site-navigation script stops at missing playground appsettings: interactive
playground/showcase publication and provenance injection were not part of this
article build, so deployment-wide navigation is not qualified by this check.

The resumed baseline exposed another stale native-branch expectation: the
explicit neoCLR profile was expected to emit through the default CLI backend.
That backend now rejects the native structural representation contract. The test
separately verifies native import plus CLI rejection and legacy .NET target-core
CLI metadata emission, with explicit target traits. No emitter restriction was
weakened; native execution remains covered by the independent native gates.

An intermediate combined-driver control returns 42 on .NET and neoCLR after the
documentation fix; artifact hashes are retained in
[the final driver receipt](validation/target-reconciliation-20261030/final-combined-driver.json).
The runtime binary is unchanged from the earlier native workload checks.

### Qualified metadata lookup correction — 2026-10-31

Runtime qualification exposed a real binding gap: the namespace-qualified fast
path selected the first merged namespace member, bypassing canonical metadata
selection. With duplicate referenced names, a base type could disagree with
`GetTypeByMetadataName` and ordinary imports. The shared binder now uses the same
selection for qualified generic and non-generic names while preserving source
precedence and early declaration lookup boundaries. This adds no target contract.
Ninety focused metadata/import/generic/capability tests pass, and a repeated
compilation repro passes 20 iterations after reproducing the old mismatch.
The isolated runtime pass was restarted with the corrected compiler. The earlier
6,832-execution baseline predates this focused correction; subsequent validation
is recorded separately rather than treating those binaries as identical.

### Self coverage and native metadata gaps — 2026-10-31

Four old Self tests expected CLI bridge emission from explicit native options.
They now retain semantic inheritance, reimplementation, interface arity and generic
result checks, and verify that CLI emission rejects without changing the output.
The native source-core probe separately inspects actual native Self signatures,
Base and explicit Derived results, and generic method signatures. Generic fixture
bodies use identity returns to isolate metadata shape from dispatch capability.

The probe exposes two metadata-library gaps: nominal-bound instance dispatch is
rejected because the native library requires structural bounds; inherited-only
conformance incorrectly substitutes Derived for the declaring Base Self. Explicit
Derived reimplementation emits successfully. These are implementation gaps, not
new language rules. Rejection controls preserve visibility until native library
conformance-owner tracking and dispatch support are corrected. Existing semantic
checks continue to enforce the intended Base-returning inheritance contract.

The runtime suite also had a stale negative check for dynamic array-comprehension
bounds. Its replacement checks portable admission and actual .NET arrays for
zero, positive and negative bounds; the focused suite passes 13 tests.

### Readonly receiver correction — 2026-10-31

Runtime qualification exposed direct address loading for readonly struct fields
in the .NET emitter. A pair of mutating calls returned 3 instead of 2 and changed
the field. Invocation receiver address selection now defers to a defensive copy
outside the declaring constructor. Focused coverage includes implicit and explicit
receivers, static readonly fields, mutable controls and constructor initialization.
This correction is general .NET behavior; it does not change native contracts.

The ten focused readonly-receiver/nearby struct tests pass. Both net10.0 and
net11.0 compiler builds pass. The refreshed combined driver returns 42 for .NET
and native controls; source-core/Self checks and three native signals consumers
pass against the frozen interpreter after both compiler fixes. See
[final target controls](validation/target-reconciliation-20261030/final-target-controls.json).

The language reference and website now group native Self, intersection types and
structural function category constraints under neoCLR only. .NET remains the
primary/default learning path. The article render includes the new grouped page,
sidebar, searchable feature cards and module/namespace examples.

### Cold import-scope correction — 2026-10-31

The final sample/editor filter exposed a query-order bug: requesting an imported
union symbol before its top-level function declaration caused deferred imports to
complete member signatures against the cached, unfinished import scope. The
parameter was permanently bound to Error even though the later union lookup
succeeded. A two-file reduction fails for symbol-first queries in either file
order, while declaration-first and diagnostics-first controls pass.

Deferred import setup now declares source type shells before resolving imports,
without completing all member signatures through that unfinished scope. The public
semantic APIs retain ownership of correct lazy binding; no editor-side workaround
or cache-specific public API is introduced. The existing hardware-sample editor
regression also requires the parameter and type query to return the same symbol.

## Final qualification summary

- The earlier full baseline passed 6,832 test executions. It predates the final
  lookup, readonly-receiver and cold import-scope corrections.
- The runtime workflow passed 2,034 executions: 28 code-generation batches, seven
  isolated project/CLI groups and the final sample/editor filter. The 96-test
  MSBuild group includes all 66 sample projects. The final 12-test sample filter
  was rerun after the cold-import correction; the preceding groups retain their
  earlier checkpoint. See the [runtime receipt](validation/target-reconciliation-20261030/runtime.json).
- After the cold-import correction, 208 namespace/import/incremental tests pass;
  the language-server suite passes 270 tests with three existing skips.
- `scripts/test-target-framework-matrix.sh` passes with SDK
  `11.0.100-rc.1.26425.128`, building Raven.Core/Raven.Macros for net10.0 and
  net11.0 and executing data-literal-macros, timer-macro and runtime-async-net11.
- The final native dependency is built in the isolated neoCLR integration
  checkout from source commit `4ca2b990bc2356ae159735976091495c9e793af0`.
  The runtime and production-library inputs remain frozen. Final target controls
  record the exact compiler/adapter/metadata/runtime artifact hashes.
- Website articles render, including grouped neoCLR features and module examples;
  feature-page links and anchors are checked. The earlier full website/API build
  and VS Code type-check pass. Publication/deployment is not part of this merge.

The remaining native Self conformance/dispatch gaps and the semantic review above
are explicit post-merge work. Native JIT/AOT, .NET Framework and nanoFramework
execution are not qualified by these checks. The existing release compiler pin
and bootstrap seed are not advanced by consolidation.
