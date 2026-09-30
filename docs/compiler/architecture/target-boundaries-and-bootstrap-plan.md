# Target boundaries and Raven bootstrap plan

Date: 2026-09-30. Status: planned; implementation and qualification remain open.

Execution evidence and current work are tracked in the [slice ledger](target-boundary-slices.md).

## Objective and order

Separate Raven's language semantics from metadata import, runtime contracts,
and code generation. Establish .NET as the first supported implementation of
those boundaries. Adopt unions, `Result`, and `Option` in the C# compiler, then
port the compiler to Raven on .NET. Implement proper neoCLR metadata and
codegen support after the Raven-authored compiler reaches parity.

The compiler's implementation language, execution host, and output target are
independent decisions. A Raven-authored compiler can run on .NET and eventually
produce neoCLR-compatible output. Running the compiler itself on neoCLR is a
separate future milestone requiring the platform and its APIs to be ready.

This plan complements the [bootstrap procedure](bootstrap-procedure.md),
[API result-shape plan](../api/result-shapes.md), and
[live semantic model direction](live-semantic-model.md). Foundation qualification
and the frozen Core dependency remain prerequisites for bootstrap v2. Boundary
discovery and reversible prototypes can start now; broad redesign does not
become a prerequisite for freezing v1. Record an ADR before changing that
allocation or establishing a new durable public target API.

## Branch strategy

Use `neoclr` to investigate real differences and prototype separation. For each
slice, classify language behavior, general target infrastructure, and neoCLR
policy separately before editing.

General production changes follow the repository integration rule:

1. Prove the proposed boundary against the experiment's requirements.
2. Implement or extract the general change on a main-based feature branch.
3. Validate it independently with ordinary .NET/CLI fixtures.
4. Integrate the reviewed change into main, then into `neoclr`.
5. Keep target-specific mappings, policies, and runtime tests on `neoclr`.

Maintain a slice ledger with source commits, dependencies, regression fixtures,
validation targets, main integration status, and experimental follow-ups. Do not
merge the experimental branch wholesale. Reassess integration after each
boundary is proven, rather than waiting for the entire refactoring.

Early integration is appropriate for an abstraction with a complete .NET
implementation and independent tests. neoCLR-specific support becomes a main
candidate only after its contracts are documented, isolated from .NET defaults,
and supported by a reproducible target validation path. Successful .NET tests
alone cannot qualify neoCLR execution.

## Boundaries to establish

[ADR-0003](decisions/0003-target-owned-metadata-and-emission.md) defines the target
as owner of metadata import, runtime contracts, and emission. Raven's semantic
model is its own design; Roslyn informs structure without prescribing these APIs.
Breaking interface changes are acceptable during this phase when controlled
consumers migrate together. Changing target may require rebuilding imported
symbols and rebinding; arbitrary importer/backend combinations are not promised.

| Boundary | Responsibility | Direction |
| --- | --- | --- |
| Language and semantic model | Syntax, binding, conversions, diagnostics, symbols, operations | Raven-owned design over target-aware symbols; semantic state remains compiler-owned. |
| Target metadata provider | Reference resolution, metadata identity, imported definitions, signatures, attributes, lifetime | Selected by the target; .NET initially owns `MetadataLoadContext`, reflection objects, and PE import details. |
| Imported symbol implementation | Lazy projection of target metadata into Raven symbols | Redesign shared contracts as needed; isolate reflection-backed implementations and concrete PE assumptions. |
| Target runtime contracts | Core identity, required types/members, representation and capability decisions | Resolve explicitly from target inputs; distinguish language meaning from target representation. |
| Emission backend | Target lowering, layout, member references, executable/debug artifacts | .NET initially owns Reflection.Emit, CLR instruction emission, and PE normalization. |
| Host and deployment | Compiler execution, macros, filesystem, SDK/MSBuild, packaging | Keep host dependencies distinct from target references and deployment tools. |

Proposed interfaces remain internal until their ownership and consumer needs
are demonstrated. Do not expose reflection types, CLR opcodes, cache controls,
or incremental-state helpers through a supposedly target-neutral boundary.
Backend-private handles are acceptable inside a matched provider/backend pair;
validate pairing and prevent handles crossing compilation lifetimes.

Keep common lowering shared only where it preserves language semantics without
choosing a runtime representation. Classify async state machines, closures,
unions, function values, tuples, and unit representation individually. Avoid
designing a universal intermediate representation before these cases establish
what is needed.

## Initial source findings

The inspected `neoclr` checkout has these starting points; audit main separately
before extracting changes:

- `Compilation.cs` owns metadata context setup, reuse, core selection, and PE
  assembly construction.
- `ReflectionTypeLoader.cs` and `Symbols/PE` project reflection objects into
  symbols. `Symbol.cs` and `CompilationSymbolLookup.cs` also contain concrete PE
  assumptions, so wrapping the loader alone is insufficient.
- `Compilation.Emit.cs` directly constructs `CodeGenerator`, including the
  macro-plugin path.
- `CodeGen/CodeGenerator.cs` owns reflection builders and runtime member maps.
  `IILBuilder` exposes reflection types and CLR opcodes; it is a .NET emission
  seam, not the platform boundary.
- Existing metadata, target-core, and incremental tests provide useful coverage.
  Their presence is not evidence that a new provider boundary is fully tested.
- The inspected checkout has no `eng/bootstrap/v1` seed directory. Verify
  foundation status before beginning Core adoption; do not infer qualification
  from the existence of bootstrap documentation.

## Phased execution

### 1. Inventory dependencies and lock observable behavior

Trace references from setup through imported symbols, binding, lowering, and
emission. Include compiler-driver, macro execution, and language-service paths.
Inventory branch differences and classify each as a general fix, a reusable
boundary, target policy, or an unresolved semantic question.

Map existing tests to the boundary invariants below. Add missing behavior
coverage before changing ownership. Record cold/warm import and semantic-query
latency, allocations, and retained memory for representative workloads before
proposing optimizations.

Exit: a dependency map, coverage-gap list, slice ledger, and smallest trustworthy
baseline for the first extraction. No claim of portability based on interfaces
alone.

### 2. Extract metadata ownership and imported symbols

Begin with reference resolution and metadata-session ownership, using the
existing .NET loader behind an internal boundary. Keep default lookup behavior
and explicit-reference behavior separately specified. Define reuse keys,
invalidation, lifetime, and failure reporting before transferring caches.

Then move assembly/type/member discovery behind that boundary, replacing
concrete PE assumptions in shared consumers with semantic contracts. Migrate one
symbol family at a time; preserve identity, generic ownership, nullability,
attributes, lazy completion, and deterministic lookup.

Exit: .NET import and semantic consumers use the extracted boundary with
unchanged observable results. Provider-private reflection objects do not leak
through the new shared contract, and changed references cannot reuse stale state.

### 3. Extract target contracts and .NET emission

Separate target selection from the compiler host. Audit existing Runtime
Contract configuration and avoid inventing parallel options for the same fact.
Introduce an internal backend entry point above `CodeGenerator`; preserve the
public .NET emit API and its diagnostic behavior.

Move reflection type/member resolution, builders, IL emission, PE normalization,
and debug emission into the .NET implementation incrementally. Include macro
plugin emission and keep host-executable plugins on an explicit host contract.
Document which lowering remains shared and which is target-owned.

Exit: .NET remains the default complete implementation, public metadata and
runtime behavior match, and target assumptions have explicit owners. A wrapper
around `CodeGenerator` is an intermediate milestone, not completion. A proper
neoCLR backend is still deferred.

### 4. Clean up and optimize within proven boundaries

Remove duplication and obsolete transitional paths one slice at a time. Rewrite
a component when its current design prevents a clean contract or measurements
justify replacement. Compare against the recorded workload and behavior tests;
do not combine a major rewrite with a source-language port.

Exit: each change has a stated correctness or measured performance benefit,
including incremental-query and memory-lifetime checks where applicable.

### 5. Adopt Raven contracts in C# — bootstrap v2

First qualify and freeze bootstrap v1 under the existing release gates. Consume
its hash-verified Core assemblies without a compiler/Core build cycle. Migrate
coherent API families using `Option` for expected absence, `Result` for expected
failure, and unions for closed alternatives. Recovery may need both a value and
diagnostics. Preserve cancellation and invariant failures as exceptional paths,
and retain nullable shapes where they model a .NET ABI.

Start with internal cross-component contracts; evaluate public API changes
explicitly and test C# and Raven consumers. Do not mechanically replace all
nullable values or mutable cache state with `Option`.

Exit: bootstrap-v2 gates pass and contract families are stable enough to port.
The compiler remains C# and .NET-hosted at this milestone.

### 6. Port components to Raven on .NET — bootstrap v3

Choose dependency-ordered components with differential-test boundaries. Run the
C# and Raven implementations on the same fixtures, comparing diagnostics,
symbols, operations, emitted contracts, and execution as appropriate. Use
idiomatic Raven where it clarifies established behavior; consult
[feature meaning](../../lang/feature-meaning.md). Fix compiler defects with
reduced regressions instead of compensating in Raven source.

Keep each replacement reversible until parity. Track any generator, build-host,
or interop glue that remains C# and explicitly define its place in the bootstrap
closure. Complete the existing prior-compiler and self-rebuild gates before
claiming a Raven-authored compiler milestone.

Exit: qualified bootstrap v3 on .NET, with provenance and behavioral/ABI parity.

### 7. Implement proper neoCLR support

After v3, implement the neoCLR metadata provider and emission path against the
proven boundaries. Determine whether neoCLR can reuse CLI emission with its own
runtime contracts or needs a distinct representation/backend from actual
platform requirements. Do not assume direct native emission is required.

Validate import, diagnostics, output admission, and execution on neoCLR, including
unsupported-feature diagnostics and required library APIs. Reassess main
integration using the branch criteria above. Compiling the compiler for execution
on neoCLR remains a separate readiness decision.

## Required boundary coverage

| Behavior | Existing starting points | Coverage to verify or add |
| --- | --- | --- |
| Reference/core selection | `MetadataCoreIdentityTests`, `TargetCoreSelectionTests`, `MetadataImportOptionsTests` | Host/target separation, missing or malformed references, identity conflicts, option changes |
| Imported symbols | `ReflectionTypeLoaderNestedTypeTests`, nullable metadata and union import tests | Nested generic ownership, substitutions, attributes, repeated/concurrent queries, provider-independent lookup |
| Incremental lifetime | `IncrementalCompilationReuseTests` | Changed file at the same path, target/provider changes, stale symbols, shared-session lifetime, discarded compilation retention |
| Emitted contracts | `CliMetadataCompatibilityTests`, `TargetMetadataEmissionTests`, `GenericReferenceEmissionTests` | Reference-only dependencies, mixed source/imported generics, member ownership, constraints, byref/pointer shapes, debug data |
| Observable execution | Feature-owned codegen/runtime tests | Closures, async, exceptions, dispatch, union/Option/Result behavior, entry points |
| Tooling and bootstrap | Semantic/LSP tests and release/bootstrap gates | Shared semantic answers, cancellation, macro host separation, C#/Raven consumers, artifact provenance |

These are audit targets, not a claim that every listed case is currently absent.
Keep neoCLR fixtures separate from ordinary CLI regressions. Assert diagnostics,
symbol/operation shape, metadata, and observable behavior rather than opcode
sequences or exact lowered shapes.

## Validation and progress tracking

Before each code slice, use the [test impact map](../../testing/test-impact-map.md)
to establish its baseline once. Run the full baseline when impact is broad or
uncertain; use focused suites otherwise. Follow repository build/generator and
whitespace-formatting rules. Keep runtime-heavy validation separate when needed.
Bootstrap transitions additionally require the
[release gates](../../testing/release-and-bootstrap-gates.md).

Record exact commits, SDKs, frameworks, commands, results, known failures, and
actual execution targets. Modern .NET results do not establish .NET Framework,
NanoFramework, or neoCLR execution. Update relevant compiler/API docs and the
changelog with implemented behavior changes, and external integration docs when
an external runtime integration changes.

First executable slice: complete phase 1 for metadata setup and reference
resolution, then extract its .NET ownership boundary. Do not start a wholesale
symbol rewrite, public target API, or Raven source port in that slice.
