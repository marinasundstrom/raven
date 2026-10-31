# Staged bootstrap procedure

Author direction, 2026-10-31: port the compiler to Raven while maintaining the
original C# implementation alongside it. Establish behavioral parity and reliable
self-hosting before freezing C# development. Continue toward idiomatic Raven,
then move the compiler host to neoCLR by implementing the APIs it needs there.

This procedure implements [ADR-0004](decisions/0004-port-first-bootstrap.md).
It supersedes the earlier mandatory sequence of a frozen C# foundation,
Raven.Core API adoption inside C#, and only then a Raven source port. API adoption
inside C# may be useful, but is not a prerequisite for starting the port.

.NET remains Raven's primary/default output target. Compiler implementation
language, compiler host runtime, and compilation output target are independent
choices. The initial Raven implementation runs on .NET and retains the explicit
.NET and neoCLR targets. Moving its host to neoCLR later does not retarget ordinary
samples, templates or getting-started workflows, nor remove .NET output support.

## 1. Maintain two implementations during the port

Keep the original C# codebase and a Raven codebase side by side. They implement
one language and corresponding compiler contracts; this is not a permanent fork
of language semantics. Repository layout and project names are implementation
choices to settle when establishing the port, not prescribed by this document.

Port coherent, dependency-ordered components behind testable boundaries. Begin
with direct, reviewable translations that preserve recognizable correspondence,
compiler behavior and architecture. Avoid broad redesign, pervasive result-type
migration or aggressive idiomatic rewrites during the initial translation.
Idiomatic Raven remains the destination, not an obstacle to reaching parity.

Preserve Roslyn-like public compiler APIs where practical so existing callers and
tests can exercise both implementations. Keep semantic caching and incremental
state inside the compiler, and keep target policies behind explicit contracts.
The [target-boundary review](bootstrap-target-boundaries.md) tracks preparatory
refactorings; completion of every architectural cleanup is not a porting gate.

For each component:

1. Identify its input fixtures, public outputs and dependency boundary.
2. Translate it faithfully and run the same cases through both implementations.
3. Compare diagnostics, syntax, symbols, operations, metadata and observable
   runtime behavior as applicable.
4. Classify discrepancies before changing either implementation; C# is the
   working reference, not an infallible specification.
5. Keep the C# implementation available while resolving gaps and validating the
   Raven replacement. Record which implementation built and ran each case.

## 2. Repair defects and gaps in both codebases

Porting is expected to reveal compiler bugs, missing language/API support and
runtime or library gaps. Reduce each failure, determine the intended contract,
and fix it at the owning layer. Do not compensate for wrong compiler behavior
with special-case Raven source merely to keep the port moving.

While both implementations are maintained, fix shared compiler defects in C#
and carry the corresponding correction into the Raven implementation. If the
component has not yet been ported, record the correction as a port requirement.
Add equivalent regression coverage for both. Fix a translation-only defect in
Raven without manufacturing an unrelated C# change. Required language/API gaps
must be implemented and documented consistently in both implementations as they
become applicable.

Use a porting ledger with these fields:

| Field | Record |
| --- | --- |
| Reproduction and provenance | Reduced source, original compiler-shaped workload, source commits and compiler artifacts |
| Expected behavior | Language/API contract, including deliberate target differences |
| Classification and owner | Shared compiler bug, translation defect, unclear semantics, missing language/API capability, runtime/library or tooling gap |
| Corrections | C# fix and corresponding Raven fix, or an explicit not-yet-ported follow-up |
| Regression coverage | Diagnostics, symbols, operations, metadata or runtime checks and retained integration workload |
| Status | Open, deferred with reason, fixed in each implementation, and validation evidence |

Compiler fixes should retain small regressions alongside the originating workload.
Trace failures through binding, lowering, emission and execution; assert stable
behavior rather than emitted opcode sequences. Unclear semantics must be decided
and documented before either implementation is treated as authoritative.

## 3. Reuse tests, port only where necessary

Run the existing tests against the Raven compiler wherever the API and harness
permit. The test suite need not be rewritten into Raven just because the compiler
is. Preserve shared input fixtures and expected results across implementations.

Where a test cannot run unchanged, adapt its harness or port it while retaining
its behavioral assertions. Record the original-to-replacement mapping and why
reuse was impractical. A test is not obsolete merely because the initial port
cannot satisfy it. Keep temporary exclusions explicit, with an owner and closure
condition; do not silently weaken coverage to claim parity.

Separate implementation coverage from runtime-target coverage. Identify .NET-only
and neoCLR-only tests with target traits and explicit names. `Target=Shared`
means target-independent assertions, not proof of execution on both runtimes.
Use the [test impact map](../../testing/test-impact-map.md) for daily changes and
[release/bootstrapping gates](../../testing/release-and-bootstrap-gates.md) for
stage acceptance. Samples, Core, macros, SDK tooling and external consumers
remain compatibility workloads, not just compiler self-build inputs.

## 4. Establish parity and reliable self-hosting

Build the Raven compiler with an exact known C# compiler artifact, then use the
resulting Raven compiler to rebuild the same Raven source. Compare the compilers'
public behavior and emitted contracts across the retained test corpus. Require
byte equality where deterministic emission promises it; otherwise record the
required semantic/ABI equivalence and explain differences.

Self-compilation alone is insufficient. Before making Raven authoritative and
freezing C# development, require:

- a reproducible build from recorded compiler and library inputs on a clean machine;
- differential parity, with discrepancies resolved or explicitly accepted as
  intentional changes to the documented contract;
- existing tests running or mapped to equivalent ported coverage, with reviewed
  exclusions and no silent coverage loss;
- successful self-rebuilds and the full applicable release/compatibility gates;
- verified Core, macro, sample, package and public consumer behavior;
- independent evidence for every claimed output target and host runtime; and
- a retained C# compiler artifact and source revision capable of rebuilding the
  accepted Raven compiler, with exact dependency and toolchain provenance.

## 5. Freeze C# and evolve idiomatic Raven

After parity and self-hosting qualification, freeze the C# codebase as the trusted
reference and bootstrap seed. Normal compiler development then continues in the
Raven codebase. Keep the C# source, artifacts and tests available for reproduction
and historical comparison; freezing does not mean deleting them.

Progressively adopt idiomatic Raven patterns, one validated change at a time.
Use [feature meaning](../../lang/feature-meaning.md) and the
[result-shape guidance](../api/result-shapes.md) to choose constructs by semantics.
Preserve compiler correctness, incremental-query behavior and explicit target
ownership while changing internal design. Public API migrations remain deliberate
compatibility decisions, not automatic consequences of changing source language.

After the C# freeze, ordinary fixes and improvements are Raven-only. An essential
correction to the frozen seed requires a reduced failure proving that the seed
cannot correctly reproduce or support the accepted bootstrap chain, or a critical
artifact-integrity issue. Publish a separately identified revision and retain the
original artifacts; never move an existing tag or silently replace a binary.
This exceptional policy does not apply to routine dual maintenance during porting.

## 6. Move the compiler host to neoCLR

Later, port the Raven compiler's hosting environment to neoCLR by implementing the
APIs and services it needs there. Inventory library, collection, text, I/O,
concurrency, metadata, reflection, macro/plugin and toolchain dependencies, then
provide native APIs or explicit host adapters behind their existing boundaries.
Do not let missing host APIs become accidental language restrictions.

Qualify the same compiler behavior and self-hosting chain on neoCLR, recording
native runtime/library revisions and independently testing supported output
targets. A .NET-hosted compiler emitting neoCLR artifacts does not establish that
the compiler itself can run on neoCLR. A neoCLR-hosted compiler must still be able
to produce .NET output where supported, with .NET remaining the default target.

## Artifact identity and build seeds

An immutable build seed and a frozen development line are different things.
During dual maintenance, retain exact seed compiler/library artifacts while
continuing to fix C# source. Deliberately select and qualify a new seed revision
when the port needs a correction; record which revision built each stage.

Every accepted milestone records source commits, compiler implementation and
host, SDK/runtime versions, target reference libraries, Core identity, bootstrap
inputs, artifact hashes, test results and known limitations. Track compiler
product version, Core package/file version, assembly compatibility identity and
bootstrap revision independently. Public release milestones remain governed by
the release gates; this plan selects no release version or bootstrap tag.

Prevent compiler/Core dependency cycles and accidental use of stale repository
outputs, global SDKs or package caches. Pin and verify the actual dependency
closure. `Raven.Macros` remains a higher-layer consumer and validation workload;
`Raven.CodeAnalysis` must not acquire a dependency on that implementation library.

The earlier v1/v2/v3 labels describe the superseded API-first sequence; do not
silently reinterpret existing tags as the new phases. Existing
`scripts/rebuild-bootstrap-v1-core.sh`, `scripts/verify-bootstrap-v1-core.sh` and
`eng/bootstrap/v1` conventions remain tooling for their explicit seed format,
not a requirement to complete C# API migration before porting. Seed corrections
use new immutable identities and retain their original provenance. Milestone
names for the revised sequence can be selected when their artifacts are qualified.
