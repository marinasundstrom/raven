# neoCLR GC facade integration — 2026-09-27

Target-only development contract on the neoclr branch. System.Runtime.GC is a Raven
static class exposing six long object counters, synchronous Collect and nullable
KeepAlive. Exact bridge validation admits these members; there is no new opcode,
compiler semantic change or Runtime Contract setting. Static long getters use
ordinary calls. Public control methods have no CLI result; their private runtime
services use the existing inhabited Void ABI, discarded by the bridge.

Counters and collection operate on the current VM execution, including its scheduler
roots; isolated workers have separate heaps. No generations, byte accounting,
finalization, tuning or native memory release is promised. neoCLR owns the collector,
root scanning and limits; Raven owns ordinary call/argument emission. Future
optimizing backends must honor KeepAlive's reachability-through-call semantics.

The neoCLR runtime-gc public consumer compiles and runs with the matching reference,
generated library and runtime. Sixteen exact public signature checks and focused
native root/counter/limit tests validate the boundary. See neoCLR docs/runtime-gc.md
for the comparison, detailed limits and evidence. No Raven compiler tests were
needed because this slice changes no compiler behavior.


## Native source checkpoint — 2026-10-06

The production GC source now compiles unchanged with internal native service adapters.
The runtime accepts both existing inhabited-Void and no-result GCCollect/GCKeepAlive
signatures; no-result calls push no value. Six counter signatures remain exact Int64.
This uses existing Raven emission, without changing .NET emission or adding a target
option. A separate artifact-only consumer verifies and executes explicit collection,
counters, shared mutation and retained object identity against the native library.

The nullable parameter contract is not yet complete through native metadata import:
KeepAlive(object?) becomes object in the imported symbol and KeepAlive(null) rejects
with RAV1503/RAV1509 before publication. The valid null-call fixture is retained next
to the passing retention fixture in neoCLR's source-heap gate. Do not treat the latter
as proof that the whole public GC contract passes. Nullable-reference annotation
encoding/reading and projection into Raven symbols must be implemented together,
following CLI annotation conventions; a GC-specific binding exception is not appropriate.
Native runtime tests separately confirm null is accepted by the service itself.

Validation uses the pinned Preview 12 Raven compiler (5f6298c123), existing Numbers/core/
seed artifacts and the rebuilt NeoCLR runtime. Nine runtime GC tests and the exact
service-signature test pass. This slice changes no Raven compiler code, so it introduces
no general compiler fix to backport to main. See neoCLR's
`docs/experiments/extended-cli-metadata/source-heap-2026-10-06.md` for evidence and scope.


## Native nullable contract completed — 2026-10-06

The earlier import blocker is resolved by Raven `d19c6e4a3` and neoCLR metadata
`119d2daf`. Emission preserves explicit callable nullable annotations from symbols;
introspection supplies them to the independent native importer. Physical call references
erase annotation-only wrappers. There is no GC-specific binding exemption or runtime
nullability change, and ordinary nonnullable parameters still reject null.

The updated source-heap gate compiles unchanged GC sources into Heap.dll and compiles
both consumers without those sources. Both verify and execute with exit 0 and exact
stdout `Native source heap passed`, including the original KeepAlive(null) fixture.
The fixed compiler snapshot, runtime, source and dependency hashes are recorded in
neoCLR's `docs/experiments/extended-cli-metadata/source-heap-nullable-2026-10-06.json`.
All 17 focused .NET nullability tests and seven native semantic consumers pass. The
nullable symbol probe also covers array elements, generic arguments and open/constructed
method scopes. Context/field annotations, new runtime checks and full System bootstrap
remain outside this gate. Published Preview 12 binaries have not been replaced.
