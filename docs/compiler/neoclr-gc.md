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
