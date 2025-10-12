# Async/await investigation

This note tracks the state of Raven's async/await pipeline and the work that
remains to match the behaviour of C#.

## Snapshot of the current implementation

### Language surface area

* `async` is accepted on top-level functions, members, lambdas, and accessors;
  parsing keeps the modifier attached so the binder can flow `IsAsync` through
  the pipeline.
* Async declarations without an explicit return type default to
  `System.Threading.Tasks.Task`, and `async func Do() -> Task<Unit>` is treated
  as sugar for `async func Do() -> Task` so entry-point expectations continue to
  mirror C# while still emitting `System.Threading.Tasks.Task` metadata.
* Global statements lower to an async `Program.MainAsync` that returns `Task` or
  `Task<int>` while `Program.Main` synchronously bridges to the awaited result.

### Binding and lowering

* `BoundAwaitExpression` validates the awaited pattern and captures awaiter and
  result types; async binders mark rewritten bodies so the lowerer can generate
  state machines.
* `AsyncLowerer` hoists locals that survive across awaits, rewrites control-flow
  constructs, and produces a `SynthesizedAsyncStateMachineTypeSymbol` whose
  `MoveNext` implements the expected `_state` dispatch and resume paths.
* Disposal and expression-bodied members flow through the same rewriter, keeping
  async `using` patterns and lambda lowering aligned with synchronous code paths.

### Emission

* Code generation creates the synthesized struct, emits `MoveNext`/`SetStateMachine`,
  and annotates async methods with the appropriate builder attributes so the CLR
  observes the async metadata.
* The bootstrap path instantiates the struct, seeds `_state = -1`, initializes
  the builder, and returns the builder task, matching the Roslyn handshake for
  both declaration bodies and global statements.
* Builders and hoisted awaiters now use `TryEmitValueTypeReceiverAddress`, keeping
  `_state`, `_builder`, and awaiter fields operating on the in-place struct so
  mutations do not spill through temporaries.

## `async Task<T>` entry points

1. **Regression coverage** – `AsyncEntryPoint_WithTaskOfInt_ThrowsBadImageFormatException`
   records the current failure: the emitted IL trips the verifier because the
   state machine is copied before `_state` and builder updates execute. This
   test must flip to asserting successful execution once the fix lands.
2. **Receiver handling** – Value-type receivers already stay by-ref, but await
   scheduling still reloads `ldarg.0` for each assignment. We need a lowering
   shape that shares the receiver across `_state` and awaiter stores so the IL
  mirrors Roslyn's `ldarg.0`, `dup`, `stfld` sequence.
3. **Generic builders** – Every call into `AsyncTaskMethodBuilder<T>` must use the
   constructed generic and pass the builder field by reference. Additional IL
   assertions should pin the closed generic metadata to prevent regressions.
4. **Runtime validation** – Once emission matches the Roslyn pattern, promote the
   console repro into an execution test that verifies the generated assembly
   completes successfully and emits the awaited value.
5. **Documentation** – Capture the invariant that async state machines must not
   copy the struct before mutating `_state` or invoking builder methods.

## Open items

* Tighten the await scheduling lowering so `_state` and hoisted awaiters share the
  same receiver load, unblocking the next IL regression for Roslyn parity.
* Add IL baselines for `Task<int>` entry points to assert the constructed builder
  metadata and opcode sequence around `AwaitUnsafeOnCompleted`.
* Integrate `ilverify`/`peverify` checks in CI once the state machine passes the
  runtime verifier.

