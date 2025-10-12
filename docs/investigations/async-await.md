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

### BadImageFormatException root cause

* **`AwaitUnsafeOnCompleted` argument mismatch** – Capturing the IL for the
  failing entry-point state machine shows Raven emitting `ldarg.0` immediately
  before invoking `AsyncTaskMethodBuilder<int>.AwaitUnsafeOnCompleted`, which
  pushes a copy of the struct onto the stack instead of the required managed
  pointer. The builder and awaiter arguments are loaded by reference, but the
  state machine itself is passed by value, so the CLR rejects the callsite that
  promises a `ref TStateMachine` parameter. 【0145d5†L16-L45】
* **Verifier reaction** – When `Program.Main` invokes the generated assembly,
  the runtime hits the invalid IL sequence, causing `BadImageFormatException`
  before the entry point can return. Fixing the issue requires the emitter to
  respect `RequiresReceiverAddress` and reuse the existing receiver load (or
  emit `ldarga.s 0`) so `AwaitUnsafeOnCompleted`, `SetResult`, and other builder
  members always observe the original state machine by reference. Future
  regression tests should assert the presence of `ldflda`/`ldarga` before async
  builder calls to prevent the by-value copy from resurfacing. 【0145d5†L33-L69】

## Implementation plan for full `async Task<T>` support

1. **Codify desired semantics**
   * Align with the language specification by documenting the expected runtime
     behaviour for `async` methods that return `Task<T>`, covering exception
     flow, return value propagation, and synchronization-context interaction.
   * Cross-check Roslyn's lowering strategy to enumerate the invariants Raven
     must preserve (e.g. builder initialization, by-ref state-machine access).
   * Capture any gaps between Raven's documentation and the intended semantics so
     the language specification can be updated alongside the implementation.
2. **Extend binder and symbol shape**
   * Ensure method and lambda symbols track their constructed `Task<T>` return
     type, including default inference when no explicit return is provided.
   * Audit call-site diagnostics to surface mismatches between declared return
     types and awaited expressions.
3. **Refine async lowering**
   * Update `AsyncLowerer` so `_state`, `_builder`, and hoisted awaiters share a
     single receiver load when mutating the state machine.
   * Emit `AsyncTaskMethodBuilder<T>` invocations using the constructed generic
     type, mirroring Roslyn's IL for `AwaitUnsafeOnCompleted` and completion
     paths.
4. **Adjust code generation**
   * Teach the emitter to produce correctly constructed generic builder fields
     and metadata handles for `Task<T>` state machines.
   * Verify the synthesized `SetStateMachine` and `MoveNext` signatures match the
     CLR's expectations for generic async builders.
5. **Regression and conformance testing**
   * Flip existing failing tests to assert successful execution and add new
     IL baselines validating builder construction, `_state` management, and
     awaited value flow.
   * Integrate runtime execution tests that await `Task<T>` entry points and
     confirm the returned result matches the awaited expression.
6. **Tooling and documentation**
   * Update developer docs (including this file and the language spec) to call
     out the async invariants and any new diagnostics introduced.
   * Add guidance for debugging async state machines and verifying emitted IL,
     leveraging `ilverify`/`peverify` once the pipeline is stable.

## Open items

* Tighten the await scheduling lowering so `_state` and hoisted awaiters share the
  same receiver load, unblocking the next IL regression for Roslyn parity.
* Add IL baselines for `Task<int>` entry points to assert the constructed builder
  metadata and opcode sequence around `AwaitUnsafeOnCompleted`.
* Integrate `ilverify`/`peverify` checks in CI once the state machine passes the
  runtime verifier.

## Step 1 – Desired semantics for `async Task<T>`

### Behavioural contract

* **Return value propagation** – Returning `expr` from an `async` method that
  declares `Task<T>` must produce a task whose `GetAwaiter().GetResult()` yields
  the same value as awaiting `expr`. If the body finishes without an explicit
  `return`, the compiler implicitly wraps the last expression (or `unit`) in
  `SetResult(default(T))`, matching C#'s handling of implicit `return;`.
* **Exception flow** – Exceptions that escape the async body before the first
  `await` propagate synchronously through the caller (they are raised prior to
  task creation). Once the state machine is active, any unhandled exception must
  call `builder.SetException(e)` and mark the task as faulted. Awaiters observe
  the fault through `GetResult()` throwing `e` wrapped in `AggregateException`
  when necessary, matching the `Task<T>` contract described in the language
  specification (`§await expressions`).
* **Cancellation** – Raven does not currently synthesize cancellation behaviour;
  tasks cancelled via `OperationCanceledException` follow the same path as other
  exceptions. The semantics section should explicitly note that the runtime
  representation is a faulted task containing the cancellation exception until a
  dedicated cancellation story exists.
* **Synchronization context** – Like C#, async Raven methods capture the current
  `SynchronizationContext` and `TaskScheduler.Current` when the first `await` is
  reached. Continuations resume on that context unless `ConfigureAwait(false)` is
  applied to the awaited task. Documenting this expectation keeps the compiler's
  behaviour aligned with developer intuition and ensures parity with Roslyn's
  lowering strategy.

### Lowering invariants to preserve

* **By-ref state machine** – The synthesized struct must never be copied while
  `_state` or `_builder` are being mutated. Roslyn achieves this by keeping the
  receiver on the evaluation stack (`ldarg.0`, `dup`, `stfld`). Our lowering
  needs to enforce the same single load and pass-by-reference behaviour so
  `AsyncTaskMethodBuilder<T>` always receives a reference to the original state
  machine.
* **Builder construction** – `AsyncTaskMethodBuilder<T>.Create()` must be called
  exactly once at the top of `MoveNext`, and the resulting builder stored into
  the hoisted `_builder` field before any awaits. The bootstrap path then calls
  `_builder.Start(ref stateMachine)` and returns `_builder.Task`.
* **SetResult/SetException balance** – Every exit path must funnel through the
  helper that sets `_state = -2`, clears hoisted awaiters, and invokes either
  `SetResult(value)` or `SetException(exception)` on the builder. This mirrors
  Roslyn's `AsyncMethodBuilderMemberCollection.Complete` pattern and guarantees
  the task transitions exactly once.
* **Awaiter plumbing** – Calls into `AwaitUnsafeOnCompleted`/`AwaitOnCompleted`
  must pass the hoisted awaiter field and the state machine by reference. Roslyn
  uses constrained calls when the awaiter is a value type to avoid boxing; Raven
  should follow the same rule to preserve allocation behaviour.

### Raven pipeline audit (Step 1 investigation)

* **Builder selection** – `SynthesizedAsyncStateMachineTypeSymbol.DetermineBuilderType`
  already constructs `AsyncTaskMethodBuilder<T>` when the constructed return type
  is available, but silently falls back to the non-generic builder whenever the
  awaited type binds to `ErrorTypeSymbol` or Raven cannot load the generic
  builder definition. Step 3 must stop relying on this fallback: emitting the
  nongeneric builder for `Task<T>` breaks result propagation and causes the
  verifier failure captured by `AsyncEntryPoint_WithTaskOfInt_ThrowsBadImageFormatException`.
  Step 1 documents that the implementation must surface a diagnostic if the
  generic builder is missing instead of degrading silently.
* **State-machine bootstrap** – `AsyncLowerer.CreateBuilderInitializationStatement`
  and `CreateBuilderStartStatement` request the builder through
  `BoundFieldAssignmentExpression(... requiresReceiverAddress: true)`, which
  proves the lowering stage expects the emitter to keep the struct by reference.
  However, the generated IL currently reloads `ldarg.0` for each store, copying
  the state machine before `_state` and `_builder` are updated. Step 1 records
  the requirement that the emitter honour the address-taken receiver so we can
  align with Roslyn's `ldarg.0`, `dup`, `stfld` pattern in Step 3.
* **Completion paths** – The lowering helper `CreateBuilderSetResultStatement`
  and the exception catch clause both call into the builder through a
  `BoundMemberAccessExpression` that expects a ref receiver. Auditing these code
  paths confirmed that the state machine only flows through
  `SetResult`/`SetException`, so Step 1 codifies the spec requirement that the
  task transitions exactly once (no alternate completion helpers are emitted).
* **Returned task plumbing** – `CreateReturnExpression` projects the builder's
  `Task` property into the rewritten method body. Because this logic is agnostic
  to `Task<T>` vs `Task`, Step 3 only needs to ensure the builder field carries
  the constructed generic so that the property access returns `Task<T>`.

### Roslyn parity baseline and reference materials

* **Async lowering blueprint** – Roslyn's `AsyncStateMachineRewriter` and
  `AsyncMethodBuilderMemberCollection` outline the exact opcode sequence (`ldarg`
  / `dup` / `stfld` for `_state`, `ldflda` for `_builder`, and constrained calls
  around `AwaitUnsafeOnCompleted`). Step 1 tracks these resources so that Step 3
  can reproduce the sequence for `Task<T>` methods. Capture explicit IL samples
  for:
  * The entry-point handshake (`Program.MainAsync` returning `Task<int>`).
  * A regular async method returning `Task<T>` with multiple awaits.
  * An async lambda returning `Task<T>` to validate closure lowering.
* **Verifier behaviour** – The regression test pins down the BadImageFormatException
  that appears when the state machine is copied. Step 1 records that we must run
  both Roslyn and Raven generated IL through `ilverify`/`peverify` once the
  lowering fix lands to prove the invariants are satisfied.
* **Specification sources** – The C# language specification (§10.15 in current
  draft) and the .NET async design docs define the observable behaviour of
  `Task<T>` result and exception propagation. Step 1 links these references to
  justify the behavioural contract listed above.

### Spec follow-ups

* The language specification currently describes await evaluation but does not
  spell out the `Task<T>` completion semantics. Capture the rules above and
  reference the pending section that will explain how result values and
  exceptions map onto the task.
* Document that `Task` vs `Task<T>` selection is driven by the annotated return
  type (with the implicit default to `Task` when the body produces `unit`), and
  that mismatch diagnostics surface when a body returns a bare `Task<T>` instead
  of the awaited result.
* Cross-link the future spec updates with the execution tests added in Step 5 so
  doc readers can see the behaviour enforced in code.

