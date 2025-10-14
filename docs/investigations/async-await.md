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
* The non-generic `System.Threading.Tasks.Task` surface is projected as
  `Task<Unit>` in Raven to mirror the language's explicit `Unit` value; the
  emitter erases that projection back to the CLR `Task` so runtime metadata
  remains source-compatible with existing async consumers.
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
* Synthesized async state machines reuse `EnsureFieldBuilder` when materialising
  `_state`, `_builder`, and hoisted-awaiters so emission no longer defines the
  same field twice before IL generation resolves the handle. 【F:src/Raven.CodeAnalysis/CodeGen/TypeGenerator.cs†L243-L399】
* Taking the address of the state machine now always pushes `ldarg.0`, ensuring
  builder calls receive a managed pointer to the struct rather than a by-value
  copy when the receiver is omitted. 【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L905-L926】

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
   * Introduce runtime smoke tests starting with a minimal `await
     Task.CompletedTask` console program before scaling up to the
     `samples/async-await.rav` scenario so each behaviour change can be
     validated incrementally.
   * Integrate runtime execution tests that await `Task<T>` entry points and
     confirm the returned result matches the awaited expression.
6. **Tooling and documentation**
   * Extend the `ravenc` CLI with a switch that shells out to `ilverify`, wiring
     in all metadata references so verification succeeds without manual setup.
   * Update developer docs (including this file and the language spec) to call
     out the async invariants and any new diagnostics introduced.
   * Add guidance for debugging async state machines and verifying emitted IL,
     leveraging `ilverify`/`peverify` once the pipeline is stable.
7. **Harden nested async state machines**
   * Mirror the method-level scheduling path by registering each lambda state
     machine’s `_state`, `_builder`, and hoisted awaiter fields with
     `CodeGenerator.AddMemberBuilder` before IL emission so `GetMemberBuilder`
     can resolve the field handles. 【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L17-L47】【F:src/Raven.CodeAnalysis/CodeGen/MethodBodyGenerator.cs†L449-L499】
   * Extend the lambda closure factory to describe awaiter and hoisted-local
     fields alongside captured variables so nested `MoveNext` bodies can emit
     stores to `<>awaiterN` without triggering `KeyNotFoundException`. 【F:src/Raven.CodeAnalysis/CodeGen/TypeGenerator.cs†L560-L623】
   * Expand regression coverage to assert metadata exists for async lambdas and
     nested functions once the registration path is in place. 【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L667-L711】
8. **Productionize verification and ergonomics**
   * Promote `ravenc --ilverify` (and a future `--peverify`) into CI so async
     regressions fail automatically without relying on manual runs.
   * Capture the generator prerequisites in the developer workflow (or wire
     them into the build) so contributors can run async tests without tripping
     over missing generated syntax/bound node files.
   * Audit diagnostics and UX around async inference so error messages remain
     actionable for both annotated and inferred `Task<T>` flows.

## Open items

* Fix async lambda emission so hoisted awaiters register their field builders
  before IL generation; this unblocks `AsyncLambda_EmitsStateMachineMetadata`
  and exercises the nested-state-machine coverage described in Step 7. 【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L17-L47】【F:src/Raven.CodeAnalysis/CodeGen/TypeGenerator.cs†L560-L623】【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L667-L711】
* Revisit await scheduling heuristics to eliminate the redundant receiver loads
  that still show up in IL when lowering complex control-flow (captured in
  Step 3’s remaining gaps).
* Restore runtime execution coverage by fixing the minimal `await
  Task.CompletedTask` program and the `samples/async-await.rav` regression so
  smoke tests can assert the generated state machines reach completion.
* Integrate the new `ravenc --ilverify` switch (or `peverify`) into CI once the
  state machine passes the runtime verifier to catch drift automatically.
* Implement open generic async support following the staged plan below.

### Roslyn open-generic async reference

To ground the remaining work in concrete behaviour, we compiled the following
C# method and inspected the generated IL with `ilspycmd`:

```csharp
class C
{
    public async Task<T> M<T>(T value)
    {
        await Task.Delay(10).ConfigureAwait(false);
        return value;
    }
}
```

The nested state machine `C.<M>d__0<T>` carries the method’s type parameter and
threads it through every metadata surface:

```
.field public valuetype AsyncTaskMethodBuilder`1<!T> '<>t__builder'
.field public !T 'value'
...
IL_0042: ldflda valuetype AsyncTaskMethodBuilder`1<!0> class C/'<M>d__0`1'<!T>::'<>t__builder'
IL_004b: call instance void AsyncTaskMethodBuilder`1<!T>::AwaitUnsafeOnCompleted<...>(!!0&, !!1&)

IL_002e: call instance void AsyncTaskMethodBuilder`1<!!T>::Start<class C/'<M>d__0`1'<!!T>>(!!0&)
IL_0039: call instance class Task`1<!0> AsyncTaskMethodBuilder`1<!!T>::get_Task()
```

Roslyn therefore:

* Reuses the cloned method type parameter for the builder field, parameter
  captures, `MoveNext` locals, and the synthesized type name.

### Await sample runtime regression

The sample harness now compiles every async sample but the emitted assemblies
still abort at runtime. Running `samples/async-await.rav` through `ravc` produces
an assembly that throws `InvalidProgramException` when the state machine reaches
the first await, and the end-to-end sample test fails with exit code 134 for the
same reason.【ed9cc9†L1-L7】【530b64†L1-L16】

Inspecting the generated IL shows the issue: while lowering `AwaitUnsafeOnCompleted`
the emitter loads the builder field address, leaving the state-machine receiver
buried underneath. The subsequent `ldflda` that is supposed to take the awaiter
field’s address instead consumes the builder pointer, so the verifier observes a
mismatched receiver type and rejects the call site.【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L894-L929】【fe416a†L1-L38】

Roslyn keeps the state-machine address on top of the stack by duplicating it
before every field access; the generated IL therefore passes both the awaiter
and the struct by reference to `AwaitUnsafeOnCompleted` without tripping the
verifier.【68cc92†L1-L37】 Raven needs a comparable mechanism—either by teaching
`EmitAddressOfExpression` to duplicate the receiver even when the async frame
already has it cached, or by extending `AsyncStateMachineILFrame` with an explicit
“borrow receiver” helper—so every async builder helper observes `ldarga.s 0`
for the state machine. Once the stack management is fixed we should add IL
assertions for the await path and flip the sample execution tests to expect the
printed output instead of an abort.
* Invokes `AsyncTaskMethodBuilder<T>.Start` and
  `AwaitUnsafeOnCompleted<…, C.<M>d__0<T>>` with the fully constructed state
  machine so `!!0` and `!!1` tokens already reflect the closed generic
  arguments.
* Emits the same constructed builder in the rewritten method body when loading
  `_builder.Task`, keeping the returned metadata closed over `T`.

Raven’s lowering now substitutes those generics, but the emitters still observe
the open definition, so the remaining steps need to flow the constructed view
through metadata caching.

### Plan: open generic async state machines

1. **Model the missing metadata**
   * Audit `SynthesizedAsyncStateMachineTypeSymbol` and related factory helpers
     to understand how method type parameters are currently discarded when
     lowering an async method.
   * Cross-check Roslyn’s `AsyncStateMachineTypeSymbol` stamping behaviour to
     catalogue which generic arguments are threaded through the generated type
     definition, `MoveNext`, and `SetStateMachine` methods.
   * Capture diagnostics gaps where open-generic async methods should error
     today (e.g. unverifiable emit) so we can flip them once support lands.
2. **Thread method generics through lowering**
   * Extend the async rewriter to surface the original `MethodSymbol` when
     synthesizing state-machine types so the emitted definition closes over the
     method’s type parameters.
   * Ensure hoisted fields, awaiter instantiations, and builder fields refer to
     those type parameters via `TypeMap` rather than constructing loose
     `TypeSymbol`s.
   * ✅ Raven now clones method type parameters onto
     `SynthesizedAsyncStateMachineTypeSymbol`, substituting them through
     parameter fields, hoisted locals, awaiter captures, and builder selection so
     open-generic async methods stamp the same metadata as Roslyn. 【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L13-L227】
   * ✅ `AsyncLowerer` threads the state-machine type map into awaiter locals and
     the `AwaitOnCompleted` instantiation, and regression tests cover a generic
     async method to lock in the behaviour. 【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1323-L1361】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncLowererTests.cs†L922-L989】
3. **Stamp constructed state machines at call-sites**
   * ✅ Update the bootstrap path that instantiates the synthesized struct so it
     passes the constructed method type arguments (e.g. `T`, `U`) alongside the
     original `Task`/`Task<T>` builder by materialising a constructed state-machine
     instance and threading its substituted fields through the async bootstrap.
     【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L118-L206】【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L85-L155】
   * ✅ Verify `AsyncTaskMethodBuilder<T>.Start` receives a closed generic instance
     whose `MoveNext` signature includes the method type arguments in the same
     order that Roslyn emits with regression coverage asserting the state-machine
     local, address-of, and builder invocations use the constructed generic.
     【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncLowererTests.cs†L860-L911】
4. **Surface constructed instances inside emission caches**
   * ✅ Rework `CodeGenerator` so the constructed async state machine tracked by
     lowering is registered alongside the definition, allowing cache lookups to
     resolve either the open symbol or the constructed instantiation. The
     registration now occurs immediately after rewriting and mirrors the mapping
     when type builders are created. 【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L19-L93】【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L822-L899】
   * ✅ Teach the generator pipeline to serve the constructed view for
     downstream consumers so locals, field accesses, and builder invocations
     never fall back to the open definition. This includes resolving field
     metadata through the constructed symbol and verifying the cache behaviour
     with a generic async IL regression test. 【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L990-L1006】【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L2046-L2051】【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L173-L226】
5. **Register cloned generics on the emitted type**
   * ✅ `TypeGenerator` now defines generic parameters for synthesized async
     state machines, registering the cloned method type parameters so metadata
     encodes the same `!!0` ordering that Roslyn produces. 【F:src/Raven.CodeAnalysis/CodeGen/TypeGenerator.cs†L138-L212】
   * ✅ Field builders resolve through the constructed type map, keeping `_state`,
     `_builder`, hoisted locals, and parameter captures stamped with substituted
     method generics. Metadata regression coverage asserts `_builder` and
     `_value` use the cloned type parameter. 【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L218-L269】
6. **Thread constructed state machines through method registration**
   * ✅ Teach `MethodGenerator` to request the `ConstructedStateMachine` that
     lowering attached to the source method, and register generic parameters
     against that constructed view instead of the open symbol so
     `MethodBuilder` handles reflect the cloned `!!0/!!1` tokens.
     【F:src/Raven.CodeAnalysis/CodeGen/MethodGenerator.cs†L24-L225】【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L108-L152】
   * ✅ Rework `CodeGenerator.GetMemberBuilder` to accept a constructed async
     context (definition + instantiation) and return substituted field/method
     builders without forcing later passes to jump back through the open
     definition. 【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L19-L142】【F:src/Raven.CodeAnalysis/CodeGen/Generators/Generator.cs†L142-L147】
   * ✅ Introduce an explicit `AsyncStateMachineEmissionContext` struct that
     packages the open symbol, constructed instance, and type map so every
     emitter that participates in async lowering can thread the same
     substitution data without re-querying caches, and add regression coverage
     asserting the context flows through method generators. 【F:src/Raven.CodeAnalysis/CodeGen/AsyncStateMachineEmissionContext.cs†L1-L68】【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L201-L260】
7. **Emit `MoveNext`/`SetStateMachine` using constructed metadata**
   * ✅ Updated `ExpressionGenerator` (and the callers that rely on it) to
     resolve builder fields, constructor handles, and async helper invocations
     through the `AsyncStateMachineEmissionContext` so the emitted IL always
     references the constructed `AsyncTaskMethodBuilder<T>` and cloned state
     machine members. 【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L2029-L2087】【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L3243-L3281】
   * ✅ Emitted `SetStateMachine` using the constructed receiver while
     preserving the `IAsyncStateMachine` parameter, matching the signature Roslyn
     produces for open generic async methods. 【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L3255-L3281】
   * ✅ Added IL regression tests that assert the generic async method’s
     `Start`, `AwaitUnsafeOnCompleted`, and `SetStateMachine` callsites stamp the
     cloned type parameter onto the builder and constructed state machine. 【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L270-L340】
8. **Stabilise constructed `MoveNext` emission**
   * ✅ Introduced `AsyncStateMachineILFrame` so `MethodBodyGenerator`,
     `StatementGenerator`, and `ExpressionGenerator` can reuse a shared receiver
     stack discipline across `_state`, `_builder`, and awaiter mutations instead
     of reloading `ldarg.0` for every store. 【F:src/Raven.CodeAnalysis/CodeGen/AsyncStateMachineILFrame.cs†L1-L93】【F:src/Raven.CodeAnalysis/CodeGen/MethodBodyGenerator.cs†L1-L462】
   * ✅ Threaded the frame through async member lookups so builder calls, hoisted
     awaiters, and state updates all consult the constructed metadata view rather
     than re-querying the open definition. 【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L1606-L3364】
   * ✅ Added IL regression coverage that asserts the duplicated receiver stays on
     the stack between the `_state` store and `AwaitUnsafeOnCompleted` call for
     generic async methods, mirroring Roslyn’s instruction ordering. 【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L320-L370】
9. **Align `SetStateMachine`/bootstrap helpers with Roslyn**
   * ✅ Captured the async bootstrap IL and verified
     `AsyncTaskMethodBuilder<T>.Start` consumes the constructed state machine by
     reference, guarding the pattern with
     `GenericAsyncMethod_StartCall_ReusesStateMachineLocalAddress` so the
     builder receiver and `Start` argument both reuse the state-machine local
     address. 【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L379-L405】
   * ✅ Exercised the synthesized `SetStateMachine` body with
     `GenericAsyncStateMachine_SetStateMachine_PassesReceiverByReference` to
     confirm the `_builder` field and generic call mirror Roslyn’s emitted
     metadata. 【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L442-L459】
   * ✅ Documented the parity so future work can focus on the remaining
     metadata-name alignment rather than recreating the bootstrap sequence.
10. **Regression coverage and documentation**
    * ✅ Extended async IL baselines so the generated state machine metadata name
      must encode the method arity and the recorded builder helpers retain their
      `!!0`/`!!1` method-generic tokens, mirroring the Roslyn IL baselines.
      【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L353-L424】
    * Add runtime executions for open-generic async methods returning both
      `Task` and `Task<T>`, and document the parity with Roslyn once `ilverify`
      passes.

### Re-evaluation insights

* Emission now retains the constructed async state machine alongside the
  definition, so subsequent steps can focus on threading the cloned generics into
  IL emission helpers without recreating substitutions on the fly. The caches
  still expose the original `TypeGenerator`, meaning the remaining work must
  ensure method bodies query the constructed mapping before resolving builder
  handles. 【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L19-L93】【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L990-L1006】
* The new `AsyncStateMachineILFrame` keeps the receiver alive across state
  updates, builder accesses, and awaiter stores so `MoveNext` now follows the
  same `ldarg.0`/`dup`/`stfld` discipline Roslyn emits. Tests guard against
  regressions by asserting no additional `ldarg.0` instructions appear between
  the `_state` store and `AwaitUnsafeOnCompleted`. 【F:src/Raven.CodeAnalysis/CodeGen/Generators/StatementGenerator.cs†L1-L83】【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L320-L370】
* Reflection on Roslyn’s emitted state machine confirms the struct carries four
  instance fields—`int <>1__state`, `AsyncTaskMethodBuilder<T> <>t__builder`, the
  captured parameter `T value`, and a `TaskAwaiter` slot—and both `MoveNext` and
  the synthesized `SetStateMachine` live on that constructed type while the
  interface method retains an `IAsyncStateMachine` parameter. Raven must register
  the cloned type parameters before emitting either method so metadata tokens
  line up with the constructed builder type that Roslyn exposes at runtime; the
  proposed emission frame should own that registration.
* Method generators now capture an `AsyncStateMachineEmissionContext`, letting
  `RegisterGenericParameters` map both the method type parameters and their
  cloned state-machine counterparts onto the same builder handles. The shared
  context gives downstream emitters access to substituted members without
  hopping back to the open definition, clearing the way for the IL rewrites in
  Step 7. 【F:src/Raven.CodeAnalysis/CodeGen/MethodGenerator.cs†L24-L225】【F:src/Raven.CodeAnalysis/CodeGen/AsyncStateMachineEmissionContext.cs†L1-L68】
* Regression coverage now asserts that async method generators expose the
  constructed context and that `GetMemberBuilder` returns substituted builder
  handles for the cloned `_builder` field. Future IL tests can therefore rely on
  the context being available when binding constructed metadata. 【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L201-L260】
* New IL baselines for the bootstrap and `SetStateMachine` helpers prove the
  constructed async state machine flows by reference during `Start` and the
  builder metadata remains stamped with the cloned type parameter. 【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L379-L405】【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L442-L459】
* Roslyn’s emitted IL for `C.<M>d__0<T>` confirms that the builder field, awaiter
  plumbing, and `Start`/`AwaitUnsafeOnCompleted` invocations all reference
  `AsyncTaskMethodBuilder<T>` with the method’s type parameter substituted. Our
  emitters currently rehydrate the open definition when emitting the bootstrap,
  so any metadata cached before substitution loses the constructed `!!0` tokens.
  The revised plan separates cache rewrites, generic registration, and IL
  generation so we can address each surface incrementally while keeping the
  constructed view front and centre.
* Reworking `CodeGenerator` to cache constructed state machines is likely to
  ripple into how nested lambdas and iterator scaffolding share builders. We may
  need a dedicated `ConstructedAsyncStateMachineRegistry` so the existing caches
  stay monomorphic for non-async types while open-generic state machines consult
  the constructed view on demand; the proposed emission frame can house that
  lookup to avoid further cache churn.
* Stamping the cloned type parameters directly onto the synthesized state machine
  lets field builders resolve to `!!0` metadata tokens, matching Roslyn’s
  emitted signatures and keeping future emission steps focused on the method body
  rewrites rather than type registration. 【F:src/Raven.CodeAnalysis/CodeGen/TypeGenerator.cs†L138-L212】【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L218-L269】
* Recording the IL after threading the constructed context through emission shows
  the async method, `MoveNext`, and `SetStateMachine` now invoke
  `AsyncTaskMethodBuilder<T>` with the cloned type parameter, confirming the
  constructed metadata is preserved end-to-end for generic async methods.

## Step 1 – Desired semantics for `async Task<T>`

### Behavioural contract

* **Return value propagation** – Returning `expr` from an `async` method that
  declares `Task<T>` must produce a task whose `GetAwaiter().GetResult()` yields
  the same value as awaiting `expr`. The builder holds on to the converted
  result value until the state machine transitions to its terminal state and the
  task completes. If the body finishes without an explicit `return`, the
  compiler implicitly wraps the last expression (or `unit`) in
  `SetResult(default(T))`, matching C#'s handling of implicit `return;`.
* **Synchronous entry segment** – Execution prior to the first `await` runs on
  the caller's stack. If that segment completes without suspending, the builder
  transitions immediately to `SetResult` and the returned task is already in the
  completed state when observed by the caller. This mirrors Roslyn's behaviour
  and guarantees that trivial async methods continue to behave like synchronous
  methods with an extra `Task<T>` wrapper.
* **Exception flow** – Exceptions that escape the async body before the first
  `await` propagate synchronously through the caller (they are raised prior to
  task creation). Once the state machine is active, any unhandled exception must
  call `builder.SetException(e)` and mark the task as faulted. Awaiters observe
  the fault through `GetResult()` rethrowing `e`; in synchronous waits the
  runtime wraps the exception in an `AggregateException`, so the spec needs to
  point out that Raven mirrors the CLR's wrapping behaviour. `finally`
  continuations and `using` disposals run before the builder is notified to keep
  observable side effects consistent with C#.
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
* **Terminal state** – Every completion path must stamp `_state = -2` before
  calling into the builder. This ensures subsequent calls to `MoveNext` exit
  immediately and mirrors Roslyn's guard against multiple completions.
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
  exceptions map onto the task. Explicitly document the synchronous entry
  segment so callers know which exceptions can surface before a task instance
  exists.
* Document that `Task` vs `Task<T>` selection is driven by the annotated return
  type (with the implicit default to `Task` when the body produces `unit`), and
  that mismatch diagnostics surface when a body returns a bare `Task<T>` instead
  of the awaited result.
* Cross-link the future spec updates with the execution tests added in Step 5 so
  doc readers can see the behaviour enforced in code.

## Step 2 – Binder and symbol shape

### Current status

* `SourceMethodSymbol` now records when an async declaration without an
  annotation requires return-type inference so the binder can defer diagnostics
  until the awaited result is known. After the body is bound once, the method
  rebinds with the inferred `Task<T>` shape so conversions and diagnostics run
  against the awaited result type. 【F:src/Raven.CodeAnalysis/Symbols/Source/SourceMethodSymbol.cs†L30-L37】【F:src/Raven.CodeAnalysis/Binder/MethodBodyBinder.cs†L19-L38】
* Top-level functions and class members mark async methods without annotations
  as inference candidates up front. Expression-bodied members reuse the same
  helper logic as block bodies so `async f() => 5` now lowers as `Task<int>` and
  reports `RAV2705` when returning an existing task instance. 【F:src/Raven.CodeAnalysis/Binder/FunctionBinder.cs†L82-L125】【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L5162-L5209】
* A shared `AsyncReturnTypeUtilities` helper normalises literal and task-shaped
  inference results so nested functions, methods, and lambdas all project their
  awaited result into a constructed `Task<T>` rather than stacking tasks. This
  keeps diagnostics for mismatched return expressions aligned with Roslyn’s
  behaviour and avoids silently accepting `Task<Task>` return chains. 【F:src/Raven.CodeAnalysis/Binder/AsyncReturnTypeUtilities.cs†L1-L53】

### Remaining gaps

* Accessors still rely on their annotated property/indexer type; verify async
  getters participate in the same inference path once setter support lands.
* Binder inference currently hinges on a full-body rebind. Explore capturing the
  awaited result type during initial binding to avoid the extra pass when the
  pipeline becomes hot.

## Step 3 – Async lowering

### Current status

* All state-machine field writes now funnel through a dedicated helper that
  marks the receiver as `requiresReceiverAddress`. The emitter recognises that
  hint and keeps `_state`, `_builder`, awaiters, and hoisted locals operating on
  the in-place struct without spilling the value through a temporary. The new
  helper also powers hoisted-local rewriters so `await` scheduling and resume
  paths share a single receiver load before each `stfld`. 【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L429-L438】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L660-L704】
* `BoundAddressOfExpression` now emits `ldarga.s 0` when the lowered tree takes
  the address of the state machine itself. `AwaitUnsafeOnCompleted` therefore
  receives a managed pointer instead of a copy, matching Roslyn's verifier-safe
  IL and unblocking runtime execution. Regression coverage inspects the IL
  sequence to confirm both the builder and the state machine arguments are fed
  by-reference without temporary locals. 【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L905-L916】【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L352-L396】
* Await expressions embedded in `try` expressions stay in expression form after
  rewriting so the async state machine can publish either the awaited result or
  the captured exception through the union return type. Semantic coverage locks
  in the behaviour for `try await` by asserting the inferred type is
  `int | System.Exception`. 【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L916-L951】【F:test/Raven.CodeAnalysis.Tests/Semantics/ExceptionHandlingTests.cs†L89-L123】

### Remaining gaps

* The lowering stage still materialises address-of nodes even when the emitter
  could reuse an existing receiver load. Inline heuristics that elide redundant
  `ldarg.0` instructions would better align with Roslyn's `dup` pattern and
  shave an extra store from hot paths.
* IL baselines continue to assert only builder/awaiter usage. Step 4 and Step 5
  should add explicit expectations for `_state = -2`, hoisted-local clears, and
  the state-machine address so regressions in emission are caught immediately.

## Step 4 – Code generation

### Current status

* `TypeGenerator` now resolves synthesized async state-machine fields through a
  dedicated helper that eagerly constructs generic types when the metadata uses
  `AsyncTaskMethodBuilder<T>`. The codegen layer therefore emits `_builder`
  with a closed `AsyncTaskMethodBuilder<int>` type handle instead of deferring
  to an open generic that loses the awaited type. The same helper keeps the
  behaviour for non-generic fields unchanged so iterator and closure emission
  still lean on `ResolveClrType` for their metadata.
* Metadata regression tests decode the state-machine type definition and assert
  that `_builder` is encoded as a generic-instance field whose single type
  argument is `System.Int32`. Additional validation now inspects the
  `MoveNext` and `SetStateMachine` signatures to guarantee both methods return
  `void`, and `SetStateMachine` takes `IAsyncStateMachine` by reference. These
  checks ensure the emitter produces the same canonical metadata as Roslyn.

### Remaining work

* The state-machine type still relies on `TypeBuilder` name matching to hook up
  interface overrides. Future work should stamp explicit interface metadata on
  `SetStateMachine` to mirror C#'s explicit implementation naming pattern.
* Method signature validation currently targets a single `Task<int>` shape.
  Extend the coverage to async lambdas and generic async methods once binder
  support lands so the code generator is exercised with type parameters.
* Async lambdas currently fail `AsyncLambda_EmitsStateMachineMetadata` because
  the generated state machine references hoisted awaiter fields (for example,
  `<>awaiter3`) that were never registered with `CodeGenerator.GetMemberBuilder`
  before IL emission. Update the lambda state-machine bootstrap to add those
  synthesized fields to `_mappings` so nested async bodies can materialize their
  awaiters and metadata before `EmitMemberILBodies` executes. 【c00fe4†L12-L47】【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L17-L47】【F:src/Raven.CodeAnalysis/CodeGen/TypeGenerator.cs†L500-L520】

## Step 5 – Regression and conformance testing

### Current status

* The `AsyncEntryPoint_WithTaskOfInt` regression now loads the emitted assembly,
  invokes the synthesized `Program.Main` bridge, and asserts that
  `MainAsync().GetAwaiter().GetResult()` returns the awaited value while
  `Console.WriteLine` observes the same output. 【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L318-L352】
* `AsyncEntryPoint_WithTask_ExecutesSuccessfully` exercises the non-generic
  `Task` projection, proving the bridge awaits `MainAsync`, returns `void`, and
  flushes awaited console writes before the process exits. 【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L352-L403】
* IL regression coverage for `Program.Main` ensures the bridge calls the async
  helper, retrieves the awaiter, and synchronously awaits the result, proving
  that the entry-point handshake mirrors Roslyn’s lowering. 【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L355-L369】
* State-machine IL baselines now assert that `_state` is stamped with `-2` via a
  single receiver load immediately before the completion call so `MoveNext`
  cannot complete twice. 【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L540-L571】

### Remaining gaps

* Port the entry-point execution test to cover async lambdas and methods invoked
  through metadata so future lowering work continues to observe the runtime
  contract.
* Integrate verifier runs (e.g. `ilverify`) into CI once async emission is
  stable to catch drift without relying solely on unit tests.

## Step 6 – Tooling and documentation

### Current status

* `ravenc` now accepts `--ilverify` and `--ilverify-path` flags. When enabled,
  the compiler shells out to `ilverify`, automatically passing every reference
  used during compilation (including `System.Private.CoreLib`) so verification
  succeeds without additional arguments. 【F:src/Raven.Compiler/Program.cs†L59-L91】【F:src/Raven.Compiler/IlVerifyRunner.cs†L12-L80】
* `IlVerifyRunner` centralises argument construction and error handling. Missing
  executables produce actionable guidance instructing authors to install the
  tool or supply an explicit path, while successful runs print the verifier
  output and confirm completion. 【F:src/Raven.Compiler/IlVerifyRunner.cs†L22-L74】
* This investigation and the language specification now document the async
  invariants alongside the new tooling so contributors know how to exercise the
  verifier when debugging state-machine failures. 【F:docs/lang/spec/language-specification.md†L1378-L1415】

### Remaining work

* Add a companion `peverify` integration once the runtime story stabilises so we
  can compare CoreCLR and desktop verification behaviour.
* Promote the new CLI switch into CI pipelines to automatically gate async code
  generation regressions on successful IL verification.

## Step 7 – Async lambdas and nested state machines

### Current status

* `CodeGenerator.GetMemberBuilder` now synthesizes field builders on demand when
  nested async state machines introduce hoisted awaiters after the initial
  scheduling pass. The `_mappings` dictionary therefore remains in sync with the
  state machine symbol so lambda emission no longer throws
  `KeyNotFoundException`. 【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L27-L45】【F:src/Raven.CodeAnalysis/CodeGen/TypeGenerator.cs†L234-L265】
* `AsyncLambda_EmitsStateMachineMetadata` exercises the repaired path by forcing
  code generation to materialize field builders for `_state`, `_builder`, and the
  synthesized awaiter before hitting the known address-taking limitation. The test
  verifies the mapping now survives nested emission. 【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L700-L738】

### Next actions

* Fold the on-demand registration back into the closure bootstrap so nested
  state machines eagerly record `_state`, `_builder`, and awaiter fields beside
  captured variables, letting Step 3’s by-ref helpers apply uniformly. 【F:src/Raven.CodeAnalysis/CodeGen/TypeGenerator.cs†L560-L623】【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L1663-L2566】
* Extend the IL regression suite with by-reference assertions once the closure
  scheduling is unified, ensuring `AwaitUnsafeOnCompleted` continues to see the
  state-machine receiver by reference. 【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L640-L738】

## Step 8 – Production verification and ergonomics

### Current status

* `AsyncAssembly_PassesIlVerifyWhenToolAvailable` runs `IlVerifyRunner` against
  the emitted async assembly whenever the `RAVEN_ILVERIFY_PATH` environment
  variable resolves to an executable. This gives the regression suite an opt-in
  verification gate that CI can enable without waiting for the full async
  pipeline to stabilize. 【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L77-L119】【F:test/Raven.CodeAnalysis.Tests/Utilities/IlVerifyTestHelper.cs†L1-L24】
* `Raven.CodeAnalysis.Tests` wires an `EnsureRavenGenerators` target into its
  build graph so `dotnet test` automatically runs the syntax, bound-node, and
  diagnostics generators before exercising the verifier hook. The manual
  bootstrap step in the repo instructions is no longer required for async
  regression coverage. 【F:test/Raven.CodeAnalysis.Tests/Raven.CodeAnalysis.Tests.csproj†L36-L47】
* `IlVerifyRunner` now resolves the executable using `--ilverify-path`,
  `RAVEN_ILVERIFY_PATH`, or the system `PATH`, and prints actionable guidance
  when resolution fails. Tests share the same helper, so the CLI and regression
  suite surface identical instructions for installing the verifier. 【F:src/Raven.Compiler/IlVerifyRunner.cs†L17-L171】【F:test/Raven.CodeAnalysis.Tests/Utilities/IlVerifyTestHelper.cs†L1-L24】
* The repository carries a local `dotnet-ilverify` manifest and the test project
  restores it before build, letting the runner fall back to `dotnet tool run
  ilverify` when no explicit path is provided. 【F:.config/dotnet-tools.json†L1-L9】【F:test/Raven.CodeAnalysis.Tests/Raven.CodeAnalysis.Tests.csproj†L36-L47】【F:src/Raven.Compiler/IlVerifyRunner.cs†L17-L220】

### Next actions

* Provision `ilverify` in CI and export `RAVEN_ILVERIFY_PATH` so the new regression
  hooks fail fast when async lowering regresses.

## Step 9 – Continuous verification gating

### Current status

* `IlVerifyRunner` inspects the repo’s tool manifest and, when the resolver cache
  shows `dotnet-ilverify` has been restored, shells out through `dotnet tool run
  ilverify`. Contributors can rely on `dotnet tool restore` rather than configuring
  `RAVEN_ILVERIFY_PATH`. 【F:src/Raven.Compiler/IlVerifyRunner.cs†L17-L220】
* `AsyncILGenerationTests` skip verification only when neither an explicit
  executable nor the restored local tool can be found, so the verification code
  path matches the CLI’s resolution logic. 【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L77-L101】【F:test/Raven.CodeAnalysis.Tests/Utilities/IlVerifyTestHelper.cs†L1-L24】
* `Raven.CodeAnalysis.Tests` now runs `dotnet tool restore` during build, ensuring
  the local verifier is available before the regression suite executes. 【F:test/Raven.CodeAnalysis.Tests/Raven.CodeAnalysis.Tests.csproj†L36-L47】

### Next actions

* Wire the CI harness to set `RAVEN_ILVERIFY_PATH` (or rely on the restored tool)
  so verification failures surface as red builds instead of optional skips.

## Step 10 – Runtime smoke tests for non-generic `Task`

### Current status

* `AsyncEntryPoint_WithTask_ExecutesSuccessfully` covers the simplest async
  entry point that returns `Task`, validating the synthesized `Program.Main`
  bridge awaits the helper, returns `void`, and preserves awaited console output
  before terminating. 【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L352-L403】

### Next actions

* Extend the smoke suite with chained awaits and nested async helpers so `Task`
  state machines remain exercised once lambdas and local functions flow through
  the runtime path.

## Step 11 – Runtime smoke tests for `Task<T>`

### Current status

* `AsyncEntryPoint_WithTaskOfInt_ExecutesViaCliSuccessfully` compiles the
  top-level `Task<int>` sample via the CLI and executes it with `dotnet`,
  asserting the process exits with the awaited value (`42`) and prints the same
  value to the console. 【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L405-L476】

### Next actions

* Scale the CLI-driven smoke tests to cover multiple awaits, error paths, and
  nested async helpers so generic state machines stay validated as new features
  land.

## Step 12 – Open generic async state machines

### Current status

* C# stamps method type parameters onto the synthesized state machine struct
  (for example, generating `<Test>d__0<T>` for `async Task<T> Test<T>(T value)`),
  so each instantiation like `Test<int>` closes both the state machine and its
  `AsyncTaskMethodBuilder<T>` field with the concrete type argument. That design
  keeps hoisted locals, parameters, and awaiters bound to the constructed `T`
  when `MoveNext` executes.
* Raven currently manufactures one `SynthesizedAsyncStateMachineTypeSymbol` per
  method definition and never clones the method's generic parameters onto the
  generated struct, leaving the state machine non-generic regardless of the
  declaration's arity. 【F:src/Raven.CodeAnalysis/Compilation.SynthesizedTypes.cs†L39-L60】【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L18-L98】
* Because the struct has no type parameters, hoisted fields such as `_value`
  retain the open method type parameter, and `AsyncLowerer` reuses that open
  symbol when it instantiates the state machine inside constructed bodies.
  When the caller supplies concrete type arguments (e.g. `await Test(42)`), the
  runtime still observes an open generic state machine, so the builder and
  hoisted locals never close over `int`, diverging from the CLR shape produced
  by Roslyn.

### Next actions

* Mirror Roslyn by projecting each method type parameter onto the synthesized
  state machine (e.g. via `SourceNamedTypeSymbol.SetTypeParameters`) so the
  struct becomes `<AsyncStateMachine<TArgs...>>` and its fields bind to the
  constructed arguments.
* Update the async lowering pipeline to request the constructed state machine
  when rewriting a `ConstructedMethodSymbol`, ensuring the `BoundObjectCreation`
  for the state machine uses the closed generic type and builder metadata.
* Add IL and execution baselines that cover `async Task<T>` methods with both
  explicit and inferred type arguments to verify the new instantiation logic and
  guard against regressions when additional async features arrive.

