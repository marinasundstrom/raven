# Async/await investigation

This note tracks the state of Raven's async/await pipeline, the issues currently
blocking parity with C#, and the work required to resolve them.

## Prioritized issues

1. **Unblock substituted async method emission** – `ConstructedMethodSymbol.GetMethodInfo`
   reflects over uncreated `TypeBuilder` instances when emitting instantiated async
   members such as `Program.Test(int)`, crashing the compiler before generic async
   methods can be materialised.
2. **Fix `async Task<T>` entry-point IL** – the generated state machine passes the
   struct receiver by value to `AsyncTaskMethodBuilder<T>.AwaitUnsafeOnCompleted`,
   producing unverifiable IL and a `BadImageFormatException` at runtime.
3. **Register async lambda metadata** – nested async state machines still miss
   `CodeGenerator.AddMemberBuilder` registrations for hoisted awaiter fields,
   preventing IL emission for coverage such as
   `AsyncLambda_EmitsStateMachineMetadata`.
4. **Harden lowering and regression coverage** – redundant receiver loads remain
   in complex await scheduling paths, runtime smoke tests are disabled, and the
   documentation/tooling pipeline lacks automated IL verification.

### 1. Unblock substituted async method emission (Priority 0)

**Impact** – `samples/test8.rav` and any async method that materialises a
constructed generic currently fail to emit because the compiler crashes before it
can hand out `MethodInfo` handles for instantiated members.

**Current behaviour** – running
`dotnet run --project src/Raven.Compiler/Raven.Compiler.csproj -- src/Raven.Compiler/samples/test8.rav -o test.dll -d pretty`
throws `System.NotSupportedException: The invoked member is not supported before
the type is created.` while lowering the async state machine, leaving `test.dll`
unproduced. 【c6da48†L1-L11】【2b0969†L1-L47】

**Evidence** – instrumentation confirms the emitter successfully resolves
`Create`, `Start`, `AwaitUnsafeOnCompleted`, and `SetResult` on
`AsyncTaskMethodBuilder<T>` before attempting to instantiate the substituted
`Program.Test(int)` backing method. Because the containing `Program` type still
has an uncreated `TypeBuilder`, enumerating `methodSearchType.GetMethods` inside
`ConstructedMethodSymbol.GetMethodInfo` throws `TypeBuilderImpl.ThrowIfNotCreated`.
【5406d5†L65-L132】【d5ec68†L1-L36】【F:src/Raven.CodeAnalysis/Symbols/Constructed/ConstructedMethodSymbol.cs†L201-L270】

**Proposed fix** – teach `ConstructedMethodSymbol` (and callers such as
`SubstitutedMethodSymbol`) to reuse the `MethodBuilder` handles recorded through
`CodeGenerator.AddMemberBuilder` instead of reflecting over incomplete
`TypeBuilder` instances. This keeps async emission on the Reflection.Emit path and
lets `samples/test8.rav` complete successfully. 【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L18-L61】

**Next steps**

* Plumb `MethodBuilder` caching through the substitution lookup so generic async
  methods resolve without reflection.
* Add regression coverage that emits a constructed async generic and asserts the
  compiler no longer throws.
* Fold the findings back into this document once the crash is unblocked.

### 2. Fix `async Task<T>` entry-point IL (Priority 1)

**Impact** – the runtime rejects the generated assembly for `async Task<T>` entry
points, so even simple `await Task.FromResult(42)` programs fail to execute.

**Current behaviour** –
`AsyncEntryPoint_WithTaskOfInt_ThrowsBadImageFormatException` currently asserts
failure because the IL copies the async state machine struct before updating
`_state` and awaiting `AwaitUnsafeOnCompleted`. 【0145d5†L16-L45】

**Evidence** – the failing IL passes the state machine by value when invoking
`AsyncTaskMethodBuilder<int>.AwaitUnsafeOnCompleted`, violating the required
`ref TStateMachine` signature. The runtime therefore throws `BadImageFormatException`
when `Program.Main` runs the emitted assembly. 【0145d5†L33-L69】

**Proposed fix** – update async lowering so `_state`, `_builder`, and hoisted
awaiters share a single receiver load (`ldarg.0`/`ldarga`) and ensure every
`AsyncTaskMethodBuilder<T>` call honours `RequiresReceiverAddress`. Once the IL
matches Roslyn’s pattern, convert the console repro into a passing runtime test
that validates the awaited value flows through the entry point.

**Next steps**

* Adjust the lowering shape to avoid copying the struct before mutating `_state`.
* Emit `AsyncTaskMethodBuilder<T>` invocations using the constructed generic type.
* Promote the repro into a runtime execution test once the verifier passes.

### 3. Register async lambda metadata (Priority 2)

**Impact** – nested async state machines cannot be emitted reliably, blocking
coverage that asserts metadata for hoisted awaiters and nested `MoveNext`
implementations. 【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L667-L711】

**Current behaviour** – lambda state machines miss `CodeGenerator.AddMemberBuilder`
registrations for hoisted awaiter fields, so emission fails when IL generation
tries to resolve the missing handles. 【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L17-L47】【F:src/Raven.CodeAnalysis/CodeGen/TypeGenerator.cs†L560-L623】

**Proposed fix** – mirror the method-level registration path by recording each
lambda state machine’s `_state`, `_builder`, and hoisted awaiter fields before IL
emission. Extend the closure factory so nested `MoveNext` bodies know about the
awaiter and hoisted-local fields they need to access.

**Next steps**

* Register lambda field builders alongside captured variables.
* Extend regression coverage to assert metadata exists for async lambdas and
  nested functions once registration succeeds.

### 4. Harden lowering and regression coverage (Priority 3)

**Impact** – even after the primary crashes are fixed, the remaining gaps in
lowering and tooling leave the async pipeline brittle and difficult to verify.

**Current behaviour** – redundant receiver loads still appear in complex await
scheduling paths, runtime smoke tests such as the minimal
`await Task.CompletedTask` program remain disabled, and developers must run
`ilverify`/`peverify` manually to validate IL. The current workflow also requires
manual generator refreshes before the compiler builds.

**Proposed fix** –

* Consolidate await scheduling so receiver loads are shared across `_state` and
  awaiter stores, mirroring Roslyn’s `ldarg.0`, `dup`, `stfld` sequence.
* Restore runtime execution coverage for minimal async programs and
  `samples/async-await.rav` once the IL is fixed.
* Integrate IL verification into the CLI (e.g. `ravenc --ilverify`) and wire it
  into CI to catch regressions automatically.
* Document the generator prerequisites so contributors can run async tests without
  missing codegen steps.

## Supporting context

### Current implementation snapshot

**Language surface area**

* `async` is accepted on top-level functions, members, lambdas, and accessors;
  parsing keeps the modifier attached so the binder can flow `IsAsync` through the
  pipeline.
* Async declarations without an explicit return type default to
  `System.Threading.Tasks.Task`, and `async func Do() -> Task<Unit>` is treated as
  sugar for `async func Do() -> Task` so entry-point expectations continue to
  mirror C# while still emitting `System.Threading.Tasks.Task` metadata.
* The non-generic `System.Threading.Tasks.Task` surface is projected as
  `Task<Unit>` in Raven to mirror the language's explicit `Unit` value; the
  emitter erases that projection back to the CLR `Task` so runtime metadata
  remains source-compatible with existing async consumers.
* Global statements lower to an async `Program.MainAsync` that returns `Task` or
  `Task<int>` while `Program.Main` synchronously bridges to the awaited result.

**Binding and lowering**

* `BoundAwaitExpression` validates the awaited pattern and captures awaiter and
  result types; async binders mark rewritten bodies so the lowerer can generate
  state machines.
* `AsyncLowerer` hoists locals that survive across awaits, rewrites control-flow
  constructs, and produces a `SynthesizedAsyncStateMachineTypeSymbol` whose
  `MoveNext` implements the expected `_state` dispatch and resume paths.
* Disposal and expression-bodied members flow through the same rewriter, keeping
  async `using` patterns and lambda lowering aligned with synchronous code paths.

**Emission**

* Code generation creates the synthesized struct, emits `MoveNext`/`SetStateMachine`,
  and annotates async methods with the appropriate builder attributes so the CLR
  observes the async metadata.
* The bootstrap path instantiates the struct, seeds `_state = -1`, initializes the
  builder, and returns the builder task, matching the Roslyn handshake for both
  declaration bodies and global statements.
* Builders and hoisted awaiters now use `TryEmitValueTypeReceiverAddress`, keeping
  `_state`, `_builder`, and awaiter fields operating on the in-place struct so
  mutations do not spill through temporaries.
* Synthesized async state machines reuse `EnsureFieldBuilder` when materialising
  `_state`, `_builder`, and hoisted-awaiters so emission no longer defines the
  same field twice before IL generation resolves the handle.
  【F:src/Raven.CodeAnalysis/CodeGen/TypeGenerator.cs†L243-L399】
* Taking the address of the state machine now always pushes `ldarg.0`, ensuring
  builder calls receive a managed pointer to the struct rather than a by-value
  copy when the receiver is omitted.
  【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L905-L926】

### Backlog aligned with the priority list

1. **Codify desired semantics for `Task<T>` async methods** – document runtime
   expectations (exception flow, return propagation, synchronization context) and
   align Raven's specification with Roslyn’s lowering strategy.
2. **Extend binder and symbol shape** – track constructed `Task<T>` return types
   on async methods and lambdas, including default inference and diagnostic
   coverage for mismatches.
3. **Refine async lowering** – share receiver loads across `_state` and hoisted
   awaiters, emit constructed generic builders, and honour `RequiresReceiverAddress`
   for every builder call.
4. **Adjust code generation** – produce correctly constructed generic builder
   fields and metadata handles, and reuse cached `MethodBuilder` instances for
   substituted methods so emission no longer reflects over uncreated types.
5. **Regression and conformance testing** – flip existing failing tests to assert
   successful execution, add IL baselines covering builder construction and
   awaited value flow, and promote runtime smoke tests that await `Task<T>` entry
   points.
6. **Tooling and documentation** – wire IL verification into the CLI/CI workflow,
   capture generator prerequisites, and document async debugging guidance.
7. **Harden nested async state machines** – register lambda field builders before
   IL generation, describe hoisted awaiters in the closure factory, and extend
   regression coverage accordingly.

## Open items

* Fix async lambda emission so hoisted awaiters register their field builders
  before IL generation; this unblocks
  `AsyncLambda_EmitsStateMachineMetadata` and exercises the nested-state-machine
  coverage described in Priority 2. 【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L17-L47】【F:src/Raven.CodeAnalysis/CodeGen/TypeGenerator.cs†L560-L623】【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L667-L711】
* Revisit await scheduling heuristics to eliminate the redundant receiver loads
  that still show up in IL when lowering complex control-flow (Priority 3).
* Restore runtime execution coverage by fixing the minimal `await Task.CompletedTask`
  program and the `samples/async-await.rav` regression so smoke tests can assert
  the generated state machines reach completion.
* Integrate the new `ravenc --ilverify` switch (or `peverify`) into CI once the
  verification tooling is stable.
