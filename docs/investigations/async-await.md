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

