# Async/await – test8 reboot

## Objective

Deliver a stable async/await experience for generic entry points by making
`samples/test8.rav` compile, run, and return its awaited value without
Reflection.Emit crashes.

```swift
import System.Console.*
import System.Threading.Tasks.*

async func Test<T>(value: T) -> Task<T> {
    await Task.Delay(10)
    return value
}

let x = await Test(42)

WriteLine(x)
```

## Key insights

* **`MoveNext` still leaks the state-machine generic.** Disassembling the
  latest `test.dll` shows the awaited call invoking
  `Task<int>.GetAwaiter()` as `TaskAwaiter<!0>` even though the async method
  expects the original `!!0`. The verifier needs to see the method generic in
  these runtime-visible MethodSpecs, so our lowering pipeline must retain the
  original type parameter for such calls.【F:docs/investigations/async-await.md†L109-L117】
* **Builder substitutions reach emission correctly.** Both
  `AsyncTaskMethodBuilder::Start` and `AwaitUnsafeOnCompleted` now instantiate
  the constructed state machine, proving that the builder metadata is flowing
  through lowering and code generation as intended even though runtime
  validation still fails.【F:docs/investigations/async-await.md†L96-L102】
* **Bound-node substitutions happen up front.** Await lowering substitutes the
  async method’s type parameters with the state-machine generics before hoisting
  locals and builder members, preventing further `!!T` leaks inside the struct
  except for APIs that must preserve the method generic, such as
  `Task<T>.GetAwaiter`.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1207-L1398】

## Task list

* [ ] Preserve the async method’s `!!0` when lowering calls that must remain on
  the original generic instantiation (e.g. `Task<T>.GetAwaiter`) so the emitted
  IL no longer substitutes the state-machine `!0` in runtime-visible MethodSpecs.
* [ ] Diff Raven’s and Roslyn’s metadata for `Program.Test<T>` (TypeSpec,
  MethodSpec, and generic parameter tables) to spot the remaining verifier
  discrepancy once the awaited call preserves `!!0`.
* [ ] Re-run the `samples/test8.rav` CLI pipeline and `ilverify` after the
  substitution fix to confirm the assembly loads and add regression coverage to
  prevent future substitutions from leaking into `MoveNext`.

## Findings – generic state machine encoding is invalid

* **Runtime still rejects the sample.** Re-running the CLI against
  `samples/test8.rav` yields the same `BadImageFormatException` before any
  user code executes, and the stack trace points at the open generic entry
  point `Program.Test<T>` when the runtime spins up the async state machine.【bef937†L1-L7】【b27cb9†L1-L8】
* **Method body now matches Roslyn's locals.** The emitted `Program.Test<T>`
  declares only the state-machine local and stores parameters, state, and the
  builder directly into the struct fields without spilling temporaries, matching
  the Roslyn baseline even though the runtime still rejects the image.【afa998†L1-L27】【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L1710-L1799】
* **ILVerify still reports stack mismatches for the builder calls.** The
  restored `ilverify` tool now makes it past the duplicated nested-type name,
  but it continues to flag the `Start` invocation in `Program.MainAsync`
  because the verifier expects a managed `ref` while the emitted IL pushes the
  address of the state machine and builder structs; the tool eventually bails
  out with the same `IndexOutOfRangeException` when resolving the generic
  MethodSpec.【958854†L1-L27】
* **Nested async state machine names no longer duplicate the containing type.**
  `TypeGenerator` now hands `DefineNestedType` the simple metadata name (plus
  arity) for synthesized async structs, so the IL encodes
  `Program/'<>c__AsyncStateMachine0`1'` instead of `Program/'Program+<>c__...`.
  The runtime still rejects the image, but the duplicated `Program+Program+`
  prefix has been eliminated.【F:src/Raven.CodeAnalysis/CodeGen/TypeGenerator.cs†L160-L181】【b15d35†L13-L38】
* **Async builder mapping is now symmetric.** The synthesized state machine
  remaps builder members for the async method through dedicated
  `MappedMethodSymbol`/`MappedPropertySymbol` wrappers so the method body sees
  `AsyncTaskMethodBuilder<!!T>` helpers while `MoveNext` keeps the substituted
  `AsyncTaskMethodBuilder<!0>` view; the runtime still fails with the same
  `BadImageFormatException`/`ilverify` crash, so another metadata edge case must
  be addressed.【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L343-L375】【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L1000-L1340】【4d3a13†L1-L8】【21c832†L1-L22】
* **Constructing builder calls now substitutes type arguments.** `MappedMethodSymbol.Construct`
  rewrites each supplied type argument through the async↔state-machine map before
  delegating to the underlying method, and `CreateBuilderStartStatement` applies the
  async-method substitution before instantiating `Start<TStateMachine>`. The emitted IL
  still prints `Program+<>c__AsyncStateMachine0`1'<!0>` for the builder MethodSpec, so the
  fix was insufficient and the runtime continues to reject the image.【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L1178-L1201】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1719-L1735】
* **Codegen now unwraps substituted builder methods.** Emission recognises
  `AsyncMethodSubstitutedMethodSymbol` wrappers and routes them back to their
  underlying builders, while async method lowering keeps the `Create` and
  `Start` invocations on the original `AsyncTaskMethodBuilder<!!T>` so the
  state machine construction proceeds without rewriting the method generic to
  the synthesized `!0` placeholder.【F:src/Raven.CodeAnalysis/MethodSymbolExtensionsForCodeGen.cs†L25-L66】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1688-L1732】
* **Builder invocations now materialise the constructed state machine.**
  Disassembling `test.dll` shows the emitted `Start` and
  `AwaitUnsafeOnCompleted` method specs instantiating the generic builder with
  `valuetype Program/'Program+<>c__AsyncStateMachine0`1'<!T>` rather than the
  async method's `!!T`, confirming that the lowerer feeds the correct
  substitution into emission even though the runtime still fails to load the
  image.【F:docs/investigations/async-await.md†L96-L102】【f755b4†L1-L8】

### IL snapshot – `MoveNext` still substitutes `!0`

The latest disassembly highlights how the awaited call now mixes method and
state-machine generics:

```il
IL_0035: ldc.i4.s 42
IL_0037: call class [System.Private.CoreLib]System.Threading.Tasks.Task`1<!!0> Program::Test<int32>(!!0)
IL_003c: call instance valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.TaskAwaiter`1<!0> class [System.Private.CoreLib]System.Threading.Tasks.Task`1<int32>::GetAwaiter()
```

`Task<T>.GetAwaiter` expects the async method's `!!0`, but the emitted IL feeds
the constructed state-machine `!0`, which explains the verifier failure that
still surfaces as `BadImageFormatException` when running the sample.【F:docs/investigations/async-await.md†L109-L117】
* **C# baseline diverges only in state-machine representation.** Performing
  the same inspection on a Roslyn-built equivalent yields the same builder
  instantiations except the reference compiler emits a class-based state
  machine (`class Program/'<Test>d__0`1'<!!T>`) where Raven still lowers to a
  struct. The verifier break therefore appears unrelated to the generic
  substitution fixes and likely stems from another metadata difference between
  the two images.【9d2479†L1-L8】
* **State machine now substitutes before builder lookup.** `GetConstructedStateMachine`
  always materialises a constructed view, and the builder members captured for
  lowering now come directly from the synthesized struct so `MoveNext` emits
  `AsyncTaskMethodBuilder<!0>` everywhere while the async method uses
  `AsyncTaskMethodBuilder<!!T>`. The runtime still rejects the image, which
  means another metadata edge-case remains. 【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L132-L190】
* **MoveNext lowering now centralises builder access.** `CreateMoveNextBody`
  instantiates a context that hands the same builder snapshot to await
  rewriting, state dispatch, completion, and exception handling so the state
  machine never re-fetches substituted members mid-lowering.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L85-L139】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L204-L312】
* **Await lowering now substitutes hoisted types up front.** The rewriter maps
  async method type parameters to the state-machine generics before allocating
  awaiter locals, storing hoisted fields, or instantiating builder helpers, so
  the lowered bound nodes always refer to `!0` members instead of leaking
  `!!T` handles into the state machine.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1207-L1398】 The latest
  `MoveNext` disassembly confirms that we still substitute the awaited
  `Task<T>.GetAwaiter` receiver with the struct's `!0`, and the runtime expects
  the method's `!!0`, so the lowering pipeline needs a targeted escape hatch
  for APIs that must retain the original method generics.【F:docs/investigations/async-await.md†L109-L117】
* **Root block disposal now happens in MoveNext assembly.** The async rewriter
  leaves the outer block untouched, and `CreateMoveNextBody` appends
  `CreateDisposeStatements` after the rewritten body so the hoisted `using`
  guard precedes completion logic, matching the regression expectations for
  long-running disposables.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L90-L112】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L607-L680】
* **Awaited type now maps before builder construction.** When the async method returns
  `Task<T>`, the awaited `T` is substituted with the state machine's `!0` before
  we instantiate `AsyncTaskMethodBuilder<T>`, preventing Reflection.Emit from caching
  the method-generic handle that previously surfaced as `AsyncTaskMethodBuilder<!!0>`.【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L521-L563】
* **Method body builder lookups reuse async generics.** `GetConstructedMembers` now
  returns a method-context view of the builder helpers so lowering wires up
  `AsyncTaskMethodBuilder<!!T>` for the local state-machine initialization; the
  new regression covers the substitution even though CLI execution still fails.【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L132-L153】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncLowererTests.cs†L1001-L1055】
* **State machine now caches builder views.** `SynthesizedAsyncStateMachineTypeSymbol`
  owns an `AsyncBuilderMemberMap` that memoizes both the struct view and any
  constructed async-method view keyed by the `_builder` field, so every caller
  now sees the same `Create`/`AwaitUnsafeOnCompleted`/`Task` handles without
  repeating substitution logic.【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L21-L148】【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L865-L914】
* **Builder field construction pinned to struct generics.** The builder type is now
  substituted before `_builder` is synthesized, so every subsequent lookup observes
  `AsyncTaskMethodBuilder<!0>` and the `SetException`/`SetResult` MethodSpecs shed their
  stray `!!0` references.【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L261-L286】
* **Builder calls now encode state-machine generics.** Updating the
  Reflection.Emit lookup to reuse the state machine's generic parameter builders
  for the original async method type parameters means the `AwaitUnsafeOnCompleted`
  and `SetResult` sites now materialise as `AsyncTaskMethodBuilder<!0>` instead
  of the verifier-breaking `!!0`; the new IL regression locks the behaviour
  down.【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L108-L146】【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L1495-L1520】
* **Generic parameter cache is now layered.** Reusing the async method after
  the state machine is emitted no longer snaps builder calls back to `!!0`
  because the runtime type map keeps a stack per type parameter and always
  consults the most recent mapping.【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L19-L137】
* **Type-parameter mapping is now explicit.** The synthesized state machine records the async method ↔ state-machine parameter pairs and surfaces helpers so emission and tests can reuse the mapping without bespoke substitutions.【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L79-L118】【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L124-L135】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncLowererTests.cs†L129-L165】

* **Lowerer tests currently freeze the wrong substitution.** `Rewrite_AsyncGenericMethod_UsesAsyncMethodTypeParametersForBuilder`
  insists that `_builder` initialization and `Create()` return values keep the async method's
  `T`, so the suite enforces the broken `AsyncTaskMethodBuilder<!!T>` encoding instead of
  validating a constructed state-machine view. We'll need to pivot these assertions once
  the substitution bug is fixed.【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncLowererTests.cs†L1014-L1068】
* **AsyncLowerer regression still red.** Filtering the unit tests to `AsyncLowerer`
  fails fast in `Rewrite_AsyncMethodWithUsingAcrossAwait_DisposesHoistedLocal` before
  the logger crashes, so we currently lack a green baseline to confirm fixes through the suite.【fa1f06†L1-L15】

## Async lowering findings

Recent spelunking through `AsyncLowerer` and the synthesized state-machine symbols surfaced a few blockers that explain why generic async members still fail and highlighted some opportunities to simplify the implementation:

* **Lowering mutates the source method.** `RewriteMethodBody` patches the original method's return type so it lines up with whatever builder type we managed to synthesize.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L112-L137】 That mutation couples lowering to symbol construction and makes it harder to reason about generic instantiations. A cleaner approach is to hand the lowerer a fully inferred return type (or a constructed method symbol) and treat the builder choice as an input instead of rewriting the method symbol on the fly.
* **State machine locals stay open generic.** The synthesized local that stores the state-machine instance is declared with the unconstructed `SynthesizedAsyncStateMachineTypeSymbol`, and every parameter assignment feeds method type-parameters directly into fields that already substituted them with synthesized equivalents.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L142-L175】【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L142-L183】 Because the left- and right-hand sides use different symbols for the same `T`, the assignment is ill-typed once generics come into play. Constructing the state machine once (e.g. `stateMachine.Construct(method.TypeParameters)`) and threading that constructed type through the rewritten body would let both sides agree without bespoke conversions.
* **Type-parameter substitution is reimplemented from scratch.** `SynthesizedAsyncStateMachineTypeSymbol` clones every method type parameter, rebuilds constraint lists, and then walks all type shapes to swap them out via `SubstituteAsyncMethodTypeParameters`.【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L191-L342】 The bespoke substitution makes the file hard to follow and still leaves the lowerer juggling mismatched symbols. If we materialise a constructed view of the state machine up front, most of those maps fall away—we can rely on existing `ConstructedNamedTypeSymbol` plumbing instead of threading custom dictionaries through every helper.
* **Builder plumbing is spread across ad-hoc helpers.** Discovering `Create`, `Start`, `SetStateMachine`, `SetResult`, and `SetException` involves scanning the builder type each time and then retrofitting substitutions manually.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1708-L1851】 Hoisting that logic into the synthesized state-machine (e.g. cache the resolved builder members per constructed type) would shrink the lowerer and make it obvious how to specialise builder invocations for generics—the call sites could simply ask the state machine for the already-substituted symbols.

**New issues – async builder substitution remains fragile**

* **Lowering still targets the open state machine.** Even with the builder map, `RewriteMethodBody` and `GetBuilderMembers` operate on the unconstructed struct symbol, so parameter and field assignments keep pairing method generics with the synthesized equivalents manually; threading a constructed state-machine symbol through lowering would let both sides agree without ad-hoc substitution.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L112-L188】【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L112-L189】 The same
  `MoveNext` IL diff shows how swapping in the struct's `!0` for the awaited
  `Task<T>` receiver trips the verifier, so preserving the async method's `!!`
  parameters for runtime-visible MethodSpecs must be part of the constructed
  state-machine plan.【F:docs/investigations/async-await.md†L109-L117】
* **Lowering fetches builder metadata piecemeal.** Each helper pulls the builder members independently, increasing the surface area for mismatched substitutions and forcing repeated null-check boilerplate.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L87-L188】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L456-L742】
* **Await rewriter re-resolves the builder.** `AwaitLoweringRewriter` used to repeat the same lookup that `CreateMoveNextBody` performed, forcing every substitution fix to be applied twice; the first clean-up passes the captured snapshot through so future refactors have a single touch point.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L89-L109】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L552-L742】
* **Verifier break likely sits outside builder substitution.** Since the
  emitted builder method specs now mirror the Roslyn baseline, the remaining
  `BadImageFormatException` probably originates from another metadata
  difference (e.g. struct-vs-class state machine layout or ancillary
  attributes). Narrowing the diff against the C# assembly should be the next
  diagnostic step before touching the lowerer again.【f755b4†L1-L8】【9d2479†L1-L8】

Together these changes would let us lower against a constructed, type-safe state machine without mutating the original method symbol, clearing the path for generic async functions.

### Refactoring opportunities

* **Teach other lowerers to share the root-block hook.** The new
  `RewriteBlockStatement` helper in the async rewriter separates inner block
  cleanup from the outer frame. Threading similar hooks through iterator and
  generator lowerers would let them plug custom completion logic without
  duplicating hoist/dispose loops.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L607-L680】
* **Move dispose scheduling behind a reusable service.** Now that hoisted
  disposal happens during MoveNext assembly, extracting the guard construction
  into a shared utility would let future refactors (e.g., pattern-based
  asynchronous `using`) reuse the same cleanup staging without reaching into
  the rewriter internals.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L90-L112】

## Guiding principles

1. Keep constructed async builders on the Reflection.Emit surface; reuse the
   `MethodBuilder` handles captured during emission instead of relying on
   `TypeBuilder.GetMethods()`.
2. Prevent constructed async state machines from materializing before their
   owning `TypeBuilder` completes.
3. Exercise the generic async entry point end to end and lock the behaviour down
   with regression coverage.

## Workstreams & tasks

### WS1 – Cache async state-machine builders for generic substitutions

| Task | Status | Owner | Notes |
| --- | --- | --- | --- |
| Extend `CodeGenerator.AddMemberBuilder`/`TryGetMemberBuilder` to record async state-machine builders keyed by definition + substitution. | ✅ Completed | Compiler team | Cache now stores constructors, `MoveNext`, and hoisted fields keyed by type arguments. |
| Update `ConstructedMethodSymbol`/`SubstitutedMethodSymbol` to consult the cache before calling `GetMethods()`. | ✅ Completed | Compiler team | Constructed lookups reuse existing `MethodBuilder` handles before falling back to reflection. |
| Verify emitter reuse via diagnostic or test that `TypeBuilder.GetMethods` is no longer invoked for constructed async generics. | ✅ Completed | Compiler team | `GenericAsyncStateMachine_UsesCachedMoveNextBuilderForTypeArguments` locks caching behaviour. |

### WS2 – Guard builder lookup against premature type creation

| Task | Status | Owner | Notes |
| --- | --- | --- | --- |
| Audit async lowering/emission call sites that invoke `GetMethodInfo(invokeConstructed, substitution)` while the state machine is under construction. | ✅ Completed | Compiler team | Builder lookups now consult cache-aware helpers across async emission surfaces. |
| Route call sites through the cache or defer lookup until `TypeGenerator.GetCompletedType` finalizes the type. | ✅ Completed | Compiler team | Source and substituted symbols reuse cached builders instead of forcing premature `TypeBuilder` materialization. |
| Add unit tests around `ConstructedMethodSymbol.GetMethodInfo` to prove the fallback path is unused when a cached builder exists. | ✅ Completed | Compiler team | Generic state-machine test asserts cached reuse before invoking reflection fallbacks. |

### WS3 – Runtime regression for `samples/test8.rav`

| Task | Status | Owner | Notes |
| --- | --- | --- | --- |
| Promote `samples/test8.rav` into the regression suite with async investigation flags enabled. | ☐ Not started | TBD | Ensure automation compiles & executes the script. |
| Execute the emitted assembly in the runtime harness and assert it prints `42` with no exceptions. | 🚧 In progress | Compiler team | CLI still throws `BadImageFormatException`; captured IL now confirms builder instantiations line up with the constructed state machine, so the remaining verifier break must stem from another metadata divergence.【f755b4†L1-L8】 |
| Diff Raven's emitted metadata against a Roslyn baseline to isolate the remaining verifier break. | ☐ Not started | TBD | Compare state-machine layout, attributes, and method specs after confirming builder substitutions match the constructed type arguments.【9d2479†L1-L8】 |
| Capture emitted IL (and pointer traces if useful) as golden files for diff-based regression coverage. | ☐ Not started | TBD | Store artifacts alongside other async regression assets. |

### WS4 – Restore iterator IL stability after cache changes

| Task | Status | Owner | Notes |
| --- | --- | --- | --- |
| Reproduce `IteratorILGenerationTests.MoveNext_DoesNotEmitStackClearingPops` locally and capture the IL diff. | ✅ Completed | Compiler team | Recorded the post-cache IL (local slot `0`, nested state-machine name) to establish the new baseline. |
| Identify why async builder caching shifts iterator local slots and patch emission or expectations accordingly. | ✅ Completed | Compiler team | Method-builder reuse now preserves only the iterator result local; updated the regression expectation to reflect slot `0` and nested type-qualified field names. |
| Extend iterator regression coverage to guard the fixed behaviour. | ☐ Not started | TBD | Add explicit assertions for local-slot numbering in iterator MoveNext. |

### WS5 – Simplify async builder plumbing

| Task | Status | Owner | Notes |
| --- | --- | --- | --- |
| Create a single source of truth for async builder members so constructed and definition views always agree. | ✅ Completed | Compiler team | `AsyncBuilderMemberMap` caches the state-machine snapshot and remaps `_builder`-keyed views for async methods, removing the duplicated discovery paths.【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L21-L148】【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L865-L914】 |
| Remove `AsyncMethodStateMachineFieldSymbol` by performing substitutions when constructing the state machine. | ✅ Completed | Compiler team | Constructed async methods now read the state machine's hoisted fields directly, so the bespoke wrapper symbol is gone.【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L304-L384】 |
| Introduce a lowering context that carries builder metadata so the dispatch, completion, and await paths share one snapshot. | ✅ Completed | Compiler team | `CreateMoveNextBody` now instantiates a shared context so dispatch, completion, and exception paths reuse the same substituted builder snapshot.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L85-L139】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L204-L312】 |
| Stop resolving builder members inside `AwaitLoweringRewriter`; pipe the snapshot captured in `CreateMoveNextBody` instead. | ✅ Completed | Compiler team | `CreateMoveNextBody` now hands the cached builder members to the rewriter so both layers share one substitution view.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L89-L109】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L552-L611】 |
| Rehome hoisted-disposal guards so the rewriter no longer duplicates cleanup and `MoveNext` anchors the guard before completion. | ✅ Completed | Compiler team | The rewriter's root block skips disposal injection and `CreateMoveNextBody` appends the hoisted field cleanup immediately before builder completion, matching the regression layout. 【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L90-L112】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L607-L680】 |
| Substitute async method type parameters while hoisting awaiters, locals, and builder calls so MoveNext bound nodes always reference the state-machine generics. | ✅ Completed | Compiler team | Await lowering now maps method generics through the state-machine substitution helper before storing fields or constructing `AwaitUnsafeOnCompleted`, preventing `!!T` handles from leaking into MoveNext.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1207-L1398】【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L536-L614】 |

## Risks & mitigations

* **Reflection.Emit cache coherence.** Cached builders must stay valid across
  substitutions; mitigate by keying entries on both the definition and the
  substitution map.
* **Test flakiness.** Runtime harness must handle async delays; mitigate by
  using deterministic delays and capturing golden outputs.
* **Terminal logger crash.** `dotnet test` fails hard when emitting long error
  messages; mitigate by downgrading to a different logger or trimming failure
  output until MSBuild logger bug is fixed.

## Definition of done

* `samples/test8.rav` builds and runs via the CLI without triggering
  `TypeBuilderImpl.ThrowIfNotCreated` or losing the awaited integer.
* Regression coverage enforces cached-builder lookup and the end-to-end runtime
  behaviour so the crash cannot silently return.
