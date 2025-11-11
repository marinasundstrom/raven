# Async/await action plan – test8 reboot

> Living action plan owner: **Compiler team** · Last updated: _2025-11-22_

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

## Current status snapshot

| Date | Status | Notes |
| --- | --- | --- |
| 2025-11-24 | 🟡 At risk | Await lowering now substitutes async method type parameters before hoisting awaiters, locals, and builder invocations so the generated bound nodes reference the state-machine generics instead of the async method's, eliminating the lingering `!!T`/`!0` mismatch ahead of runtime validation.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1207-L1398】 |
| 2025-11-23 | 🟡 At risk | Hoisted-disposal guards now live at the end of `MoveNext` by skipping async rewriter cleanup for the root block and appending the state machine's `HoistedLocalsToDispose` during MoveNext assembly, restoring the expected guard order while keeping the builder substitutions intact.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L90-L112】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L607-L680】 |
| 2025-11-22 | 🟡 At risk | The synthesized async state machine now owns an `AsyncBuilderMemberMap` so both MoveNext lowering and constructed method views query the same cached builder metadata keyed by the `_builder` field while we continue debugging the runtime verifier break for `Program.Test<T>`.【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L21-L148】【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L865-L914】 |
| 2025-11-21 | 🟡 At risk | MoveNext lowering now threads a shared builder context through dispatch, completion, and catch handling so await lowering, state transitions, and exception paths all reuse the same substituted snapshot while we still chase the runtime verifier break for `Program.Test<T>`.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L85-L139】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L204-L312】 |
| 2025-11-20 | 🟡 At risk | Constructed async methods now consume the state machine's hoisted parameter and builder fields directly, eliminating the wrapper substitution while we still chase the runtime verifier break for `Program.Test<T>`.【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L304-L384】 |
| 2025-11-19 | 🟡 At risk | The async method view now remaps the state-machine builder snapshot instead of rediscovering members, so both layers share the same `Create`/`AwaitUnsafeOnCompleted` definitions while we continue tracking the verifier break for `Program.Test<T>`.【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L304-L384】 |
| 2025-11-18 | 🟡 At risk | `AwaitLoweringRewriter` now reuses the builder snapshot captured in `CreateMoveNextBody`, reducing duplication while we continue tracking the verifier break for `Program.Test<T>`.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L89-L109】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L552-L611】 |
| 2025-11-17 | 🔴 Blocked | `ravc` now substitutes the async builder using the state-machine generic, so `MoveNext` and the method body agree on `AsyncTaskMethodBuilder<!0>` vs `AsyncTaskMethodBuilder<!!T>`; however the runtime still throws `BadImageFormatException` when materializing `Program.Test<T>` so the verifier is rejecting another TypeSpec in the generated image.【bef937†L1-L7】【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L132-L180】
| 2025-11-16 | 🟡 At risk | Constructed async state-machine members now expose builder lookups remapped to the async method's generics, and a new lowering regression proves the `Create` site instantiates `AsyncTaskMethodBuilder<!!T>` for the method body; runtime validation is still pending.【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L132-L153】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncLowererTests.cs†L1001-L1055】 |
| 2025-11-15 | 🟡 At risk | Substituting the awaited `Task<T>` result before instantiating the builder now hands `AsyncTaskMethodBuilder<!0>` the struct parameter instead of the method generic; still need CLI/`ilverify` confirmation that the runtime loads the image.【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L521-L563】 |
| 2025-11-14 | 🟡 At risk | Substituting the builder type before field synthesis keeps `AsyncTaskMethodBuilder<!0>` anchored to the struct parameter so `SetException`/`SetResult` no longer encode `!!0`; need a fresh CLI+`ilverify` pass to confirm the runtime accepts the image.【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L261-L286】 |
| 2025-11-13 | 🟡 At risk | Exposed explicit async↔state-machine type-parameter mappings and taught the emitter to reuse them; new semantic coverage guards the round-trip ahead of runtime validation.【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L79-L118】【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L108-L140】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncLowererTests.cs†L110-L165】 |
| 2025-11-12 | 🟡 At risk | Layered the generic-parameter cache so async methods retain the state machine's `!0` builder even after the original method re-registers its `T`; awaiting runtime validation and a fresh CLI run before closing the loop.【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L24-L43】【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L108-L146】 |
| 2025-11-11 | 🟡 At risk | Patched the emitter to map the async method's type parameters onto the synthesized state machine's generic parameter builders, so builder calls now instantiate over `!0`; a new IL regression proves the `MoveNext` builder invocations all see type-level generics, but the runtime fix still needs end-to-end validation.【025e9d†L1-L7】【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L115-L139】【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L1495-L1520】 |
| 2025-11-10 | 🔴 Blocked | CLI run still throws `BadImageFormatException` while JIT-compiling `Program.Test<T>` because the emitted state-machine `TypeSpec` injects the method's `T` via `ELEMENT_TYPE_VAR` rather than `ELEMENT_TYPE_MVAR`, so the verifier can't materialise the constructed type.【155a99†L1-L8】【d19e55†L6-L18】【eb2897†L1-L20】 |
| 2025-11-09 | 🟡 At risk | Iterator baseline has been updated: the cached iterator `MoveNext` now stores its result in local slot `0` and records the nested state-machine type name (`C+<>c__Iterator0`). Completion tests unrelated to async continue to fail under the TerminalLogger, so runtime validation remains pending. |

## Latest findings – generic state machine encoding is invalid

* **Runtime still rejects the sample.** Re-running the CLI against
  `samples/test8.rav` yields the same `BadImageFormatException` before any
  user code executes, and the stack trace points at the open generic entry
  point `Program.Test<T>` when the runtime spins up the async state machine.【bef937†L1-L7】
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
  `!!T` handles into the state machine.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1207-L1398】
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

### Next steps

* Re-run the CLI sample (and `ilverify`) to confirm the `AsyncTaskMethodBuilder<!0>`
  substitutions unblock the runtime and eliminate the `BadImageFormatException`.
* After correcting the remaining substitutions, re-run both the CLI sample and
  `ilverify` to confirm the assembly loads and the verifier no longer crashes;
  promote a regression to guard the fixed encoding going forward.

## Async lowering findings

Recent spelunking through `AsyncLowerer` and the synthesized state-machine symbols surfaced a few blockers that explain why generic async members still fail and highlighted some opportunities to simplify the implementation:

* **Lowering mutates the source method.** `RewriteMethodBody` patches the original method's return type so it lines up with whatever builder type we managed to synthesize.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L112-L137】 That mutation couples lowering to symbol construction and makes it harder to reason about generic instantiations. A cleaner approach is to hand the lowerer a fully inferred return type (or a constructed method symbol) and treat the builder choice as an input instead of rewriting the method symbol on the fly.
* **State machine locals stay open generic.** The synthesized local that stores the state-machine instance is declared with the unconstructed `SynthesizedAsyncStateMachineTypeSymbol`, and every parameter assignment feeds method type-parameters directly into fields that already substituted them with synthesized equivalents.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L142-L175】【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L142-L183】 Because the left- and right-hand sides use different symbols for the same `T`, the assignment is ill-typed once generics come into play. Constructing the state machine once (e.g. `stateMachine.Construct(method.TypeParameters)`) and threading that constructed type through the rewritten body would let both sides agree without bespoke conversions.
* **Type-parameter substitution is reimplemented from scratch.** `SynthesizedAsyncStateMachineTypeSymbol` clones every method type parameter, rebuilds constraint lists, and then walks all type shapes to swap them out via `SubstituteAsyncMethodTypeParameters`.【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L191-L342】 The bespoke substitution makes the file hard to follow and still leaves the lowerer juggling mismatched symbols. If we materialise a constructed view of the state machine up front, most of those maps fall away—we can rely on existing `ConstructedNamedTypeSymbol` plumbing instead of threading custom dictionaries through every helper.
* **Builder plumbing is spread across ad-hoc helpers.** Discovering `Create`, `Start`, `SetStateMachine`, `SetResult`, and `SetException` involves scanning the builder type each time and then retrofitting substitutions manually.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1708-L1851】 Hoisting that logic into the synthesized state-machine (e.g. cache the resolved builder members per constructed type) would shrink the lowerer and make it obvious how to specialise builder invocations for generics—the call sites could simply ask the state machine for the already-substituted symbols.

**New issues – async builder substitution remains fragile**

* **Lowering still targets the open state machine.** Even with the builder map, `RewriteMethodBody` and `GetBuilderMembers` operate on the unconstructed struct symbol, so parameter and field assignments keep pairing method generics with the synthesized equivalents manually; threading a constructed state-machine symbol through lowering would let both sides agree without ad-hoc substitution.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L112-L188】【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L112-L189】
* **Lowering fetches builder metadata piecemeal.** Each helper pulls the builder members independently, increasing the surface area for mismatched substitutions and forcing repeated null-check boilerplate.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L87-L188】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L456-L742】
* **Await rewriter re-resolves the builder.** `AwaitLoweringRewriter` used to repeat the same lookup that `CreateMoveNextBody` performed, forcing every substitution fix to be applied twice; the first clean-up passes the captured snapshot through so future refactors have a single touch point.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L89-L109】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L552-L742】

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
| Execute the emitted assembly in the runtime harness and assert it prints `42` with no exceptions. | ☐ Not started | TBD | Capture logs for post-run validation. |
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
