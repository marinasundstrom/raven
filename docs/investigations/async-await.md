# Async/await action plan – test8 reboot

> Living action plan owner: **Compiler team** · Last updated: _2025-11-16_

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
  point `Program.Test<T>` when the runtime spins up the async state machine.【025e9d†L1-L7】
* **Awaited type now maps before builder construction.** When the async method returns
  `Task<T>`, the awaited `T` is substituted with the state machine's `!0` before
  we instantiate `AsyncTaskMethodBuilder<T>`, preventing Reflection.Emit from caching
  the method-generic handle that previously surfaced as `AsyncTaskMethodBuilder<!!0>`.【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L521-L563】
* **Method body builder lookups reuse async generics.** `GetConstructedMembers` now
  returns a method-context view of the builder helpers so lowering wires up
  `AsyncTaskMethodBuilder<!!T>` for the local state-machine initialization; the
  new regression covers the substitution even though CLI execution still fails.【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L132-L153】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncLowererTests.cs†L1001-L1055】
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
  consults the most recent mapping.【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L24-L43】【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L108-L146】
* **Type-parameter mapping is now explicit.** The synthesized state machine records the async method ↔ state-machine parameter pairs and surfaces helpers so emission and tests can reuse the mapping without bespoke substitutions.【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L79-L118】【F:src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs†L124-L135】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncLowererTests.cs†L129-L165】

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

Together these changes would let us lower against a constructed, type-safe state machine without mutating the original method symbol, clearing the path for generic async functions.

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
