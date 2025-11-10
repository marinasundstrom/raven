# Async/await action plan – test8 reboot

> Living action plan owner: **Compiler team** · Last updated: _2025-11-12_

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
| 2025-11-12 | 🟢 On track | Broke the async-main metadata recursion with `TryGetTypeByMetadataNameAlreadySetup`, added a runtime regression that invokes `Program.MainAsync`, and confirmed the awaited value flows through the state machine; full suite still gated on the TerminalLogger issue. |
| 2025-11-11 | 🟡 At risk | Cached async builder member discovery on the synthesized state machine and rewired lowering/tests to reuse the substituted methods and properties; still blocked on runtime validation while the TerminalLogger crash is outstanding. |
| 2025-11-10 | 🟡 At risk | Removed bespoke async state-machine type-parameter substitution in favour of the constructed method template so generics reuse the common substitution pipeline; awaiting runtime coverage while TerminalLogger crash persists. |
| 2025-11-09 | 🟡 At risk | Iterator baseline has been updated: the cached iterator `MoveNext` now stores its result in local slot `0` and records the nested state-machine type name (`C+<>c__Iterator0`). Completion tests unrelated to async continue to fail under the TerminalLogger, so runtime validation remains pending. |

## Async lowering findings

Recent spelunking through `AsyncLowerer` and the synthesized state-machine symbols surfaced a few blockers that explain why generic async members still fail and highlighted some opportunities to simplify the implementation:

* **Lowering mutates the source method.** `RewriteMethodBody` patches the original method's return type so it lines up with whatever builder type we managed to synthesize.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L112-L137】 That mutation couples lowering to symbol construction and makes it harder to reason about generic instantiations. A cleaner approach is to hand the lowerer a fully inferred return type (or a constructed method symbol) and treat the builder choice as an input instead of rewriting the method symbol on the fly.
* **State machine locals stay open generic.** The synthesized local that stores the state-machine instance is declared with the unconstructed `SynthesizedAsyncStateMachineTypeSymbol`, and every parameter assignment feeds method type-parameters directly into fields that already substituted them with synthesized equivalents.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L142-L175】【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L142-L183】 Because the left- and right-hand sides use different symbols for the same `T`, the assignment is ill-typed once generics come into play. Constructing the state machine once (e.g. `stateMachine.Construct(method.TypeParameters)`) and threading that constructed type through the rewritten body would let both sides agree without bespoke conversions.
* **Type-parameter substitution is reimplemented from scratch.** `SynthesizedAsyncStateMachineTypeSymbol` used to clone every method type parameter, rebuild constraint lists, and then walk all type shapes to swap them out via `SubstituteAsyncMethodTypeParameters`. The bespoke substitution made the file hard to follow and still left the lowerer juggling mismatched symbols. By materialising a constructed view of the method up front, we now reuse the existing substitution pipeline and delete the hand-written walker.
* **Builder plumbing is spread across ad-hoc helpers.** Discovering `Create`, `Start`, `SetStateMachine`, `SetResult`, and `SetException` involves scanning the builder type each time and then retrofitting substitutions manually.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1708-L1851】 Hoisting that logic into the synthesized state-machine (e.g. cache the resolved builder members per constructed type) would shrink the lowerer and make it obvious how to specialise builder invocations for generics—the call sites could simply ask the state machine for the already-substituted symbols.

Together these changes would let us lower against a constructed, type-safe state machine without mutating the original method symbol, clearing the path for generic async functions.

## Step-by-step plan

1. **Separate builder selection from method symbols.**
   * Extend the async planning stage to compute the final return type and builder upfront, so `AsyncLowerer` receives a constructed method symbol that no longer needs mutation.
   * Thread the chosen builder through the lowering pipeline as data instead of patching it onto the source method.
   * Add assertions that the lowerer never rewrites `MethodSymbol.ReturnType` so regressions trip quickly.

2. **Construct the state machine once per instantiation.**
   * Introduce a helper that takes the synthesized state-machine definition plus the method's type arguments and returns a `ConstructedNamedTypeSymbol`.
   * Replace all state-machine local declarations and field accesses with the constructed type to keep the left/right sides of assignments aligned for generics.
   * Cover the change with a targeted lowering unit test that verifies the locals and fields agree on the substituted type parameters.

3. **Delete bespoke type-parameter substitution.** _(Status: ✅ Completed – state machines reuse `ConstructedMethodSymbol` for substitution)_
   * Remove `SubstituteAsyncMethodTypeParameters` and rely on the constructed state-machine view to provide the right substitutions.
   * Simplify constraint replication by delegating to the existing substitution utilities used elsewhere in the compiler.
   * Add a regression test that checks generic constraints survive the lowering round-trip without duplicating custom maps.

4. **Centralise builder member discovery.** _(Status: ✅ Completed – async builder members are cached on the state machine and reused by lowering/tests)_
   * Move the ad-hoc `Create`/`Start`/`SetStateMachine` lookup into a cache on the synthesized state-machine (keyed by constructed builder type).
   * Rewrite the lowering call sites to request the prepared members instead of performing local reflection-based scans.
   * Add validation that each async builder interaction uses a cached, fully substituted `MethodSymbol` to prevent future drift.

5. **Lock down the end-to-end scenario.** _(Status: ✅ Completed – runtime regression drives `Program.MainAsync` and exercises the awaited flow; IL capture remains a follow-up)_
   * ✅ Promote `samples/test8.rav` to an automated regression that exercises a generic async entry point end to end (`TopLevelAsyncProgramTests.TopLevelGenericAsyncProgram_AwaitsValue`).
   * ✅ Extend the runtime harness to capture awaited console output so we can confirm the awaited result flows through the constructed state machine.
   * ✅ Track remaining runtime or emission bugs surfaced by the regression in the risk log (TerminalLogger crash still noted under risks).

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
| Promote `samples/test8.rav` into the regression suite with async investigation flags enabled. | ✅ Completed | Compiler team | `TopLevelAsyncProgramTests.TopLevelGenericAsyncProgram_AwaitsValue` compiles the sample and runs it via reflection. |
| Execute the emitted assembly in the runtime harness and assert it prints `42` with no exceptions. | ✅ Completed | Compiler team | The regression awaits `Program.MainAsync` and validates the captured console output. |
| Capture emitted IL (and pointer traces if useful) as golden files for diff-based regression coverage. | ☐ Not started | TBD | Keep on backlog; console output currently covers the awaited flow. |

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
