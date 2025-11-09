# Async/await action plan – test8 reboot

> Living action plan owner: **Compiler team** · Last updated: _2025-11-09_

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
| 2025-11-09 | 🟡 At risk | Iterator baseline has been updated: the cached iterator `MoveNext` now stores its result in local slot `0` and records the nested state-machine type name (`C+<>c__Iterator0`). Completion tests unrelated to async continue to fail under the TerminalLogger, so runtime validation remains pending. |

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
