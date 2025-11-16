# Async/await feature investigation

## Objective
Track the overall completeness of Raven's async/await implementation. The original work validated the `test8.rav` generic sample end-to-end; the investigation now stays open to capture regressions, gaps in test coverage, and the work required to reach feature parity with Roslyn. The immediate focus is understanding why awaits inside guarded regions fail to run and ensuring the existing pattern-matching sample behaves correctly when the awaited operation throws.

**Immediate targets**

1. Instrument `samples/async-try-catch.rav` so the resulting `async-try-catch.dll` pinpoints why the awaited block never reaches its handler even though the process exits cleanly (exit code `0`).【F:src/Raven.Compiler/samples/async-try-catch.rav†L1-L16】
2. Apply the same diagnostics to `samples/http-client.rav` (`http-client.dll`) because it reproduces the identical failure pattern when awaiting inside a `try/catch`.【F:src/Raven.Compiler/samples/http-client.rav†L1-L33】
3. Stress `samples/try-match-async.rav` by forcing the awaited target to throw so we understand how the `try ... match` construct behaves when the awaited Task faults, not just when it produces a value.【F:src/Raven.Compiler/samples/try-match-async.rav†L1-L9】

## Current findings
* The constructed async state machine now reuses its own generic parameter when emitting builder completions. `ConstructedNamedTypeSymbol.ResolveRuntimeTypeArgument` first reuses any registered runtime type for the state-machine parameter before falling back to the async-method mapping, so the builder MemberRefs resolve to `AsyncTaskMethodBuilder<!T>` instead of leaking the async method’s `!!0`.【F:src/Raven.CodeAnalysis/Symbols/Constructed/ConstructedNamedTypeSymbol.cs†L268-L304】
* The emitted IL for `Program/'<>c__AsyncStateMachine0`1'::MoveNext` now calls `AsyncTaskMethodBuilder<!T>.SetResult(!0)`, matching Roslyn’s baseline and eliminating the verifier mismatch that produced `AsyncTaskMethodBuilder<!!0>` earlier in the investigation.【F:docs/investigations/async-await.md†L33-L38】
* The compiled `test8.rav` sample still executes successfully and prints `42`, confirming the earlier `BadImageFormatException` fix continues to hold end-to-end.【88982c†L1-L2】
* Re-running `samples/async-await.rav` through the CLI emits `/tmp/async-await.dll` and the binary prints the expected `first:1`, `sum:6`, and `done` messages, so the `Task.FromResult` inference regression no longer reproduces.【413905†L1-L17】【27dee1†L1-L4】
* Await expressions that live inside a `try` block currently generate invalid IL: both `samples/async-try-catch.rav` and `samples/http-client.rav` abort with `InvalidProgramException` before reaching their handlers, so `await` inside a guarded region is temporarily regressed until the async state machine wires its dispatch blocks without re-entering the method prologue.【4907a6†L1-L6】【52225f†L1-L6】
* Await-heavy CLI sample status is tracked below for quick reference as regressions crop up in new areas of the lowering pipeline.

| Sample | Status | Notes |
| --- | --- | --- |
| `async-await.rav` | ✅ runs | Builds and prints the async flow (`first:1`, `sum:6`, `done`).【413905†L1-L17】【27dee1†L1-L4】 |
| `async-try-catch.rav` | ❌ fails | Runtime aborts with `InvalidProgramException`, so the sample never reaches its catch block.【4907a6†L1-L6】 |
| `http-client.rav` | ❌ fails | Hits the same `InvalidProgramException` during `MoveNext`, so `await` inside the `try/catch` cannot complete yet.【52225f†L1-L6】 |
| `test6.rav` | ✅ runs | Continues to build/run after the await lowering updates.【d6680d†L1-L5】 |
| `test7.rav` | ✅ runs | Exercises awaiting `Task.FromResult` in an async helper without issues.【7d6784†L1-L2】 |
| `test8.rav` | ✅ runs | Emits the generic async state machine correctly and prints `42`.【88982c†L1-L2】 |
| `try-match-async.rav` | ⚠️ verify | Succeeds when the awaited operation completes successfully, but the sample still needs coverage for the faulted-await path described above.【F:src/Raven.Compiler/samples/try-match-async.rav†L1-L9】 |
* Targeted regression tests covering await lowering, the builder `Start<TStateMachine>` MethodSpec, and the `SetResult` MemberRef all pass, guarding the substitution pipeline against regressions.【9329ec†L1-L11】【0f004d†L1-L6】【5e9a18†L1-L8】

### Guarded await failure mode

*The `async-try-catch.rav` sample aborts with `InvalidProgramException` because the MoveNext dispatcher jumps directly into the guarded block that owns a suspended await, violating the CLI “single-entry” rule for protected regions.*【4907a6†L1-L6】

1. `CreateMoveNextBody` always prepends a dispatcher that runs before the rewritten body and lives outside the user’s `try/catch`. Each `StateDispatch` recorded by the await rewriter is emitted as an `if (state == N) goto <resumeLabel>` ahead of the entry label, so the resumption path bypasses the statement that opens the `try` block produced for the source program.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L81-L120】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L240-L283】
2. `StateDispatchInjector` also seeds every block that contains a resumption label with a local dispatcher, but this happens *inside* the guarded region. When the outer dispatcher jumps straight to the label, control transfers into the `try` body without entering at its start, which is precisely the pattern rejected by the CLR verifier.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1621-L1750】

**Fix direction.** We need to prevent `CreateStateDispatchStatements` from targeting labels that sit in a protected region. Instead, the outer dispatcher should only redirect to safe landing pads (one per `try` block) that are placed *inside* the guard. The localized dispatch that `StateDispatchInjector` adds inside the landing pad can then jump to the actual resume label without violating the single-entry constraint. Bringing this in line with Roslyn requires:

* Extending `StateDispatch` to track the `BoundBlockStatement` (or `BoundTryStatement`) that owns each resume label so we know when a guard is involved during dispatch construction.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1380-L1445】
* Teaching `CreateStateDispatchStatements` to emit “guard entry” labels instead of the final resume targets whenever the owning block is a `try` body; the first statement inside that guard-specific block can then be the injected `if (state == N) goto <resumeLabel>` so the jump originates within the protected region.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L240-L283】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1621-L1750】
* Once the dispatcher is split, re-running `async-try-catch.rav` and `http-client.rav` should succeed because every resumption path now flows through the same entry point the verifier expects, and the `try` body can safely execute its catch logic after the awaited operation throws.【F:src/Raven.Compiler/samples/async-try-catch.rav†L1-L16】【F:src/Raven.Compiler/samples/http-client.rav†L1-L33】

### IL snapshot – builder completion uses the state-machine generic

```il
IL_00b4: ldarg.0
IL_00b5: ldflda valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<!0>
          valuetype Program/'<>c__AsyncStateMachine0`1'<!T>::_builder
IL_00c0: call instance void valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<!T>::
          SetResult(!0)
```

## Task list
* Add metadata regression coverage for `AsyncTaskMethodBuilder<T>.SetException` mirroring the `SetResult` test so the completion path remains pinned to the state-machine generic parameter.【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncLowererTests.cs†L1195-L1252】
* Diff the MethodSpec table for `test8` against Roslyn to confirm awaiter helpers (`GetAwaiter`, `GetResult`, `get_IsCompleted`) also stay on the async-method generics now that the runtime substitution prefers the state-machine parameter.【F:src/Raven.CodeAnalysis/Symbols/Constructed/ConstructedNamedTypeSymbol.cs†L268-L304】
* Re-run `async-await.rav` periodically to guard the `Task.FromResult` inference path, ensuring the await-heavy sample keeps compiling and running end-to-end.【413905†L1-L17】【27dee1†L1-L4】
* Capture the invalid IL emitted for `async-try-catch.rav` and `http-client.rav` (e.g., via `ilverify` or `ilspycmd`) so the faulty state-machine blocks can be compared against Roslyn’s version and patched before enabling awaits inside guarded regions again.【F:src/Raven.Compiler/samples/async-try-catch.rav†L1-L16】【F:src/Raven.Compiler/samples/http-client.rav†L1-L33】
* Extend `try-match-async.rav` with a deliberate faulting await (throwing inside the awaited Task) and assert the resulting pattern match routes to the `Exception` arm, proving the lowered state machine raises the exception before the pattern switch executes.【F:src/Raven.Compiler/samples/try-match-async.rav†L1-L9】
* Keep `async-try-catch.rav` in the manual sample rotation so the async try/catch lowering (and `catch (Exception)` binding) stay covered whenever lowering changes land.【F:src/Raven.Compiler/samples/async-try-catch.rav†L1-L16】【39032d†L1-L17】【78df97†L1-L4】
* Expand runtime coverage with additional generic async samples (e.g., nested awaits or multiple type parameters) to ensure the substitution logic scales beyond the `Test<T>` scenario.【9329ec†L1-L11】

## Strategy
1. **Baseline the metadata** – Use Mono.Cecil (or `ilspycmd`) to diff Raven’s `test8` output against Roslyn’s, capturing every mismatch in generic instantiation, local signature, and async builder field layout. Feed the discrepancies back into the lowering/codegen steps so no runtime-visible MethodSpec falls back to the state-machine `!0` placeholder.
2. **Fix the remaining substitutions** – Audit builder- and await-related call sites (now focusing on `SetResult`, `SetException`, and hoisted-field accesses) so they always instantiate with the async method’s generic parameters before emission. Add targeted tests that assert the constructed MethodSpecs carry the right generics once the fixes land.【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L2678-L2706】
3. **Reconcile locals and temporaries** – Align the async state-machine locals with Roslyn by trimming redundant temporaries and ensuring disposal guards reuse existing hoisted locals, which prevents verifier-visible signature drift.
4. **Verify end-to-end** – After metadata and locals match, rerun `ilverify` and the runtime sample to confirm the image loads without `BadImageFormatException`, then lock the behaviour down with regression coverage (unit tests plus IL snapshots where necessary).
