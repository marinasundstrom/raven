# Async/await feature investigation

## Objective
Track the overall completeness of Raven's async/await implementation. The original work validated the `test8.rav` generic sample end-to-end; the investigation now stays open to capture regressions, gaps in test coverage, and the work required to reach feature parity with Roslyn. The immediate focus is hardening the new guarded-await fix, documenting its dispatcher shape, and exercising the remaining samples that mix exception handling with `await`.

**Immediate targets**

1. Backfill IL-level regression coverage for awaits that suspend inside nested `try/catch` blocks so the new guarded dispatcher shape is exercised during unit tests (extending `AsyncILGenerationTests` is the fastest path).【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L195-L260】
2. Stress `samples/try-match-async.rav` by forcing the awaited target to throw so we understand how the `try ... match` construct behaves when the awaited Task faults, not just when it produces a value.【F:src/Raven.Compiler/samples/try-match-async.rav†L1-L9】
3. Capture a Roslyn diff (or IL snapshot) of the guard-aware dispatcher so future lowering changes keep routing resumption state through the synthesized landing pads inside each protected region.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L248-L295】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1655-L1853】

## Current findings
* The constructed async state machine now reuses its own generic parameter when emitting builder completions. `ConstructedNamedTypeSymbol.ResolveRuntimeTypeArgument` first reuses any registered runtime type for the state-machine parameter before falling back to the async-method mapping, so the builder MemberRefs resolve to `AsyncTaskMethodBuilder<!T>` instead of leaking the async method’s `!!0`.【F:src/Raven.CodeAnalysis/Symbols/Constructed/ConstructedNamedTypeSymbol.cs†L268-L304】
* The emitted IL for `Program/'<>c__AsyncStateMachine0`1'::MoveNext` now calls `AsyncTaskMethodBuilder<!T>.SetResult(!0)`, matching Roslyn’s baseline and eliminating the verifier mismatch that produced `AsyncTaskMethodBuilder<!!0>` earlier in the investigation.【F:docs/investigations/async-await.md†L33-L38】
* The compiled `test8.rav` sample still executes successfully and prints `42`, confirming the earlier `BadImageFormatException` fix continues to hold end-to-end.【108430†L1-L3】
* Re-running `samples/async-await.rav` through the CLI emits `/tmp/async-await.dll` and the binary prints the expected `first:1`, `sum:6`, and `done` messages, so the `Task.FromResult` inference regression no longer reproduces.【9c222c†L2-L4】
* Await expressions that live inside nested `try` blocks now resume from a chain of guard-aware landing pads instead of jumping directly into a protected region. `AwaitLoweringRewriter` captures the entire stack of enclosing `try` blocks for every resumption label, `StateDispatchInjector` assigns entry labels to each guard in that stack, and the outer dispatcher routes suspended states through every guard entry before flowing to the actual resume label.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1472-L1523】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1684-L1812】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L2116-L2145】
* A new `NestedTryAwaitExpressionAsyncAssembly_PassesIlVerifyWhenToolAvailable` regression test compiles a script with nested `try`/`finally`/`catch` blocks and verifies the generated IL passes `ilverify`, so the multi-guard dispatcher remains covered even when optional tooling is installed.【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L195-L339】
* `UsingTryAwaitExpressionAsyncAssembly_PassesIlVerifyWhenToolAvailable` adds another IL-level regression that mimics the `http-client.rav` disposal pattern, keeping awaits that resume inside stacked `using let` statements under verifier coverage.【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L309-L346】
* A fresh manual rotation confirms every await-heavy CLI sample (`async-await`, `async-try-catch`, `http-client`, `test6`, `test7`, `test8`, and `try-match-async`) compiles and prints the expected values, so the guarded dispatcher now survives realistic workloads beyond the original repro programs.【9c222c†L2-L4】【85e300†L2-L4】【4e41b6†L2-L4】【de211e†L2-L4】【89b5ba†L2-L2】【108430†L1-L3】【74185c†L2-L2】
* Await-heavy CLI sample status is tracked below for quick reference as regressions crop up in new areas of the lowering pipeline.

| Sample | Status | Notes |
| --- | --- | --- |
| `async-await.rav` | ✅ runs | Builds and prints the async flow (`first:1`, `sum:6`, `done`).【9c222c†L2-L4】 |
| `async-try-catch.rav` | ✅ runs | Compiles and prints `value:42`, `caught:boom`, and `completed`, proving awaits inside the guard now resume through the landing pad.【85e300†L2-L4】 |
| `http-client.rav` | ✅ runs | Compiles and catches the `HttpRequestException`, printing the `403` status text instead of crashing the state machine.【4e41b6†L2-L4】 |
| `test6.rav` | ✅ runs | Continues to build/run after the await lowering updates.【de211e†L2-L4】 |
| `test7.rav` | ✅ runs | Exercises awaiting `Task.FromResult` in an async helper without issues.【89b5ba†L2-L2】 |
| `test8.rav` | ✅ runs | Emits the generic async state machine correctly and prints `42`.【108430†L1-L3】 |
| `try-match-async.rav` | ⚠️ verify | Succeeds when the awaited operation completes successfully, but the sample still needs coverage for the faulted-await path described above.【74185c†L2-L2】【F:src/Raven.Compiler/samples/try-match-async.rav†L1-L9】 |

### Sample compilation harness

Use the following Bash snippet from the repo root to rebuild and execute every await-heavy CLI sample in one pass. Extend the `SAMPLES` array whenever new async demos appear, and keep the `try-match-async` entry once its deliberate faulting path lands so regressions surface quickly.

```bash
#!/usr/bin/env bash
set -euo pipefail

SAMPLES=(
  async-await
  async-try-catch
  http-client
  test6
  test7
  test8
  try-match-async
)

for sample in "${SAMPLES[@]}"; do
  src="src/Raven.Compiler/samples/${sample}.rav"
  out="/tmp/${sample}.dll"

  echo "==> Building ${sample}"
  dotnet run --project src/Raven.Compiler -- "$src" -o "$out"

  echo "==> Running ${sample}"
  dotnet "$out"
  echo
done
```

* Targeted regression tests covering await lowering, the builder `Start<TStateMachine>` MethodSpec, and the `SetResult` MemberRef all pass, guarding the substitution pipeline against regressions.【9329ec†L1-L11】【0f004d†L1-L6】【5e9a18†L1-L8】

### Guarded await fix

The guarded-await regression is resolved by teaching the state-machine lowering to keep every resumption path inside the same protected-region entry point the CLR verifier expects:

1. `AwaitLoweringRewriter` now records the entire stack of enclosing `try` blocks for each `StateDispatch` entry (using `_tryBlocks`) so it knows when a resume label must re-enter multiple guards. The dispatch metadata stores those `BoundBlockStatement`s alongside the state and label.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1472-L1523】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L2106-L2145】
2. `StateDispatchInjector` groups guard-owned dispatches, synthesizes `guardN` labels for every protected block in the path, and injects those landing pads inside the guard before emitting the local dispatcher that jumps to the next guard (or the actual resume label when the innermost guard is reached).【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1684-L1812】
3. `CreateStateDispatchStatements` consults the guard-entry map and routes the outer dispatcher through the outermost guard’s landing pad instead of targeting the resume label directly, so every `goto` flows through each guard before resuming the awaited block.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L248-L275】

With that wiring in place, both guarded samples compile and run again: `async-try-catch.rav` prints `value:42`, `caught:boom`, and `completed`, and `http-client.rav` now executes its catch handler and surfaces the `403 (Forbidden)` status text instead of crashing the process.【85e300†L2-L4】【4e41b6†L2-L4】

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
* Re-run `async-await.rav` periodically to guard the `Task.FromResult` inference path, ensuring the await-heavy sample keeps compiling and running end-to-end.【9c222c†L2-L4】
* Capture IL snapshots (e.g., via `ilverify` or `ilspycmd`) for the guard-entry dispatcher so future regressions reveal themselves immediately; the new landing pads originate in `StateDispatchInjector` and flow through `CreateStateDispatchStatements`.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L248-L295】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1655-L1853】
* Extend `try-match-async.rav` with a deliberate faulting await (throwing inside the awaited Task) and assert the resulting pattern match routes to the `Exception` arm, proving the lowered state machine raises the exception before the pattern switch executes.【F:src/Raven.Compiler/samples/try-match-async.rav†L1-L9】
* Keep `async-try-catch.rav` in the manual sample rotation so the async try/catch lowering (and `catch (Exception)` binding) stay covered whenever lowering changes land.【F:src/Raven.Compiler/samples/async-try-catch.rav†L1-L16】【39032d†L1-L17】【78df97†L1-L4】
* Expand runtime coverage with additional generic async samples (e.g., nested awaits or multiple type parameters) to ensure the substitution logic scales beyond the `Test<T>` scenario.【9329ec†L1-L11】

## Strategy
1. **Baseline the metadata** – Use Mono.Cecil (or `ilspycmd`) to diff Raven’s `test8` output against Roslyn’s, capturing every mismatch in generic instantiation, local signature, and async builder field layout. Feed the discrepancies back into the lowering/codegen steps so no runtime-visible MethodSpec falls back to the state-machine `!0` placeholder.
2. **Fix the remaining substitutions** – Audit builder- and await-related call sites (now focusing on `SetResult`, `SetException`, and hoisted-field accesses) so they always instantiate with the async method’s generic parameters before emission. Add targeted tests that assert the constructed MethodSpecs carry the right generics once the fixes land.【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L2678-L2706】
3. **Reconcile locals and temporaries** – Align the async state-machine locals with Roslyn by trimming redundant temporaries and ensuring disposal guards reuse existing hoisted locals, which prevents verifier-visible signature drift.
4. **Verify end-to-end** – After metadata and locals match, rerun `ilverify` and the runtime sample to confirm the image loads without `BadImageFormatException`, then lock the behaviour down with regression coverage (unit tests plus IL snapshots where necessary).
