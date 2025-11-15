# Async/await test8 investigation

## Objective
Emit the async sample below through `ravc`, load the resulting `test.dll`, and match the IL that Roslyn produces for the equivalent C# code—particularly the constructed async state machine and its builder calls.

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

## Current findings
* The constructed async state machine now reuses its own generic parameter when emitting builder completions. `ConstructedNamedTypeSymbol.ResolveRuntimeTypeArgument` first reuses any registered runtime type for the state-machine parameter before falling back to the async-method mapping, so the builder MemberRefs resolve to `AsyncTaskMethodBuilder<!T>` instead of leaking the async method’s `!!0`.【F:src/Raven.CodeAnalysis/Symbols/Constructed/ConstructedNamedTypeSymbol.cs†L268-L304】
* The emitted IL for `Program/'<>c__AsyncStateMachine0`1'::MoveNext` now calls `AsyncTaskMethodBuilder<!T>.SetResult(!0)`, matching Roslyn’s baseline and eliminating the verifier mismatch that produced `AsyncTaskMethodBuilder<!!0>` earlier in the investigation.【F:docs/investigations/async-await.md†L30-L35】
* The compiled `test8.rav` sample still executes successfully and prints `42`, confirming the earlier `BadImageFormatException` fix continues to hold end-to-end.【88982c†L1-L2】
* Re-running `samples/async-await.rav` through the CLI emits `/tmp/async-await.dll` and the binary prints the expected `first:1`, `sum:6`, and `done` messages, so the `Task.FromResult` inference regression no longer reproduces.【413905†L1-L17】【27dee1†L1-L4】
* `samples/async-try-catch.rav` now intentionally throws an `InvalidOperationException` after the awaited work so the catch block logs `caught:boom` before completing, exercising the async try/catch lowering end-to-end.【F:src/Raven.Compiler/samples/async-try-catch.rav†L1-L16】【39032d†L1-L17】【78df97†L1-L4】
* Await-heavy CLI sample status is tracked below for quick reference as regressions crop up in new areas of the lowering pipeline.

| Sample | Status | Notes |
| --- | --- | --- |
| `async-await.rav` | ✅ runs | Builds and prints the async flow (`first:1`, `sum:6`, `done`).【413905†L1-L17】【27dee1†L1-L4】 |
| `async-try-catch.rav` | ✅ runs | Throws and catches an `InvalidOperationException` after awaiting, so the handler prints `value:42`, `caught:boom`, and `completed`.【F:src/Raven.Compiler/samples/async-try-catch.rav†L1-L16】【39032d†L1-L17】【78df97†L1-L4】 |
| `http-client.rav` | ✅ runs | Builds and predictably catches the remote `403` response instead of crashing.【2a1401†L1-L4】 |
| `test6.rav` | ✅ runs | Continues to build/run after the await lowering updates.【d6680d†L1-L5】 |
| `test7.rav` | ✅ runs | Exercises awaiting `Task.FromResult` in an async helper without issues.【7d6784†L1-L2】 |
| `test8.rav` | ✅ runs | Emits the generic async state machine correctly and prints `42`.【88982c†L1-L2】 |
| `try-match-async.rav` | ✅ runs | Still builds and executes its awaited pattern matching sample.【a00ac9†L1-L3】 |
* Targeted regression tests covering await lowering, the builder `Start<TStateMachine>` MethodSpec, and the `SetResult` MemberRef all pass, guarding the substitution pipeline against regressions.【9329ec†L1-L11】【0f004d†L1-L6】【5e9a18†L1-L8】

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
* Keep `async-try-catch.rav` in the manual sample rotation so the async try/catch lowering (and `catch (Exception)` binding) stay covered whenever lowering changes land.【F:src/Raven.Compiler/samples/async-try-catch.rav†L1-L16】【39032d†L1-L17】【78df97†L1-L4】
* Expand runtime coverage with additional generic async samples (e.g., nested awaits or multiple type parameters) to ensure the substitution logic scales beyond the `Test<T>` scenario.【9329ec†L1-L11】

## Strategy
1. **Baseline the metadata** – Use Mono.Cecil (or `ilspycmd`) to diff Raven’s `test8` output against Roslyn’s, capturing every mismatch in generic instantiation, local signature, and async builder field layout. Feed the discrepancies back into the lowering/codegen steps so no runtime-visible MethodSpec falls back to the state-machine `!0` placeholder.
2. **Fix the remaining substitutions** – Audit builder- and await-related call sites (now focusing on `SetResult`, `SetException`, and hoisted-field accesses) so they always instantiate with the async method’s generic parameters before emission. Add targeted tests that assert the constructed MethodSpecs carry the right generics once the fixes land.【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L2678-L2706】
3. **Reconcile locals and temporaries** – Align the async state-machine locals with Roslyn by trimming redundant temporaries and ensuring disposal guards reuse existing hoisted locals, which prevents verifier-visible signature drift.
4. **Verify end-to-end** – After metadata and locals match, rerun `ilverify` and the runtime sample to confirm the image loads without `BadImageFormatException`, then lock the behaviour down with regression coverage (unit tests plus IL snapshots where necessary).
