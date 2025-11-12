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
* The async method lowered for `Test<T>` now reuses the builder helpers from the synthesized state machine without leaking `AsyncTaskMethodBuilder<!0>` into runtime-visible calls. The new builder cache hands the method body a fresh substitution so `Create`, `Start`, and `get_Task` retain the async method’s `!!0` instead of the struct’s `!0` placeholder.【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L960-L1012】
* Await lowering no longer substitutes `GetAwaiter`/`GetResult` with the state machine map, so the generated calls keep the async method’s own type parameters (`!!0`) when materializing the awaiter and its result.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1349-L1370】
* Emission now prefers `callvirt` whenever an instance receiver is a reference type, eliminating the earlier `call` opcode on `Task<T>.GetAwaiter()` that diverged from Roslyn and was a likely verifier hazard.【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L2699-L2706】
* Mono.Cecil diffs still show subtle gaps versus Roslyn: our async state machine locals remain wider (`System.Int32` temporaries for the awaited result), and the runtime continues to throw `BadImageFormatException`, so there is likely another metadata mismatch beyond the `callvirt` fix.【F:docs/investigations/async-await.md†L74-L76】【e61182†L1-L8】
* Roslyn’s IL keeps the awaited helper MethodSpecs on the original method’s `!!0`, while our `MoveNext` body still routes some constructed calls (e.g., builder `AwaitUnsafeOnCompleted`) through the state-machine substitution; the runtime-visible MethodSpecs must be verified to ensure they reference the method definition generics instead of the struct placeholder.【F:docs/investigations/async-await.md†L47-L60】【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L2699-L2706】

### IL snapshot – async builder calls now preserve `!!0`

```il
IL_0018: call valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<!0>
           valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<!!T>::Create()
IL_002b: call instance void valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<!!T>::Start
           <valuetype Program/'<>c__AsyncStateMachine0`1'<!0>>(!!0&)
```

### IL snapshot – `MainAsync` await now matches Roslyn

```il
IL_0038: call class [System.Private.CoreLib]System.Threading.Tasks.Task`1<!!0> Program::Test<int32>(!!0)
IL_003d: call instance valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.TaskAwaiter`1<!0>
           class [System.Private.CoreLib]System.Threading.Tasks.Task`1<int32>::GetAwaiter()
```

## Task list
* Diff Raven and Roslyn IL for `test8` to isolate the remaining structural differences (locals, generic instantiations, or metadata tables) now that `GetAwaiter` uses `callvirt`.
* Bring up `ilverify` against both images so the verifier pinpoints the exact `BadImageFormatException` trigger instead of relying on the runtime failure.【e61182†L1-L8】
* Collapse redundant temporaries in the async state machine (e.g., the extra `System.Int32` locals) to match Roslyn’s local signatures before rerunning the runtime/ILVerify checks.
* Capture regression coverage that locks in the `callvirt` behaviour for reference-type instance calls so the async pipeline cannot regress to `call`.

## Strategy
1. **Baseline the metadata** – Use Mono.Cecil (or `ilspycmd`) to diff Raven’s `test8` output against Roslyn’s, capturing every mismatch in generic instantiation, local signature, and async builder field layout. Feed the discrepancies back into the lowering/codegen steps that still substitute the state-machine `!0` in runtime-visible MethodSpecs.【F:docs/investigations/async-await.md†L47-L76】
2. **Fix the remaining substitutions** – Audit builder- and await-related call sites (especially `AwaitUnsafeOnCompleted`, `Start`, and hoisted-field accesses) so they always instantiate with the async method’s generic parameters before emission. Add targeted tests that assert the constructed MethodSpecs carry `!!0` once the fix lands.【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L2678-L2706】
3. **Reconcile locals and temporaries** – Align the async state-machine locals with Roslyn by trimming redundant temporaries and ensuring disposal guards reuse existing hoisted locals, which prevents verifier-visible signature drift.【F:docs/investigations/async-await.md†L71-L76】
4. **Verify end-to-end** – After metadata and locals match, rerun `ilverify` and the runtime sample to confirm the image loads without `BadImageFormatException`, then lock the behaviour down with regression coverage (unit tests plus IL snapshots where necessary).【e61182†L1-L8】
