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
* The async method lowered for `Test<T>` now reuses the builder helpers from the synthesized state machine without leaking `AsyncTaskMethodBuilder<!0>` into runtime-visible calls. The new builder cache hands the method body a fresh substitution so `Create`, `Start`, and `get_Task` retain the async method’s `!!0` instead of the struct’s `!0` placeholder.【F:src/Raven.CodeAnalysis/Symbols/Synthesized/SynthesizedAsyncStateMachineTypeSymbol.cs†L960-L1012】【F:docs/investigations/async-await.md†L39-L51】
* Await lowering no longer substitutes `GetAwaiter`/`GetResult` with the state machine map, so the generated calls keep the async method’s own type parameters (`!!0`) when materializing the awaiter and its result. Raven’s new IL now matches Roslyn’s baseline for the critical `Task<int>.GetAwaiter()`/`TaskAwaiter<int>.GetResult()` pair.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1349-L1370】

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
* Diff Raven and Roslyn IL for `test8` to spot any residual `call`/`callvirt` mismatches or metadata gaps after aligning the awaiter substitutions.
* Re-run the CLI (`ravc`) and Roslyn baselines under `ilverify`, documenting any surviving stack mismatches once the builder generics line up.
* Capture regression coverage that locks in the corrected async-method builder substitutions so future refactors cannot reintroduce the `!0` leak into runtime metadata.
