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
* `MainAsync`’s state machine still invokes `Task<int>.GetAwaiter()` with a `TaskAwaiter<!0>` return, so the lingering verifier failure must stem from other metadata differences after the async-method builder fix.【F:docs/investigations/async-await.md†L53-L63】

### IL snapshot – async builder calls now preserve `!!0`

```il
IL_0018: call valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<!0>
           valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<!!T>::Create()
IL_002b: call instance void valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<!!T>::Start
           <valuetype Program/'<>c__AsyncStateMachine0`1'<!0>>(!!0&)
```

### IL snapshot – `MainAsync` still emits `TaskAwaiter<!0>`

```il
IL_0038: call class [System.Private.CoreLib]System.Threading.Tasks.Task`1<!!0> Program::Test<int32>(!!0)
IL_003d: call instance valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.TaskAwaiter`1<!0>
           class [System.Private.CoreLib]System.Threading.Tasks.Task`1<int32>::GetAwaiter()
```

## Task list
* Verify the regenerated `test.dll` under ILSpy to confirm `AsyncTaskMethodBuilder::Start<Program/'<>c__AsyncStateMachine0`1'<!!0>>` now references the async method generic, then diff the TypeSpec/MethodSpec tables against Roslyn’s output to pinpoint any remaining discrepancies.
* Re-run the CLI (`ravc`) and Roslyn baselines under `ilverify`, documenting any surviving stack mismatches once the builder generics line up.
* Capture regression coverage that locks in the corrected async-method builder substitutions so future refactors cannot reintroduce the `!0` leak into runtime metadata.
