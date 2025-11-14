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
* Await lowering now routes awaited expressions back through the async-method view before emitting the rewritten blocks, so the bound tree no longer pins the helper invocations to the state-machine-only substitutions.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1005-L1035】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1504-L1537】 The awaiter field stores and hoisted locals still project through the state-machine substitution where needed, but the invocations of `GetAwaiter`, `get_IsCompleted`, and `GetResult` now flow through the async-method substitution helpers before lowering.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1380-L1412】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1422-L1446】 Even after the rewrite, the emitted IL for `Program/'<>c__AsyncStateMachine1'::MoveNext` continues to encode `TaskAwaiter<!0>` just like Roslyn’s baseline, so the remaining substitution leak must come from the metadata we hand to Reflection.Emit rather than the bound tree itself.【F:docs/investigations/async-await.md†L96-L112】
* The await-expression substituter now recognises the async builder field and skips the state-machine substitution for its methods and containing types, keeping builder calls in the struct-generic context while awaited helpers still remap through the async-method view.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L568-L699】
* The builder prologue now consults the async method’s original definition for its generic parameters before constructing the state machine view so the lowerer never materialises a cache based on the already-substituted state-machine type parameters.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1820-L1867】 After wiring the runtime substitution helpers to remap nested state-machine type arguments back through the async method’s generic parameter builders, the emitted `AsyncTaskMethodBuilder<T>.Start<TStateMachine>` MethodSpec now encodes `Program+<>c__AsyncStateMachine0`1<!!0>` with a `GenericMethodParameter`, matching Roslyn and unblocking the metadata regression test.【F:src/Raven.CodeAnalysis/Symbols/Constructed/ConstructedNamedTypeSymbol.cs†L231-L287】【F:src/Raven.CodeAnalysis/Symbols/Constructed/ConstructedMethodSymbol.cs†L214-L332】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncLowererTests.cs†L1140-L1188】
* Builder completion now remaps `SetResult`/`SetException` through the async-method substitution before emission, keeping the MethodSpecs anchored to the method’s `!!0` placeholder while the receiver still targets the constructed state machine.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L310-L349】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L471-L492】 Runtime validation still fails with `BadImageFormatException`, so the remaining metadata mismatch sits elsewhere in the async pipeline.【6374d7†L1-L10】
* The regression test `AsyncLowererTests.Emit_GenericAsyncMethod_UsesMethodGenericsInBuilderStart` now passes, confirming the builder start MethodSpec projects the async method’s `!!0` placeholder instead of leaking the state machine’s `!0`.【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncLowererTests.cs†L1140-L1188】【8e575e†L1-L3】
* Emission now prefers `callvirt` whenever an instance receiver is a reference type, eliminating the earlier `call` opcode on `Task<T>.GetAwaiter()` that diverged from Roslyn and was a likely verifier hazard.【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L2699-L2706】
* Async method bodies now zero-initialize the synthesized state machine local instead of invoking its parameterless constructor, mirroring Roslyn’s pattern and removing the redundant `newobj` before the builder setup.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L121-L205】
* Dumped both Raven’s `test8.dll` and a Roslyn baseline with ILSpy: builder MethodSpecs, async state machine fields, and the `MoveNext` IL for both the async entry point and `Test<T>` now match byte-for-byte apart from attribute trivia, yet the runtime still raises `BadImageFormatException`. This points to a subtler metadata or verification quirk that IL equivalence alone doesn’t expose.【38fd66†L1-L24】【91a48f†L1-L6】
* `MoveNext` now constructs `AwaitUnsafeOnCompleted` with the hoisted awaiter field but immediately rewraps the builder call through the async-method substitution so the resulting MethodSpec keeps the async method’s `!!0` while still scheduling the state machine receiver.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1380-L1399】
* The async metadata formatter now emits nested type names one segment at a time, so attributes such as `AsyncStateMachineAttribute` reference ``Program+<>c__AsyncStateMachine0`1`` instead of the invalid ``Program+Program+<>c__AsyncStateMachine0`1`` string we previously generated.【F:src/Raven.CodeAnalysis/Symbols/ISymbol.cs†L371-L407】
* A Roslyn baseline compiled from the equivalent C# sample also encodes the awaiter call as `TaskAwaiter<!0>`, which confirms that the opcode stream we emit already matches the reference compiler and that the outstanding verifier break sits in metadata rather than the instruction sequence.【F:docs/investigations/async-await.md†L96-L112】
* Roslyn’s own `Program.<Main>d__1` IL also displays the awaiter call as `TaskAwaiter<!0>`, so the remaining `BadImageFormatException` is almost certainly a metadata problem (e.g., a MethodSpec or attribute instantiation) rather than the opcode stream itself.【cd58ba†L1-L3】
* Mono.Cecil diffs still show subtle gaps versus Roslyn: our async state machine locals remain wider (`System.Int32` temporaries for the awaited result), and the runtime continues to throw `BadImageFormatException`, so there is likely another metadata mismatch beyond the `callvirt` fix.【F:docs/investigations/async-await.md†L74-L76】【e61182†L1-L8】
* Despite the constructor-free initialization, loading the sample assembly still fails with `BadImageFormatException`, so the remaining mismatch likely sits in the MethodSpec metadata rather than the state machine layout.【00adbe†L1-L8】
* Comparing the Roslyn baseline against Raven with a metadata inspector now shows matching `GenericMethodParameter` entries for `AsyncTaskMethodBuilder<T>.Start<TStateMachine>`, so the remaining verifier gap must come from other MethodSpecs (`SetResult`, `SetException`, or awaiter helpers) or from the broader metadata layout.【F:docs/investigations/async-await.md†L74-L112】
* Roslyn’s IL keeps every builder helper instantiated over the async method’s generics; now that `AwaitUnsafeOnCompleted` matches, we still need to confirm the remaining helpers (`SetResult`, `SetException`, hoisted-field stores) follow the same pattern before rerunning IL validation.【F:docs/investigations/async-await.md†L47-L60】【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L2699-L2706】
* `mreader` still reports the MemberRef for `AsyncTaskMethodBuilder<T>.SetResult` with the declaring type materialised as ``AsyncTaskMethodBuilder`1<!!0>`` while the parameter is `!0`, so the builder continues to borrow the async method’s generic placeholder instead of the state machine’s type parameter and the verifier break persists.【a10a83†L1-L15】

### IL snapshot – async builder calls now preserve `!!0`

```il
IL_0018: call valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<!0>
           valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<!!T>::Create()
IL_002b: call instance void valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<!!T>::Start
           <valuetype Program/'<>c__AsyncStateMachine0`1'<!0>>(!!0&)
```

### IL snapshot – `MainAsync` await matches Roslyn’s `TaskAwaiter<!0>`

```il
IL_0038: call class [System.Private.CoreLib]System.Threading.Tasks.Task`1<!!0> Program::Test<int32>(!!0)
IL_003d: callvirt instance valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.TaskAwaiter`1<!0>
           class [System.Private.CoreLib]System.Threading.Tasks.Task`1<int32>::GetAwaiter()
```

### IL snapshot – `MoveNext` await still materialises `TaskAwaiter<!0>`

```il
IL_0069: ldflda valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<!0>
          valuetype Program/'<>c__AsyncStateMachine0`1'<!T>::_builder
IL_0075: call instance void valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1<!T>::
          AwaitUnsafeOnCompleted<valuetype [System.Private.CoreLib]System.Runtime.CompilerServices.TaskAwaiter`1<!0>,
          valuetype Program/'<>c__AsyncStateMachine0`1'<!T>>(!!0&, !!1&)
```

## Task list
* Diff Raven and Roslyn IL for `test8` to isolate the remaining structural differences (locals, generic instantiations, or metadata tables) now that both `Start` and `AwaitUnsafeOnCompleted` line up with Roslyn.
* Inspect the MethodSpec table emitted for `test8` (via `System.Reflection.Metadata`) and compare it to Roslyn’s so we can confirm every awaiter and builder instantiation now points at the async method’s `!!0` placeholder instead of the state-machine `!0`.
* Trace any remaining MethodSpec mismatches—`Start` and `AwaitUnsafeOnCompleted` now line up, but we still need to audit `SetResult`, `SetException`, and the awaiter helpers—to ensure Reflection.Emit never falls back to the state-machine generic parameter builders.
* Verify the remaining MethodSpecs (`Program/'<>c__AsyncStateMachine1'::MoveNext` and async-builder completion helpers) after the start-method fix to ensure Roslyn parity across the entire async pipeline.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L572-L642】
* Inspect the emitted MethodSpecs for `Program/'<>c__AsyncStateMachine1'::MoveNext` to confirm the awaiter call matches Roslyn’s `TaskAwaiter<!0>` MethodSpec and to identify any remaining metadata mismatches that could lead to the runtime verifier failure.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L572-L642】
* Bring up `ilverify` against both images so the verifier pinpoints the exact `BadImageFormatException` trigger instead of relying on the runtime failure.【e61182†L1-L8】
* Expand the IL diff to cover the entire state machine body—including stack behaviour around `System.Unit` bookkeeping—to rule out subtle evaluation-stack mismatches that could still irritate the verifier despite the broad opcode parity.
* Collapse redundant temporaries in the async state machine (e.g., the extra `System.Int32` locals) to match Roslyn’s local signatures before rerunning the runtime/ILVerify checks.
* Capture regression coverage that locks in the `callvirt` behaviour for reference-type instance calls so the async pipeline cannot regress to `call`.

## Strategy
1. **Baseline the metadata** – Use Mono.Cecil (or `ilspycmd`) to diff Raven’s `test8` output against Roslyn’s, capturing every mismatch in generic instantiation, local signature, and async builder field layout. Feed the discrepancies back into the lowering/codegen steps so no runtime-visible MethodSpec falls back to the state-machine `!0` placeholder.【F:docs/investigations/async-await.md†L47-L76】
2. **Fix the remaining substitutions** – Audit builder- and await-related call sites (now focusing on `SetResult`, `SetException`, and hoisted-field accesses) so they always instantiate with the async method’s generic parameters before emission. Add targeted tests that assert the constructed MethodSpecs carry the right generics once the fixes land.【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L2678-L2706】
3. **Reconcile locals and temporaries** – Align the async state-machine locals with Roslyn by trimming redundant temporaries and ensuring disposal guards reuse existing hoisted locals, which prevents verifier-visible signature drift.【F:docs/investigations/async-await.md†L71-L76】
4. **Verify end-to-end** – After metadata and locals match, rerun `ilverify` and the runtime sample to confirm the image loads without `BadImageFormatException`, then lock the behaviour down with regression coverage (unit tests plus IL snapshots where necessary).【e61182†L1-L8】
