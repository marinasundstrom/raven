# Async/await feature investigation

## Objective
Track the overall completeness of Raven's async/await implementation. The original work validated the `samples/async/async-generic-compute.rav` generic sample end-to-end; the investigation now stays open to capture regressions, gaps in test coverage, and the work required to reach feature parity with Roslyn. The immediate focus is hardening the new guarded-await fix, documenting its dispatcher shape, and exercising the remaining samples that mix exception handling with `await`.

**Immediate targets**

1. Backfill IL-level regression coverage for awaits that suspend inside nested `try/catch` blocks so the new guarded dispatcher shape is exercised during unit tests (extending `AsyncILGenerationTests` is the fastest path).【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L195-L260】
2. Stress `samples/async/try-match-async.rav` by forcing the awaited target to throw so we understand how the `try ... match` construct behaves when the awaited Task faults, not just when it produces a value.【F:src/Raven.Compiler/samples/async/try-match-async.rav†L1-L9】
3. Capture a Roslyn diff (or IL snapshot) of the guard-aware dispatcher so future lowering changes keep routing resumption state through the synthesized landing pads inside each protected region.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L248-L295】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1655-L1853】

## Current findings
* The constructed async state machine now reuses its own generic parameter when emitting builder completions. `ConstructedNamedTypeSymbol.ResolveRuntimeTypeArgument` first reuses any registered runtime type for the state-machine parameter before falling back to the async-method mapping, so the builder MemberRefs resolve to `AsyncTaskMethodBuilder<!T>` instead of leaking the async method’s `!!0`.【F:src/Raven.CodeAnalysis/Symbols/Constructed/ConstructedNamedTypeSymbol.cs†L268-L304】
* The emitted IL for `Program/'<>c__AsyncStateMachine0`1'::MoveNext` now calls `AsyncTaskMethodBuilder<!T>.SetResult(!0)`, matching Roslyn’s baseline and eliminating the verifier mismatch that produced `AsyncTaskMethodBuilder<!!0>` earlier in the investigation.【F:docs/investigations/async-await.md†L33-L38】
* The compiled `samples/async/async-generic-compute.rav` sample still executes successfully and prints `42`, confirming the earlier `BadImageFormatException` fix continues to hold end-to-end.【108430†L1-L3】
* Re-running `samples/async/async-await.rav` through the CLI emits `/tmp/async-await.dll` and the binary prints the expected `first:1`, `sum:6`, and `done` messages, so the `Task.FromResult` inference regression no longer reproduces.【9c222c†L2-L4】
* Await expressions that live inside nested `try` blocks now resume from a chain of guard-aware landing pads instead of jumping directly into a protected region. `AwaitLoweringRewriter` captures the entire stack of enclosing `try` blocks for every resumption label, `StateDispatchInjector` assigns entry labels to each guard in that stack, and the outer dispatcher routes suspended states through every guard entry before flowing to the actual resume label.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1472-L1523】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1684-L1812】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L2116-L2145】
* A new `NestedTryAwaitExpressionAsyncAssembly_PassesIlVerifyWhenToolAvailable` regression test compiles a script with nested `try`/`finally`/`catch` blocks and verifies the generated IL passes `ilverify`, so the multi-guard dispatcher remains covered even when optional tooling is installed.【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L195-L339】
* `UsingTryAwaitExpressionAsyncAssembly_PassesIlVerifyWhenToolAvailable` adds another IL-level regression that mimics the `samples/async/http-client.rav` disposal pattern, keeping awaits that resume inside stacked `using let` statements under verifier coverage.【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L309-L346】
* A fresh manual rotation confirms every `samples/async/*.rav` CLI demo (`async-await`, `async-try-catch`, `http-client`, `async-file-io`, `async-task-return`, `async-generic-compute`, and `try-match-async`) compiles and prints the expected values, so the guarded dispatcher now survives realistic workloads beyond the original repro programs.【9c222c†L2-L4】【85e300†L2-L4】【4e41b6†L2-L4】【de211e†L2-L4】【89b5ba†L2-L2】【108430†L1-L3】【74185c†L2-L2】
* Async methods/functions that omit explicit return annotations now participate in a two-pass inference pipeline: `TypeMemberBinder`/`FunctionBinder` seed async declarations with `Task` and flag them for inference, and `MethodBodyBinder` re-binds the body with the inferred `Task<T>` produced by `AsyncReturnTypeUtilities` + `ReturnTypeCollector`. Expression-bodied functions route through the same helper before we emit their implicit return block, keeping tests like `TopLevelAwait_WithReturnExpression_SynthesizesTaskOfInt` green.【F:src/Raven.CodeAnalysis/Binder/TypeMemberBinder.cs†L325-L351】【F:src/Raven.CodeAnalysis/Binder/FunctionBinder.cs†L55-L179】【F:src/Raven.CodeAnalysis/Binder/MethodBodyBinder.cs†L19-L70】【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L5584-L5618】【F:src/Raven.CodeAnalysis/Binder/AsyncReturnTypeUtilities.cs†L5-L67】【F:src/Raven.CodeAnalysis/Binder/ReturnTypeCollector.cs†L8-L109】【F:test/Raven.CodeAnalysis.Tests/Semantics/AsyncMethodTests.cs†L239-L262】
* Await-heavy CLI sample status is tracked below for quick reference as regressions crop up in new areas of the lowering pipeline.

* Decompiling the current `try-match-async` binary shows the synthesized `Foo` state machine already catches the thrown `Exception` and routes it through `AsyncTaskMethodBuilder<int>.SetException`, so the fault should complete the awaited Task instead of leaving it pending.【6c6f5d†L3-L13】 A hang dump from a timed-out run captured `Program.Main` blocking in `TaskAwaiter.HandleNonSuccessAndDebuggerNotification` while waiting for `MainAsync` to finish, so the deadlock appears to happen before the Task ever transitions rather than because the throw fails to fault the builder.【a8fde7†L1-L6】

| Sample | Status | Notes |
| --- | --- | --- |
| `samples/async/async-await.rav` | ✅ runs | Builds and prints the async flow (`first:1`, `sum:6`, `done`).【9c222c†L2-L4】 |
| `samples/async/async-try-catch.rav` | ✅ runs | Compiles and prints `value:42`, `caught:boom`, and `completed`, proving awaits inside the guard now resume through the landing pad.【85e300†L2-L4】 |
| `samples/async/http-client.rav` | ✅ runs | Compiles and catches the `HttpRequestException`, printing the `403` status text instead of crashing the state machine.【4e41b6†L2-L4】 |
| `samples/async/async-file-io.rav` | ✅ runs | Continues to build/run after the await lowering updates.【de211e†L2-L4】 |
| `samples/async/async-task-return.rav` | ✅ runs | Exercises awaiting `Task.FromResult` in an async helper without issues.【89b5ba†L2-L2】 |
| `samples/async/async-generic-compute.rav` | ✅ runs | Emits the generic async state machine correctly and prints `42`.【108430†L1-L3】 |
| `samples/async/try-match-async.rav` | ⚠️ verify | Succeeds when the awaited operation completes successfully, but the sample still needs coverage for the faulted-await path described above.【74185c†L2-L2】【F:src/Raven.Compiler/samples/async/try-match-async.rav†L1-L9】 |

### Sample compilation harness

Use the following Bash snippet from the repo root to rebuild and execute every await-heavy CLI sample in one pass. Extend the `SAMPLES` array whenever new async demos appear, and keep the `try-match-async` entry once its deliberate faulting path lands so regressions surface quickly.

```bash
#!/usr/bin/env bash
set -euo pipefail

SAMPLES=(
  async/async-await
  async/async-try-catch
  async/http-client
  async/async-file-io
  async/async-task-return
  async/async-generic-compute
  async/try-match-async
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

With that wiring in place, both guarded samples compile and run again: `samples/async/async-try-catch.rav` prints `value:42`, `caught:boom`, and `completed`, and `samples/async/http-client.rav` now executes its catch handler and surfaces the `403 (Forbidden)` status text instead of crashing the process.【85e300†L2-L4】【4e41b6†L2-L4】

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
* Diff the MethodSpec table for `async-generic-compute` against Roslyn to confirm awaiter helpers (`GetAwaiter`, `GetResult`, `get_IsCompleted`) also stay on the async-method generics now that the runtime substitution prefers the state-machine parameter.【F:src/Raven.CodeAnalysis/Symbols/Constructed/ConstructedNamedTypeSymbol.cs†L268-L304】
* Re-run `samples/async/async-await.rav` periodically to guard the `Task.FromResult` inference path, ensuring the await-heavy sample keeps compiling and running end-to-end.【9c222c†L2-L4】
* Investigate the async lambda inference regression spotted in `await Task.Run(async () => 42)` where the lambda should infer as `Func<Task<int>>`, warn about the synchronous body, and lower `42` to `return Task.FromResult(42);`. The current inference fails whenever an `async` lambda lacks an `await`, so tighten the fix without breaking existing LINQ lambda inference. (Check section: _Snippet: Lambda inference issue_)
* Capture IL snapshots (e.g., via `ilverify` or `ilspycmd`) for the guard-entry dispatcher so future regressions reveal themselves immediately; the new landing pads originate in `StateDispatchInjector` and flow through `CreateStateDispatchStatements`.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L248-L295】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1655-L1853】
* Extend `samples/async/try-match-async.rav` with a deliberate faulting await (throwing inside the awaited Task) and assert the resulting pattern match routes to the `Exception` arm, proving the lowered state machine raises the exception before the pattern switch executes.【F:src/Raven.Compiler/samples/async/try-match-async.rav†L1-L9】
* Keep `samples/async/async-try-catch.rav` in the manual sample rotation so the async try/catch lowering (and `catch (Exception)` binding) stay covered whenever lowering changes land.【F:src/Raven.Compiler/samples/async/async-try-catch.rav†L1-L16】【39032d†L1-L17】【78df97†L1-L4】
* Expand runtime coverage with additional generic async samples (e.g., nested awaits or multiple type parameters) to ensure the substitution logic scales beyond the `Test<T>` scenario.【9329ec†L1-L11】

## Strategy
1. **Baseline the metadata** – Use Mono.Cecil (or `ilspycmd`) to diff Raven’s `async-generic-compute` output against Roslyn’s, capturing every mismatch in generic instantiation, local signature, and async builder field layout. Feed the discrepancies back into the lowering/codegen steps so no runtime-visible MethodSpec falls back to the state-machine `!0` placeholder.
2. **Fix the remaining substitutions** – Audit builder- and await-related call sites (now focusing on `SetResult`, `SetException`, and hoisted-field accesses) so they always instantiate with the async method’s generic parameters before emission. Add targeted tests that assert the constructed MethodSpecs carry the right generics once the fixes land.【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L2678-L2706】
3. **Reconcile locals and temporaries** – Align the async state-machine locals with Roslyn by trimming redundant temporaries and ensuring disposal guards reuse existing hoisted locals, which prevents verifier-visible signature drift.
4. **Verify end-to-end** – After metadata and locals match, rerun `ilverify` and the runtime sample to confirm the image loads without `BadImageFormatException`, then lock the behaviour down with regression coverage (unit tests plus IL snapshots where necessary).

## Snippet: Lambda inference issue

Use the snippet below to reproduce the inference regression:

1. Run the CLI: `dotnet run -- samples/async/async-inference.rav -o test.dll -d pretty`
2. The sample under test contains the following code:

   ```
   import System.Console.*
   import System.Threading.Tasks.*

   // Inference issue:
   let t = await Task.Run(async () => 42)

   WriteLine(t)
   ```

3. The compiler currently fails with three diagnostics:

   ```
   samples/async/async-inference.rav(5,15): error RAV1501: No overload for method 'Run' takes 1 arguments
   samples/async/async-inference.rav(5,36): error RAV1503: Cannot convert from 'int' to 'unit'
   samples/async/async-inference.rav(7,1): error RAV0121: The call is ambiguous between the following methods or properties: 'WriteLine' and 'WriteLine'
   ```

### Investigation notes – async lambda inference

* Re-running the repro sample from the repo root via `dotnet run --project src/Raven.Compiler -- src/Raven.Compiler/samples/async/async-inference.rav -o /tmp/async-inference-regression.dll -d pretty` still emits the three diagnostics above, so the regression remains live after the guarded-await work.【711d7b†L1-L15】
* `BindLambdaExpression` falls back to the first candidate delegate whenever the enclosing argument position does not provide a concrete target type. In the `Task.Run` call the recorded delegates arrive in metadata order, so the async lambda is forced onto the `Func<Task>` overload, and the body literal (`42`) is then checked against `unit` because async lambdas extract the result type from the chosen delegate’s `Task` return value.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Lambda.cs†L33-L220】
* The recorded candidates themselves already contain the correct `Func<Task<T>>` shape—`ComputeLambdaDelegateTargets` walks every accessible `Task.Run` overload and stores each delegate type against the lambda syntax—but there is no scoring heuristic to prefer the generic overload once multiple delegates survive the initial arity filter.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Lambda.cs†L360-L434】
* Because `GetTargetType` never looks through the enclosing `await` expression, the lambda binder never sees the fact that the awaited expression feeds an implicitly typed binding (`let t = …`) that eventually flows into `WriteLine`. The missing parent/await context leaves overload resolution with two viable delegate shapes and no way to choose the `Func<Task<int>>` overload that would make the async lambda legal.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L2165-L2345】
* Closing the gap likely requires pushing awaited-context information down into lambda binding (e.g., by teaching `GetTargetType` or `RecordLambdaTargets` to consider the async result type) and then ranking the candidate delegates so `Task.Run(Func<Task<T>>)` wins whenever the awaited result feeds a non-unit value. That keeps LINQ scenarios intact while allowing `async () => 42` to lower into `return Task.FromResult(42);` instead of tripping the current `int`→`unit` conversion error.
* Whatever scoring heuristic lands also needs to cover async methods/functions/lambdas that omit `await` entirely. Once the compiler understands that the body returns a non-`unit` literal/expression, it should synthesize the `Task.FromResult` lowering automatically and infer the delegate or method return type from that value—matching Roslyn’s behavior for `async` members that complete synchronously and ensuring callers still see the correct `Task<T>` shape.
* Keep Raven’s equivalent of Roslyn warning CS1998 in play for these await-less async bodies. Async methods/functions/lambdas that promise `Task`/`Task<T>` but never `await` should continue to trigger the diagnostic that warns they will run synchronously so we don’t silently accept code paths that ought to remain synchronous methods.

### Snippet: try/match faulted await regression

1. Compile the snippet below (stored anywhere, e.g., `/tmp/try-match-regression.rav`) via `dotnet run --project src/Raven.Compiler -- /tmp/try-match-regression.rav -o /tmp/try-match-regression.dll -d pretty`.
2. The sample forces a `try ... match` around a faulting awaited call:

   ```
   import System.*
   import System.Threading.Tasks.*

   func Foo() -> Task<int> {
       await Task.Delay(200)
       throw new Exception("")
       return 0
   }

   let result = try await Foo() match {
       int value => value.ToString()
       Exception ex => ex.Message
   }

   Console.WriteLine(result)
   ```

3. The compiler fails before it even reasons about the `match` because the async method body still triggers `error RAV1503: Cannot convert from '0' to 'Task'` at the `return 0;` statement even though `Foo` explicitly promises `Task<int>`. The inference/lowering pipeline is evidently dropping the generic return information when an async method with a real `await` feeds into the guarded `try ... match` lowering, so `ReturnTypeCollector` or the rebinding pass still believes the method returns `Task`.【e51693†L1-L20】
4. Adding the missing `async` modifier to `Foo` makes the sample build/run immediately, which proves the guarded `try await … match` lowering itself is healthy once the compiler understands the enclosing method is async.【ed5bd1†L1-L18】 The original snippet omits `async`, so the binder never unwraps `Task<T>` when checking `return 0` and the conversion fails as soon as we emit diagnostics for the body.【3c1de4†L1-L18】

### Investigation notes – async method inference

* The declaration binders already distinguish async methods that need return-type inference. `TypeMemberBinder` and `FunctionBinder` default those declarations to `Task`, mark them via `SourceMethodSymbol.RequireAsyncReturnTypeInference`, and defer diagnostics through `SourceMethodSymbol.ShouldDeferAsyncReturnDiagnostics` so the initial bind can complete without generating false “cannot convert `int` to `Task`” errors.【F:src/Raven.CodeAnalysis/Binder/TypeMemberBinder.cs†L325-L351】【F:src/Raven.CodeAnalysis/Binder/FunctionBinder.cs†L55-L125】【F:src/Raven.CodeAnalysis/Symbols/Source/SourceMethodSymbol.cs†L93-L108】【F:src/Raven.CodeAnalysis/Symbols/Source/SourceMethodSymbol.cs†L206-L223】
* `MethodBodyBinder.BindBlockStatement` (and `BlockBinder.BindFunction` for expression-bodied functions) detect the inference flag, run `AsyncReturnTypeUtilities.InferAsyncReturnType` over the bound body or expression, update the method symbol to `Task<T>`, and immediately re-bind the body so conversions, return statements, and trailing-expression checks see the finalized type.【F:src/Raven.CodeAnalysis/Binder/MethodBodyBinder.cs†L19-L70】【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L5584-L5618】【F:src/Raven.CodeAnalysis/Binder/AsyncReturnTypeUtilities.cs†L5-L67】
* `AsyncReturnTypeUtilities` simply normalizes whatever `ReturnTypeCollector` discovers in the body (literal `42`, `await Foo()`, implicit final expression, etc.) and wraps it in the right `Task` shape. Because `ReturnTypeCollector` never descends into nested lambdas, we still need to flow the inferred `Task<T>` back into lambda binding so async method groups passed as delegates prefer the `Func<Task<T>>` overload instead of the zero-result alternative.【F:src/Raven.CodeAnalysis/Binder/ReturnTypeCollector.cs†L8-L109】【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Lambda.cs†L33-L315】
* `AsyncLowerer.Analyze` records whether a method actually contains an `await`, so once inference starts producing `Task<T>` for await-less bodies we can keep warning (CS1998-equivalent) callers without forcing them to spell out `Task<T>` themselves. The combination of `SourceMethodSymbol.SetContainsAwait` plus the existing diagnostics in `MethodBodyBinder` gives us the plumbing we need to emit the warning only after inference completes.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L15-L33】【F:src/Raven.CodeAnalysis/Binder/MethodBodyBinder.cs†L54-L96】
* Remaining work: audit async methods/functions that feed into delegate inference (e.g., method groups passed to `Task.Run`) and ensure the post-inference `Task<T>` flows into `GetTargetType`/`RecordLambdaTargets`. Today `GetTargetType` still chooses the first viable delegate whenever the enclosing call-site stays ambiguous, so any async method without `await` that returns a literal continues to fall back to `Func<Task>` even after inference updated its signature.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L2165-L2260】【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Lambda.cs†L317-L479】
* The RAV1503 repro above is ultimately a missing-`async` problem, not a guarded-match lowering failure. `FunctionBinder` only marks script-level `func` declarations as async when the modifier is present, so `_methodSymbol.IsAsync` stays `false` and `BindReturnStatement` keeps demanding a `Task`-typed return even though the body just produces `int`. Meanwhile, `BindAwaitExpression` walks up the binder stack looking for *any* async symbol, so the implicitly async script entry point makes `await` legal inside `Foo` even though the function itself never opted in. This combination lets `await` compile while the return-type checker still enforces synchronous semantics, which is why adding `async` immediately clears the conversion error.【F:src/Raven.CodeAnalysis/Binder/FunctionBinder.cs†L53-L125】【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Statements.cs†L527-L590】【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L730-L835】
* Follow-up: either teach `IsAwaitExpressionAllowed` to respect the *local* method symbol (so `await` in a non-async `func` produces `RAV1905`) or automatically mark `func` declarations as async when we first encounter an awaited expression. Until one of those hooks lands, the compiler will continue producing misleading `Cannot convert … to Task` diagnostics instead of telling the author their helper needs the `async` modifier.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L730-L835】【F:src/Raven.CodeAnalysis/Binder/MethodBodyBinder.cs†L19-L70】

### How guarded `await` flows through try expressions

* The async lowerer does not treat `try` expressions as a special async shape; it simply walks the nested expression tree and rewrites every encountered `BoundAwaitExpression` into the standard block-expression state machine fragment (store awaiter → schedule continuation → return → resume label → get result). The `VisitTryExpression` override only rebinds the inner expression, so all of the await scheduling/resume work continues to be owned by `LowerAwaitExpressionToBlock` and the surrounding state dispatch machinery.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1010-L1090】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1380-L1569】
* At emit time the try-expression code generator wraps the rewritten expression in a real CLR `try/catch`. For block-shaped await payloads it registers the async resume labels before starting the exception block, then emits the scheduler/return/resume statements inside that protected region. The catch converts `Exception` back into the union return type, so faulted awaits keep flowing as data instead of tearing down the async state machine. The enclosing scope also sets an exception-exit label so the lowered `return` emitted by the await scheduler uses `leave` to exit the try region cleanly.【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L3660-L3765】
* Because there is no extra async-only handling in the try-expression rewriter, any hang at runtime points back to the await block itself failing to reschedule `MoveNext` (e.g., never calling `AwaitOnCompleted` or failing to clear the state), not to a missing `SetException` path in the try expression. The async runtime will still funnel unhandled exceptions through the synthetic `catch(Exception)` installed around the state machine body, so the outstanding investigation remains focused on why the awaited task never transitions rather than on missing exception plumbing in the guarded expression.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L124-L210】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L1700-L1839】
