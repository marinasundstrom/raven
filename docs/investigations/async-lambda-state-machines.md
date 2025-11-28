# Async lambda state machine investigation

## Context
- Reproduced runtime failure for nested async lambdas compiled via `async/async-inference.rav`, where executing the emitted assembly throws `InvalidProgramException` from the synthesized async state machine.
- The last change that walked lambdas during await lowering was reverted; the crash persists, indicating structural issues with how async lambdas are lowered and emitted.
- Symptoms point to awaiter fields being emitted on the outer state machine and referenced from lambdas compiled as static methods (`Cannot take address of instance field '<>awaiter6' in a static context`).

## Hypothesis
- Async lambdas are currently rewritten inside the enclosing async method’s state machine rather than owning dedicated state machines. Their awaits end up referencing fields that belong to the parent state machine or are otherwise treated as static, producing invalid IL.
- Each async lambda should be lowered into its own async state machine (mirroring C#), with closure capture reused but awaiter slots local to the lambda-generated type.

## Immediate goals
- Confirm lowering/emission paths for async lambdas and whether state machine synthesis is skipped or reuses the outer machine.
- Identify where awaiter locals/fields for lambdas are allocated and how lambda bodies are rewritten before IL emission.
- Define the changes needed so every async lambda produces its own state machine while preserving capture sharing semantics.

## Investigation plan (re-evaluated for implementation)
- [x] **Trace async lambda lowering entry point**: Walk `AsyncLowerer.VisitLambdaExpression` (and related visitors) to confirm whether async lambdas trigger a nested lowering pipeline or are inlined into the outer method’s state machine.
- [x] **Inspect state machine synthesis for lambdas**: Follow `MethodBodyGenerator.EmitLambda` → `EmitLambdaBody` → `AsyncStateMachineRewriter` (or equivalent) to see when a lambda is considered async and whether a new `SynthesizedAsyncStateMachineTypeSymbol` is created per lambda.
- [x] **Map awaiter field ownership**: Identify the code that allocates awaiter fields/locals for async lambdas. Verify whether those fields live on the outer state machine or should be scoped to a lambda-specific machine.
- [x] **Design per-lambda state machine creation**: Outline the required API changes so lambdas invoke the async rewriting pipeline independently (similar to methods), ensuring the rewritten lambda body no longer depends on the parent machine’s fields.
- [x] **Audit closure interactions**: Ensure closure structs/classes continue to carry captured locals while async state machines for lambdas only own awaiters and builder/state fields, matching C# behavior for captured variables.
- [x] **Add regression coverage**: Create codegen tests for nested async lambdas in `Task.Run` and nested delegate scenarios, asserting valid IL execution (e.g., via `RecordingILBuilderFactory` or running emitted assembly) and preventing ambiguous overload regressions.
- [x] **Inspect Task.Run overload inference**: Capture binder/bound-tree output for `async/async-inference.rav` (with `-bt`) to understand how the async lambda’s delegate type is inferred and why the `Task.Run(Func<Task<TResult>?>)` overload is skipped.
- [x] **Keep generic delegates replayable**: Permit async lambda replay when delegate returns a type-parameterized task so generic overloads stay eligible during binding.
- [ ] **Align async async-lambda inference with C#**: Adjust delegate inference and overload selection so awaitful async lambdas are inferred as `Func<Task<T>?>` (or `Func<Task?>`) instead of synchronous delegates, letting `Task.Run(Func<Task<TResult>?>)` bind without ambiguity.
- [ ] **Re-validate Task.Run overload resolution**: After inference updates, re-run `async/async-inference.rav` to confirm it binds to `Task<TResult> Run<TResult>(Func<Task<TResult>?>)` and keeps other overloads (e.g., `WriteLine`) unambiguous.
- [ ] **Validate end-to-end**: Re-run the original sample and relevant unit tests to confirm `InvalidProgramException` is resolved and overload resolution behavior remains intact.
- [ ] **Run sample test procedure**: Follow the testing steps in `samples/README.md` to verify sample programs remain intact after async-lambda changes.
- [x] **Normalize `Task<Unit>` to `Task`**: Treat `Task<Unit>` as interchangeable with non-generic `Task` during resolution and emission to mirror the `Unit`→`void` mapping for async returns.
- [x] **Audit async nullability during inference**: Ensure async return inference reuses the shared async return helper so nullable task-shaped delegates stay eligible without double-wrapping async lambda results.

### Redo plan (aligning inference with C# and LINQ scenarios)
- [ ] **Re-derive delegate inference rules**: Model C# async-lambda delegate inference (including return type shaping for awaitful bodies) so we infer `Func<Task<T>?>`/`Func<Task?>` even before overload resolution, preventing fallbacks to synchronous delegates.
- [ ] **Retool overload resolution for async lambdas**: Ensure overload candidate filtering respects the async-shaped delegate results, particularly for APIs that accept both synchronous and task-returning delegates (e.g., `Task.Run`, LINQ `Select`/`SelectMany`).
- [ ] **Exercise LINQ extension resolution**: Add focused cases where async lambdas appear in LINQ-style extension method calls so the binder handles generic extension lookup plus delegate inference without ambiguity.
- [ ] **Rebuild diagnostics and dumps**: Update bound-tree/binder diagnostics to surface full candidate signatures (including extension methods) when ambiguities remain, aiding future debugging of LINQ and `Task.Run` overload picks.
- [ ] **Cross-validate with samples/tests**: After redoing the inference stack, rerun `async/async-inference.rav` with `-bt` and the samples test procedure, and add LINQ-centric regression tests that assert correct overload binding for async lambdas.
- [x] **Honor `Unit`/`void` equivalence during inference**: Treat `Unit` and `void` as interchangeable in conversion checks so delegate selection can pick void-returning delegates (e.g., `Action`) when the lambda result is `Unit`.

### Updated implementation path
- [x] **Extend async rewriter entry points**: Added `AsyncLowerer.Rewrite(SourceLambdaSymbol, …)` plus an `AsyncRewriteResult` wrapper so async-aware callers can flow analysis, state-machine handles, and rewritten bodies for lambdas alongside the existing method overloads.
- [x] **Synthesize per-lambda state machines**: Mirror method state-machine creation by introducing a `RewriteAsyncLambda` helper that produces a `SynthesizedAsyncStateMachineTypeSymbol` per async lambda and returns the rewritten body plus generated `MoveNext`.
- [x] **Integrate into lambda emission**: Update `ExpressionGenerator.EmitLambdaExpression`/`MethodBodyGenerator.EmitLambda` to route async lambdas through the new rewrite hook before IL emission, replacing the current direct emit path for `lambda.IsAsync`.
- [x] **Plumb closure/hoisted locals**: Ensure lambda state machines reuse captured variables from enclosing closures while owning their own awaiter/storage fields; verify hoisted locals are referenced through the closure parameter instead of outer state-machine fields.
- [x] **Add execution/regression tests**: Expand `async/async-inference.rav` coverage with IL validation or runtime assertions, plus targeted unit tests that execute nested async lambdas to prevent regressions during implementation.

### Findings after step 1
- `LambdaLowerer.Rewrite` only rewrites await-less async lambdas via `AsyncLowerer.RewriteAwaitlessLambdaBody`; async lambdas that contain awaits are merely lowered for control flow/closures without entering the async state-machine pipeline. This means the async rewriter never creates a state machine for a lambda body that awaits.
- `AsyncLowerer.Rewrite` is invoked for async methods and top-level statements during code generation, but lambdas never call into it, so the async state machine creation path is currently method-only.

### Findings after step 2
- `ExpressionGenerator.EmitLambdaExpression` constructs a `MethodGenerator` for each lambda and immediately emits IL for the lambda body via `MethodGenerator.EmitLambdaBody` without checking `lambda.IsAsync` or invoking `AsyncLowerer`. No async analysis or rewriting happens along this path.
- `MethodBodyGenerator.EmitLambda` emits the lambda body directly (block or return statement) with no async rewriting phase and no `SynthesizedAsyncStateMachineTypeSymbol` creation. Awaiter locals are never projected into a lambda-specific state machine, explaining why awaits currently reuse the enclosing method’s fields.

### Findings after step 3
- Await lowering allocates awaiter storage on whichever `SynthesizedAsyncStateMachineTypeSymbol` is passed to `AwaitLoweringRewriter`. Each await creates a `<>awaiter{state}` field via `_stateMachine.AddHoistedLocal(...)`, plus a `<>awaiterLocal{n}` local inside `MoveNext` for resume paths. There is no notion of a lambda-specific state machine here; all awaiter fields belong to the state machine provided by the caller (currently the enclosing method’s machine).
- Because async lambdas never invoke `AsyncLowerer.Rewrite`, their awaits bypass the rewriter that creates these awaiter fields. When awaits appear in lambdas inside an async method, any awaited expression that does get lowered happens against the outer method’s state machine, which matches the observed invalid IL where a lambda tries to take the address of `<>awaiter*` fields from a static context.

### Findings after step 4
- The async pipeline needs an entry point for lambdas that mirrors method emission:
  - Detect `BoundLambdaExpression.IsAsync` in `ExpressionGenerator.EmitLambdaExpression`/`MethodBodyGenerator.EmitLambda` and drive `AsyncLowerer.Rewrite` for the lambda body before IL is generated.
  - Synthesize a `SynthesizedAsyncStateMachineTypeSymbol` per async lambda (sibling to method machines) by reusing `AsyncStateMachineRewriter.RewriteAsyncMethod` but parameterized over a lambda symbol/body.
  - Teach the rewriter to emit a generated `MoveNext` that returns `Task`/`Task<T>` matching the delegate, wrapping the original lambda body in a state machine and preserving closure captures.
- Method/lambda rewriters must expose a stable surface that accepts the lambda symbol, lowered body, and `ContainingType` so that awaiter fields are allocated on the lambda’s own machine rather than the outer method’s.
- The plan is to introduce a lambda-aware async rewrite stage inside `MethodGenerator.EmitLambdaBody` that produces a rewritten body and synthesized state machine before handing control back to `ExpressionGenerator` for IL emission. This keeps lambda IL generation consistent with methods while isolating awaiter ownership.

### Findings after step 5
- Captured lambdas emit as instance methods on a synthesized closure type (`TypeGenerator.EnsureLambdaClosure`), with one public field per captured symbol; async lambdas currently share this path without any async awareness.
- `MethodGenerator.DefineMethodBuilder` swaps the containing type to the closure for captured lambdas and relaxes accessibility, keeping the closure responsible for captured values while leaving awaiters/state untouched.
- `MethodBodyGenerator.EmitLambda`/`InitializeCapturedParameters` populate closure fields from lambda parameters before executing the body, so nested lambdas read captures through the closure instance; async rewriting never adds awaiter fields to closures.
- When synthesizing per-lambda async state machines, we must thread the closure instance into the generated machine (for example, as `this` or via a parameter field) while keeping awaiters, builder, and state fields local to the lambda machine to avoid reusing the outer method’s storage.

### Findings after step 6
- Added target regression scenarios to drive implementation:
  - A nested async lambda passed to `Task.Run` that awaits before returning a value (mirrors `async/async-inference.rav`).
  - An async lambda inside another async lambda to validate state-machine isolation across multiple nesting levels.
  - Async lambdas that capture variables via closures to confirm capture plumbing coexists with per-lambda state machines.
- Prefer IL verification via `RecordingILBuilderFactory` for structural checks (awaiter fields on lambda-owned state machines, no outer-state references) and runtime execution that asserts awaited results are produced without `InvalidProgramException`.
- These tests will anchor the implementation and protect overload-resolution behavior by reusing the existing `Task.Run` and `WriteLine` scenarios from the sample.

### Findings after step 7
- Added a lambda-aware async state-machine path that allocates a dedicated `SynthesizedAsyncStateMachineTypeSymbol` for each async lambda, reusing the method lowering pipeline for body rewriting and `MoveNext` synthesis.
- The async state-machine factory now caches by `IMethodSymbol`, allowing lambdas to synthesize machines alongside methods without clashing with method-backed caches.

### Findings after step 8
- Async lambda state machines now rewrite `this` references to use the captured closure field on the lambda’s own state machine, keeping captured values on the closure while awaiters and state live on the lambda machine.

### Findings after step 9
- Added runtime regression coverage for async lambdas passed to `Task.Run` and nested async lambdas with captures to ensure per-lambda state machines execute without invalid IL and return expected results.

### Findings after step 10
- Binder/bound-tree output for `async/async-inference.rav` (invoked with `-bt`) shows the awaited lambda passed to `Task.Run` being inferred as `Func<int>` rather than an async `Func<Task<int>?>`, with candidate delegates including both task-returning and synchronous shapes. Because the lambda’s delegate type collapses to `Func<int>`, overload resolution treats the invocation as ambiguous and emits `RAV0121`, never choosing `Task<TResult> Run<TResult>(Func<Task<TResult>?> function)`.

### Findings after step 11
- Running `dotnet run --project src/Raven.Compiler -- samples/async/async-inference.rav -bt` still prints the awaited lambda inside `Task.Run` as `LambdaExpression [Type=Func<int>, …, CandidateDelegates=[Action, Func<System.Threading.Tasks.Task?>, Func<System.Threading.Tasks.Task<System.Threading.Tasks.Task.TResult>?>, Func<TResult>, Func<int>]]`, leaving `t2` as an ambiguous `ErrorExpression` and producing `RAV0121` for both `Task.Run` and `WriteLine`. The lambda result type remains `Int32` rather than `Task<int>`, confirming delegate inference continues to favor synchronous shapes despite the `await` in the body.【2a94c6†L6-L33】

### Findings after step 12
- Re-running `dotnet run --project src/Raven.Compiler -- samples/async/async-inference.rav -bt` with generated syntax/bound files in place shows the bound tree still assigning the lambda `Type=Func<int>` and listing the same candidate delegates. The ambiguous diagnostics enumerate only method names (`Run`/`Run`, `WriteLine`/`WriteLine`) without printing full signatures, so the bound-tree dump must be paired with symbol tables to see which overloads were considered. The invocation node reports `InvocationExpression [Type=Task<int>, Symbol=Task<int> Run, Method=Task<int> Run]`, indicating overload resolution never committed to the nullable-task delegate overload despite the awaited body.【d7f9c3†L1-L35】

### Findings after step 13
- Updated overload type inference to feed async lambdas through `AsyncReturnTypeUtilities.InferAsyncReturnType` so type-parameter inference sees `Task`/`Task<T>`-shaped returns instead of raw body types. Re-running the `async/async-inference.rav` sample now fails earlier in overload resolution: the first async lambda reports `RAV1501` (`Task.Run` overload not found) and the awaited block lambda binds as `Action`, selecting the non-generic `Task Run` overload and leaving `WriteLine` ambiguous because `t2` remains `Unit`. The delegate inference is still not picking the `Func<Task<T>?>` shape needed for `Task.Run`’s generic overload, so further alignment with C#’s async-lambda delegate selection is required.【853682†L47-L76】

### Findings after step 14
- Updated async-lambda delegate selection and type-argument inference to unwrap nullable task return types when extracting async results, then drive generic inference from the unwrapped results. Despite the improved unwrapping, rerunning `dotnet run --project src/Raven.Compiler -- samples/async/async-inference.rav -bt` still reports `RAV1501` for the awaitless async lambda and `RAV0121` ambiguities for `Task.Run`/`WriteLine`, indicating delegate inference remains stuck on synchronous shapes and the `Task.Run(Func<Task<TResult>?>)` overload is still skipped.【50d5bb†L1-L35】

### Findings after step 15
- Overload resolution now rejects async lambdas for non-task-returning delegates, so async arguments only keep task-shaped candidates during method selection.【F:src/Raven.CodeAnalysis/OverloadResolver.cs†L734-L861】
- Rerunning `async/async-inference.rav` shows the block async lambda now binds to `Task.Run` with a `Func<Task?>` delegate and the `WriteLine` ambiguity clears, but generic inference still fails to pick `Task<TResult> Run<TResult>(Func<Task<TResult>?>)`—`t2` lowers to `Unit` and the async lambda’s return value is converted to `Unit`, leaving the sample incorrect.【15ec4f†L1-L35】
- Executing the reduced sample that only contains the awaited `Task.Run(async () => { await Task.Delay(200); return 42 })` now compiles without overload diagnostics but crashes in codegen with the original `Cannot take address of instance field '<>awaiter6' in a static context` stack trace, confirming state-machine emission issues resurface once overload resolution succeeds.【375045†L1-L60】

### Findings after step 16
- Relaxed async return validation to accept nullable `Task`/`Task<T>` types so nullable task delegates stay eligible for async lambdas during overload and delegate selection.【F:src/Raven.CodeAnalysis/Binder/Binder.cs†L1464-L1475】
- Rerunning `async/async-inference.rav` with `-bt` now keeps the async lambda typed as `Func<Task?>` and still fails `Task.Run` with `RAV1501`, while `t2` remains `Unit` and `WriteLine` stays ambiguous—delegate inference continues to collapse to the non-generic task overload instead of the `Func<Task<int>?>`-based generic overload.【9c16b8†L1-L31】

### Findings after step 17
- Updated lambda delegate selection to favor task-returning delegates (including generic `Task<T>` delegates) and to classify candidates against inferred async results even when initial inference is unavailable.
- Despite preferring the `Task<T>`-shaped delegate in code, the bound-tree dump for `async/async-inference.rav` still shows the awaited lambda bound as `Func<Task?>` and `Task.Run` resolving to the non-generic overload, so generic inference remains blocked and the sample continues to report `RAV1501`/`RAV0121` diagnostics.【de9a27†L55-L79】

### Findings after step 18
- Adjusted async lambda return inference to favor inferred async return types over delegate hints and to strip `Unit` entries from union-based return inference so `return` expressions with values aren’t collapsed to `Task`.
- Re-running `dotnet run --project src/Raven.Compiler -- samples/async/async-inference.rav -bt` still shows the awaited `Task.Run` lambda typed as `Func<Task?>`, and overload resolution continues to reject the generic `Task<TResult> Run<TResult>(Func<Task<TResult>?>)` overload with `RAV1501`/`RAV0121` diagnostics, so delegate inference remains unresolved.【5f5391†L1-L35】

### Findings after step 19
- Extended async lambda return inference to reuse collected return types when the initial inference yields `Unit`/error, feeding the async return calculator with the collected result type before selecting delegates.
- Despite the fallback, rerunning `dotnet run --project src/Raven.Compiler -- samples/async/async-inference.rav -bt` still prints the awaited `Task.Run` lambda as `Func<System.Threading.Tasks.Task?>` and binds the call to the non-generic `Task Run` overload, leaving `t2` as `Unit` and `WriteLine` ambiguous.【7fb426†L9-L35】

### Findings after step 20
- Added an identity conversion between `Unit` and `void` so delegate selection can treat `void`-returning delegates (e.g., `Action`) as compatible when lambda inference yields `Unit`, aligning with IL emission where `Unit` maps to `System.Void`.【F:src/Raven.CodeAnalysis/Compilation.Conversions.cs†L28-L45】
- Re-running `dotnet run --project src/Raven.Compiler -- samples/async/async-inference.rav -bt` still infers the `Task.Run` lambda as `Func<Task?>`, reports `RAV1501` for `Task.Run`, warns that the async lambda lacks awaits, and leaves `t2` as `Unit`, so overload selection remains unresolved despite the `Unit`/`void` equivalence fix.【788f06†L8-L38】【788f06†L39-L48】

### Findings after step 21
- Allowing lambda replay to skip conversions when the delegate return type is a type parameter now keeps generic `Task.Run` overloads viable; the expression-bodied `async () => 42` binds to `Task.Run<TResult>` but infers `TResult=()` while the block async lambda still reports ambiguity and yields an `ErrorExpression` for the awaited call.【f6a597†L5-L40】

### Findings after step 22
- Prioritized collected return-type inference for async lambdas so block-bodied async delegates reuse explicit `return` types when shaping async return kinds, avoiding collapses to `Task` or nested task shapes when returns supply concrete values.
- Added an implicit identity conversion between `Task<Unit>` and `Task` to align async return normalization with the `Unit`↔`void` mapping and ease overload selection for void-returning async delegates.

### Findings after step 23
- Guarded async return inference against double-wrapping task-shaped body types so `async` lambdas keep an existing `Task`/`Task<T>` result instead of inflating to nested tasks during delegate selection.
- Re-running `dotnet run --project src/Raven.Compiler -- samples/async/async-inference.rav -bt` still shows the expression-bodied async lambda bound as `Func<Task<Task.TResult>?>` and the `Task.Run` call choosing the non-generic overload, while the block-bodied async lambda remains an ambiguous `ErrorExpression`, leaving overload resolution unchanged for `t2` and the final `WriteLine`.【ec9ae2†L5-L39】

### Findings after step 24
- Added a fallback in `ReturnTypeCollector` so expression-bodied lambdas use their expression type when no return statements are collected, allowing async expression bodies to infer task-shaped returns for overload resolution.
- Re-running `dotnet run --project src/Raven.Compiler -- samples/async/async-inference.rav -bt` now binds the expression-bodied `async () => 42` argument to the generic `Task.Run<TResult>` overload with `TResult=int`, and `WriteLine` sees `t` as `Int32`. The block-bodied async lambda still reports `RAV0121` on `Task.Run`, leaving `t2` and the final `WriteLine` ambiguous.【47df6f†L7-L41】

### Findings after step 25
- Enabled async-aware return collection in `ReturnTypeCollector` so async lambdas infer task-shaped returns directly, but the `-bt` dump shows the expression-bodied `Task.Run` lambda now bound as `Func<Task<Task.TResult>?>` and the block-bodied async lambda still lands on an ambiguous `ErrorExpression` with `RAV1503`/`RAV0121` diagnostics. Overload resolution remains unresolved for the block lambda despite the task wrapping.【6f6619†L5-L40】

### Findings after step 26
- Routed async lambda return inference through `AsyncReturnTypeUtilities.InferAsyncReturnType` and `ExtractAsyncResultType`, keeping nullable task returns intact and preventing local inference from wrapping already task-shaped bodies.
- The `-bt` dump still shows the expression-bodied `Task.Run(async () => 42)` lambda as `Func<Task<Task.TResult>?>` and now reports `RAV1503` for converting `Task` to `int`, while the block-bodied async lambda remains an ambiguous `ErrorExpression`, indicating delegate inference is still substituting `Task.Run`’s type parameter into the async body despite the nullability-aware helper.【57a61b†L64-L96】

### Findings after step 27
- Adjusted lambda replay in overload inference to reuse the async-shaped return type already computed during binding (via `AsyncReturnTypeUtilities.InferAsyncReturnType`), rather than re-inferring from the raw body, so nullable/task normalization survives generic inference.
- Re-running `dotnet run --project src/Raven.Compiler -- samples/async/async-inference.rav -bt` still shows the expression-bodied `Task.Run(async () => 42)` argument inferred as `Func<Task<Task.TResult>?>` and the block-bodied async lambda as an ambiguous `ErrorExpression`, with diagnostics `RAV0121`/`RAV1503` unchanged, so overload resolution remains unresolved for the `Task.Run` sample.【72d8e4†L5-L40】【72d8e4†L41-L55】

### Findings after step 28
- Switched async lambdas to infer return types from the raw body/collected returns before wrapping them in `Task`/`Task<T>`, preserving the body result type for conversion checks and avoiding double task-wrapping during binding.
- Rerunning `dotnet run --project src/Raven.Compiler -- samples/async/async-inference.rav -bt` now removes the `RAV1503` conversion and leaves only the overload ambiguities: the expression-bodied async lambda is still printed as `Func<Task<Task.TResult>?>`, and the block-bodied async lambda remains an ambiguous `ErrorExpression` with `RAV0121` on both `Task.Run` and `WriteLine`.【a8e7da†L5-L34】【a8e7da†L35-L44】

### Findings after step 29
- Adjusted async lambda return selection to avoid flowing type-parameterized delegate returns directly into the lambda’s return type, instead reusing async inference (body/collected returns) when the target delegate contains type parameters.
- Despite the change, rerunning `dotnet run --project src/Raven.Compiler -- samples/async/async-inference.rav -bt` still shows the expression-bodied `Task.Run` argument printed as `Func<Task<Task.TResult>?>`, and the block-bodied async lambda remains an ambiguous `ErrorExpression` with `RAV0121` diagnostics on both `Task.Run` and `WriteLine`, so overload resolution is still unresolved.【73bc49†L5-L36】【73bc49†L37-L47】

### Findings after step 30
- Preserved ambiguous overload candidates on `BoundErrorExpression` so `-bt` dumps now list the competing `Task.Run` overloads that triggered ambiguity, keeping binder output actionable for overload investigations.【791409†L16-L24】

### Findings after step 31
- Discovered that lambda replay short-circuited async return inference when the bound body was a block typed as `Unit`, so delegate replay never consulted collected return types. Replay now reuses collected returns for `Unit`-typed bodies (including async `ReturnTypeCollector.InferAsync`), keeping block async lambdas shaped like their `return` values instead of `Unit` during overload resolution.

### Findings after step 32
- Async delegate selection now treats type-parameter returns as compatible for async lambdas and prefers them when an async result was already inferred, keeping generic `Task<T>` delegates (e.g., `Task.Run(Func<Task<T>?>)`) eligible instead of discarding them before inference can flow.
- Re-running `async/async-inference.rav` with `-bt` still crashes during emission with `Unable to resolve runtime type for type parameter: TResult`, so the block-bodied `Task.Run` lambda remains ambiguous/unbound even though the generic async overload stays in the candidate set.

### Findings after step 33
- During overload inference, async lambdas that rebound to delegates with type-parameter returns now fall back to async return inference from the lambda body so the return shape includes concrete results (e.g., `Task<int>` from `return 42`).
- Despite the fallback, running `dotnet run --project src/Raven.Compiler -- samples/async/async-inference.rav -bt` still crashes in codegen with `Unable to resolve runtime type for type parameter: TResult`, indicating the block-bodied `Task.Run` lambda continues to flow an unconstrained `TResult` into emission.
