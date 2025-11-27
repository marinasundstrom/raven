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
- [ ] **Validate end-to-end**: Re-run the original sample and relevant unit tests to confirm `InvalidProgramException` is resolved and overload resolution behavior remains intact.
- [ ] **Run sample test procedure**: Follow the testing steps in `samples/README.md` to verify sample programs remain intact after async-lambda changes.

### Updated implementation path
- [x] **Extend async rewriter entry points**: Added `AsyncLowerer.Rewrite(SourceLambdaSymbol, …)` plus an `AsyncRewriteResult` wrapper so async-aware callers can flow analysis, state-machine handles, and rewritten bodies for lambdas alongside the existing method overloads.
- [x] **Synthesize per-lambda state machines**: Mirror method state-machine creation by introducing a `RewriteAsyncLambda` helper that produces a `SynthesizedAsyncStateMachineTypeSymbol` per async lambda and returns the rewritten body plus generated `MoveNext`.
- [x] **Integrate into lambda emission**: Update `ExpressionGenerator.EmitLambdaExpression`/`MethodBodyGenerator.EmitLambda` to route async lambdas through the new rewrite hook before IL emission, replacing the current direct emit path for `lambda.IsAsync`.
- [ ] **Plumb closure/hoisted locals**: Ensure lambda state machines reuse captured variables from enclosing closures while owning their own awaiter/storage fields; verify hoisted locals are referenced through the closure parameter instead of outer state-machine fields.
- [ ] **Add execution/regression tests**: Expand `async/async-inference.rav` coverage with IL validation or runtime assertions, plus targeted unit tests that execute nested async lambdas to prevent regressions during implementation.

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
