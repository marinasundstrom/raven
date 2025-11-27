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

## Investigation plan
- [x] **Trace async lambda lowering entry point**: Walk `AsyncLowerer.VisitLambdaExpression` (and related visitors) to confirm whether async lambdas trigger a nested lowering pipeline or are inlined into the outer method’s state machine.
- [x] **Inspect state machine synthesis for lambdas**: Follow `MethodBodyGenerator.EmitLambda` → `EmitLambdaBody` → `AsyncStateMachineRewriter` (or equivalent) to see when a lambda is considered async and whether a new `SynthesizedAsyncStateMachineTypeSymbol` is created per lambda.
- [ ] **Map awaiter field ownership**: Identify the code that allocates awaiter fields/locals for async lambdas. Verify whether those fields live on the outer state machine or should be scoped to a lambda-specific machine.
- [ ] **Design per-lambda state machine creation**: Outline the required API changes so lambdas invoke the async rewriting pipeline independently (similar to methods), ensuring the rewritten lambda body no longer depends on the parent machine’s fields.
- [ ] **Audit closure interactions**: Ensure closure structs/classes continue to carry captured locals while async state machines for lambdas only own awaiters and builder/state fields, matching C# behavior for captured variables.
- [ ] **Add regression coverage**: Create codegen tests for nested async lambdas in `Task.Run` and nested delegate scenarios, asserting valid IL execution (e.g., via `RecordingILBuilderFactory` or running emitted assembly) and preventing ambiguous overload regressions.
- [ ] **Validate end-to-end**: Re-run the original sample and relevant unit tests to confirm `InvalidProgramException` is resolved and overload resolution behavior remains intact.

### Findings after step 1
- `LambdaLowerer.Rewrite` only rewrites await-less async lambdas via `AsyncLowerer.RewriteAwaitlessLambdaBody`; async lambdas that contain awaits are merely lowered for control flow/closures without entering the async state-machine pipeline. This means the async rewriter never creates a state machine for a lambda body that awaits.
- `AsyncLowerer.Rewrite` is invoked for async methods and top-level statements during code generation, but lambdas never call into it, so the async state machine creation path is currently method-only.

### Findings after step 2
- `ExpressionGenerator.EmitLambdaExpression` constructs a `MethodGenerator` for each lambda and immediately emits IL for the lambda body via `MethodGenerator.EmitLambdaBody` without checking `lambda.IsAsync` or invoking `AsyncLowerer`. No async analysis or rewriting happens along this path.
- `MethodBodyGenerator.EmitLambda` emits the lambda body directly (block or return statement) with no async rewriting phase and no `SynthesizedAsyncStateMachineTypeSymbol` creation. Awaiter locals are never projected into a lambda-specific state machine, explaining why awaits currently reuse the enclosing method’s fields.
