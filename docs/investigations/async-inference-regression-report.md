# Async inference regression investigation

## Context
The `samples/async/async-inference-regression.rav` sample still emits a runnable DLL that crashes at runtime after the async/await pipeline fails to produce a valid task for the awaited `Task.Run` lambda. The goal of this investigation is to pinpoint where the compiler diverges from C# semantics for async lambdas without `await` and to outline a plan to align Raven’s lowering with the C# behavior (warning plus `Task.FromResult`/`Task.CompletedTask` rewrite).

## Observations
- The bound tree for the sample shows `Task.Run` being bound to the non-generic overload, the async lambda being typed as `Func<Task>` with a raw `42` literal body, and the awaited variable `t` being typed as `Task`. No wrapper rewrites occur, so `WriteLine` ultimately receives the task object instead of the integer result.【e93a8d†L32-L47】
- `AsyncLowerer.Analyze` only requests state-machine rewriting when an `await` is present; async methods (including synthesized `Main`) that lack `await` return the original body unchanged, so no `Task.FromResult`/`Task.CompletedTask` is injected for return sites.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L15-L43】
- The async lowerer explicitly skips lambdas, so async lambdas never receive state-machine or return-shape rewriting even when the enclosing method is rewritten. They keep their literal bodies and the async return type chosen by lambda binding.【F:src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs†L2236-L2239】
- Lambda binding computes async return types from the body type and delegate targets (for example, inferring `Task<T>` from `T`), but it only applies conversions to the *result* type (`T`) while leaving the body as-is. When no `await` appears later in lowering, the generated method still returns the raw expression even though its signature promises `Task`/`Task<T>`.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Lambda.cs†L192-L338】

## How C# behaves
C# emits warning `CS1998` when an async lambda or method contains no `await`. The compiler still returns a `Task`/`Task<T>` by wrapping expression-bodied returns in `Task.FromResult` (or `Task.CompletedTask` for `Task`/`void` shapes) and by avoiding state-machine synthesis. Async lambdas follow the same pattern so overload resolution can pick the correct generic `Task.Run` overload and the runtime sees a completed task that carries the expression result.

## Progress
- Added warning `RAV2706` to flag async methods, local functions, and lambdas that contain no `await`, matching C#’s `CS1998`-style guidance while keeping compilation successful.
- Introduced an await-less async lowering path that rewrites returns to `Task.FromResult`/`Task.CompletedTask` without synthesizing a state machine and applied it to async lambdas during lambda lowering.

## Proposed plan
- [x] **Detect await-less async bodies**: Extend async analysis to flag async methods, local functions, and lambdas with zero `await` occurrences, issuing a redundant-async diagnostic akin to `CS1998` while keeping compilation successful.
- [x] **Synchronous async lowering path**: Add a lowering pass that, when async bodies lack `await`, bypasses state-machine synthesis and rewrites every return expression (including expression-bodied members) to `Task.FromResult(<expr>)` or `Task.CompletedTask` according to the async return shape. Ensure this runs for async lambdas as well as methods.
- [x] **Integrate with lambda lowering**: Route async lambdas through the same await-less rewrite so their bodies produce completed tasks, enabling overload resolution to prefer `Task.Run<T>` and eliminating invalid casts/`NullReferenceException` at runtime.
- [ ] **Validation**: Rebuild and run `samples/async/async-inference-regression.rav`, decompile the emitted IL to confirm the lambda returns `Task.FromResult(42)`, and add regression coverage (unit test or sample harness) that asserts async lambdas without `await` return completed tasks instead of raw values.
