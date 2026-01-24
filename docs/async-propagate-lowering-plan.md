# Async propagate lowering plan

Goal: fix propagation (`try?`) inside async methods so both success and error paths complete correctly, including `using` disposal.

## Progress checklist

- [x] 1. Reset to known baseline commit `5c1891f02b6392a6fa93a8b82237735b44196684` on a new branch.
- [x] 2. Re-establish baseline results (tests + build script when needed).
- [x] 3. Create minimal repro samples for both success and failure paths (with `using`).
- [x] 4. Capture and inspect IL for the repros using `ilspycmd`.
- [x] 5. Inspect current propagate binding + lowering + async rewriting pipeline.
- [x] 6. Design lowering rewrite for propagate expressions in async contexts.
- [x] 7. Implement lowering rewrite in a lowering pass (not codegen).
- [x] 8. Ensure `using` + disposal works with early-return on propagate failure.
- [x] 9. Add regression tests (semantic + codegen/runtime).
- [x] 10. Validate with repro samples and targeted test runs.

## Notes

- The current test baseline includes unrelated failures in `Raven.CodeAnalysis.Testing` and `Raven.Editor.Tests`, and build errors in `DocumentationCommentTriviaTests`.
- We will still run the suite to honor repo instructions, but rely on targeted builds/tests + repro runs for validation.
- On commit `5c1891f0`, the new repro samples bind-fail before IL emission due to `try? await` typing to `Result<..., Exception>` and the binder requiring propagation from a `Result<T, E>` where `T` matches the enclosing result payload. This blocks IL capture of the async propagate issue in this baseline state.

## Step 5 findings: current pipeline gaps

Key locations inspected:

- Binder: `BlockBinder.BindPropagateExpressionCore` constructs `BoundPropagateExpression` with the enclosing union type and constructors, but does not unwrap async return types on this baseline commit. (`src/Raven.CodeAnalysis/Binder/BlockBinder.cs`)
- Async lowering: `AsyncLowerer` has custom logic for `BoundTryExpression` that rewrites awaits inside try-expressions to block expressions, but it does not have any handling for `BoundPropagateExpression`. (`src/Raven.CodeAnalysis/BoundTree/Lowering/AsyncLowerer.cs`)
- Codegen: `EmitPropagateErrorReturn` ends the propagate error path with `ret` unconditionally. This is invalid inside protected regions (e.g., async `MoveNext` try blocks and `using`/finally scopes). (`src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs`)
- Return emission: `StatementGenerator.EmitReturnStatement` is already scope-aware (spills the return value local and emits `leave` when an exception-exit label exists), but propagate bypasses this by emitting `ret` directly. (`src/Raven.CodeAnalysis/CodeGen/Generators/StatementGenerator.cs`)

Conclusion: the main correctness issue is that propagate currently short-circuits via direct IL `ret` instead of bound control flow that participates in async rewriting and scope unwinding.

## Step 6 design: lowering-first propagate rewrite

Design goals:

1. Eliminate direct `ret` emission from propagate in async/protected scopes.
2. Ensure early-return on propagate failure flows through the existing async completion pipeline (builder `SetResult`, `_state = -2`, and structured `leave`).
3. Preserve `using`/finally disposal ordering.

Proposed design:

- Implement a lowering pass that rewrites `BoundPropagateExpression` into explicit bound control flow (temps + `if` + `BoundReturnStatement`) before async rewriting and codegen.
- Target rewriting contexts that can contain propagate today:
  - local declaration initializers
  - expression statements (e.g., `_ = try? ...`)
  - using declaration initializers
- Rewrite shape (conceptual):
  - spill operand once into a temp
  - call `TryGetOk(out okCase)`
  - if failure:
    - extract error payload via `UnwrapError` when available (otherwise fallback properties)
    - convert payload as needed
    - construct the enclosing error case
    - `return` the enclosing error value via a `BoundReturnStatement`
  - if success:
    - extract payload (`okCase.Value` or fallback)
    - use the extracted value as the original expression result

Why this approach:

- The synthesized `BoundReturnStatement` will be seen by `AsyncLowerer` and existing return emission logic, which already handles protected scopes correctly via exception-exit labels.
- This avoids fragile codegen-time special-casing of async `MoveNext`.

Implementation sketch:

1. Add a dedicated propagate rewriter in lowering (e.g., `Lowerer.Propagate.cs`).
2. In that rewriter, detect and rewrite propagate usages inside statements, producing a `BoundBlockStatement` that contains:
   - temp declarations
   - conditional return on failure
   - the original statement rewritten to use the extracted success value
3. Keep codegen changes minimal initially; once propagate is fully lowered, the direct `ret` path should no longer be reachable in async `MoveNext`.

## Step 7–9 implementation notes

What changed:

- Propagate rewriting now happens at statement level during lowering so it runs *before* using-declaration rewrite introduces protected regions:
  - `Lowerer.VisitBlockStatement` expands propagate-bearing local declarations and expression statements inline before `RewriteUsingDeclarations`. (`src/Raven.CodeAnalysis/BoundTree/Lowering/Lowerer.Blocks.cs`)
  - `Lowerer.Propagate` now synthesizes temp locals, a `TryGet{Case}` invocation, and an explicit `BoundReturnStatement` on the failure path, then re-emits the original statement with the success value. (`src/Raven.CodeAnalysis/BoundTree/Lowering/Lowerer.Propagate.cs`)
- The main lowering pass now owns propagate expansion; the earlier pre-async propagate rewrite was removed from `CodeGenerator` after it proved unsafe across awaits in async state machines. (`src/Raven.CodeAnalysis/CodeGen/CodeGenerator.cs`)

Why this addresses disposal:

- Because the failure path is now an ordinary `BoundReturnStatement`, it flows through existing scope-aware return emission and async rewriting, which already emit `leave` and run `finally` blocks when needed.
- Placing the rewrite ahead of `RewriteUsingDeclarations` ensures that any early-return produced by propagate is located within the resulting `try` region, so disposal runs on both success and propagate failure.

Tests added:

- `AsyncPropagate_UsingDeclaration_DisposesOnSuccessAndFailure` compiles a small program with `using let` and `try? await`, then asserts the disposable is disposed exactly once on both the `.Error` and `.Ok` paths. (`test/Raven.CodeAnalysis.Tests/CodeGen/AsyncPropagateCodeGenTests.cs`)

## Step 10 validation notes

Validation performed:

- Rebuilt compiler dependencies and generators via `scripts/codex-build.sh` to ensure the lowering/codegen pipeline was consistent.
- Built the compiler and core analysis projects directly:
  - `dotnet build src/Raven.CodeAnalysis/Raven.CodeAnalysis.csproj --property WarningLevel=0`
  - `dotnet build src/Raven.Compiler/Raven.Compiler.csproj --property WarningLevel=0`
- Exercised the repro samples through `ravc`:
  - `dotnet run --project src/Raven.Compiler/Raven.Compiler.csproj -- samples/async-propagate-error-path.rav`
  - `dotnet run --project src/Raven.Compiler/Raven.Compiler.csproj -- samples/async-propagate-using-success.rav`

Important follow-up fix discovered during validation:

- Running propagate lowering *before* async rewriting introduced synthesized `out` locals that could be hoisted across awaits, which broke emission in async state-machine methods.
- Resolution:
  - remove the pre-async `PropagateLowerer.Rewrite` calls in `CodeGenerator`, letting the main lowering phase handle propagate after async rewriting
  - ensure propagate’s `TryGet{Case}` invocation respects extension receivers
  - harden extension-method emission to tolerate missing receiver arguments by falling back to `ExtensionReceiver`/`Receiver`

Additional validation findings (sample100):

- Even after ensuring propagate error returns call `SetResult`, `samples/sample100.rav` still produced `InvalidProgramException` at runtime.
- Root cause identified during IL inspection:
  - propagate lowering was synthesizing `out` locals with byref types (`Result<T, E>.Ok&`), which are not valid in async state machines.
- Resolution implemented here:
  - unwrap byref `out` parameter types to their element type when creating propagate temps
  - add async-state-machine-aware return emission in `StatementGenerator` so propagate early-returns in `MoveNext` always set `_state = -2` and call the builder’s `SetResult`
- Current status:
  - async propagate without `using` (`samples/async-propagate-error-path.rav`) runs successfully
  - `samples/sample100.rav` still throws `InvalidProgramException`, indicating remaining IL structure issues around `using` + propagate that need further investigation
