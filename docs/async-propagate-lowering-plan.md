# Async propagate lowering plan

Goal: fix propagation (`try?`) inside async methods so both success and error paths complete correctly, including `using` disposal.

## Progress checklist

- [x] 1. Reset to known baseline commit `5c1891f02b6392a6fa93a8b82237735b44196684` on a new branch.
- [x] 2. Re-establish baseline results (tests + build script when needed).
- [x] 3. Create minimal repro samples for both success and failure paths (with `using`).
- [x] 4. Capture and inspect IL for the repros using `ilspycmd`.
- [ ] 5. Inspect current propagate binding + lowering + async rewriting pipeline.
- [ ] 6. Design lowering rewrite for propagate expressions in async contexts.
- [ ] 7. Implement lowering rewrite in a lowering pass (not codegen).
- [ ] 8. Ensure `using` + disposal works with early-return on propagate failure.
- [ ] 9. Add regression tests (semantic + codegen/runtime).
- [ ] 10. Validate with repro samples and targeted test runs.

## Notes

- The current test baseline includes unrelated failures in `Raven.CodeAnalysis.Testing` and `Raven.Editor.Tests`, and build errors in `DocumentationCommentTriviaTests`.
- We will still run the suite to honor repo instructions, but rely on targeted builds/tests + repro runs for validation.
- On commit `5c1891f0`, the new repro samples bind-fail before IL emission due to `try? await` typing to `Result<..., Exception>` and the binder requiring propagation from a `Result<T, E>` where `T` matches the enclosing result payload. This blocks IL capture of the async propagate issue in this baseline state.
