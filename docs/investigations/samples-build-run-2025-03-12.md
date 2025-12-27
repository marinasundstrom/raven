# Samples build/run investigation (2025-03-12)

## Commands

```bash
scripts/codex-build.sh

cd samples
RAVEN_CORE=../src/Raven.Core/bin/Debug/net9.0/net9.0/Raven.Core.dll bash build.sh
OUTPUT_DIR=output bash run.sh
```

## Results

### Build pre-step

`scripts/codex-build.sh` completed successfully, including Raven.Core emission and a final Raven.Compiler build. This ensured `ravc` was available before running the samples.

### Sample build

`build.sh` completed with 62/78 successful compilations and 16 failures. The failures were:

- `async-test.rav` (pattern/DU errors: RAV2104/RAV0103/RAV1501)
- `extensions.rav` (missing `SumNumbers` symbol)
- `generic-math-error.rav` (constraint error RAV0320)
- `introduction.rav` (operator/convert errors RAV0024/RAV1503)
- `linq.rav` (ambiguous `Where` overload)
- `result.rav` (missing `Ok` overload and `ToString`)
- `test-result.rav` (missing `Ok` overload)
- `test-result2.rav` (missing `Ok` overload)
- `test-result3.rav` (emission crash: null `FieldInfo` in `ExpressionGenerator.EmitAssignmentExpression`)
- `async/async-await.rav` (type mismatch in `+`)
- `async/async-program-main.rav` (missing `UnwrapOrThrow`, DU pattern errors)
- `async/async-task-return.rav` (type conversion error)
- `async/http-client-result-extension.rav` (DU pattern errors, missing members like `EnsureSuccessStatusCode`)
- `async/http-client-result.rav` (DU pattern errors, missing members like `EnsureSuccessStatusCode`)
- `async/http-client.rav` (missing members like `EnsureSuccessStatusCode`)
- `async/try-match-async.rav` (match exhaustiveness errors)

`build.sh` also warned that `Raven.Core.dll` was not found in the expected `net9.0/net9.0` path and could not copy `TestDep.dll` into `samples/output`.

### Sample run

`run.sh` reported 53 successes and 23 failures. The failures fall into three buckets:

- Missing runtimeconfig (`libhostpolicy.so` errors) for several assemblies (e.g., `async-await.dll`, `extensions.dll`, `result.dll`, `test-result*.dll`, `try-match-async.dll`).
- BadImageFormat/TypeLoad errors in async/generator samples (`async-file-io.dll`, `async-generic-compute.dll`, `async-inference.dll`, `async-try-catch.dll`, `generator.dll`, `generics.dll`).
- Missing `TestDep.dll` for `type-unions.dll`.

## Assessment

The Codex build script successfully prepared `ravc` and `Raven.Core.dll`, allowing most samples to compile. Remaining failures are concentrated in a small set of samples with known DU/pattern/type-inference issues and an emission crash in `test-result3.rav`. Runtime failures still include missing runtimeconfig files and async/generator IL issues, plus missing `TestDep.dll` in the output directory.
