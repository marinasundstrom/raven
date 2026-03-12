# Raven samples

## Running a single sample

1. `cd samples`
2. Compile from the sample directory with:

   ```
   dotnet run --project ../src/Raven.Compiler --property WarningLevel=0 -- <file>.rav -o <file>.dll
   ```

   Use the relative path for nested samples (for example, `dotnet run --project ../src/Raven.Compiler  --property WarningLevel=0 -- async/async-await.rav -o async-await.dll`).
3. Execute the emitted assembly with `dotnet <file>.dll` (or rely on `build.sh`/`run.sh` for batch work).

Async sample note:
- `samples/async/async-valuetask.rav` demonstrates `ValueTask` and `ValueTask<T>` in async functions.

## Batch scripts and framework selection

`build.sh` now accepts a target framework flag and defaults to `net10.0`:

```bash
# default
./build.sh

# explicit
./build.sh -f net11.0
```

Equivalent environment override:

```bash
DOTNET_VERSION=net11.0 ./build.sh
```

`run.sh` also accepts the same framework flag and defaults to `net10.0`:

```bash
# default
./run.sh

# explicit
./run.sh -f net11.0
```

## Feature-first sample organization

Feature-based categorization for promoting samples into proper cases:
- `samples/cases/FEATURE_CATALOG.md`
- Control-flow folder examples: `samples/control-flow/for-loop.rav`, `samples/control-flow/for-range.rav`
- Categories are aligned to language spec chapters under `docs/lang/spec/`.

## Project Samples

For `.ravenproj` + NuGet restore/cache behavior, see:

- `samples/projects/nuget-demo/README.md`
- `samples/projects/aspnet-minimal-api/README.md`
- `samples/projects/runtime-async-net11/README.md`
- `samples/projects/analyzer-editorconfig/README.md` (project-local `.editorconfig` analyzer severity overrides)
- `samples/projects/efcore-expression-trees/README.md` (EF Core query + expression-tree progress target)

### Compiler options

These are the main options available for when debugging the compiler:

* **Print highlighted syntax** - `-d pretty`
* **Print tokens as syntax is being parsed** - `-ps`
* **Print syntax tree** - `-s`
* **Print binder tree** - `-b`
* **Print binder tree and bound tree** - `-bt`
* **Print defined symbols** - `--symbols`

## Sample compilation and execution status

Running `RAVEN_CORE=../src/Raven.Core/bin/Debug/net10.0/net10.0/Raven.Core.dll bash build.sh` (which copies the referenced Raven.Core.dll into `output/`) currently produces 36/64 compilation successes and 28 failures, most of which stem from unresolved `Error` types during code generation. Running `OUTPUT_DIR=output bash run.sh` against the successfully emitted DLLs still completes, but the failing samples listed below are skipped.

### Current failure investigation (Raven.Core reference)

The 28 failing samples from the latest run fall into two buckets:

* **Emission crashes caused by `Error` types flowing into code generation** — 22 samples (such as `catch.rav`, `classes.rav`, `extensions.rav`, `foo2.rav`, `function-types.rav`, `generator.rav`, `io.rav`, `result-linq-first-or-error-basic.rav`, `option-basic.rav`, `parse-number.rav`, `reflection.rav`, `patterns/try-expression-match.rav`, `type-unions.rav`, `unit.rav`, `async-file-io.rav`, `async-generic-task-return.rav`, `async-await-inference.rav`, `async-task-return.rav`, `async-try-catch.rav`, `http-client.rav`, `result-parse-static-helper.rav`, and `test10.rav`) abort while emitting because `ErrorTypeSymbol` reaches the back-end. This aligns with the recent short-circuiting changes that preserve `BoundErrorExpression`/`ErrorTypeSymbol` instead of fabricating placeholder bindings, so codegen now needs to tolerate or skip these error-typed members.
* **Front-end diagnostics from invalid discriminated-union/pattern usage** — 6 samples (`cases/ledger-shaping-linq-summary.rav`, `cases/status-ledger-enum-summary.rav`, `result-parse-match.rav`, `async/async-await.rav`, `async/http-client-result-extension.rav`, and `async/http-client-result.rav`) fail with binding diagnostics such as `RAV0024` and `RAV2104`. These predate the short-circuit changes and indicate missing union cases or error operands in the source rather than codegen crashes.

The table below reflects the intended pass status; update entries as failures are fixed.

| Sample | Status | Notes |
| --- | --- | --- |
| `classes.rav` | ✅ Run | Executes successfully when built with `--raven-core` (prints a `Name` report and unit values). |
| `extensions.rav` | ✅ Run | Executes successfully; `CountItems` works as expected. |
| `foo.rav` | ✅ Run | Executes successfully (prints `1`). |
| `general.rav` | ✅ Run | Executes successfully (prints the list contents and "Hello, World!"). |
| `interfaces.rav` | ✅ Run | Executes successfully (shows init/do/dispose output). |
| `cases/ledger-shaping-linq-summary.rav` | ✅ Run | Compiles and executes successfully. |
| `io.rav` | ✅ Run | Compiles and runs (expects an argument, otherwise reports zero files). |
| `result-linq-first-or-error-basic.rav` | ✅ Run | Compiles and runs (prints the selected item wrapped in `Result`). |
| `cases/status-ledger-enum-summary.rav` | ✅ Run | Executes successfully, emitting the critical value report and tuple output. |
| `pattern-matching.rav` | ✅ Run | Compiles and prints `else`. |
| `reflection.rav` | ✅ Run | Compiles and prints the reflected `System.Object` member list. |
| `result-parse-match.rav` | ✅ Run | Compiles and prints union/error handling output when Raven.Core is referenced. |
| `result-parse-static-helper.rav` | ✅ Run | Compiles and prints parsed value output when Raven.Core is referenced. |
| `test10.rav` | ✅ Run | Compiles and prints `(2, test)`. |
| `test9.rav` | ✅ Run | Compiles and prints `()` once Raven.Core types are available. |
| `patterns/try-expression-match.rav` | ✅ Run | Compiles and prints the formatted exception message (`Format invalid: ...`). |
| `tuples/tuples-basic.rav` | ✅ Run | Compiles and prints basic tuple creation + named access output. |
| `tuples/tuples-pattern-match.rav` | ✅ Run | Compiles and prints tuple-pattern match output. |
| `tuples/tuples-nullable-match.rav` | ✅ Run | Compiles and prints tuple-pattern output from optional tuple state. |
| `type-unions.rav` | ✅ Run | Compiles and runs successfully with `TestDep.dll` copied by `build.sh`. |
| `async/async-try-match-expression.rav` | ✅ Run | Compiles and prints the handled exception output when built with Raven.Core. |

## Case strategy

For sample deduplication and one-file-per-concept guidance, see:
- `samples/cases/CASE_STRATEGY.md`
