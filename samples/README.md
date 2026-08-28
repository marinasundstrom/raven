# Raven samples

## Running a single sample

Build the tools and load the repository-local commands:

```bash
dotnet build src/Raven/Raven.csproj -f net10.0
dotnet build src/Raven.Compiler/Raven.Compiler.csproj -f net10.0
source scripts/raven-env.sh
```

Then run a standalone Raven file directly:

```bash
rvn run samples/scripts/hello.rvn -- Raven
rvn samples/scripts/hello.rvn -- Raven
```

See [`samples/scripts/README.md`](scripts/README.md) for the focused file-based
application example.

For direct compiler-driver work:

```bash
cd samples
dotnet run --project ../src/Raven.Compiler --property WarningLevel=0 -- <file>.rvn -o <file>.dll
dotnet <file>.dll
```

Use the relative path for nested samples (for example,
`async/async-await.rav`). The batch `build.sh` and `run.sh` scripts remain
available for broad sample checks.

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

`build.sh` writes per-sample compile timing reports after every run:

- `samples/output/<tfm>/build-report.tsv`
- `samples/output/<tfm>/build-report.md`

Use these reports to spot cold one-shot compile regressions and compare timing
between feature areas without scraping terminal output.

## Feature-first sample organization

Feature-based categorization for promoting samples into proper cases:
- `samples/cases/FEATURE_CATALOG.md`
- Control-flow folder examples: `samples/control-flow/for-loop.rav`, `samples/control-flow/for-range.rav`
- Categories are aligned to language spec chapters under `docs/lang/spec/`.

## Project Samples

For `.rvnproj` + NuGet restore/cache behavior, see:

- `samples/projects/nuget-demo/README.md`
- `samples/projects/aspnet-minimal-api/README.md`
- `samples/projects/myservicebus-rabbitmq/README.md` (NuGet messaging interop,
  dependency-injected Raven consumers, RabbitMQ publish/subscribe and
  request/response, plus a Docker broker helper)
- `samples/projects/runtime-async-net11/README.md`
- `samples/projects/conditional-compilation/README.md` (`DefineConstants`,
  nested conditional branches, and VS Code inactive-code highlighting)
- `samples/projects/analyzer-editorconfig/README.md` (project-local `.editorconfig` analyzer severity overrides)
- `samples/projects/custom-analyzer/README.md` (custom diagnostics loaded through an `<Analyzer>` project item)
- `samples/projects/source-generator/README.md` (generated Raven source loaded through a `<SourceGenerator>` project item)
- `samples/projects/markdown-docs/README.md` (Markdown-first comments, editor highlighting, and default Markdown/XML projections)
- `samples/projects/syntax-tree-api/README.md` (parse and inspect Raven syntax trees from Raven code)
- `samples/projects/syntax-quoter/README.md` (generate Raven SyntaxFactory code from Raven source)
- `samples/projects/macro-declarations/README.md` (author a same-project macro with `macro`)
- `samples/projects/macro-capabilities/README.md` (attach editor capabilities to a macro through adjacent namespace functions)
- `samples/projects/macro-dsl/README.md` (minimal token DSL with an embedded Raven expression and source mapping)
- `samples/projects/proto-actor-dsl/README.md` (Proto.Actor POC with a Swift-like Raven actor declaration macro)
- `samples/projects/macro-token-stream/README.md` (replace Raven's macro token stream with a custom lexer)
- `samples/projects/macro-quote/README.md` (construct another macro's expansion with `quote!`)
- `samples/projects/error-macro/README.md` (derive the standard `IError` contract for a union)
- `samples/projects/embed-file-content/README.md` (embed a UTF-8 file as a compile-time string)
- `samples/projects/sha256-digest/README.md` (compute a SHA-256 digest during compilation)
- `samples/projects/timer-macro/README.md` (expand a Raven block into `Stopwatch` instrumentation)
- `samples/projects/data-literal-macros/README.md` (construct JSON and XML platform values with Raven expression splices)
- `samples/projects/efcore-expression-trees/README.md` (EF Core query + expression-tree progress target)
- `samples/projects/greenhouse-monitor/README.md` (simulated IoT telemetry,
  unions and patterns, and reproducible Native AOT publishing for macOS or
  Linux Arm64 devices such as Raspberry Pi)
- `samples/projects/mfrc522-rfid/README.md` (MFRC522 RFID polling over SPI,
  RGB status output, typed reader failures, unions, records, and Raspberry Pi
  wiring)
- `samples/projects/nanoframework-temperature/README.md` (DHT sensor state,
  exhaustive union patterns, GPIO output, and nanoFramework `NFMRK2` packaging)
- `samples/projects/nanoframework-dht22-display/README.md` (isolated GP2 DHT22
  polling and SH1106 temperature display for Pico W hardware diagnostics)
- `samples/projects/nanoframework-blinky/README.md` (minimal Raspberry Pi
  Pico-family GPIO blink, board profiles, nanoFramework packaging and deployment
  commands, and the current RP2350 firmware boundary)
- `samples/projects/nanoframework-wifi-http/README.md` (Pico W Wi-Fi connection,
  synchronous HTTP request, external GPIO success indicator, and credential-aware
  deployment)

### Source file naming and organization

Raven project samples follow the same organization expected of ordinary Raven
projects:

- Use `Main.rvn` only when the file contains the `Main` entry point by itself.
- Use `Program.rvn` for top-level statements or a program file that encompasses
  multiple functions or types, including its `Main` entry point.
- Use `<Type>.rvn` for a file centered on one primary type. Closely related
  enums, unions, helper types, and functions may remain beside it when that
  primary type clearly owns the concept.
- Use `<area-name>.rvn` for a related collection of types and functions, similar
  to a module or domain area.
- Use `<function>.rvn` for a function that deserves its own file.
- As a sample grows, extract types, functions, or domain areas into appropriately
  named files, especially when the organization itself is part of what the
  sample demonstrates.

The convention is meant to make intent visible, not to require one file per
type in every small example.

### Project build workflow

Project samples should build and run through the .NET SDK:

```bash
dotnet build samples/projects/hello-world/HelloWorld.rvnproj --property WarningLevel=0
dotnet run --project samples/projects/hello-world/HelloWorld.rvnproj --property WarningLevel=0
```

To build the project sample set, use:

```bash
scripts/build-project-samples.sh
```

This is separate from `samples/build.sh`, which compiles the standalone
`.rav` and `.rvn` samples outside `samples/projects`.

With `scripts/raven-env.sh` sourced, the equivalent frontend commands are:

```bash
rvn build samples/projects/hello-world/HelloWorld.rvnproj --property WarningLevel=0
rvn run samples/projects/hello-world/HelloWorld.rvnproj --property WarningLevel=0
rvn clean samples/projects/hello-world/HelloWorld.rvnproj --property WarningLevel=0
```

Use `rvnc` only for direct compiler-driver checks against individual files or
project files. Use `rvn dev` for compiler debug views:

```bash
rvn dev syntax samples/cases/quote-summary-linq-result-option.rav
rvn dev bound-tree samples/cases/quote-summary-linq-result-option.rav
rvn dev symbols samples/cases/quote-summary-linq-result-option.rav
```

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
