# Raven samples

## Running a single sample

1. `cd samples`
2. Compile from the sample directory with:

   ```
   dotnet run --project ../src/Raven.Compiler -- <file>.rav -o <file>.dll
   ```

   Use the relative path for nested samples (for example, `dotnet run --project ../src/Raven.Compiler -- async/async-await.rav -o async-await.dll`).
3. Execute the emitted assembly with `dotnet <file>.dll` (or rely on `build.sh`/`run.sh` for batch work).

### Compiler options

These are the main options available for when debugging the compiler:

* **Print highlighted syntax** - `-d pretty`
* **Print syntax tree** - `-s`
* **Print binder tree** - `-b`
* **Print binder tree and bound tree** - `-bt`

## Sample compilation and execution status

Running `RAVEN_CORE=../src/Raven.Core/bin/Debug/net9.0/net9.0/Raven.Core.dll bash build.sh` produced 64/64 compilation successes (the script copies the referenced Raven.Core.dll into `output/`). Running `OUTPUT_DIR=output bash run.sh` succeeded for all 62 emitted DLLs (skipping only the excluded `goto.dll`, `parse-number.dll`, `Raven.Core.dll`, and `TestDep.dll`).

| Sample | Status | Notes |
| --- | --- | --- |
| `classes.rav` | ✅ Run | Executes successfully when built with `--raven-core` (prints a `Name` report and unit values). |
| `extensions.rav` | ✅ Run | Executes successfully; `CountItems` works as expected. |
| `foo.rav` | ✅ Run | Executes successfully (prints `1`). |
| `general.rav` | ✅ Run | Executes successfully (prints the list contents and "Hello, World!"). |
| `interfaces.rav` | ✅ Run | Executes successfully (shows init/do/dispose output). |
| `introduction.rav` | ✅ Run | Compiles and executes successfully. |
| `io.rav` | ✅ Run | Compiles and runs (expects an argument, otherwise reports zero files). |
| `linq.rav` | ✅ Run | Compiles and runs (prints the reversed list). |
| `main.rav` | ✅ Run | Executes successfully, emitting the critical value report and tuple output. |
| `pattern-matching.rav` | ✅ Run | Compiles and prints `else`. |
| `reflection.rav` | ✅ Run | Compiles and prints the reflected `System.Object` member list. |
| `test-result.rav` | ✅ Run | Compiles and prints union/error handling output when Raven.Core is referenced. |
| `test-result2.rav` | ✅ Run | Compiles and prints parsed value output when Raven.Core is referenced. |
| `test10.rav` | ✅ Run | Compiles and prints `(2, test)`. |
| `test9.rav` | ✅ Run | Compiles and prints `()` once Raven.Core types are available. |
| `try-match.rav` | ✅ Run | Compiles and prints the formatted exception message (`Format invalid: ...`). |
| `tuples.rav` | ✅ Run | Compiles and prints tuple element values. |
| `tuples2.rav` | ✅ Run | Compiles and prints `tuple False foo`. |
| `type-unions.rav` | ✅ Run | Compiles and runs successfully with `TestDep.dll` copied by `build.sh`. |
| `async/try-match-async.rav` | ✅ Run | Compiles and prints the handled exception output when built with Raven.Core. |
