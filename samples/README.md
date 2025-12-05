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

Running `bash build.sh` (which now passes `--raven-core` when `Raven.Core.dll` is available) produced 61 successes and 3 failures (`test-result.rav`, `test-result2.rav`, and `test3.rav`). `bash run.sh` succeeded for 37 of the emitted DLLs; the remaining executions failed with `System.Unit` type load errors in the generated option/result assemblies, access-reduction `TypeLoadException`s on generated `ToString` overrides in discriminated union cases, or a missing `TestDep.dll` dependency for `type-unions.rav`.

| Sample | Status | Notes |
| --- | --- | --- |
| `classes.rav` | ❌ Run | Fails at runtime with a `System.Unit` type load error in the generated option assembly. |
| `extensions.rav` | ✅ Run | Executes successfully; the previous `MethodAccessException` in `CountItems` is resolved. |
| `foo.rav` | ✅ Run | Executes successfully (prints `1`) after fixing metadata-generic constraint resolution. |
| `general.rav` | ✅ Run | Executes successfully (prints the List contents and "Hello, World!"). |
| `interfaces.rav` | ❌ Run | Fails at runtime with a `System.Unit` type load error in the generated option assembly. |
| `introduction.rav` | ✅ Run | Compiles and executes successfully. |
| `io.rav` | ✅ Run | Compiles and runs (expects an argument, otherwise reports zero files). |
| `linq.rav` | ✅ Run | Compiles and runs (prints the reversed list). |
| `main.rav` | ✅ Run | Executes after the conversion fix; tuple element names still print an unexpected `id` value. |
| `pattern-matching.rav` | ✅ Run | Compiles and prints `else`. |
| `reflection.rav` | ✅ Run | Compiles and prints the reflected `System.Object` member list. |
| `test-result.rav` | ❌ Compile | Emission fails with an unsupported union conversion in `parseNumber`. |
| `test-result2.rav` | ❌ Compile | Emission fails with an unsupported union conversion in `IntHelpers.ParseNumber`. |
| `test10.rav` | ✅ Run | Compiles and prints `(2, test)`. |
| `test9.rav` | ❌ Run | Fails at runtime with a `System.Unit` type load error in the generated option assembly. |
| `try-match.rav` | ✅ Run | Compiles and prints the formatted exception message (`Format invalid: ...`). |
| `tuples.rav` | ✅ Run | Compiles and prints tuple element values. |
| `tuples2.rav` | ✅ Run | Compiles and prints `tuple False foo`. |
| `type-unions.rav` | ❌ Run | Fails at runtime without `TestDep.dll` beside the output assembly. |
| `async/try-match-async.rav` | ❌ Run | Fails at runtime with a `System.Unit` type load error in the generated option assembly. |
