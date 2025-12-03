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

After adding binder support for arrow-expression bodies, the arrow-heavy samples were rerun from the `samples/` directory with `dotnet run --project ../src/Raven.Compiler -- <file>.rav -o <file>.dll` and executed with `dotnet <file>.dll` where applicable. The table highlights the latest failures observed; rows not mentioned above still reflect the previous ravc sweep. A recent fix to generic member substitution now lets the `List<T>` samples bind and run. Samples that call `UnwrapOrThrow` are expected to throw when the result is an error case.

| Sample | Status | Notes |
| --- | --- | --- |
| `classes.rav` | ✅ Run | Executes successfully; the MethodAccessException in the indexer and invocation operator is resolved. |
| `extensions.rav` | ✅ Run | Executes successfully; the previous `MethodAccessException` in `CountItems` is resolved. |
| `foo.rav` | ✅ Run | Executes successfully (prints `1`) after fixing metadata-generic constraint resolution. |
| `general.rav` | ✅ Run | Executes successfully (prints the List contents and "Hello, World!"). |
| `interfaces.rav` | ✅ Run | Compiles and executes successfully. |
| `introduction.rav` | ✅ Run | Compiles and executes successfully. |
| `io.rav` | ✅ Run | Compiles and runs (expects an argument, otherwise reports zero files). |
| `linq.rav` | ✅ Run | Compiles and runs (prints the reversed list). |
| `main.rav` | ✅ Run | Executes after the conversion fix; tuple element names still print an unexpected `id` value. |
| `pattern-matching.rav` | ✅ Run | Compiles and prints `else`. |
| `reflection.rav` | ❌ Compile | Stack overflow when resolving metadata types. |
| `test-result.rav` | ❌ Run (expected) | Throws `InvalidOperationException` when `UnwrapOrThrow` observes the `Error` case. |
| `test-result2.rav` | ✅ Run | Compiles and executes successfully (prints `23`). |
| `test10.rav` | ✅ Run | Compiles and prints `(2, test)`. |
| `test9.rav` | ❌ Compile | Still rejected with RAV0135 (“Assignment expressions are only allowed as statements”). |
| `try-match.rav` | ❌ Compile | Stack overflow while resolving metadata types. |
| `tuples.rav` | ✅ Run | Compiles and prints tuple element values. |
| `tuples2.rav` | ✅ Run | Compiles and prints `tuple False foo`. |
| `type-unions.rav` | ❌ Compile | Stack overflow while resolving metadata types. |
| `async/try-match-async.rav` | ❌ Compile | Stack overflow while resolving metadata types. |
