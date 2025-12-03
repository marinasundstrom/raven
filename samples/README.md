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

After adding binder support for arrow-expression bodies, the arrow-heavy samples were rerun from the `samples/` directory with `dotnet run --project ../src/Raven.Compiler -- <file>.rav -o <file>.dll` and executed with `dotnet <file>.dll` where applicable. The table highlights the latest failures observed; rows not mentioned above still reflect the previous ravc sweep.

| Sample | Status | Notes |
| --- | --- | --- |
| `classes.rav` | ⚠️ Run | Compiles, but running `/tmp/classes.dll` raises `MethodAccessException` when invoking the indexer. |
| `extensions.rav` | ⚠️ Run | Compiles, but running `/tmp/extensions.dll` hits `MethodAccessException` while calling `CountItems`. |
| `foo.rav` | ❌ Compile | Stack overflow in metadata type resolution during generic constraint projection. |
| `general.rav` | ❌ Compile | Stack overflow while resolving metadata type names. |
| `interfaces.rav` | ✅ Run | Compiles and executes successfully. |
| `introduction.rav` | ❌ Compile | Stack overflow when resolving metadata types. |
| `io.rav` | ❌ Compile | Stack overflow when resolving metadata types. |
| `linq.rav` | ❌ Compile | Stack overflow while resolving metadata types. |
| `main.rav` | ❌ Compile | Null reference in `Compilation.ClassifyConversion` while binding locals. |
| `pattern-matching.rav` | ❌ Compile | Null reference while validating match arm conversions. |
| `reflection.rav` | ❌ Compile | Stack overflow when resolving metadata types. |
| `test-result.rav` | ❌ Compile | Stack overflow while resolving metadata types during emission. |
| `test-result2.rav` | ❌ Compile | Stack overflow while resolving metadata types. |
| `test10.rav` | ❌ Compile | Null reference in `Compilation.ClassifyConversion` while binding locals. |
| `test9.rav` | ❌ Compile | Still rejected with RAV0135 (“Assignment expressions are only allowed as statements”). |
| `try-match.rav` | ❌ Compile | Stack overflow while resolving metadata types. |
| `tuples.rav` | ❌ Compile | Null reference in `Compilation.ClassifyConversion` while binding locals. |
| `tuples2.rav` | ❌ Compile | Null reference in `Compilation.ClassifyConversion` while binding locals. |
| `type-unions.rav` | ❌ Compile | Stack overflow while resolving metadata types. |
| `async/try-match-async.rav` | ❌ Compile | Stack overflow while resolving metadata types. |
