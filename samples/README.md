# Raven samples

## Running a single sample

1. `cd samples`
2. Compile from the sample directory with:

   ```
   dotnet run --project ../src/Raven.Compiler -- <file>.rav -o <file>.dll
   ```

   Use the relative path for nested samples (for example, `dotnet run --project ../src/Raven.Compiler -- async/async-await.rav -o async-await.dll`).
3. Execute the emitted assembly with `dotnet <file>.dll` (or rely on `build.sh`/`run.sh` for batch work).

## Sample compilation and execution status

Re-running every sample from `samples/` with `dotnet run --project ../src/Raven.Compiler -- <file>.rav -o <file>.dll` followed by `dotnet <file>.dll` produced the following results. Successful entries left DLLs under `.status-output/` for later inspection.

| Sample | Status | Notes |
| --- | --- | --- |
| `arrays.rav` | ✅ Emitted / ✅ Ran | Running the DLL prints `3`, `1`, `42`, `3`. |
| `async/async-await.rav` | ✅ Emitted / ✅ Ran | The async sample finishes with `first:1`, `sum:6`, and `done`. |
| `catch.rav` | ✅ Emitted / ✅ Ran | Prints `Foo`. |
| `classes.rav` | ✅ Emitted / ✅ Ran | Prints `Hello`, `John`, the projected record, and the trailing unit value. |
| `collections.rav` | ✅ Emitted / ✅ Ran | Produces the expected hero roster. |
| `enums.rav` | ✅ Emitted / ✅ Ran | Outputs `C`, `Grades`, `B`, `Grades`. |
| `foo.rav` | ✅ Emitted / ✅ Ran | Invocation prints `1`. |
| `function-types.rav` | ✅ Emitted / ✅ Ran | Displays `result = 10`, `chained = 20`, and `combined = 30`. |
| `general.rav` | ✅ Emitted / ✅ Ran | Prints `Hello, World!` followed by `1`, `42`, `3`. |
| `generator.rav` | ✅ Emitted / ✅ Ran | Shows `42` and the odd sequence `3, 5, 7, 9`. |
| `generics/generics.rav` | ✅ Emitted / ✅ Ran | Execution prints `2`, `2`, `3`. |
| `generics/generics2.rav` | ✅ Emitted / ✅ Ran | Running the DLL prints `ok`. |
| `goto.rav` | ✅ Emitted / ⚠️ Not run | Build succeeds but the program would loop forever, so execution is skipped. |
| `interfaces.rav` | ✅ Emitted / ✅ Ran | Runtime output remains `Init`, `Do`, `Dispose 1`. |
| `introduction.rav` | ✅ Emitted / ✅ Ran | Prints `Empty input.` followed by the summary lines. |
| `io.rav` | ✅ Emitted / ⚠️ Requires args | Pass a directory argument (for example `.`) so the program can enumerate files and report the count. |
| `lambda.rav` | ✅ Emitted / ✅ Ran | Shows the captured lambda results and closure state transitions. |
| `linq.rav` | ✅ Emitted / ✅ Ran | Outputs the reversed list `3`, `2`, `1`. |
| `main.rav` | ✅ Emitted / ✅ Ran | Prints the critical value banner and tuple projection. |
| `match.rav` | ✅ Emitted / ✅ Ran | Runtime pattern output stays `Int32`, `String`, `foo`. |
| `parse-number.rav` | ✅ Emitted / ⚠️ Interactive loop | Compilation succeeds but the program waits for console input, so execution is skipped. |
| `pattern-matching.rav` | ✅ Emitted / ✅ Ran | Prints `else`. |
| `reflection.rav` | ✅ Emitted / ✅ Ran | Lists the reflected members without throwing. |
| `string-interpolation.rav` | ✅ Emitted / ✅ Ran | Outputs the Hebrew greeting from `Console.WriteLine`. |
| `test.rav` | ✅ Emitted / ✅ Ran | Prints the lambda totals `7`, `5`, and `5`. |
| `test2.rav` | ✅ Emitted / ✅ Ran | Produces `42`, `Hello, World!`, and `Hello, 2`. |
| `test3.rav` | ❌ Fails | Top-level program synthesis still recurses in `SynthesizedMainMethodSymbol.ResolveReturnType`. |
| `tokenizer.rav` | ⚠️ Hangs | `timeout 3` aborts compilation, indicating the tokenizer still fails to terminate. |
| `try-match.rav` | ✅ Emitted / ⚠️ Input mismatch | Running with the default `'foo'` argument reports the format error and exits. |
| `tuples.rav` | ⚠️ Emitted with warning / ✅ Ran | Compilation warns about the redundant catch-all, and the program prints the tuple projections. |
| `tuples2.rav` | ✅ Emitted / ✅ Ran | Runtime output remains `tuple False foo`. |
| `type-unions.rav` | ✅ Emitted / ⚠️ External dependency | Copy `src/TestDep/bin/Debug/net9.0/TestDep.dll` beside the emitted DLL before running so the union projections print. |
| `unit.rav` | ✅ Emitted / ✅ Ran | Outputs `Hello` and the unit literals. |

**Runtime observations.** The async sample completes, `reflection.rav` now runs to completion, and `type-unions.rav` still requires copying `TestDep.dll` next to the emitted assembly. The interactive samples remain non-turnkey: `io.rav` expects a directory argument, `parse-number.rav` loops waiting for input, `goto.rav` is an intentional infinite loop, and `try-match.rav` reports a format error for its default `'foo'` argument. `tokenizer.rav` still fails to terminate, and `test3.rav` continues to hit the entry-point synthesis recursion.
