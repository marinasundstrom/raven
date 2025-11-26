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

Re-running every sample from `samples/` with `dotnet run --project ../src/Raven.Compiler -- <file>.rav -o <file>.dll` followed by `dotnet <file>.dll` produced the following results. Successful entries left DLLs under `output/` for later inspection.

| Sample | Status | Notes |
| --- | --- | --- |
| `arrays.rav` | ✅ Emitted / ✅ Ran | Running the DLL prints `3`, `1`, `42`, `3`. |
| `async/async-await.rav` | ✅ Emitted / ✅ Ran | Finishes with `first:1`, `sum:6`, and `done`. |
| `async/async-file-io.rav` | ✅ Emitted / ✅ Ran | Runtime output lists the staged writes `alpha`, `beta`, `gamma`. |
| `async/async-generic-compute.rav` | ✅ Emitted / ❌ Ran | Crashes with an `InvalidProgramException` while unwrapping the async generic result; the async state machine regressed after previously working. |
| `async/async-inference-regression.rav` | ❌ Fails | Binder errors persist (`Run` overload count, `int` vs `unit`, ambiguous `WriteLine`); still never worked. |
| `async/async-task-return.rav` | ✅ Emitted / ✅ Ran | Prints the awaited task result `43`. |
| `async/async-try-catch.rav` | ✅ Emitted / ✅ Ran | Logs `value:42`, `caught:boom`, and `completed`. |
| `async/http-client-result.rav` | ❌ Fails | Target-typed binding for `Ok`/`Error` on `Result<string>` is missing, so the `Task<Result<string>>` return does not compile. |
| `async/http-client.rav` | ✅ Emitted / ⚠️ HTTP dependency | Execution attempts an external request and exits with a 403 in the sandbox. |
| `async/try-match-async.rav` | ❌ Fails | Binding still reports `Cannot convert from '0' to 'Task'` due to a longstanding inference issue, so no runnable DLL. |
| `catch.rav` | ✅ Emitted / ✅ Ran | Prints `Foo`. |
| `classes.rav` | ✅ Emitted / ✅ Ran | Prints `Hello`, `John`, the projected record, and the trailing unit value. |
| `collections.rav` | ✅ Emitted / ✅ Ran | Produces the expected hero roster. |
| `discard.rav` | ✅ Emitted / ✅ Ran | Prints `Test` twice to demonstrate discards. |
| `discriminated-unions-generics.rav` | ✅ Emitted / ✅ Ran | Prints `ok 99`, `error 'boom'`, `ok 42`. |
| `discriminated-unions-generics2.rav` | ✅ Emitted / ✅ Ran | Emits `ok 99` followed by `error 'boom'`. |
| `discriminated-unions-generics3.rav` | ⚠️ Missing | File is not present in the repository. |
| `discriminated-unions.rav` | ✅ Emitted / ✅ Ran | Prints `identifier 'alpha'`, `number 42`, `unknown token`. |
| `discriminated-unions2.rav` | ❌ Fails | RAV1011 errors complain about file-scope code appearing after declarations; could compile if statements preceded the function and the union. |
| `enums.rav` | ✅ Emitted / ✅ Ran | Outputs `C`, `Grades`, `B`, `Grades`. |
| `extensions.rav` | ✅ Emitted / ✅ Ran | Outputs `Count: 2`, `Sum: 3`, `Value: 4`. |
| `foo.rav` | ✅ Emitted / ✅ Ran | Invocation prints `1`. |
| `foo2.rav` | ✅ Emitted / ✅ Ran | Prints `4`. |
| `func-references.rav` | ✅ Emitted / ✅ Ran | Shows `Foo 3`. |
| `function-types.rav` | ✅ Emitted / ✅ Ran | Displays `result = 10`, `chained = 20`, and `combined = 30`. |
| `general.rav` | ✅ Emitted / ✅ Ran | Prints `Hello, World!` followed by `1`, `42`, `3`. |
| `generator.rav` | ✅ Emitted / ✅ Ran | Shows `42` and the odd sequence `3, 5, 7, 9`. |
| `generics/generics.rav` | ✅ Emitted / ✅ Ran | Execution prints `2`, `2`, `3`. |
| `generics/generics2.rav` | ✅ Emitted / ✅ Ran | Running the DLL prints `ok`. |
| `generics/nested-classes.rav` | ✅ Emitted / ✅ Ran | Completes without console output after exercising nested generic types. |
| `goto.rav` | ✅ Emitted / ⚠️ Not run | Build succeeds but the program would loop forever, so execution is skipped. |
| `if-in-function.rav` | ✅ Emitted / ✅ Ran | Evaluates the nested function and prints `0`. |
| `interfaces.rav` | ✅ Emitted / ✅ Ran | Runtime output remains `Init`, `Do`, `Dispose 1`. |
| `introduction.rav` | ✅ Emitted / ✅ Ran | Prints `Empty input.` followed by the summary lines. |
| `io.rav` | ✅ Emitted / ✅ Ran | Ran with `.` to list the sample directory contents; running without arguments still prints the usage banner. |
| `lambda.rav` | ✅ Emitted / ✅ Ran | Shows the captured lambda results and closure state transitions. |
| `linq.rav` | ✅ Emitted / ✅ Ran | Outputs the reversed list `3`, `2`, `1`. |
| `main.rav` | ✅ Emitted / ✅ Ran | Prints the critical value banner and tuple projection. |
| `match.rav` | ✅ Emitted / ✅ Ran | Runtime pattern output stays `Int32`, `String`, `foo`. |
| `option.rav` | ✅ Emitted / ❌ Ran | `UnwrapOrThrow` throws `InvalidOperationException: Option is None` as intended. |
| `parse-number.rav` | ✅ Emitted / ⚠️ Interactive loop | Compilation succeeds but the program waits for console input, so execution is skipped. |
| `pattern-matching.rav` | ✅ Emitted / ✅ Ran | Prints `else`. |
| `pipe-operator.rav` | ✅ Emitted / ✅ Ran | Produces `Result: 7`. |
| `pipe-operator2.rav` | ✅ Emitted / ✅ Ran | Prints `Result: 7`. |
| `pipe-operator3.rav` | ✅ Emitted / ✅ Ran | Emits `42`. |
| `reflection.rav` | ✅ Emitted / ✅ Ran | Lists the reflected members (for example `System.Object`, `Equals`, `ToString`). |
| `result.rav` | ✅ Emitted / ❌ Ran | `UnwrapOrThrow` throws `InvalidOperationException: Wrong number` as intended. |
| `shadowing.rav` | ⚠️ Emitted with warning / ✅ Ran | Compilation warns that `a` shadows a previous declaration; runtime emits no output. |
| `string-interpolation.rav` | ✅ Emitted / ✅ Ran | Outputs the Hebrew greeting from `Console.WriteLine`. |
| `test.rav` | ✅ Emitted / ✅ Ran | Prints the lambda totals `7`, `5`, and `5`. |
| `test2.rav` | ✅ Emitted / ✅ Ran | Produces `42`, `Hello, World!`, and `Hello, 2`. |
| `test3.rav` | ✅ Emitted / ✅ Ran | Completes without console output. |
| `test4.rav` | ✅ Emitted / ✅ Ran | Completes silently after exercising property override compatibility. |
| `test5.rav` | ✅ Emitted / ✅ Ran | Prints `Foo: Hejtest 2 3`. |
| `test9.rav` | ❌ Fails | Now rejects the assignment expression with RAV0135; this is expected after the behavior change. |
| `test10.rav` | ✅ Emitted / ✅ Ran | Prints `(2, test)`. |
| `test-result.rav` | ✅ Emitted / ❌ Ran | Throws `InvalidOperationException` while parsing `foo`. |
| `test-result2.rav` | ✅ Emitted / ✅ Ran | Accessibility issue fixed; prints `23`. |
| `tokenizer.rav` | ❌ Compile timeout | Compilation still hangs and is terminated after a 120s timeout. |
| `try-match.rav` | ✅ Emitted / ⚠️ Input mismatch | Running with the default `'foo'` argument reports the format error and exits. |
| `tuples.rav` | ⚠️ Emitted with warning / ✅ Ran | Compilation warns that all match cases are already handled; the program then prints the tuple projections. |
| `tuples2.rav` | ✅ Emitted / ✅ Ran | Runtime output remains `tuple False foo`. |
| `type-unions.rav` | ✅ Emitted / ❌ Ran | Fails at runtime unless `TestDep.dll` is built and copied next to the DLL. |
| `unit.rav` | ✅ Emitted / ✅ Ran | Outputs `Hello` and the unit literals. |

**Runtime observations.** The async suite still has gaps: `async-inference-regression.rav` and `try-match-async.rav` fail during binding because of outstanding inference issues, `async-generic-compute.rav` regressed into an `InvalidProgramException` during async state-machine execution, and the HTTP client sample exits with a 403 in the sandbox while the `Ok`/`Error` bindings for `Result<string>` remain missing. Interactive or non-terminating samples require manual handling: `io.rav` needs an argument (ran with `.`), `parse-number.rav` waits for console input, and `goto.rav` intentionally never exits. `option.rav`, `result.rav`, and `test-result.rav` still throw while unwrapping failures (the first two intentionally), while `test-result2.rav` now compiles and prints `23`. `test9.rav` fails with RAV0135 as expected after the recent behavior change, and `discriminated-unions2.rav` would need statements to precede both the function and the union declarations. `try-match.rav` reports a format error for its default `'foo'` argument, `tokenizer.rav` still times out after 120s, `type-unions.rav` currently fails to run without a locally built `TestDep.dll`, and `discriminated-unions-generics3.rav` remains absent.
