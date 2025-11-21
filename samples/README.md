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
| `async/async-generic-compute.rav` | ✅ Emitted / ❌ Ran | Crashes with an `InvalidProgramException` while unwrapping the async generic result. |
| `async/async-inference-regression.rav` | ❌ Fails | Binder errors persist (`Run` overload count, `int` vs `unit`, ambiguous `WriteLine`). |
| `async/async-task-return.rav` | ✅ Emitted / ✅ Ran | Prints the awaited task result `43`. |
| `async/async-try-catch.rav` | ✅ Emitted / ✅ Ran | Logs `value:42`, `caught:boom`, and `completed`. |
| `async/http-client.rav` | ✅ Emitted / ⚠️ HTTP dependency | Execution attempts an external request and exits with a 403 in the sandbox. |
| `async/try-match-async.rav` | ❌ Fails | Binding still reports `Cannot convert from '0' to 'Task'`, so no runnable DLL. |
| `catch.rav` | ✅ Emitted / ✅ Ran | Prints `Foo`. |
| `classes.rav` | ✅ Emitted / ✅ Ran | Prints `Hello`, `John`, the projected record, and the trailing unit value. |
| `collections.rav` | ✅ Emitted / ✅ Ran | Produces the expected hero roster. |
| `discard.rav` | ✅ Emitted / ✅ Ran | Prints `Test` twice to demonstrate discards. |
| `discriminated-unions-generics.rav` | ✅ Emitted / ✅ Ran | Prints `ok 99`, `error 'boom'`, `ok 42`. |
| `discriminated-unions-generics2.rav` | ✅ Emitted / ✅ Ran | Emits `ok 99` followed by `error 'boom'`. |
| `discriminated-unions-generics3.rav` | ⚠️ Missing | File is not present in the repository. |
| `discriminated-unions.rav` | ✅ Emitted / ✅ Ran | Prints `identifier 'alpha'`, `number 42`, `unknown token`. |
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
| `io.rav` | ✅ Emitted / ⚠️ Arg required | Running without arguments prints the usage banner; provide a directory to list entries. |
| `lambda.rav` | ✅ Emitted / ✅ Ran | Shows the captured lambda results and closure state transitions. |
| `linq.rav` | ✅ Emitted / ✅ Ran | Outputs the reversed list `3`, `2`, `1`. |
| `main.rav` | ✅ Emitted / ✅ Ran | Prints the critical value banner and tuple projection. |
| `match.rav` | ✅ Emitted / ✅ Ran | Runtime pattern output stays `Int32`, `String`, `foo`. |
| `option.rav` | ✅ Emitted / ❌ Ran | `UnwrapOrThrow` throws `InvalidOperationException: Option is None`. |
| `parse-number.rav` | ✅ Emitted / ⚠️ Interactive loop | Compilation succeeds but the program waits for console input, so execution is skipped. |
| `pattern-matching.rav` | ✅ Emitted / ✅ Ran | Prints `else`. |
| `pipe-operator.rav` | ✅ Emitted / ✅ Ran | Produces `Result: 7`. |
| `pipe-operator2.rav` | ✅ Emitted / ✅ Ran | Prints `Result: 7`. |
| `pipe-operator3.rav` | ✅ Emitted / ✅ Ran | Emits `42`. |
| `reflection.rav` | ✅ Emitted / ✅ Ran | Lists the reflected members without throwing. |
| `result.rav` | ✅ Emitted / ❌ Ran | `UnwrapOrThrow` throws `InvalidOperationException: Wrong number`. |
| `shadowing.rav` | ⚠️ Emitted with warning / ✅ Ran | Compilation warns that `a` shadows a previous declaration; runtime emits no output. |
| `string-interpolation.rav` | ✅ Emitted / ✅ Ran | Outputs the Hebrew greeting from `Console.WriteLine`. |
| `test.rav` | ✅ Emitted / ✅ Ran | Prints the lambda totals `7`, `5`, and `5`. |
| `test2.rav` | ✅ Emitted / ✅ Ran | Produces `42`, `Hello, World!`, and `Hello, 2`. |
| `test3.rav` | ✅ Emitted / ✅ Ran | Completes without console output. |
| `test4.rav` | ✅ Emitted / ✅ Ran | Completes silently after exercising property override compatibility. |
| `test5.rav` | ✅ Emitted / ✅ Ran | Prints `Foo: Hejtest 2 3`. |
| `test9.rav` | ✅ Emitted / ✅ Ran | Produces the unit literal `()`. |
| `test10.rav` | ✅ Emitted / ✅ Ran | Prints `(2, test)`. |
| `test-result.rav` | ✅ Emitted / ❌ Ran | Throws `InvalidOperationException` while parsing `foo`. |
| `test-result2.rav` | ✅ Emitted / ✅ Ran | Prints `23`. |
| `tokenizer.rav` | ❌ Compile timeout | Compilation hung and had to be interrupted even with a 30s timeout. |
| `try-match.rav` | ✅ Emitted / ⚠️ Input mismatch | Running with the default `'foo'` argument reports the format error and exits. |
| `tuples.rav` | ⚠️ Emitted with warning / ✅ Ran | Compilation warns that all match cases are already handled; the program then prints the tuple projections. |
| `tuples2.rav` | ✅ Emitted / ✅ Ran | Runtime output remains `tuple False foo`. |
| `type-unions.rav` | ✅ Emitted / ⚠️ Needs dependency | Copy `TestDep.dll` beside the emitted sample DLL before running. |
| `unit.rav` | ✅ Emitted / ✅ Ran | Outputs `Hello` and the unit literals. |

**Runtime observations.** The async suite still has gaps: `async-inference-regression.rav` and `try-match-async.rav` fail during binding, and `async-generic-compute.rav` aborts at runtime with an `InvalidProgramException`. Interactive or non-terminating samples remain non-turnkey: `io.rav` needs an argument, `parse-number.rav` loops waiting for input, and `goto.rav` intentionally never exits. `try-match.rav` reports a format error for its default `'foo'` argument, `tokenizer.rav` still hangs even under a timeout, the HTTP client sample exits with a 403, and `type-unions.rav` runs cleanly once `TestDep.dll` is copied beside the emitted assembly. Newly validated results include `test-result2.rav` running to completion while `option.rav`, `result.rav`, and `test-result.rav` throw during unwrapping; `discriminated-unions-generics3.rav` is no longer present in the repository.
