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

Re-running every sample from `samples/` with `dotnet run --project ../src/Raven.Compiler -- <file>.rav -o <file>.dll` followed by `dotnet <file>.dll` produced the following results. Successful entries left DLLs under `.status-output/` for later inspection.

| Sample | Status | Notes |
| --- | --- | --- |
| `arrays.rav` | ✅ Emitted / ✅ Ran | Running the DLL prints `3`, `1`, `42`, `3`. |
| `async/async-await.rav` | ✅ Emitted / ✅ Ran | The async sample finishes with `first:1`, `sum:6`, and `done`. |
| `async/async-file-io.rav` | ✅ Emitted / ✅ Ran | Runtime output lists the staged writes `alpha`, `beta`, `gamma`. |
| `async/async-generic-compute.rav` | ✅ Emitted / ✅ Ran | Shows the "Before/After" delay log before printing `42`. |
| `async/async-inference-regression.rav` | ❌ Fails | Binder errors persist (`int` can't satisfy the `unit` return type and `WriteLine` overloads stay ambiguous). |
| `async/async-task-return.rav` | ✅ Emitted / ✅ Ran | Prints the awaited task result `43`. |
| `async/async-try-catch.rav` | ✅ Emitted / ✅ Ran | Logs `value:42`, `caught:boom`, and `completed`. |
| `async/http-client.rav` | ✅ Emitted / ⚠️ HTTP dependency | Execution attempts an external request and exits with a 403 in the sandbox. |
| `async/try-match-async.rav` | ❌ Fails | Still reports `Cannot convert from '0' to 'Task'` for the awaited `try match`. |
| `catch.rav` | ✅ Emitted / ✅ Ran | Prints `Foo`. |
| `classes.rav` | ✅ Emitted / ✅ Ran | Prints `Hello`, `John`, the projected record, and the trailing unit value. |
| `collections.rav` | ✅ Emitted / ✅ Ran | Produces the expected hero roster. |
| `discard.rav` | ✅ Emitted / ✅ Ran | Prints `Test` twice to demonstrate discards. |
| `discriminated-unions.rav` | ❌ Fails | `.Ok(...)`/`.Error(...)` now resolve to the generated case constructors, but the match arms still use the leading-dot syntax that the current pattern binder does not recognize, so they report `RAV1001`/`RAV0103` before exhaustiveness analysis. The file keeps its union declarations after the global statements so it satisfies the ordering rule while we focus on pattern binding, lowering, and code generation. |
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
| `interfaces.rav` | ✅ Emitted / ✅ Ran | Runtime output remains `Init`, `Do`, `Dispose 1`. |
| `introduction.rav` | ✅ Emitted / ✅ Ran | Prints `Empty input.` followed by the summary lines. |
| `io.rav` | ✅ Emitted / ⚠️ Requires args | Pass a directory argument (for example `.`) so the program can enumerate files and report the count. |
| `lambda.rav` | ✅ Emitted / ✅ Ran | Shows the captured lambda results and closure state transitions. |
| `linq.rav` | ✅ Emitted / ✅ Ran | Outputs the reversed list `3`, `2`, `1`. |
| `main.rav` | ✅ Emitted / ✅ Ran | Prints the critical value banner and tuple projection. |
| `match.rav` | ✅ Emitted / ✅ Ran | Runtime pattern output stays `Int32`, `String`, `foo`. |
| `parse-number.rav` | ✅ Emitted / ⚠️ Interactive loop | Compilation succeeds but the program waits for console input, so execution is skipped. |
| `pattern-matching.rav` | ✅ Emitted / ✅ Ran | Prints `else`. |
| `pipe-operator.rav` | ✅ Emitted / ✅ Ran | Produces `Result: 7`. |
| `pipe-operator2.rav` | ✅ Emitted / ✅ Ran | Prints `Result: 7`. |
| `pipe-operator3.rav` | ✅ Emitted / ✅ Ran | Emits `42`. |
| `reflection.rav` | ✅ Emitted / ✅ Ran | Lists the reflected members without throwing. |
| `shadowing.rav` | ⚠️ Emitted with warning / ✅ Ran | Compilation warns that `a` shadows a previous declaration; runtime emits no output. |
| `string-interpolation.rav` | ✅ Emitted / ✅ Ran | Outputs the Hebrew greeting from `Console.WriteLine`. |
| `test.rav` | ✅ Emitted / ✅ Ran | Prints the lambda totals `7`, `5`, and `5`. |
| `test2.rav` | ✅ Emitted / ✅ Ran | Produces `42`, `Hello, World!`, and `Hello, 2`. |
| `test3.rav` | ❌ Fails | Top-level program synthesis still recurses in `SynthesizedMainMethodSymbol.ResolveReturnType`. |
| `test4.rav` | ✅ Emitted / ✅ Ran | Completes silently after exercising property override compatibility. |
| `test5.rav` | ✅ Emitted / ✅ Ran | Prints `Foo: Hejtest 2 3`. |
| `test9.rav` | ✅ Emitted / ✅ Ran | Produces the unit literal `()`. |
| `test10.rav` | ✅ Emitted / ✅ Ran | Prints `(2, test)`. |
| `tokenizer.rav` | ⚠️ Hangs | `timeout 3` aborts compilation, indicating the tokenizer still fails to terminate. |
| `try-match.rav` | ✅ Emitted / ⚠️ Input mismatch | Running with the default `'foo'` argument reports the format error and exits. |
| `tuples.rav` | ⚠️ Emitted with warning / ✅ Ran | Compilation warns about the redundant catch-all, and the program prints the tuple projections. |
| `tuples2.rav` | ✅ Emitted / ✅ Ran | Runtime output remains `tuple False foo`. |
| `type-unions.rav` | ✅ Emitted / ⚠️ External dependency | Copy `src/TestDep/bin/Debug/net9.0/TestDep.dll` beside the emitted DLL before running so the union projections print. |
| `unit.rav` | ✅ Emitted / ✅ Ran | Outputs `Hello` and the unit literals. |

**Runtime observations.** The async sample completes, `reflection.rav` now runs to completion, and `type-unions.rav` still requires copying `TestDep.dll` next to the emitted assembly. The interactive samples remain non-turnkey: `io.rav` expects a directory argument, `parse-number.rav` loops waiting for input, `goto.rav` is an intentional infinite loop, and `try-match.rav` reports a format error for its default `'foo'` argument. `tokenizer.rav` still fails to terminate, and `test3.rav` continues to hit the entry-point synthesis recursion. Additional regressions remain: the HTTP client sample needs external network access (and currently exits with 403), `async/async-inference-regression.rav` and `async/try-match-async.rav` still fail to bind, and `discriminated-unions.rav` is blocked on the issues called out above.

### Discriminated union gaps

To get `discriminated-unions.rav` to build, we still need to:

1. ✅ `BindInvocationExpression`'s member-binding path now recognizes when the `.Case` shorthand produced a `BoundTypeExpression`, so the call is routed through `BindConstructorInvocation` instead of falling back to `Invoke`. The new `MemberBindingInvocation_TargetTypedCase_BindsConstructor` test covers this path to keep regressions out. 【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L3496-L3540】【F:test/Raven.CodeAnalysis.Tests/Semantics/DiscriminatedUnionSemanticTests.cs†L38-L74】
2. Extend the pattern binder so the leading-dot syntax inside match arms is treated as a union case test (i.e., a declaration pattern against the nested case struct). Right now `BindPattern` only handles discards, variable declarations, tuples, and logical combinations, which is why the sample's `.Identifier(...)` arms surface parser/binder errors rather than matching on the union. 【F:src/Raven.CodeAnalysis/BoundTree/BoundIsPatternExpression.cs†L241-L377】
3. ✅ Keep the file's type declarations (including the discriminated unions) after the top-level statements so the existing `FileScopedCodeOutOfOrder` diagnostic never fires. 【F:samples/discriminated-unions.rav†L1-L38】
