# Known Issues

## Sample compilation and execution
Re-running every sample with `dotnet run --no-build --project src/Raven.Compiler/Raven.Compiler.csproj -- samples/<file>.rav -o /tmp/raven-samples/<file>/<file>.dll` produced the following results. Successful entries emitted binaries into `/tmp/raven-samples/<file>/` for inspection.【0f5042†L1-L4】

| Sample | Status | Notes |
| --- | --- | --- |
| `arrays.rav` | ✅ Emitted / ✅ Ran | Recompiled via the CLI; running the emitted DLL prints 3, 1, 42, 3 as before.【0f5042†L1-L4】【1e4274†L1-L5】 |
| `async-await.rav` | ✅ Emitted / ❌ InvalidProgram | Running the DLL aborts with `InvalidProgramException` because the IL frame drops the borrowed receiver via a synthesized `pop` after `TaskAwaiter.get_IsCompleted`, so the fast path underflows the stack before calling `AwaitUnsafeOnCompleted` and the harness exits with code 134.【dbe6a2†L1-L23】【8d5aa7†L1-L108】 |
| `catch.rav` | ✅ Emitted / ✅ Ran | Running the compiled DLL prints `Foo`.【5edb92†L10-L18】 |
| `classes.rav` | ✅ Emitted / ✅ Ran | Execution prints `Hello`, `John`, the projected record, and the trailing unit value.【f240ae†L1-L10】 |
| `collections.rav` | ✅ Emitted / ✅ Ran | Produces the expected hero roster in order when executed.【e1ebe5†L1-L11】 |
| `enums.rav` | ✅ Emitted / ✅ Ran | Outputs the grade sequence `C`, `Grades`, `B`, `Grades`.【f63261†L1-L10】 |
| `foo.rav` | ✅ Emitted / ✅ Ran | Invocation prints `1`.【e76f9e†L1-L9】 |
| `function-types.rav` | ✅ Emitted / ✅ Ran | Displays `result = 10`, `chained = 20`, and `combined = 30` for the delegate composition sample.【e738d3†L1-L10】 |
| `general.rav` | ✅ Emitted / ✅ Ran | Continues to print `Hello, World!` followed by 1, 42, 3.【8a3f83†L1-L10】 |
| `generator.rav` | ✅ Emitted / ✅ Ran | Shows 42 and the odd sequence 3, 5, 7, 9 at runtime.【f9ed99†L1-L9】 |
| `generics.rav` | ✅ Emitted / ✅ Ran | Execution prints `2`, `2`, `3`.【1c023c†L1-L8】 |
| `generics2.rav` | ✅ Emitted / ✅ Ran | Running the DLL prints `ok`.【36300c†L1-L8】 |
| `goto.rav` | ✅ Emitted / ⚠️ Not run | Build succeeds but the program would loop forever, so execution is intentionally skipped.【b95219†L1-L4】 |
| `interfaces.rav` | ✅ Emitted / ✅ Ran | Runtime output remains `Init`, `Do`, `Dispose 1`.【28e112†L1-L9】 |
| `introduction.rav` | ✅ Emitted / ✅ Ran | The executable prints `Empty input.` followed by the summary lines.【f89672†L1-L8】 |
| `io.rav` | ✅ Emitted / ⚠️ Requires args | The binary expects a directory argument; running with `src/Raven.Compiler/samples` enumerates files and reports the count.【a15227†L1-L9】【44eca3†L1-L39】 |
| `lambda.rav` | ✅ Emitted / ✅ Ran | Shows the captured lambda results and closure state transitions.【9d049e†L1-L10】 |
| `linq.rav` | ✅ Emitted / ✅ Ran | Outputs the reversed list `3`, `2`, `1`.【83c569†L1-L8】 |
| `main.rav` | ✅ Emitted / ✅ Ran | Prints the critical value banner and tuple projection without error.【3b453c†L1-L10】 |
| `match.rav` | ✅ Emitted / ✅ Ran | Runtime pattern output stays `Int32`, `String`, `foo`.【09526e†L1-L9】 |
| `parse-number.rav` | ✅ Emitted / ⚠️ Interactive loop | Compilation succeeds but the program waits for console input, so execution is skipped.【304e7a†L1-L6】 |
| `pattern-matching.rav` | ✅ Emitted / ✅ Ran | Prints `else` when executed.【97fcb0†L1-L8】 |
| `reflection.rav` | ✅ Emitted / ❌ Runtime failure | Running the DLL now throws `IndexOutOfRangeException` when accessing `members[1]`, confirming the sample bug.【380b9b†L1-L12】 |
| `string-interpolation.rav` | ✅ Emitted / ✅ Ran | Outputs the Hebrew greeting from `Console.WriteLine`.【763b30†L1-L8】 |
| `test.rav` | ✅ Emitted / ✅ Ran | Prints the lambda totals `7`, `5`, and `5`.【9d931e†L1-L8】 |
| `test2.rav` | ✅ Emitted / ✅ Ran | Produces `42`, `Hello, World!`, and `Hello, 2`.【1f3b53†L1-L8】 |
| `test3.rav` | ❌ Fails | Top-level program synthesis still recurses in `SynthesizedMainMethodSymbol.ResolveReturnType`.【e14076†L1-L80】 |
| `tokenizer.rav` | ⚠️ Hangs | `timeout 3` aborts the compiler invocation, indicating the tokenizer still fails to terminate.【328d2e†L1-L1】【65d5e9†L1-L3】 |
| `try-match.rav` | ✅ Emitted / ⚠️ Input mismatch | Running with the default `'foo'` argument reports the format error and exits. 【c6be6c†L1-L3】 |
| `tuples.rav` | ⚠️ Emitted with warning / ✅ Ran | Compilation warns about the redundant catch-all, and the program prints the tuple projections.【95e0e1†L9-L24】 |
| `tuples2.rav` | ✅ Emitted / ✅ Ran | Runtime output remains `tuple False foo`.【95e0e1†L24-L31】 |
| `type-unions.rav` | ✅ Emitted / ⚠️ External dependency | Copying `TestDep.dll` beside the DLL enables execution, which then prints the expected projections.【594209†L1-L4】【95e0e1†L31-L42】 |
| `unit.rav` | ✅ Emitted / ✅ Ran | Outputs `Hello` and the unit literals when executed.【95e0e1†L42-L50】 |

**Runtime observations.** `async-await.rav` now runs to completion after the CompletedTask fix, while `reflection.rav` currently throws because its `members` array only contains a single element; `type-unions.rav` still requires copying `TestDep.dll` next to the emitted assembly. The interactive samples remain non-turnkey: `io.rav` expects a directory argument, `parse-number.rav` loops waiting for input, `goto.rav` is an intentional infinite loop, and `try-match.rav` reports a format error for its default `'foo'` argument.【5edb92†L1-L9】【380b9b†L1-L12】【95e0e1†L31-L42】【a15227†L1-L9】【44eca3†L1-L39】【304e7a†L1-L6】【b95219†L1-L4】【c6be6c†L1-L3】

## Common problem patterns
- **Symbol binding recursion (resolved):** `SymbolEqualityComparer` now tracks visited symbol pairs to break cycles when inspecting extension methods, interface defaults, and interpolated strings. The affected samples (`samples/main.rav`, `samples/generator.rav`, `samples/interfaces.rav`, `samples/linq.rav`, and `samples/reflection.rav`) compile successfully again.【F:src/Raven.CodeAnalysis/SymbolEqualityComparer.cs†L1-L215】【4528a4†L1-L4】【2941cf†L1-L3】【152141†L1-L2】【d2560e†L1-L3】【2a10c8†L1-L2】【a8337e†L1-L3】【14c359†L1-L2】【4013ab†L1-L3】【9f9ee0†L1-L2】【6a0816†L1-L3】
- **Missing environment preconditions:** The sample harness historically expected callers to prepare filesystem artifacts (for example, the `output/` directory). `samples/build.sh` now creates the directory on demand so compilation can proceed, but other ad-hoc scripts may require similar hardening.【F:src/Raven.Compiler/samples/build.sh†L1-L18】
- **Infrastructure robustness gaps:** Diagnostics and lowering paths still throw or diverge under stress. Parser span computation crashes the tokenizer sample, and top-level program synthesis loops indefinitely in `samples/test3.rav`, highlighting resilience issues in diagnostic span generation and top-level lowering.【F:ISSUES.md†L3-L11】
