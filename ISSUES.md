# Known Issues

## Sample compilation and execution
Re-running every sample with `dotnet run -- samples/<file>.rav -o output/<file>.dll` produced the following results. Successful entries emitted binaries into `src/Raven.Compiler/output/` for inspection.【0dd0c1†L1-L32】

| Sample | Status | Notes |
| --- | --- | --- |
| `arrays.rav` | ✅ Emitted | |
| `async-await.rav` | ✅ Emitted | |
| `catch.rav` | ✅ Emitted | |
| `classes.rav` | ✅ Emitted | |
| `collections.rav` | ✅ Emitted | |
| `enums.rav` | ✅ Emitted | |
| `foo.rav` | ✅ Emitted | |
| `function-types.rav` | ✅ Emitted | |
| `general.rav` | ✅ Emitted | |
| `generator.rav` | ✅ Emitted | |
| `generics.rav` | ✅ Emitted | |
| `generics2.rav` | ✅ Emitted | |
| `goto.rav` | ✅ Emitted | |
| `interfaces.rav` | ❌ Fails | Binder crashes while resolving interpolated string concatenation during diagnostic gathering.【bc7ee1†L1-L26】
| `introduction.rav` | ✅ Emitted | |
| `io.rav` | ✅ Emitted | |
| `lambda.rav` | ✅ Emitted | |
| `linq.rav` | ✅ Emitted | |
| `main.rav` | ✅ Emitted | |
| `match.rav` | ✅ Emitted | |
| `parse-number.rav` | ✅ Emitted | |
| `pattern-matching.rav` | ✅ Emitted | |
| `reflection.rav` | ✅ Emitted | |
| `string-interpolation.rav` | ✅ Emitted | |
| `test.rav` | ✅ Emitted | |
| `test2.rav` | ✅ Emitted | |
| `test3.rav` | ❌ Fails | Top-level program synthesis still recurses inside `SynthesizedMainMethodSymbol.ResolveReturnType`.【58d0b2†L1-L120】
| `tokenizer.rav` | ⚠️ Hangs | Trivia lexing never terminates; the build must be canceled manually.【55f5d9†L1-L4】
| `try-match.rav` | ✅ Emitted | |
| `tuples.rav` | ⚠️ Emitted with warning | Compilation reports the redundant-pattern diagnostic but still emits IL.【79f2e5†L1-L3】
| `tuples2.rav` | ✅ Emitted | |
| `type-unions.rav` | ❌ Fails | Emission aborts when `ResolveRuntimeMethodInfo` cannot locate the target CLR method.【73dafa†L1-L18】
| `unit.rav` | ✅ Emitted | |

## Common problem patterns
- **Symbol binding recursion (resolved):** `SymbolEqualityComparer` now tracks visited symbol pairs to break cycles when inspecting extension methods, interface defaults, and interpolated strings. The affected samples (`samples/main.rav`, `samples/generator.rav`, `samples/interfaces.rav`, `samples/linq.rav`, and `samples/reflection.rav`) compile successfully again.【F:src/Raven.CodeAnalysis/SymbolEqualityComparer.cs†L1-L215】【4528a4†L1-L4】【2941cf†L1-L3】【152141†L1-L2】【d2560e†L1-L3】【2a10c8†L1-L2】【a8337e†L1-L3】【14c359†L1-L2】【4013ab†L1-L3】【9f9ee0†L1-L2】【6a0816†L1-L3】
- **Missing environment preconditions:** The sample harness historically expected callers to prepare filesystem artifacts (for example, the `output/` directory). `samples/build.sh` now creates the directory on demand so compilation can proceed, but other ad-hoc scripts may require similar hardening.【F:src/Raven.Compiler/samples/build.sh†L1-L18】
- **Infrastructure robustness gaps:** Diagnostics and lowering paths still throw or diverge under stress. Parser span computation crashes the tokenizer sample, and top-level program synthesis loops indefinitely in `samples/test3.rav`, highlighting resilience issues in diagnostic span generation and top-level lowering.【F:ISSUES.md†L3-L11】
