# Raven.CodeAnalysis.Tests

This project is the compiler coverage hub. Keep the default path fast and focused on syntax, binding, semantic model, diagnostics, symbol shape, operations shape, and project model behavior that must stay correct for Raven code to compile.

Runtime, reflection, generated IL, process, NuGet, MSBuild, and sample-project coverage is still useful, but it is secondary coverage. Keep it isolated from the baseline unless it is the only practical way to prove a compiler behavior.

## Test Tiers

| Tier | What belongs here | Command |
|---|---|---|
| Baseline | Fast syntax and semantic tests that protect language compilation and compiler APIs | `scripts/test-baseline.sh` |
| Feature suite | Focused area checks after changing a feature | `scripts/test-feature-suite.sh <suite>` |
| Runtime isolated | CodeGen, sample, reflection, emitted-assembly, and project-heavy integration tests | `scripts/test-runtime-isolated.sh` |
| Feature runtime | Runtime/emission overlap for one area | `scripts/test-feature-suite.sh <suite> --runtime` |
| Language-server perf | Opt-in language-server latency and interaction-budget checks | `scripts/test-language-server-perf.sh` |
| Samples | End-to-end sample project build after compiler/runtime changes | `FORCE_REBUILD=1 samples/build.sh` |

All ad hoc `dotnet test` commands should use `/property:WarningLevel=0`.

The full baseline can take around 20 minutes on a typical local machine. Treat it as the broad safety gate, not the first tool for every edit: start with the smallest matching feature suite or test-class filter, then broaden to the baseline when the change affects shared compiler behavior or before handing off stabilization work. Part of the cleanup goal is to keep refactoring and reorganizing tests so selective runs become more accurate, faster, and easier to choose.

The full sample build is also a smoke gate, not a default inner-loop command: `FORCE_REBUILD=1 samples/build.sh` can take roughly 5-6 minutes because it recompiles Raven.Core/compiler dependencies and every sample, while `samples/run.sh` usually finishes in seconds once the sample DLLs are built. Prefer targeted sample filters when validating one language area, then run the full sample build/run pass for compiler/runtime stabilization.

Run suites because their owning behavior changed, not because they are nearby:

| Touched area | Usual validation |
|---|---|
| Parser, binder, semantic model, symbol display, diagnostics, operations | Focused `Raven.CodeAnalysis.Tests` class/filter or matching feature suite |
| Compiler driver, Raven project loading, MSBuild targets, target frameworks, references | Targeted project-loading/compiler-driver tests, then sample build only when compatibility changed |
| Emit, lowering, runtime execution, metadata shape, reflection | Focused runtime/CodeGen test or isolated runtime suite |
| Language-server handlers, document store, workspace manager, request scheduling, presentation formatting | Focused `Raven.LanguageServer.Tests` filter |
| Language-server latency budgets or metrics thresholds | `scripts/test-language-server-perf.sh` only |
| Console editor behavior | `test/Raven.Editor.Tests` only |

Do not run `Raven.LanguageServer.Tests` merely because a language feature changed. Run it when the change touches language-server code, editor-facing presentation, or a bug was observed only through the language-server path and needs an LSP-specific guard.

## Coverage Gaps

A skipped test is not covered by the baseline. Keep skipped tests visible and either restore them as fast syntax/semantic coverage, move them into isolated runtime coverage, or delete/replace them when they are stale.

## Project Boundaries

Keep compiler and editor-adjacent coverage in the project that owns the behavior:

| Project | Owns |
|---|---|
| `test/Raven.CodeAnalysis.Tests` | Compiler API, syntax, binding, semantic model, diagnostics, symbols, operations, project loading, and reduced compiler regressions from samples |
| `test/Raven.LanguageServer.Tests` | Language-server request handling, document snapshots, diagnostics publication, hover/completion/inlay presentation, request cancellation, and VS Code-facing workspace behavior |
| `test/Raven.LanguageServer.Perf.Tests` | Opt-in language-server metrics, latency budgets, and performance instrumentation; do not include in baseline behavior gates |
| `test/Raven.Editor.Tests` | The `Raven.Editor` console editor only; do not use this project or namespace for language-server or VS Code integration coverage |

When a language-server failure exposes a compiler semantic bug, reduce it into `Raven.CodeAnalysis.Tests` first, then keep a narrow `Raven.LanguageServer.Tests` guard only for the language-server path that made the bug visible.

Current explicit gaps:

| Area | Gap | Preferred cleanup |
|---|---|---|
| Reference assembly diagnostics | file-scoped code and missing-main diagnostics require reference assemblies in some environments | Make the harness provide stable references or rewrite as compiler-only diagnostics |
| Language server coverage | request/hover/workspace integration tests can run for minutes under the baseline runner | Split fast request/mapper/semantic presentation tests from workspace integration tests, then guard the latter separately before restoring them to a default gate |
| Runtime CodeGen | stale runtime/reflection/emitted-shape classes are excluded from `scripts/test-runtime-isolated.sh`: `AsyncEntryPointBridgeTests`, `AsyncPropagateCodeGenTests`, `AsyncTryAwaitCodeGenTests`, `ByRefCodeGenTests`, `ExpressionBodyCodeGenTests`, `FunctionExpressionCodeGenTests`, `GenericInvocationCodeGenTests`, `MacroCodeGenTests`, `MemberBindingCodeGenTests`, `PdbSequencePointTests`, `PrimaryConstructorParameterCodeGenTests`, `ProjectFileNuGetReferenceTests`, `PropertyTests`, `RuntimeAsyncCodeGenTests`, `RuntimeSymbolResolverTests`, `TrailingBlockCodeGenTests`, `TryExpressionCodeGenTests`, `TypeOfExpressionCodeGenTests`, `TypeResolutionPrecedenceTests`, `UnionCodeGenTests` | Reintroduce only focused runtime behavior checks; avoid emitted instruction/lowered shape assertions |
| Project/CLI runtime | `MsBuildSampleProjectCompilationTests` can trip the runtime hang guard through `rvn`/MSBuild sample compilation | Replace with a bounded CLI smoke test or rely on `FORCE_REBUILD=1 samples/build.sh` for sample coverage |
| Runtime CodeGen | legacy async, positional-pattern, and list middle-rest collection-pattern CodeGen skips | Replace with isolated runtime behavior tests, not emitted-shape assertions |

## Area Runs

Use `scripts/test-feature-suite.sh --list` to see curated feature suites. Pick the smallest suite that matches the changed behavior:

| Changed area | Start with |
|---|---|
| Parser, syntax nodes, trivia, recovery | syntax or parser test class filters, then `scripts/test-baseline.sh` |
| Function calls, overload resolution, optional/named/params args, callable `self` | `scripts/test-feature-suite.sh overload-resolution` |
| Functions, lambdas, async/await, async lowering | `scripts/test-feature-suite.sh functions-async` |
| Match, `is`, pattern variables, exhaustiveness | `scripts/test-feature-suite.sh patterns` |
| Extension declarations, extension lookup, metadata extension members | `scripts/test-feature-suite.sh extensions` |
| Partial types and members | `scripts/test-feature-suite.sh partials` |
| Macros and macro-expanded documents | `scripts/test-feature-suite.sh macros` |
| Imports, aliases, namespaces, escaped identifiers, multi-file lookup | `scripts/test-feature-suite.sh imports-and-namespaces` |
| Target frameworks, reference assembly paths, Raven project targeting | `scripts/test-feature-suite.sh framework-and-targeting` |

After a source change that can affect emitted IL, also run the matching `--runtime` suite or `scripts/test-runtime-isolated.sh`. After changes that affect project loading, Raven.Core, the compiler driver, or sample compatibility, run `FORCE_REBUILD=1 samples/build.sh`.

## Cleanup Rules

When a test fails, first classify it:

1. Compiler behavior is wrong: fix the compiler and keep or add focused coverage.
2. The test asserts stale syntax, symbol display, diagnostic wording, cache identity, lowered shape, or emitted instructions: rewrite it to assert stable language behavior or delete it if it no longer protects anything.
3. The test uses reflection, subprocesses, emitted assemblies, NuGet restore, MSBuild, or samples: move it to the isolated runtime path unless it is a small, crucial end-to-end guard.

Avoid baseline tests that can hang. If runtime/reflection coverage is required, keep it narrow, guarded by the isolated scripts, and separate from fast syntax/semantic coverage.
