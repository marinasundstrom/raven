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
| Samples | End-to-end sample project build after compiler/runtime changes | `FORCE_REBUILD=1 samples/build.sh` |

All ad hoc `dotnet test` commands should use `/property:WarningLevel=0`.

## Coverage Gaps

A skipped test is not covered by the baseline. Keep skipped tests visible and either restore them as fast syntax/semantic coverage, move them into isolated runtime coverage, or delete/replace them when they are stale.

Current explicit gaps:

| Area | Gap | Preferred cleanup |
|---|---|---|
| Completion | `self.` member-access completion returns no members | Fix completion/semantic lookup and keep as baseline coverage |
| Reference assembly diagnostics | file-scoped code and missing-main diagnostics require reference assemblies in some environments | Make the harness provide stable references or rewrite as compiler-only diagnostics |
| Partial properties/events | definition/implementation merge still reports duplicate/missing counterpart diagnostics | Finish merge/declared-symbol registration so both parts collapse to one member symbol |
| Positional pattern assignment | skipped semantic coverage for tuple/nominal deconstruction assignments | Restore as fast semantics when current syntax and binding behavior are clear |
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
