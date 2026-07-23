# Test Impact Map

Use this map to pick the smallest trustworthy pre-change baseline and
post-change validation set. The full baseline is still the broad safety gate,
but it is not the default inner-loop command for scoped compiler work.

All ad hoc `dotnet test` commands should include `/property:WarningLevel=0`.

## Selection Rules

1. Start with the row that owns the code you are about to touch.
2. Run the listed pre-change baseline before editing when the current behavior is
   not already known.
3. After editing, run the same focused set plus any new regression tests.
4. Add runtime, sample, or language-server coverage only when that layer is part
   of the changed behavior.
5. Escalate to `scripts/test-baseline.sh` when the change crosses several rows,
   changes shared test infrastructure, changes generated syntax or bound-model
   inputs, or leaves the failure source ambiguous.

## Impact Map

| Changed area | Pre-change baseline | Post-change validation |
|---|---|---|
| Union symbols, union conversion, union nullability, C# union import/export | `scripts/test-feature-suite.sh unions` | Same suite; add `scripts/test-feature-suite.sh unions --runtime` for emit/runtime behavior; run `dotnet test test/Raven.Core.Tests/Raven.Core.Tests.csproj /property:WarningLevel=0` when Raven.Core unions are affected |
| Pattern matching, `match`, `is`, pattern variables, exhaustiveness | `scripts/test-feature-suite.sh patterns` | Same suite; add focused `CodeGen/Patterns` or runtime tests when lowering/emission changes |
| Parser, syntax nodes, trivia, recovery | Focused syntax/parser test class filters | Same filters; run `scripts/codex-build.sh` when syntax model definitions or generators change |
| Binder, semantic model APIs, symbol display, operations | Focused semantic/operation test class filters for the feature | Same filters plus any affected feature suite; full baseline only for shared binder behavior that spans features |
| Lowering | Focused lowering tests for the feature | Same tests plus focused runtime/codegen tests proving observable behavior |
| Code generation, emitted metadata shape, reflection, runtime execution | Focused `CodeGen` test class or method filter | Same filter; add `scripts/test-runtime-isolated.sh` only when the change affects broad runtime/emission paths |
| Diagnostics and analyzers | Focused diagnostics/analyzer test class filters | Same filters; include code-fix tests only when fix behavior changes |
| Overload resolution, conversions, target typing, invocation binding | `scripts/test-feature-suite.sh overload-resolution` | Same suite; add runtime overlap when emitted calls/conversions change |
| Functions, lambdas, async/await, propagation | `scripts/test-feature-suite.sh functions-async` | Same suite; add `--runtime` for async state-machine or emitted-control-flow changes |
| Extensions and metadata extension members | `scripts/test-feature-suite.sh extensions` | Same suite; add completion tests when editor-facing lookup changes |
| Imports, aliases, namespaces, escaped identifiers, multi-file lookup | `scripts/test-feature-suite.sh imports-and-namespaces` | Same suite; add project/sample coverage only when project loading or generated imports change |
| Target frameworks, reference assembly paths, Raven project targeting | `scripts/test-feature-suite.sh framework-and-targeting` | Same suite; add sample project builds when SDK/MSBuild compatibility changes |
| Compiler driver, Raven project loading, MSBuild targets | Focused compiler-driver/project-loading tests | Same filters; add `FORCE_REBUILD=1 samples/build.sh` when sample compatibility or build outputs change |
| Raven.Core source or public standard-library behavior | Focused `test/Raven.Core.Tests` filter | Same filter; add relevant compiler feature suite if compiler behavior changed too |
| Language-server request handling, hover/completion/inlay presentation | Focused `test/Raven.LanguageServer.Tests` filter | Same filter; add compiler tests first when the bug is semantic rather than LSP-owned |
| Project-backed language-server workspace, analyzer lanes, document sync | Focused `test/Raven.LanguageServer.Integration.Tests` filter | Same filter; avoid running this for ordinary compiler-only language changes |
| Console editor behavior | Focused `test/Raven.Editor.Tests` filter | Same filter |
| Browser WebAssembly playground, Monaco integration, static publish | `dotnet build src/Raven.Playground/Raven.Playground.csproj --property WarningLevel=0` | `scripts/test-playground-browser.sh` |

## Full Baseline Triggers

Run `scripts/test-baseline.sh` instead of only a targeted baseline when:

- the change touches multiple compiler layers and the owner is unclear;
- test infrastructure, suite filters, generators, or shared fixtures changed;
- generated syntax or bound-model artifacts changed;
- prior command output indicates a test-host crash or nondeterministic failure
  that targeted filters cannot isolate;
- preparing a stabilization handoff where broad confidence matters more than
  inner-loop speed.

For runtime-heavy stabilization, run `scripts/test-runtime-isolated.sh`
separately instead of folding those tests into the fast baseline.
