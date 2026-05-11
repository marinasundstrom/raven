# Quick Assessment (Test Sorting)

Date: 2026-05-10
Scope: `test/Raven.CodeAnalysis.Tests` (`*.cs`, excluding `bin/`, `obj/`, and `TestResults/`)

## Snapshot

- Total test files scanned: `453`
- Current top-level distribution:
  - `Semantics`: 189
  - `Syntax`: 82
  - `CodeGen`: 73
  - `Diagnostics`: 47
  - `Workspaces`: 23
  - `Symbols`: 12
  - `Completion`: 10
  - `Other` (Bugs/Utilities/Text/etc): 17

## Current Cleanup Status

Recently completed:

- Removed scratch tests from `Syntax/Foo.cs` and `Syntax/Sandbox.cs`.
- Removed skipped legacy type-union emission coverage from `CodeGen/Metadata/UnionEmissionTests.cs`.
- Removed legacy nullable-via-type-union coverage from `CodeGen/Metadata/NullableAttributeEmissionTests.cs`.
- Removed legacy tuple type-union match codegen cases from `CodeGen/Patterns/MatchExpressionCodeGenTests.cs`.
- Updated baseline/runtime scripts so CodeGen filtering is derived from the `CodeGen/` folder, not just fully-qualified namespace substrings.
- Fixed the operator-binding semantic API blocker that previously aborted the baseline through target-type recursion.
- Reworked a stale null-flow operator test to assert Roslyn-like `x != null` narrowing with user-defined equality.
- Fixed `GetDeclaredSymbol(VariableDeclaratorSyntax)` so field declarators resolve as fields instead of entering local binding.
- Fixed early method signature symbols so `ref` and `out` parameters are marked mutable like fully bound parameter symbols.
- Removed stale semicolon-separated collection literal tests; current collection syntax uses commas, and comma-based coverage already exercises the same inference paths.
- Updated stale object-initializer coverage from the old `Type { ... }` form to current `Type with { ... }` syntax.
- Updated match-refactoring output expectations to match current formatter output.
- Updated macro project-system tests to use current `with` object-initializer syntax and real temp project files for output-path evaluation.
- Updated stale diagnostic spans/expectations for fixed-array keyword parsing, duplicate labels, catch type spans, static class instantiation, and explicit/static property access.
- Redesigned unstable constant-narrowing diagnostics to assert the accepted diagnostic family instead of a single order-sensitive diagnostic.
- Redesigned property semantic coverage from synthesized accessor body shape to compiler API symbol shape after normal binding.
- Cleaned up exception-handling diagnostics for current catch-pattern support, nullable guard behavior, and current malformed-pattern diagnostics.

Current baseline result after the script isolation cleanup:

- `scripts/test-baseline.sh` reaches the end of the core `Raven.CodeAnalysis.Tests` pass instead of aborting.
- First complete core result after baseline isolation: `123` failed, `2599` passed, `16` skipped, `2738` total.
- Latest core result after Roslyn-like semantic API fixes and stale-test cleanup: `104` failed, `2614` passed, `16` skipped, `2734` total.
- Latest core result after diagnostic/test redesign pass: `81` failed, `2637` passed, `16` skipped, `2734` total.
- The failure list is now actionable: stale syntax expectations, public semantic API gaps, macro/integration drift, and brittle lowered/codegen shape assertions.

## Remaining Stale Or Brittle Buckets

### 0. Recently Resolved Baseline Blocker

The baseline previously aborted before completion because
`Semantics/Binding/OperatorBindingTests.OperatorUsage_NullableMixedEquality_BindsWithoutOperatorDiagnostic`
triggered a stack overflow through repeated `BlockBinder.BindIdentifierName` and `BlockBinder.GetTargetType`
calls. That target-typing recursion is now fixed; keep the operator-binding tests in the core baseline as public semantic API coverage.

### 1. Skipped Tests To Revisit

These should either become focused current-behavior tests or be removed:

- Conditional element access codegen in `CodeGen/ConditionalAccessTests.cs`.
- Positional pattern codegen in `CodeGen/Patterns/PositionalPatternCodeGenTests.cs`.
- Legacy async lowering/codegen shape cases in `CodeGen/Async/AsyncTryAwaitCodeGenTests.cs` and `CodeGen/Async/AsyncFunctionExpressionStateMachineTests.cs`.
- Positional pattern assignment semantics in `Semantics/Patterns/PatternAssignmentSemanticTests.cs`.
- Environment-dependent reference assembly diagnostics in `Semantics/Diagnostics/FileScopedCodeDiagnosticsTests.cs` and `Semantics/Diagnostics/EntryPointDiagnosticsTests.cs`.
- Pending metadata accessibility enforcement in `Semantics/Diagnostics/AccessibilityDiagnosticsTests.cs`.
- Self member-access completion in `Completion/CompletionServiceMemberAccessTests.cs`.

### 2. Baseline Noise To Separate

Tests that exercise sample projects, macro project builds, or language-server sample replays should not run in the core baseline. Keep them in integration/sample suites:

- `Raven.LanguageServer.Tests` tests with `Sample` in the name.
- `Workspaces/MsBuildSampleProjectCompilationTests.cs`.
- Macro project reference tests that build throwaway `.rvnproj` macro assemblies.

### 3. Shape Assertions To Replace

Keep exact lowered/IL shape assertions only as development scaffolding. Prefer diagnostics, symbols, metadata shape, or observable runtime behavior:

- Lowered-tree mapping and lowerer-shape assertions in `Semantics/Lowering/`.
- Synthesized union helper/body tests that assert internal body availability rather than public symbol or emitted behavior.
- Opcode/called-member assertions in CodeGen tests when runtime behavior or metadata shape would prove the same contract.
- Exact full-string refactoring outputs where formatting-only differences are irrelevant; normalize or parse and compare syntax structure.

## Organization Recommendation

Keep the primary layer split:

- `Syntax/`
- `Semantics/`
- `CodeGen/`
- `Diagnostics/`
- `Completion/`
- `Workspaces/`

Within each layer, prefer feature cohesion over broad flat folders. The current `Semantics/Functions/*` and `CodeGen/Functions/*` split is the right direction, but older broad files remain:

- Move invocation and overload coverage into `Semantics/Functions/Invocation/` and `CodeGen/Functions/Invocation/`.
- Move async semantic coverage into `Semantics/Functions/Async/`.
- Keep parser-only pattern tests under `Syntax/`; keep semantic pattern binding under `Semantics/Patterns/`; keep emission/runtime behavior under `CodeGen/Patterns/`.
- Keep `Bugs/` temporary. Promote reduced regressions into the layer/feature folder once the behavior can be named generically.

## Recommended Next Pass

1. Continue public semantic API stabilization first. Highest-value buckets are declared symbols, type/base/interface resolution, nullable/default/type-info queries, lambda/contextual function binding, and completion through public semantic queries.
2. Triage the stale syntax buckets: nested recursive pattern parser expectations, parenthesized type-only catch syntax, and stored-property parser expectations.
3. Convert fragile lowered/IL/synthesized-body assertions in touched areas into symbol, diagnostic, metadata, or runtime assertions.
4. Split or update macro semantic tests so macro API drift is either fixed or isolated as integration coverage.
5. Add explicit test traits/categories for `CodeGen`, `Sample`, `Integration`, and `Development`, then simplify the scripts to trait filters.
