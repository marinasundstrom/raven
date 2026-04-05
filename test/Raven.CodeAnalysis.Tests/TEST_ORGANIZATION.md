# Test Organization

Use folders to reflect the primary test layer:

- `Syntax/`: parser/lexer/tree shape only.
- `Semantics/`: binding, symbols, type checking, diagnostics from semantic analysis.
- `CodeGen/`: IL/runtime emission and execution behavior.
- `Diagnostics/`: analyzer/code-fix and educational diagnostics behavior.
- `Bugs/`: regression tests that don't yet fit cleanly in one layer.

## Placement rules

1. If a test's main assertion depends on `Emit(...)`, runtime reflection, or IL shape, place it under `CodeGen/`.
2. If a test's main assertion uses semantic model APIs (`GetTypeInfo`, `GetSymbolInfo`, `GetBoundNode`), place it under `Semantics/`.
3. If a test validates tokens/nodes/parse recovery only, place it under `Syntax/`.
4. Prefer adding new tests to the correct layer instead of adding to `Bugs/`.

Regression tests are not a special category by default. If a failing case belongs to overload resolution, metadata loading, pattern matching, or any other identifiable feature area, turn it into a normal test under that feature and name it for the language behavior being asserted, not the issue number or report source.

## Within-layer grouping

Within a layer, group by feature area instead of leaving broad families flat.

- `Semantics/Functions/Async/`: async binding and async function semantics.
- `Semantics/Functions/Invocation/`: overload resolution, named/optional/params arguments, method references, and invocation rules.
- `Semantics/Functions/Lambdas/`: lambda capture and inference.
- `Semantics/Functions/Iterators/`: iterator binding semantics.
- `Semantics/Functions/Parameters/`: parameter passing rules that are not primarily overload selection.
- `CodeGen/Async/`: async lowering/runtime behavior.
- `CodeGen/Functions/Invocation/`: invocation-oriented emission and runtime binding.
- `CodeGen/Patterns/`: match/pattern emission coverage.
- `Utilities/Framework/`: target framework and reference-assembly tests.

When a regression area exists in multiple layers, keep the feature name aligned across those layers. Example: overload-resolution and invocation rules live under `Semantics/Functions/Invocation/`, and the related emission coverage lives under `CodeGen/Functions/Invocation/`.

## Regression suites

Use `scripts/test-feature-suite.sh` for targeted regression passes. Current suites:

- `overload-resolution`
- `functions-async`
- `patterns`
- `extensions`
- `partials`
- `macros`
- `imports-and-namespaces`
- `framework-and-targeting`

The suite script is intentionally curated rather than purely namespace-driven. Some regressions cut across folders, so the suite definition should reflect the real feature boundary.

## Bugs folder

`Bugs/` is a temporary holding area only.

- Move a regression out of `Bugs/` as soon as you can state the behavior generically.
- Prefer reducing external dependencies and harnesses before promoting a regression test.
- If a test still depends on a package harness, sample project, or mixed syntax/semantic assertions, keep it temporary until it can be split or simplified.

## Subfolders

- Use subfolders when a layer grows by concern (example: `CodeGen/Metadata/` for attribute/type emission tests).
