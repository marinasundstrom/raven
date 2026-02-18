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

## Subfolders

- Use subfolders when a layer grows by concern (example: `CodeGen/Metadata/` for attribute/type emission tests).
