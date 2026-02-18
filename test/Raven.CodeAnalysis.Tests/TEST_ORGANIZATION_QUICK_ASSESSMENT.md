# Quick Assessment (Test Sorting)

Date: 2026-02-18
Scope: `test/Raven.CodeAnalysis.Tests` (`*.cs`)

## Snapshot

- Total test files scanned: `349`
- Current top-level distribution:
  - `Syntax`: 70
  - `Semantics`: 142
  - `CodeGen`: 59
  - `Diagnostics`: 24
  - `Other` (Bugs/Utilities/etc): 54

## Quick Heuristic Result

Using a light signal model (`Emit/Assembly/PE` => codegen, semantic-model API => semantics, parser/token API => syntax, explicit diagnostic assertions => diagnostics), there are `57` location mismatches.

Important: many are **intentional mixed tests**, not pure misplacements.

## Sorted Triage

### 1. Move Now (high confidence)

Already completed in this reorg pass:

- `Semantics/*Emission*` moved to `CodeGen/Metadata/`
- `Semantics/DiscriminatedUnionGenericsTests.cs` moved to `CodeGen/Runtime/`
- `Semantics/*Diagnostics*` and other diagnostics-first files moved to `Semantics/Diagnostics/`
- `Semantics/*Lowerer*` and lowerer-shape tests moved to `Semantics/Lowering/`
- `Semantics/PartialClassTests.cs` split into:
  - `Semantics/PartialClassTests.cs` (semantic merge behavior)
  - `Semantics/Diagnostics/PartialClassDiagnosticsTests.cs` (duplicate declaration diagnostic)
  - `CodeGen/Runtime/PartialClassCodeGenTests.cs` (emit smoke)
- `Semantics/NamespaceDirectiveTests.cs` updated for current behavior and syntax-only assertion moved to `Syntax/NamespaceDirectiveSyntaxTests.cs`
- `Semantics/MatchStatementTests.cs` split into:
  - `Semantics/MatchStatementTests.cs` (binding/semantic behavior)
  - `Semantics/Diagnostics/MatchStatementDiagnosticsTests.cs` (exhaustiveness/redundancy/ignored-value diagnostics)
- `Semantics/SealedHierarchyTests.cs` split into:
  - `Syntax/SealedHierarchySyntaxTests.cs` (parse/bodyless syntax coverage)
  - `Semantics/Diagnostics/SealedHierarchyDiagnosticsTests.cs` (sealed hierarchy diagnostics)
  - `Semantics/SealedHierarchyTests.cs` (semantic/model/emit behavior)
- `Semantics/SealedHierarchyTests.cs` now avoids `Emit` for semantic assertions; IL-specific checks moved to `CodeGen/Runtime/SealedHierarchyCodeGenTests.cs`
- Moved additional diagnostics-first semantics files to `Semantics/Diagnostics/`:
  - `BreakAndContinueStatementTests.cs`
  - `GotoStatementTests.cs`
  - `ExceptionHandlingTests.cs`
  - `ConstructorInitializerTests.cs`
  - `DefaultParameterTests.cs`
- Moved emit-metadata async tests from `Semantics/Lowering/AsyncLowererTests.cs` to `CodeGen/Metadata/AsyncLowererCodeGenMetadataTests.cs`
- Reduced direct `Emit(...)` usage in `Semantics/` from 31 to 2 (remaining in `TypeResolutionPrecedenceTests.cs` metadata fixture setup)

### 2. Split Then Move (mixed responsibility)

These are mixed tests that combine semantic assertions with emit/runtime checks. Split into two files before moving.

- `/Users/robert/Projects/Raven/test/Raven.CodeAnalysis.Tests/Semantics/ExternSemanticTests.cs`

### 3. Keep In Place (despite heuristic mismatch)

These look syntax-heavy but are still primarily syntax/parser behavior (including parser diagnostics), so they should remain under `Syntax/`.

- `/Users/robert/Projects/Raven/test/Raven.CodeAnalysis.Tests/Syntax/AttributeParsingTests.cs`
- `/Users/robert/Projects/Raven/test/Raven.CodeAnalysis.Tests/Syntax/Parser/ArgumentAndParameterListTests.cs`
- `/Users/robert/Projects/Raven/test/Raven.CodeAnalysis.Tests/Syntax/Parser/DirectiveOrderTests.cs`
- `/Users/robert/Projects/Raven/test/Raven.CodeAnalysis.Tests/Syntax/TupleTypeSyntaxTest.cs`

### 4. Diagnostics-heavy Semantics (do not mass-move)

A large set in `Semantics/` is diagnostics-focused but still tied to semantic rules. Keep folder placement for now; consider adding a naming suffix (`*DiagnosticsTests`) as the organizational marker instead of moving.

Examples:

- `/Users/robert/Projects/Raven/test/Raven.CodeAnalysis.Tests/Semantics/Diagnostics/WithExpressionTests.cs`
- `/Users/robert/Projects/Raven/test/Raven.CodeAnalysis.Tests/Semantics/Diagnostics/ThrowStatementDiagnosticsTests.cs`
- `/Users/robert/Projects/Raven/test/Raven.CodeAnalysis.Tests/Semantics/Diagnostics/TypeModifierDiagnosticsTests.cs`
- `/Users/robert/Projects/Raven/test/Raven.CodeAnalysis.Tests/Semantics/Diagnostics/VariableRedeclarationTests.cs`

## Recommended Next Sort Step

1. Split `PartialClassTests` into semantic-only and emit/runtime assertions.
2. Continue moving clearly diagnostics-first files from `Semantics/` to `Semantics/Diagnostics/`.

This keeps moves safe and avoids churn from broad folder-only reshuffles.
