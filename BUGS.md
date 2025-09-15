# BUGS

## Overview
`dotnet build` succeeds but `dotnet test test/Raven.CodeAnalysis.Tests` still reports skipped tests. The remaining known issue is grouped below by root cause.

## Prioritized failing test categories

- Pattern matching and `is`/`as` expressions can produce null references or missing diagnostics.
- Cast expressions mis-handle primitive types and report incorrect conversion diagnostics.
- Conditional access emission is incomplete.
- Parser misclassifies multi-line comments as block statements.

## Current failing tests

- `CastExpressionTests.ExplicitCast_Invalid_ProducesDiagnostic` – mismatched `RAV1503` diagnostics reference the literal `1` instead of the source and target types (bug per explicit cast rules).
- `CastExpressionTests.ExplicitCast_Numeric_NoDiagnostic` – emitting `RAV1503` and `RAV0103` for built-in numeric casts shows primitive type resolution failures (bug).
- `AsExpressionTests.AsCast_ReferenceType_ProducesNullableType` – `as` casts on reference types yield `null` instead of a nullable target type (bug).
- `AsExpressionDiagnosticTests.AsCast_Invalid_ProducesDiagnostic` – diagnostics for invalid `as` casts report literal values rather than types (bug).
- `SampleProgramsTests.Sample_should_load_into_compilation("type-unions.rav")` – `BoundIsPatternExpression` throws `NullReferenceException` while binding patterns (bug; pattern matching not defined in the spec).
- `MissingReturnTypeAnnotationAnalyzerTests.MethodWithBranchesReturningVoid_NoDiagnostic` – same `BoundIsPatternExpression` null reference during semantic analysis (bug; spec gap).
- `PatternVariableTests.PatternVariableInIfCondition_EmitsSuccessfully` – pattern binding null reference prevents emission (bug; spec gap).
- `LiteralTypeFlowTests.IfExpression_InferredLiteralUnion` – literal unions are not inferred through `if` expressions, contradicting literal type and union rules (bug).
- `MultiLineCommentTriviaTest.MultiLineCommentTrivia_IsLeadingTriviaOfToken` – parser casts `EmptyStatementSyntax` to `BlockStatementSyntax` when handling comments (bug).
- `ConditionalAccessTests.ConditionalAccess_NullableValue_ReturnsValue` – code generation throws `NotSupportedException` for nullable conditional access (bug).

## Skipped tests

- `EntryPointDiagnosticsTests.ConsoleApp_WithoutMain_ProducesDiagnostic` – requires reference assemblies.
- `FileScopedCodeDiagnosticsTests.FileScopedCode_AfterDeclaration_ProducesDiagnostic` – requires reference assemblies.
- `FileScopedCodeDiagnosticsTests.Library_WithFileScopedCode_ProducesDiagnostic` – requires reference assemblies.
- `FileScopedCodeDiagnosticsTests.MultipleFiles_WithFileScopedCode_ProducesDiagnostic` – requires reference assemblies.

## Recently fixed

- `CollectionExpressionTests.ArrayCollectionExpressions_SpreadEnumerates` – array spreads now enumerate elements correctly.
- `LiteralTypeFlowTests.IfExpression_InferredLiteralUnion` – `if` expressions now preserve literal types when inferring unions.
- Literal arguments now convert to their underlying primitive types before overload resolution, fixing tests such as `StringInterpolationTests.InterpolatedString_FormatsCorrectly`, `NamespaceResolutionTest.ConsoleDoesContainWriteLine_ShouldNot_ProduceDiagnostics`, `ImportResolutionTest.WildcardTypeImport_MakesStaticMembersAvailable`, `Issue84_MemberResolutionBug.CanResolveMember`, `NullableTypeTests.ConsoleWriteLine_WithStringLiteral_Chooses_StringOverload`, and `TargetTypedExpressionTests.TargetTypedMethodBinding_UsesAssignmentType`.
- Analyzer configuration flags are respected so analyzer diagnostics can be suppressed; the `MissingReturnTypeAnnotationAnalyzerTests.*` suite now passes.
- `AnalyzerInfrastructureTests.GetDiagnostics_IncludesCompilerAndAnalyzerDiagnostics` – parser now buffers the requested position before rewinding, preventing `Position outside of buffer bounds` exceptions.
- `GreenTreeTest.FullWidth_Equals_Source_Length` – skipping tokens now trims duplicated leading trivia and restores newline handling so the parsed tree's full width matches the source length.
- `TypeSymbolInterfacesTests.Interfaces_ExcludeInheritedInterfaces` – class symbols now track only direct interfaces, excluding inherited ones.
- `NamespaceResolutionTest.ConsoleDoesNotContainWriteLine2_Should_ProduceDiagnostics` – diagnostic span now targets the undefined member name rather than the entire expression.
- `SymbolQueryTests.CallingInstanceMethodAsStatic_ProducesDiagnostic` – test now expects the diagnostic to highlight only the undefined member name.
- `MemberAccessMissingIdentifierTests.MemberAccessWithoutIdentifier_ReportsDiagnostic` – parser now reports a diagnostic instead of throwing when a member access is missing its identifier.
- Parser newline handling now treats line continuations as trivia and correctly skips tokens to end-of-file, so newline-related parser tests pass.
- Literal type flow defaults integers and floating-point literals to their expected primitive types, restoring correct type inference for numeric literals.

- `ExplicitReturnInIfExpressionTests.*` – return statements in expression contexts now report `RAV1900` and their surrounding blocks infer union member types correctly.

- `ImportResolutionTest.ImportNonNamespaceOrType_Should_ProduceDiagnostic` – diagnostic span now excludes the wildcard, highlighting only the invalid target.
- `ImportResolutionTest.OpenGenericTypeWithoutTypeArguments_Should_ProduceDiagnostic` – open generic type references without type arguments now report `RAV0305`.
- `Syntax.Tests.Sandbox.Test` – trimmed excessive output and re-enabled to verify successful compilation.
- `SemanticClassifierTests.ClassifiesTokensBySymbol` – import binder now surfaces static members from wildcard type imports, allowing classifiers to resolve symbols correctly.
- `CodeGeneratorTests.Emit_ShouldAlwaysIncludeUnitType` – emitted assemblies now define `System.Unit`, enabling successful emission when functions return `unit` implicitly.
- Diagnostic verifier now safely formats expected diagnostic messages, preventing `FormatException` crashes when argument counts mismatch.
- `AliasResolutionTest.AliasDirective_UsesAlias_Tuple_TypeMismatch_ReportsDiagnostic` – tuple alias assignments now emit `RAV1503` when element types mismatch.
- `VersionStampTests.GetNewerVersion_InSameTick_IncrementsLocal` – deterministic timestamp seeding validates same-tick local increments reliably.

- Union arguments now participate in overload resolution using their common denominator type, ensuring the most specific overload is selected.

## Conclusion
Implementing remaining union conversion checks and reference-assembly diagnostics will bring the test suite closer to green.

## Fix strategy and specification notes

- **Workspace and utility failures** – Audit highlighters and tooling hooks so diagnostics surface consistently; add integration tests for console rendering.

### Specification ambiguities

- The spec lacks guidance on incremental syntax tree behavior, leaving update semantics open to interpretation.
- Line-continuation versus newline-as-terminator rules could be elaborated to avoid parser ambiguity.
- Import resolution for generic types and namespace/member precedence needs explicit wording.
- The specification defines `if` expressions for expression positions and `if` statements for statement positions, but the term *imperative context* (no ancestor expression) could be made more explicit.
- Pattern matching and conditional access semantics are not yet covered, so the expected behavior of related constructs is unclear.
