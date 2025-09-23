# BUGS

## Overview
`dotnet build` succeeds but `dotnet test test/Raven.CodeAnalysis.Tests` still reports skipped tests. The remaining known issue is grouped below by root cause.

## Prioritized failing test categories

- Method-group overload resolution prefers ambiguity over selecting the best match.

## Current failing tests

- `MethodReference` overload disambiguation – a simple program such as `let callback: System.Action<string> = Logger.Log` still reports `RAV2202` even though the class defines both `Log(string)` and `Log(object)`. The binder's method-group conversion treats every overload that accepts the delegate parameter type via an implicit conversion as equally valid and immediately marks the binding ambiguous instead of preferring the exact signature. See `BlockBinder.ConvertMethodGroupToDelegate`, which surfaces ambiguity whenever more than one candidate survives compatibility filtering rather than running overload resolution to pick the best match.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L820-L852】

### MethodReference overload disambiguation details

#### Problem analysis

* `ConvertMethodGroupToDelegate` currently stops after filtering the method group down to candidates that are merely *compatible* with the target delegate. Whenever more than one candidate survives, it always records ambiguity and reports `RAV2202`, even if one overload is a strictly better match (e.g., an identity conversion beats a reference conversion).
* `IsCompatibleWithDelegate` only checks parameter/ref-kind equality and whether each delegate parameter type is implicitly convertible to the method parameter type, so several overloads may pass that filter without any ranking information.
* The compiler already has rich overload-resolution logic that scores conversions, prefers identity matches, and breaks ties by specificity. That logic lives in `OverloadResolver.ResolveOverload`, which computes scores from the supplied argument expressions and tracks ambiguous candidates.
* Existing tests demonstrate the desired diagnostics pipeline for method references, but they do not cover a “string vs. object” scenario, so the regression slipped through.

#### Strategy to fix

1. **Reuse overload-resolution scoring for delegate conversions.** Extend the conversion path so that, when multiple methods remain after compatibility filtering, we feed the delegate’s `Invoke` signature into the overload resolver instead of immediately flagging ambiguity. We can do this either by synthesizing lightweight `BoundExpression` placeholders for each delegate parameter type or by adding a dedicated helper (e.g., `ResolveDelegateMethodGroup`) inside `OverloadResolver` that works directly from parameter-type metadata. This helper must mirror the existing scoring rules—identity beats reference or boxing conversions, numeric conversions rank above user-defined ones, etc.—so the same heuristics used for invocation apply to delegate creation.
2. **Handle `ref`/`out` delegates correctly.** Preserve the current compatibility guard that requires matching `RefKind`, but make sure the new resolution path can still evaluate such overloads. If we synthesize placeholder expressions, ensure they present the right by-ref surface (for example, by creating a tiny internal `DelegateArgumentExpression` that reports a `ByRefTypeSymbol` when needed) so the resolver treats them like real `ref` arguments.
3. **Update `ConvertMethodGroupToDelegate` outcomes.** When the resolver picks a single best overload, reuse that symbol when constructing the new `BoundMethodGroupExpression` so downstream codegen/semantic queries see the resolved method. If the resolver reports ambiguity, fall back to the existing diagnostic flow (`RAV2202`). If no candidate matches, keep reporting `RAV2203`. This preserves the current diagnostic behavior while allowing true disambiguation.
4. **Augment tests.** Add a semantic-model test that ensures `System.Action<string>` picks the `Log(string)` overload when `Log(object)` is also present, validating symbol selection and candidate reporting. Add or adjust a diagnostic test to assert no diagnostic is produced for that scenario, and re-evaluate whether the existing `Action<int>` + `{int,double}` test should now expect success (matching the resolver’s scoring rules) or be refocused on a truly ambiguous case, such as two equally ranked reference conversions.
5. **Regression coverage.** Once the binder change is in place, run the full semantic and diagnostic test suites to ensure method-group invocation scenarios (which already rely on `OverloadResolver`) continue to behave and that delegate inference still flows through correctly.

## Skipped tests

- `EntryPointDiagnosticsTests.ConsoleApp_WithoutMain_ProducesDiagnostic` – requires reference assemblies.
- `FileScopedCodeDiagnosticsTests.FileScopedCode_AfterDeclaration_ProducesDiagnostic` – requires reference assemblies.
- `FileScopedCodeDiagnosticsTests.Library_WithFileScopedCode_ProducesDiagnostic` – requires reference assemblies.
- `FileScopedCodeDiagnosticsTests.MultipleFiles_WithFileScopedCode_ProducesDiagnostic` – requires reference assemblies.

## Recently fixed

- `AsExpressionTests.AsCast_ReferenceType_ProducesNullableType` – semantic model now returns a nullable target type for reference-type `as` casts.
- `CastExpressionTests.ExplicitCast_Numeric_NoDiagnostic` – built-in numeric casts now resolve primitive types correctly.
- `MultiLineCommentTriviaTest.MultiLineCommentTrivia_IsLeadingTriviaOfToken` – multi-line comments are now treated as trivia rather than block statements.
- `LiteralTypeFlowTests.IfExpression_InferredLiteralUnion` – type inference now preserves literal union members produced by conditional expressions.
- `CastExpressionTests.ExplicitCast_Invalid_ProducesDiagnostic` and `AsExpressionDiagnosticTests.AsCast_Invalid_ProducesDiagnostic` – invalid cast diagnostics now report source and target types instead of literal values.
- `CollectionExpressionTests.ArrayCollectionExpressions_SpreadEnumerates` – array spreads now enumerate elements correctly.
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
