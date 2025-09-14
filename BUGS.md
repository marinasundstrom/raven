# BUGS

## Overview
`dotnet build` succeeds but `dotnet test test/Raven.CodeAnalysis.Tests` currently reports 13 failing tests and 8 skipped tests. The failures cluster into the categories below based on shared root causes.

## Prioritized failing test categories

1. **Import and symbol resolution failures**  \\
   Import directives and member lookups mis-handle ordering and aliasing. The spec requires all imports to precede alias or member declarations within a scope【F:docs/lang/spec/language-specification.md†L392-L394】.

2. **Literal type conversions missing**  \\
   Literal arguments retain their literal types instead of converting to their underlying primitives before overload resolution, contrary to the specification【F:docs/lang/type-system.md†L76-L78】.  \\
   Failing tests:
   - `StringInterpolationTests.InterpolatedString_FormatsCorrectly`
   - `NamespaceResolutionTest.ConsoleDoesContainWriteLine_ShouldNot_ProduceDiagnostics`
   - `ImportResolutionTest.WildcardTypeImport_MakesStaticMembersAvailable`
   - `Issue84_MemberResolutionBug.CanResolveMember`
   - `NullableTypeTests.ConsoleWriteLine_WithStringLiteral_Chooses_StringOverload`
   - `TargetTypedExpressionTests.TargetTypedMethodBinding_UsesAssignmentType`

3. **Union features incomplete**  \
   Assigning or emitting unions is partially implemented. The spec states that converting a union to a target succeeds only if every member converts to the target type【F:docs/lang/spec/language-specification.md†L199-L201】.  \
   Failing tests:
   - `UnionEmissionTests.CommonBaseClass_WithNull_UsesBaseTypeAndNullable`

4. **Analyzer diagnostics ignored**  \
   Analyzer configuration flags are ignored, so analyzer diagnostics either fail to run or cannot be suppressed.  \
   Failing tests:
   - `MissingReturnTypeAnnotationAnalyzerTests.MethodWithoutAnnotation_SuggestsInferredReturnType`
   - `MissingReturnTypeAnnotationAnalyzerTests.MethodWithoutAnnotation_WithMultipleReturnTypes_SuggestsUnion`
   - `MissingReturnTypeAnnotationAnalyzerTests.FunctionStatementWithoutAnnotation_SuggestsInferredReturnType`

## Current failing tests

- `StringInterpolationTests.InterpolatedString_FormatsCorrectly` – binder cannot resolve string concatenation because literal segments keep their literal types.
- `NamespaceResolutionTest.ConsoleDoesContainWriteLine_ShouldNot_ProduceDiagnostics` – `Console.WriteLine` with a string literal reports `RAV1501` because the literal fails to convert to `string`.
- `AliasResolutionTest.AliasDirective_UsesAlias_Tuple_TypeMismatch_ReportsDiagnostic` – expected `RAV1503` diagnostic is missing for tuple element mismatch.
- `ImportResolutionTest.WildcardTypeImport_MakesStaticMembersAvailable` – wildcard import of `System.Console` still triggers `RAV1501` when a string literal is passed to `WriteLine`.
- `LiteralTypeFlowTests.IfExpression_InferredLiteralUnion` – literal branches of an `if` expression lose their literal types and are inferred as `String | Int32`.
- `Issue84_MemberResolutionBug.CanResolveMember` – `DateTime.Parse` with a string literal fails with `RAV1501`.
- `CollectionExpressionTests.ArrayCollectionExpressions_SpreadEnumerates` – spread operator in array collection expressions fails to emit bytecode.
- `GreenTreeTest.FullWidth_Equals_Source_Length` – parsed tree reports a full width one character larger than the source text.
- `UnionEmissionTests.CommonBaseClass_WithNull_UsesBaseTypeAndNullable` – emitting a union type with `null` does not succeed.
- `ExpressionSemanticTest.WriteLine_WithUnitVariable_ShouldNot_ProduceDiagnostics` – writing a `unit` value to `Console.WriteLine` triggers `RAV1501`.
- `UnionConversionTests.UnionAssignedToObject_ReturnsNoDiagnostic` – assigning a union of classes to `object` still reports `RAV1503` diagnostics.
- `NullableTypeTests.ConsoleWriteLine_WithStringLiteral_Chooses_StringOverload` – invocation symbol is null because the string literal fails to convert to `string`.
- `TargetTypedExpressionTests.TargetTypedMethodBinding_UsesAssignmentType` – target-typed `.Parse("42")` call fails with `RAV1501` when the string literal keeps its literal type.

## Skipped tests

- `TypeSymbolInterfacesTests.Interfaces_ExcludeInheritedInterfaces` – interface declarations not implemented.
- `Syntax.Tests.Sandbox.Test` – skipped due to excessive output until tooling supports large trees.
- `EntryPointDiagnosticsTests.ConsoleApp_WithoutMain_ProducesDiagnostic` – requires reference assemblies.
- `VersionStampTests.GetNewerVersion_InSameTick_IncrementsLocal` – same-tick version increments are unreliable across environments.
- `UnionEmissionTests.CommonInterface_UsesInterfaceInSignature` – interface declarations not implemented.
- `FileScopedCodeDiagnosticsTests.FileScopedCode_AfterDeclaration_ProducesDiagnostic` – requires reference assemblies.
- `FileScopedCodeDiagnosticsTests.Library_WithFileScopedCode_ProducesDiagnostic` – requires reference assemblies.
- `FileScopedCodeDiagnosticsTests.MultipleFiles_WithFileScopedCode_ProducesDiagnostic` – requires reference assemblies.

## Recently fixed

- `AnalyzerInfrastructureTests.GetDiagnostics_IncludesCompilerAndAnalyzerDiagnostics` – parser now buffers the requested position before rewinding, preventing `Position outside of buffer bounds` exceptions.
- `TypeSymbolInterfacesTests.Interfaces_ExcludeInheritedInterfaces` – class symbols now track only direct interfaces, excluding inherited ones.
- `NamespaceResolutionTest.ConsoleDoesNotContainWriteLine2_Should_ProduceDiagnostics` – diagnostic span now targets the undefined member name rather than the entire expression.
- `SymbolQueryTests.CallingInstanceMethodAsStatic_ProducesDiagnostic` – test now expects the diagnostic to highlight only the undefined member name.
- `MemberAccessMissingIdentifierTests.MemberAccessWithoutIdentifier_ReportsDiagnostic` – parser now reports a diagnostic instead of throwing when a member access is missing its identifier.
- Parser newline handling now treats line continuations as trivia and correctly skips tokens to end-of-file, so newline-related parser tests pass.
- Literal type flow defaults integers and floating-point literals to their expected primitive types, restoring correct type inference for numeric literals.

- `ExplicitReturnInIfExpressionTests.*` – return statements in expression contexts now report `RAV1900` and their surrounding blocks infer union member types correctly.

- `ImportResolutionTest.ImportNonNamespaceOrType_Should_ProduceDiagnostic` – diagnostic span now excludes the wildcard, highlighting only the invalid target.
- `ImportResolutionTest.OpenGenericTypeWithoutTypeArguments_Should_ProduceDiagnostic` – open generic type references without type arguments now report `RAV0305`.
- `Syntax.Tests.Sandbox.Test` – disabled excessive output that caused logger failures during test execution.
- `SemanticClassifierTests.ClassifiesTokensBySymbol` – import binder now surfaces static members from wildcard type imports, allowing classifiers to resolve symbols correctly.
- `CodeGeneratorTests.Emit_ShouldAlwaysIncludeUnitType` – emitted assemblies now define `System.Unit`, enabling successful emission when functions return `unit` implicitly.
- Diagnostic verifier now safely formats expected diagnostic messages, preventing `FormatException` crashes when argument counts mismatch.

## Conclusion
The failing tests point to regressions across parsing, binding, diagnostics, and tooling. Each category above groups tests sharing the same underlying issue, guiding future investigation.

## Fix strategy and specification notes

- **Import and symbol resolution** – Enforce ordering and wildcard rules for `import` directives and alias resolution as described in the specification【F:docs/lang/spec/language-specification.md†L392-L394】.
- **Union features** – Implement missing union conversion checks and metadata emission following the rule that every member must convert to the target type【F:docs/lang/spec/language-specification.md†L199-L201】.
- **Analyzer diagnostics** – Revisit the diagnostic pipeline so analyzer and compiler warnings share configuration and reporting. Ensure `DiagnosticOptions` flow into analyzer drivers and add tests for custom suppressions.
- **Workspace and utility failures** – Audit highlighters and tooling hooks so diagnostics surface consistently; add integration tests for console rendering.

### Specification ambiguities

- The spec lacks guidance on incremental syntax tree behavior, leaving update semantics open to interpretation.
- Line-continuation versus newline-as-terminator rules could be elaborated to avoid parser ambiguity.
- Import resolution for generic types and namespace/member precedence needs explicit wording.
- The specification defines `if` expressions for expression positions and `if` statements for statement positions, but the term *imperative context* (no ancestor expression) could be made more explicit.
