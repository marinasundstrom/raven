# BUGS

## Overview
`dotnet build` succeeds but `dotnet test test/Raven.CodeAnalysis.Tests` currently reports 55 failing tests. The failures cluster into the categories below based on shared root causes.

## Prioritized failing test categories

1. **Parser newline handling and diagnostics**  \
   Newline tokens sometimes terminate statements even inside line continuations and missing-token scenarios throw instead of producing diagnostics. The spec states that statements end at newlines except inside parentheses, brackets, or braces【F:docs/lang/spec/language-specification.md†L56-L58】.  \
   Failing tests:
   - `ParserNewlineTests.Statement_NewlineIsTrivia_WhenInLineContinuation`
   - `ParserNewlineTests.Terminator_SkipsTokens_UntilEndOfFile`
   - `MemberAccessMissingIdentifierTests.MemberAccessWithoutIdentifier_ReportsDiagnostic`

2. **Imperative context and control-flow forms**  \
   `if`, `while`, and `for` have both expression and statement forms. `return` is allowed only when the construct is in statement position with no enclosing expression, but the compiler conflates these modes. The spec notes that control-flow constructs are expressions yet also have statement forms, and that `return` inside value contexts is invalid【F:docs/lang/spec/language-specification.md†L81-L83】【F:docs/lang/spec/language-specification.md†L103-L104】【F:docs/lang/spec/language-specification.md†L108-L115】.  \
   Failing tests:
   - `ImperativeContextTests.IfStatement_BranchesCanBeStatements`
   - `ImperativeContextTests.IfStatement_BranchesCanBeExpressions`

3. **Literal type flow broken**  \
   Numeric literals do not default to the expected primitive types, breaking type inference. The spec requires integers to default to `int` (promoting to `long` as needed) and floating-point literals to default to `double` unless suffixed with `f`/`F`【F:docs/lang/spec/language-specification.md†L169-L172】.  \
   Failing tests:
   - `LiteralTypeFlowTests.LiteralType_Double_UsesUnderlyingDouble`
   - `LiteralTypeFlowTests.VariableDeclaration_WithDoubleLiteral_InferredDouble`
   - `LiteralTypeFlowTests.VariableDeclaration_WithFloatSuffix_InferredFloat`
   - `LiteralTypeFlowTests.VariableDeclaration_WithLargeInteger_InferredLong`
   - `LiteralTypeFlowTests.LiteralType_Float_UsesUnderlyingSingle`
   - `LiteralTypeFlowTests.LiteralType_Long_UsesUnderlyingInt64`

4. **Return type inference and unit diagnostics**  \
   Early-return analysis miscomputes union return types and fails to warn about missing or mismatched returns. Functions without explicit return types should fall back to `unit` according to the spec【F:docs/lang/spec/language-specification.md†L40-L45】.  \
   Failing tests:
   - `EarlyReturnTypeInferenceTests.ReturnTypeCollector_InfersUnionFromImplicitFinalExpression`
   - `EarlyReturnTypeInferenceTests.ReturnTypeCollector_InfersUnionFromEarlyReturns`
   - `ReturnStatementUnitTests.NonUnitMethod_EmptyReturn_ReportsDiagnostic`
   - `ReturnStatementUnitTests.NonUnitMethod_ReturnExpression_NotAssignable_ReportsDiagnostic`
   - `ExplicitReturnInIfExpressionTests.ExplicitReturnInIfExpression_GlobalInitializer_ProducesDiagnostics`
   - `ExplicitReturnInIfExpressionTests.ExplicitReturnInIfExpressionInitializerProducesDiagnostics`

5. **Import and symbol resolution failures**  \
   Import directives and member lookups mis-handle ordering and aliasing. The spec requires all imports to precede alias or member declarations within a scope【F:docs/lang/spec/language-specification.md†L392-L394】.  \
   Failing tests:
   - `ImportResolutionTest.OpenGenericTypeWithoutTypeArguments_Should_ProduceDiagnostic`
   - `ImportResolutionTest.ImportNonNamespaceOrType_Should_ProduceDiagnostic`
   - `NamespaceResolutionTest.ConsoleDoesNotContainWriteLine2_Should_ProduceDiagnostics`
   - `SymbolQueryTests.CallingInstanceMethodAsStatic_ProducesDiagnostic`
   - `CompletionServiceTests.GetCompletions_OnNamespaceAlias_ReturnsMembers`
   - `SemanticClassifierTests.ClassifiesTokensBySymbol`

6. **Union features incomplete**  \
   Assigning or emitting unions is partially implemented. The spec states that converting a union to a target succeeds only if every member converts to the target type【F:docs/lang/spec/language-specification.md†L199-L201】.  \
   Failing tests:
   - `UnionConversionTests.UnionNotConvertibleToExplicitType_ProducesDiagnostic`
   - `UnionEmissionTests.CommonBaseClass_WithNull_UsesBaseTypeAndNullable`

7. **Analyzer diagnostics ignored**  \
   Analyzer configuration flags are ignored, so analyzer diagnostics either fail to run or cannot be suppressed.  \
   Failing tests:
   - `AnalyzerInfrastructureTests.GetDiagnostics_IncludesCompilerAndAnalyzerDiagnostics`
   - `DiagnosticOptionsTests.RunAnalyzers_False_DisablesAnalyzerDiagnostics`
   - `MissingReturnTypeAnnotationAnalyzerTests.MethodWithoutAnnotation_SuggestsInferredReturnType`
   - `MissingReturnTypeAnnotationAnalyzerTests.MethodWithoutAnnotation_WithMultipleReturnTypes_SuggestsUnion`
   - `MissingReturnTypeAnnotationAnalyzerTests.FunctionStatementWithoutAnnotation_SuggestsInferredReturnType`

8. **Workspace and utility failures**  \
   Tooling hooks do not load sample code correctly.  \
   Failing tests:
   - `Syntax.Tests.Sandbox.Test`

9. **Code generation**  \
   Emitted assemblies omit the mandatory `unit` type. The spec treats `unit` as the implicit return type for functions without annotations【F:docs/lang/spec/language-specification.md†L40-L45】.  \
   Failing tests:
   - `CodeGeneratorTests.Emit_ShouldAlwaysIncludeUnitType`


## Conclusion
The failing tests point to regressions across parsing, binding, diagnostics, and tooling. Each category above groups tests sharing the same underlying issue, guiding future investigation.

## Fix strategy and specification notes

- **Parser newline handling** – Honor the statement terminator rules that treat newlines as separators outside of brackets or braces【F:docs/lang/spec/language-specification.md†L56-L58】.
- **Imperative context** – Clarify and implement the distinction between expression forms and statement forms of control-flow constructs like `if` and `while`, and forbid `return` inside expression contexts as the specification requires【F:docs/lang/spec/language-specification.md†L81-L83】【F:docs/lang/spec/language-specification.md†L103-L104】【F:docs/lang/spec/language-specification.md†L108-L115】.
- **Literal type flow** – Propagate numeric literal types according to the defaulting rules (`int`/`long` and `double`/`float`) so downstream inference sees the correct primitive types【F:docs/lang/spec/language-specification.md†L169-L172】.
- **Return type inference and unit diagnostics** – Align type inference with union semantics and the `unit` return rules in the specification【F:docs/lang/spec/language-specification.md†L40-L45】.
- **Import and symbol resolution** – Enforce ordering and wildcard rules for `import` directives and alias resolution as described in the specification【F:docs/lang/spec/language-specification.md†L392-L394】.
- **Union features** – Implement missing union conversion checks and metadata emission following the rule that every member must convert to the target type【F:docs/lang/spec/language-specification.md†L199-L201】.
- **Analyzer diagnostics** – Revisit the diagnostic pipeline so analyzer and compiler warnings share configuration and reporting. Ensure `DiagnosticOptions` flow into analyzer drivers and add tests for custom suppressions.
- **Workspace and utility failures** – Audit highlighters and tooling hooks so diagnostics surface consistently; add integration tests for console rendering.
- **Code generation and sample program loading** – Always emit the `unit` type and populate sample programs to verify end-to-end execution【F:docs/lang/spec/language-specification.md†L40-L45】.

### Specification ambiguities

- The spec lacks guidance on incremental syntax tree behavior, leaving update semantics open to interpretation.
- Line-continuation versus newline-as-terminator rules could be elaborated to avoid parser ambiguity.
- Import resolution for generic types and namespace/member precedence needs explicit wording.
- The specification defines `if` expressions for expression positions and `if` statements for statement positions, but the term *imperative context* (no ancestor expression) could be made more explicit.
