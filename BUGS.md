# BUGS

## Overview
`dotnet build` succeeds but `dotnet test test/Raven.CodeAnalysis.Tests` currently reports 20 failing tests. The failures cluster into the categories below based on shared root causes.

## Prioritized failing test categories

1. **Return type inference and unit diagnostics**  \
   Early-return analysis miscomputes union return types and fails to warn about missing or mismatched returns. Functions without explicit return types should fall back to `unit` according to the spec【F:docs/lang/spec/language-specification.md†L40-L45】.  \
   Failing tests:
   - `ExplicitReturnInIfExpressionTests.ExplicitReturnInIfExpression_GlobalInitializer_ProducesDiagnostics`
   - `ExplicitReturnInIfExpressionTests.ExplicitReturnInIfExpressionInitializerProducesDiagnostics`

2. **Import and symbol resolution failures**  \
   Import directives and member lookups mis-handle ordering and aliasing. The spec requires all imports to precede alias or member declarations within a scope【F:docs/lang/spec/language-specification.md†L392-L394】.  \
   Failing tests:
   - `ImportResolutionTest.OpenGenericTypeWithoutTypeArguments_Should_ProduceDiagnostic`
   - `ImportResolutionTest.ImportNonNamespaceOrType_Should_ProduceDiagnostic`
   - `SemanticClassifierTests.ClassifiesTokensBySymbol`

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

5. **Workspace and utility failures**  \
   Tooling hooks do not load sample code correctly.  \
   Failing tests:
   - `Syntax.Tests.Sandbox.Test`

6. **Code generation**  \
   Emitted assemblies omit the mandatory `unit` type. The spec treats `unit` as the implicit return type for functions without annotations【F:docs/lang/spec/language-specification.md†L40-L45】.  \
   Failing tests:
 - `CodeGeneratorTests.Emit_ShouldAlwaysIncludeUnitType`


## Recently fixed

- `AnalyzerInfrastructureTests.GetDiagnostics_IncludesCompilerAndAnalyzerDiagnostics` – parser now buffers the requested position before rewinding, preventing `Position outside of buffer bounds` exceptions.
- `TypeSymbolInterfacesTests.Interfaces_ExcludeInheritedInterfaces` – class symbols now track only direct interfaces, excluding inherited ones.
- `NamespaceResolutionTest.ConsoleDoesNotContainWriteLine2_Should_ProduceDiagnostics` – diagnostic span now targets the undefined member name rather than the entire expression.
- `SymbolQueryTests.CallingInstanceMethodAsStatic_ProducesDiagnostic` – test now expects the diagnostic to highlight only the undefined member name.
- `MemberAccessMissingIdentifierTests.MemberAccessWithoutIdentifier_ReportsDiagnostic` – parser now reports a diagnostic instead of throwing when a member access is missing its identifier.
- Parser newline handling now treats line continuations as trivia and correctly skips tokens to end-of-file, so newline-related parser tests pass.
- Literal type flow defaults integers and floating-point literals to their expected primitive types, restoring correct type inference for numeric literals.

## Conclusion
The failing tests point to regressions across parsing, binding, diagnostics, and tooling. Each category above groups tests sharing the same underlying issue, guiding future investigation.

## Fix strategy and specification notes

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
