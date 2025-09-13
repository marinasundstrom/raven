# BUGS

## Overview
`dotnet build` succeeds but `dotnet test test/Raven.CodeAnalysis.Tests` currently reports 53 failing tests. The failures cluster into the categories below based on shared root causes.

## Prioritized failing test categories

1. **Parser newline handling and diagnostics**  \
   Newline tokens sometimes terminate statements even inside line continuations and missing-token scenarios throw instead of producing diagnostics. The spec states that statements end at newlines except inside parentheses, brackets, or braces【F:docs/lang/spec/language-specification.md†L56-L58】.  \
   Failing tests:
   - `ParserNewlineTests.Statement_NewlineIsTrivia_WhenInLineContinuation`
   - `ParserNewlineTests.Terminator_SkipsTokens_UntilEndOfFile`

2. **Literal type flow broken**  \
   Numeric literals do not default to the expected primitive types, breaking type inference. The spec requires integers to default to `int` (promoting to `long` as needed) and floating-point literals to default to `double` unless suffixed with `f`/`F`【F:docs/lang/spec/language-specification.md†L169-L172】.  \
   Failing tests:
   - `LiteralTypeFlowTests.LiteralType_Double_UsesUnderlyingDouble`
   - `LiteralTypeFlowTests.VariableDeclaration_WithDoubleLiteral_InferredDouble`
   - `LiteralTypeFlowTests.VariableDeclaration_WithFloatSuffix_InferredFloat`
   - `LiteralTypeFlowTests.VariableDeclaration_WithLargeInteger_InferredLong`
   - `LiteralTypeFlowTests.LiteralType_Float_UsesUnderlyingSingle`
   - `LiteralTypeFlowTests.LiteralType_Long_UsesUnderlyingInt64`

3. **Return type inference and unit diagnostics**  \
   Early-return analysis miscomputes union return types and fails to warn about missing or mismatched returns. Functions without explicit return types should fall back to `unit` according to the spec【F:docs/lang/spec/language-specification.md†L40-L45】.  \
   Failing tests:
   - `ExplicitReturnInIfExpressionTests.ExplicitReturnInIfExpression_GlobalInitializer_ProducesDiagnostics`
   - `ExplicitReturnInIfExpressionTests.ExplicitReturnInIfExpressionInitializerProducesDiagnostics`

4. **Import and symbol resolution failures**  \
   Import directives and member lookups mis-handle ordering and aliasing. The spec requires all imports to precede alias or member declarations within a scope【F:docs/lang/spec/language-specification.md†L392-L394】.  \
   Failing tests:
   - `ImportResolutionTest.OpenGenericTypeWithoutTypeArguments_Should_ProduceDiagnostic`
   - `ImportResolutionTest.ImportNonNamespaceOrType_Should_ProduceDiagnostic`
   - `SemanticClassifierTests.ClassifiesTokensBySymbol`

5. **Union features incomplete**  \
   Assigning or emitting unions is partially implemented. The spec states that converting a union to a target succeeds only if every member converts to the target type【F:docs/lang/spec/language-specification.md†L199-L201】.  \
   Failing tests:
   - `UnionEmissionTests.CommonBaseClass_WithNull_UsesBaseTypeAndNullable`

6. **Analyzer diagnostics ignored**  \
   Analyzer configuration flags are ignored, so analyzer diagnostics either fail to run or cannot be suppressed.  \
   Failing tests:
   - `MissingReturnTypeAnnotationAnalyzerTests.MethodWithoutAnnotation_SuggestsInferredReturnType`
   - `MissingReturnTypeAnnotationAnalyzerTests.MethodWithoutAnnotation_WithMultipleReturnTypes_SuggestsUnion`
   - `MissingReturnTypeAnnotationAnalyzerTests.FunctionStatementWithoutAnnotation_SuggestsInferredReturnType`

7. **Workspace and utility failures**  \
   Tooling hooks do not load sample code correctly.  \
   Failing tests:
   - `Syntax.Tests.Sandbox.Test`

8. **Code generation**  \
   Emitted assemblies omit the mandatory `unit` type. The spec treats `unit` as the implicit return type for functions without annotations【F:docs/lang/spec/language-specification.md†L40-L45】.  \
   Failing tests:
 - `CodeGeneratorTests.Emit_ShouldAlwaysIncludeUnitType`


## Recently fixed

- `AnalyzerInfrastructureTests.GetDiagnostics_IncludesCompilerAndAnalyzerDiagnostics` – parser now buffers the requested position before rewinding, preventing `Position outside of buffer bounds` exceptions.
- `SemanticClassifierTests.ClassifiesTokensBySymbol` – parser now tolerates missing namespace terminators, eliminating `ArgumentNullException` crashes.
- `TypeSymbolInterfacesTests.Interfaces_ExcludeInheritedInterfaces` – class symbols now track only direct interfaces, excluding inherited ones.
- `NamespaceResolutionTest.ConsoleDoesNotContainWriteLine2_Should_ProduceDiagnostics` – diagnostic span now targets the undefined member name rather than the entire expression.
- `SymbolQueryTests.CallingInstanceMethodAsStatic_ProducesDiagnostic` – test now expects the diagnostic to highlight only the undefined member name.
- `MemberAccessMissingIdentifierTests.MemberAccessWithoutIdentifier_ReportsDiagnostic` – parser now reports a diagnostic instead of throwing when a member access is missing its identifier.

## Conclusion
The failing tests point to regressions across parsing, binding, diagnostics, and tooling. Each category above groups tests sharing the same underlying issue, guiding future investigation.

## Fix strategy and specification notes

- **Parser newline handling** – Honor the statement terminator rules that treat newlines as separators outside of brackets or braces【F:docs/lang/spec/language-specification.md†L56-L58】.
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
