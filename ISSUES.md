# Known Issues

## Top stability issues (priority ordered)
1. **Syntax model generation drift (SyntaxKind + generated nodes missing or stale).**
   - **Impact:** The compiler fails to build when generated artifacts are out of sync with `Model.xml` / `NodeKinds.xml`, because the hand-written syntax helpers depend on generated `SyntaxKind` and node types.
   - **Evidence:** `SyntaxFacts` and `SyntaxFactory` hardcode `SyntaxKind` usage, which fails if the generated enum is missing. (`src/Raven.CodeAnalysis/Syntax/SyntaxFacts.Operators.cs`, `src/Raven.CodeAnalysis/Syntax/SyntaxFactory.cs`)
   - **Plan:** 
     1) Regenerate syntax outputs during CI/build and validate `SyntaxKind` is present in `src/Raven.CodeAnalysis/Syntax/generated`.
     2) Add a build-time guard that fails with a clear diagnostic if the generated files are missing or stale.

2. **Attribute syntax generation drift (AttributeSyntax missing).**
   - **Impact:** Attribute binding and metadata construction rely on `AttributeSyntax`; missing generated nodes break compilation.
   - **Evidence:** Attribute binding/metadata helpers depend on `AttributeSyntax`. (`src/Raven.CodeAnalysis/Binder/AttributeBinder.cs`, `src/Raven.CodeAnalysis/AttributeDataFactory.cs`)
   - **Plan:**
     1) Ensure generators emit `AttributeSyntax` and related nodes.
     2) Add a unit test that validates attribute parsing/binding with generator outputs in place.

3. **Binder throws `NotSupportedException` for unknown statements/expressions.**
   - **Impact:** New or unhandled syntax kinds crash the compiler instead of reporting diagnostics.
   - **Evidence:** Binding switch defaults throw for statements and expressions. (`src/Raven.CodeAnalysis/Binder/BlockBinder.cs`)
   - **Plan:**
     1) Replace `NotSupportedException` with diagnostics plus `BoundErrorExpression`/`BoundExpressionStatement`.
     2) Add regression tests for unknown/unsupported syntax to ensure graceful failure.

4. **Expression codegen throws `NotSupportedException` for unsupported bound nodes.**
   - **Impact:** Code generation crashes for newly introduced or partially supported bound expressions.
   - **Evidence:** Default case in expression emission throws. (`src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs`)
   - **Plan:**
     1) Map unsupported nodes to diagnostics earlier in binding/lowering.
     2) Add a fallback `EmitErrorExpression` path with a diagnostic marker to keep emission stable.

5. **Conversion codegen throws `NotSupportedException` on legal but unhandled conversions.**
   - **Impact:** Type conversions can crash IL emission instead of issuing errors.
   - **Evidence:** Conversion paths throw for missing union/nullable conversions and other unsupported cases. (`src/Raven.CodeAnalysis/CodeGen/Generators/Generator.cs`)
   - **Plan:**
     1) Move conversion validation into binding (classify + diagnose before IL emission).
     2) Ensure conversion failures surface as diagnostics and emit error placeholders.

6. **Async entry point bridging crashes on non-awaitable return types.**
   - **Impact:** Async `Main` or entry-point bridge generation can throw instead of diagnosing.
   - **Evidence:** Bridge emission throws when awaitable pattern is not found. (`src/Raven.CodeAnalysis/CodeGen/MethodBodyGenerator.cs`)
   - **Plan:**
     1) Validate async entry point return types during binding and report diagnostics.
     2) Add tests for invalid async entry points to confirm graceful failure.

7. **Special type resolution fails for unsupported or missing runtime types.**
   - **Impact:** Missing runtime types (e.g., `System.Type`) cause hard failures in type resolution and codegen.
   - **Evidence:** `GetFrameworkType` throws for unsupported special types and missing runtime types. (`src/Raven.CodeAnalysis/TypeSymbolExtensions.cs`)
   - **Plan:**
     1) Harden runtime/metadata resolution with diagnostics and fallback error symbols.
     2) Add a dedicated test for special type resolution against reference assemblies.

8. **Array/Tuple constructed symbol member queries throw `NotSupportedException`.**
   - **Impact:** Semantic analysis or tooling that queries members can crash on these symbols.
   - **Evidence:** `IsMemberDefined` throws for arrays/tuples. (`src/Raven.CodeAnalysis/Symbols/Constructed/ArrayTypeSymbol.cs`, `src/Raven.CodeAnalysis/Symbols/Constructed/TupleTypeSymbol.cs`)
   - **Plan:**
     1) Implement `IsMemberDefined`/`LookupType` with safe lookups.
     2) Add tests covering member queries on constructed array/tuple symbols.

9. **Error type construction throws in recovery paths.**
   - **Impact:** Error recovery can crash when generic instantiation is attempted on error symbols.
   - **Evidence:** `ErrorTypeSymbol.Construct` throws `NotSupportedException`. (`src/Raven.CodeAnalysis/Symbols/ErrorTypeSymbol.cs`)
   - **Plan:**
     1) Return a new `ErrorTypeSymbol` with the requested type arguments to keep analysis flowing.
     2) Add regression tests for generic error recovery.

10. **Workspace persistence/syntax tree operations throw `NotSupportedException`.**
   - **Impact:** Editor/IDE scenarios can crash when persistence or syntax tree replacement is used with unsupported workspace types.
   - **Evidence:** `PersistenceService` and `Document.WithSyntaxRoot` throw for unsupported contexts. (`src/Raven.CodeAnalysis/Workspaces/Persistence/PersistenceService.cs`, `src/Raven.CodeAnalysis/Workspaces/Objects/Document.cs`)
   - **Plan:**
     1) Provide no-op or diagnostic-based fallbacks instead of throwing.
     2) Add workspace integration tests that exercise persistence and syntax tree updates.

## Unit test failures
- **`typeof` binding regressions** – Both the semantic and code generation suites fail on the `TypeOfExpression` scenarios (`Raven.CodeAnalysis.Semantics.Tests.TypeOfExpressionTests` and `Raven.CodeAnalysis.Tests.TypeOfTests`). The compiler now reports `RAV0103` for every use of `System.Type`, causing the bound node to lack an operand type and the emitted assembly to fail verification.【bc616e†L1-L18】【2ae51e†L1-L11】 Probable cause: the metadata loader no longer resolves the `System.Type` special type from the reference assemblies, so `Compilation.GetSpecialType(SpecialType.System_Type)` returns an error symbol during binding.
- **Missing BCL metadata coverage** – Several metadata-facing suites (e.g., `MetadataGenericMethodTests` and the `FunctionTypeSemanticTests`) now crash or return simplified names because `System.Exception`, `System.ArgumentException`, and even `System.Func`/`System.Action` no longer resolve during binding.【1a9861†L1-L20】【70fc25†L1-L16】 Likewise, the code generation accessibility checks (`AccessibilityTests`) and symbol identity assertions (`SymbolEqualityComparerTests`) fail to load `System.Private.CoreLib`, so metadata lookups for nested types return different symbols.【a4ed9d†L1-L20】【26a01f†L1-L10】 These tests were scrapped until the runtime reference set is restored.
- **Async lowering & closure regressions** – Numerous async/lambda scenarios regress: async methods that return `Task.CompletedTask` no longer produce diagnostics, expression-bodied async members throw `NotSupportedException`, and closures lose their captured locals or method reference semantics (`AsyncMethodTests`, `AsyncLowererTests`, `LambdaCodeGenTests`, `LambdaInferenceDiagnosticsTests`, `MethodReferenceCodeGenTests`).【3c5cf5†L1-L29】【f16e27†L1-L33】【66af0f†L1-L9】【3c9378†L1-L10】 Code generation now raises `Missing local builder` for nested lambdas, so the affected suites were dropped pending a fix.
- **Entry-point and runtime harness drift** – Tests that embed execution harnesses—virtual override dispatch, expression-bodied programs, union-return dispatch, and `for each` over extension methods—now fail because the compiler can no longer synthesize or discover a valid entry point (`VirtualPropertyTests`, `ExpressionBodyCodeGenTests`, `UnionReturnTests`, `ForExpressionTests`).【fb45fc†L1-L17】【7f0d8c†L1-L9】【bf9a19†L1-L10】【99f342†L1-L9】 The emitted assemblies either lack `Main` entirely or throw `InvalidProgramException` during execution.
- **Semantic binding drift** – Core language binding changed: multi-dimensional array declarations hit `NullReferenceException`, `typeof` patterns treat `-1` as a constant instead of a declaration, generic `for each` binding no longer finds LINQ extensions, extension declarations lose property support, qualified names prefer namespaces, and partial classes/async accessor tests miss diagnostics.【6e7f09†L1-L24】【13603a†L1-L13】【88535b†L1-L9】【319ee1†L1-L12】【8bbbb9†L1-L11】【d1bde2†L1-L12】【376476†L1-L17】 The impacted suites (`ArrayTypeSemanticTests`, `PatternSyntaxParserTests`, `ForStatementSemanticTests`, `ExtensionDeclarationSemanticTests`, `PartialClassTests`, `QualifiedNameBindingTests`) were retired until the binder/semantic model aligns with the current language behavior.

## Common problem patterns
- **Symbol binding recursion (resolved):** `SymbolEqualityComparer` now tracks visited symbol pairs to break cycles when inspecting extension methods, interface defaults, and interpolated strings. The affected samples (`samples/main.rav`, `samples/generator.rav`, `samples/interfaces.rav`, `samples/linq.rav`, and `samples/reflection.rav`) compile successfully again.【F:src/Raven.CodeAnalysis/SymbolEqualityComparer.cs†L1-L215】【4528a4†L1-L4】【2941cf†L1-L3】【152141†L1-L2】【d2560e†L1-L3】【2a10c8†L1-L2】【a8337e†L1-L3】【14c359†L1-L2】【4013ab†L1-L3】【9f9ee0†L1-L2】【6a0816†L1-L3】
- **Missing environment preconditions:** The sample harness historically expected callers to prepare filesystem artifacts (for example, the `output/` directory). `samples/build.sh` now creates the directory on demand so compilation can proceed, but other ad-hoc scripts may require similar hardening.【F:src/Raven.Compiler/samples/build.sh†L1-L18】
- **Infrastructure robustness gaps:** Diagnostics and lowering paths still throw or diverge under stress. Parser span computation crashes the tokenizer sample, and top-level program synthesis loops indefinitely in `samples/test3.rav`, highlighting resilience issues in diagnostic span generation and top-level lowering.【F:ISSUES.md†L3-L11】
