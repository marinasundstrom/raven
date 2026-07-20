# Analyzers

Raven ships with Roslyn-style analyzers that inspect syntax and semantics and
report diagnostics. Analyzers run as part of compilation and can surface suggestions or
warnings for code that compiles but might benefit from additional annotations or fixes.

For the analyzer specification, authoring guidance, API surface, and configuration model, see
[Analyzers](../analyzers/README.md) and the
[analyzer specification](../analyzers/specification.md). This page tracks the
built-in analyzer set and the diagnostics-vs-refactoring boundary.
The live compiler/workspace/LSP ownership model for analyzer scheduling and
diagnostic publication is described in
[Live semantic model](../architecture/live-semantic-model.md).

Raven currently provides analyzers for two different contexts:

- **MissingReturnTypeAnnotationAnalyzer** (Raven) – reports a diagnostic when a Raven
  function omits an explicit return type. The implementation is intentionally simple and
  serves as a reference for building analyzers that operate on Raven syntax and semantics.
  When the inferred type is `Unit` (the language's `void`), the analyzer suppresses the
  suggestion.
- **MemberCanBePrivateAnalyzer** (Raven, `RAV9016`) – reports when an internal/public
  member is only used from within its declaring type and can be narrowed to `private`.
- **MemberCanBeStaticAnalyzer** (Raven, `RAV9017`) – reports instance members that do not
  access instance state and can be made `static`.
- **UnusedPropertyAnalyzer** (Raven, `RAV9018`) – reports properties
  that are never referenced. In console applications, all properties are considered; in
  library-style outputs, only non-public properties are considered. Properties required
  by a virtual/override or interface contract are excluded.
- **UnusedMethodAnalyzer** (Raven, `RAV9019`) – reports ordinary
  methods that are never invoked. In non-library applications, all ordinary methods are
  considered (entry points are excluded, including async entry points that require a
  synthesized bridge); in library-style outputs, only non-public methods are considered.
  Methods required by a virtual/override or interface contract are excluded.
- **UnusedLocalAnalyzer** (Raven, `RAV9027`) – reports unused local variables and local
  pattern bindings.
- **UnusedParameterAnalyzer** (Raven, `RAV9030`) – reports unused callable parameters.
  Parameters on override and virtual, override, and interface implementation methods are
  excluded because the contract fixes the signature.
  `UnusedVariableAnalyzer` remains as a compatibility analyzer name for existing project
  configuration; disabling `UnusedVariableAnalyzer` disables both local and parameter
  checks.
- **UninitializedPropertyAnalyzer** (Raven, `RAV9006`) – reports stored properties,
  including explicit auto-accessor and implicit stored-property forms, that are not
  initialized by a property initializer, constructor, or instance initializer block.
- **UninitializedFieldAnalyzer** (Raven, `RAV9032`) – reports explicit private fields
  that are not initialized by a field initializer, constructor, or instance initializer
  block.
- **UnusedImportDirectiveAnalyzer** (Raven, `RAV9031`) – reports wildcard namespace
  imports whose declaring compilation-unit or namespace scope does not reference any
  imported type, top-level namespace member, or nested namespace member. Nested namespaces
  are included in the declaring import scope.
- **UnusedExpressionResultAnalyzer** (Raven, `RAV9034`) – reports standalone expressions
  whose value is known to be unused when the expression is a conservative composition of
  literals, variables, unary operators, binary operators, and tuples. Calls and other
  potentially effectful expressions are excluded.
- **UnhandledMemberReturnValueAnalyzer** (Raven, `RAV9029`) – reports bare member
  invocations, property accesses, or field accesses whose returned value is not handled.
  Assign the returned value to a target, assign it to `_`, return it, or pass it on. The
  analyzer is disabled by default while it uses whole-analyzer mode; enable `full` mode in
  the project file or with the CLI, then control `RAV9029` severity through `.editorconfig`.
- **DisposableObjectAnalyzer** (Raven, `RAV9033`) – reports disposable objects produced
  by calls or object creation when they are assigned to an ordinary local or discarded
  without a matching `use` declaration or direct `Dispose()` call before scope exit. The
  analyzer intentionally starts with conservative local flow and does not attempt ownership
  inference through arbitrary calls.

The `Raven.Compiler` CLI uses `RavenWorkspace` to attach analyzers during compilation. Any
analyzer diagnostics appear alongside regular compilation errors and warnings.

## Diagnostic vs refactoring boundary

Not every built-in rewrite should stay diagnostic-backed. The current rule is:

- Keep an analyzer when the user should see a problem or policy signal in the diagnostics list.
- Use a context-driven refactoring/suggestion when the rewrite is primarily stylistic, reversible,
  and not meaningfully "wrong" code.

Built-in analyzers that should remain diagnostic-backed include:

- `MissingReturnTypeAnnotationAnalyzer`
- `EventDelegateMustBeNullableAnalyzer`
- `NonNullDeclarationsAnalyzer`
- `VarCanBeLetAnalyzer`
- `UninitializedPropertyAnalyzer`
- `UninitializedFieldAnalyzer`
- `PreferNewLineBetweenDeclarationsAnalyzer`
- `UnnecessaryTrailingSeparatorAnalyzer`
- `ThrowStatementUseResultAnalyzer`
- `MemberCanBePrivateAnalyzer`
- `MemberCanBeStaticAnalyzer`
- `UnusedPropertyAnalyzer`
- `UnusedMethodAnalyzer`
- `UnusedImportDirectiveAnalyzer`
- `UnhandledMemberReturnValueAnalyzer`
- `DisposableObjectAnalyzer`
- `PreferDuLinqExtensionsAnalyzer`
- `PreferIsNullOverEqualityAnalyzer`
- `ConstructorParameterNamingAnalyzer`

`PreferLetInsteadOfValAnalyzer` (`RAV9035`) remains available as an optional
style-policy analyzer with a corresponding code fix, but it is not registered
by `AddBuiltInAnalyzers`. Hosts or projects that want to enforce `let` for
immutable lexical bindings must add it explicitly. It does not apply to
properties or other signature-like declarations, where `val` remains the
preferred read-only spelling.

The following built-in rewrites are now exposed through the refactoring/suggestion pipeline instead of
the built-in analyzer set:

- `PreferTargetTypedUnionCaseAnalyzer`
- `PreferTargetTypedUnionCaseInTargetTypedContextAnalyzer`
- `SingleStatementBlockBodyAnalyzer`
- `ExpressionBodyToBlockBodyAnalyzer`
- `RedundantAccessorDeclarationAnalyzer`
- `StringConcatenationAnalyzer` (`RAV9021` / `RAV9022`)

These are treated as editor actions because they mostly express alternative source shapes, not
correctness or policy violations. They show up as on-demand suggestions rather than occupying the
normal diagnostics stream.

`PreferNewLineBetweenDeclarationsAnalyzer` is intentionally not in that promotion list: it is closer
to formatting policy than refactoring and should eventually be handled by formatting configuration
instead of either diagnostics or refactoring providers.

`UnnecessaryTrailingSeparatorAnalyzer` (`RAV9028`) currently covers comma-delimited separated lists
with a closing delimiter, such as parameter lists, type-argument lists, and similar comma-only
syntax forms. It warns when a real trailing comma is present before the closing token. Implicit
newline-delimited boundaries represented by `SyntaxKind.None` are not reported.

Built-in code fixes currently include:
- `RAV2100` (`Add missing match arm`)
- `RAV2103` (`Remove redundant catch-all arm`)
- `RAV9016` (`Make member private`)
- `RAV9017` (`Make member static`)
- `RAV9018` (`Remove unused property`)
- `RAV9012` (`Use 'Option<T>'`, `Rewrite nullable flow to Option pattern matching`)

Built-in refactorings currently include:
- `Convert if/else to match`
- target-typed union-case rewrites
- expression-body/block-body conversions
- redundant accessor removal
- string concatenation rewrites

The `RAV9012` split is intentional:

- the diagnostic-backed fixes handle Raven's nullable/`Option<T>` guidance and stop at
  `Option<T>` plus `if maybeValue is Some(...)` style flow
- the separate `Convert if/else to match` refactoring owns control-flow reshaping and can
  be applied independently to pattern-based `if` statements, including non-`Option` code

`RAV9012` reports explicit nullable declaration annotations. It does not report inferred
target declarations such as `val x = ...`, even when the initializer has a nullable type.

For suggestion payload conventions (`--suggestions`, rewrite property keys, and option
gating), see `docs/compiler/development/suggestions.md`.

Analyzer severities can be configured through `.editorconfig` using standard keys such as
`dotnet_diagnostic.<ID>.severity`, `dotnet_diagnostic.*.severity`, and
`dotnet_analyzer_diagnostic.severity`.
The language server watches `.editorconfig` and reapplies diagnostic severity changes to open
projects without reopening the project.

Analyzer authors should keep analyzer participation separate from diagnostic severity.
`DiagnosticDescriptor.DefaultSeverity` supplies the default level, and `.editorconfig` should
remap that level by diagnostic ID. If a built-in analyzer needs a project-file mode, implement
`ICompilationOptionsAwareAnalyzer.ShouldAnalyze` and model the mode on `CompilationOptions`;
do not use descriptor disabled-by-default state or severity options to decide whether the
analyzer runs.

## Semantic API usage

Analyzers should use Raven's Roslyn-shaped semantic query APIs for local questions:

- `SemanticModel.GetDeclaredSymbol(...)`
- `SemanticModel.GetSymbolInfo(...)`
- `SemanticModel.GetTypeInfo(...)`
- `SemanticModel.GetOperation(...)`
- focused Raven semantic helpers such as `SemanticModel.GetMatchExhaustiveness(...)`
- narrow `Compilation` lookup APIs when a rule explicitly needs compilation-level symbols

These methods may lazily bind compiler-owned semantic state. They are intended to answer the
requested symbol or type question without forcing full diagnostic collection as a side effect.
Prefer asking one of these narrow APIs for the syntax node currently being analyzed over walking
global namespace views or recomputing broad semantic state.

Avoid broad diagnostic APIs from analyzer callbacks, including:

- `Compilation.GetDiagnostics(...)`
- `SemanticModel.GetDiagnostics(...)`
- workspace/project/document diagnostic APIs
- diagnostics-with-analyzers APIs

Those APIs can force broad binding, collect binder diagnostics, or re-enter analyzer execution.
That is appropriate for compiler and language-server diagnostic pipelines, but it is usually the
wrong shape inside an analyzer that is already running as part of diagnostic production.

If an analyzer only needs to avoid running on syntactically invalid input, use syntax diagnostics
from the current tree as a cheap guard. Code fixes should use the diagnostics supplied by
`CodeFixContext` instead of recomputing diagnostics.

Built-in analyzers may use internal helpers when needed, but they should preserve the same
boundary: semantic query APIs answer narrow semantic questions; diagnostic APIs produce
diagnostics.

When an analyzer stores symbols for later matching, always use
`SymbolEqualityComparer.Default`. Raven's lazy binding and diagnostic binding paths can
return different symbol instances for the same declaration, especially after another semantic
query has already materialized part of a binder-owned cache. Analyzer logic must compare
what the symbol represents, not which object instance was returned first. This applies to
candidate maps, used-symbol sets, de-duplication, and comparisons against well-known symbols.

`RAV9029` is off by default. Project files control the analyzer mode, and `.editorconfig`
controls severity. Deprecated `.ravenproj` files can use `ReturnedValueHandlingMode="full"` or
`EnableReturnedValueAnalyzer="true|false"` while they remain supported. MSBuild-style `.rvnproj` files can use
`<RavenReturnedValueHandlingMode>full</RavenReturnedValueHandlingMode>` or
`<RavenEnableReturnedValueAnalyzer>true|false</RavenEnableReturnedValueAnalyzer>`. The only
non-off mode today is `full`.

For a concrete project sample that disables `RAV9012` (not-use-null), `RAV9013` (don't use
throw), and `RAV9014` (prefer Result/Option-based extensions), see
`samples/projects/analyzer-editorconfig/README.md`.

In the future, analyzers may offer code fixes such as suggesting a common base class or type
union when no annotation was added.
