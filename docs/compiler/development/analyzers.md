# Analyzers

Raven ships with Roslyn-style source code analyzers that inspect syntax and semantics and
report diagnostics. Analyzers run as part of compilation and can surface suggestions or
warnings for code that compiles but might benefit from additional annotations or fixes.

Raven currently provides analyzers for two different contexts:

- **MissingReturnTypeAnnotationAnalyzer** (Raven) – reports a diagnostic when a Raven
  function omits an explicit return type. The implementation is intentionally simple and
  serves as a reference for building analyzers that operate on Raven syntax and semantics.
  When the inferred type is `Unit` (the language's `void`), the analyzer suppresses the
  suggestion.
- **MatchExhaustivenessAnalyzer** (Raven) – reports informational diagnostics when a
  match expression is one case away from being exhaustive, or when a discard pattern could
  be replaced by a specific missing case to make the match exhaustive.
- **MemberCanBePrivateAnalyzer** (Raven, `RAV9016`) – reports when an internal/public
  member is only used from within its declaring type and can be narrowed to `private`.
- **MemberCanBeStaticAnalyzer** (Raven, `RAV9017`) – reports instance members that do not
  access instance state and can be made `static`.
- **UnusedPropertyAnalyzer** (Raven, `RAV9018`) – reports properties
  that are never referenced. In console applications, all properties are considered; in
  library-style outputs, only non-public properties are considered.
- **UnusedMethodAnalyzer** (Raven, `RAV9019`) – reports ordinary
  methods that are never invoked. In console applications, all ordinary methods are
  considered (entry points are excluded); in library-style outputs, only non-public methods
  are considered.
- **UnusedVariableAnalyzer** (Raven, `RAV9027` / `RAV9030`) – reports unused local
  variables, local pattern bindings, and callable parameters.
- **UnhandledMemberReturnValueAnalyzer** (Raven, `RAV9029`) – reports bare member
  invocations, property accesses, or field accesses whose returned value is not handled.
  Assign the returned value to a target, assign it to `_`, return it, or pass it on. The
  analyzer is disabled by default while it uses whole-analyzer mode; enable `full` mode in
  the project file or with the CLI, then control `RAV9029` severity through `.editorconfig`.

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
- `VarCanBeValAnalyzer`
- `MatchExhaustivenessAnalyzer`
- `PreferValInsteadOfLetAnalyzer`
- `AutoPropertyInitializationAnalyzer`
- `PreferNewLineBetweenDeclarationsAnalyzer`
- `UnnecessaryTrailingSeparatorAnalyzer`
- `ThrowStatementUseResultAnalyzer`
- `MemberCanBePrivateAnalyzer`
- `MemberCanBeStaticAnalyzer`
- `UnusedPropertyAnalyzer`
- `UnusedMethodAnalyzer`
- `UnhandledMemberReturnValueAnalyzer`
- `PreferDuLinqExtensionsAnalyzer`
- `PreferIsNullOverEqualityAnalyzer`
- `ConstructorParameterNamingAnalyzer`

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

`RAV9029` is off by default. Project files control the analyzer mode, and `.editorconfig`
controls severity. Legacy `.ravenproj` files can use `ReturnedValueHandlingMode="full"` or
`EnableReturnedValueAnalyzer="true|false"`. MSBuild-style `.rvnproj` files can use
`<RavenReturnedValueHandlingMode>full</RavenReturnedValueHandlingMode>` or
`<RavenEnableReturnedValueAnalyzer>true|false</RavenEnableReturnedValueAnalyzer>`. The only
non-off mode today is `full`.

For a concrete project sample that disables `RAV9012` (not-use-null), `RAV9013` (don't use
throw), and `RAV9014` (prefer Result/Option-based extensions), see
`samples/projects/analyzer-editorconfig/README.md`.

In the future, analyzers may offer code fixes such as suggesting a common base class or type
union when no annotation was added.
