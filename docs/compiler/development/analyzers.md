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
The `Raven.Compiler` CLI uses `RavenWorkspace` to attach analyzers during compilation. Any
analyzer diagnostics appear alongside regular compilation errors and warnings.

Built-in code fixes currently include:
- `RAV9016` (`Make member private`)
- `RAV9017` (`Make member static`)
- `RAV9018` (`Remove unused property`)

For suggestion payload conventions (`--suggestions`, rewrite property keys, and option
gating), see `docs/compiler/development/suggestions.md`.

Analyzer severities can be configured through `.editorconfig` using standard keys such as
`dotnet_diagnostic.<ID>.severity`, `dotnet_diagnostic.*.severity`, and
`dotnet_analyzer_diagnostic.severity`.

For a concrete project sample that disables `RAV9012` (not-use-null), `RAV9013` (don't use
throw), and `RAV9014` (prefer Result/Option-based extensions), see
`samples/projects/analyzer-editorconfig/README.md`.

In the future, analyzers may offer code fixes such as suggesting a common base class or type
union when no annotation was added.
