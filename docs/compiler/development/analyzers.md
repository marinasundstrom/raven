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
- **TypeUnionAnalyzer** (C#) – enforces the semantics of `[TypeUnion]` attributes in C#
  code. Members annotated with `TypeUnionAttribute` must use a CLR type that is assignable
  from all declared union members (e.g., `object`, `dynamic`, or a suitable base type), and
  any value assigned, returned, or matched against them must implicitly convert to at least
  one of the declared union members. The analyzer also validates pattern and switch cases
  and only permits `null` when `null` is included in the union.

The `Raven.Compiler` CLI uses `RavenWorkspace` to attach analyzers during compilation. Any
analyzer diagnostics appear alongside regular compilation errors and warnings.

Analyzer severities can be configured through `.editorconfig` using standard keys such as
`dotnet_diagnostic.<ID>.severity`, `dotnet_diagnostic.*.severity`, and
`dotnet_analyzer_diagnostic.severity`.

For a concrete project sample that disables `RAV9012` (not-use-null), `RAV9013` (don't use
throw), and `RAV9014` (prefer Result/Option-based extensions), see
`samples/project-files/analyzer-editorconfig/README.md`.

In the future, analyzers may offer code fixes such as suggesting a common base class or type
union when no annotation was added.
