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
- **TypeUnionAnalyzer** (C#) – enforces the semantics of `[TypeUnion]` attributes in C#
  code. Members annotated with `TypeUnionAttribute` must use `object` or `dynamic` as
  their CLR type, and any value assigned, returned, or matched against them must implicitly
  convert to at least one of the declared union members. The analyzer also validates
  pattern and switch cases and only permits `null` when `null` is included in the union.

The `Raven.Compiler` CLI uses `RavenWorkspace` to attach analyzers during compilation. Any
analyzer diagnostics appear alongside regular compilation errors and warnings.

In the future, analyzers may offer code fixes such as suggesting a common base class or type
union when no annotation was added.
