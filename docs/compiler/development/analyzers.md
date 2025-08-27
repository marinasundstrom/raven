# Analyzers

Raven ships with Roslyn-style source code analyzers that inspect syntax and semantics and
report diagnostics. Analyzers run as part of compilation and can surface suggestions or
warnings for code that compiles but might benefit from additional annotations or fixes.

Currently available analyzers:

- **MissingReturnTypeAnnotationAnalyzer** – reports a diagnostic when a function omits an
  explicit return type. The analyzer computes the inferred type and suggests annotating the
  method for clarity.

The `Raven.Compiler` CLI uses `RavenWorkspace` to attach analyzers during compilation. Any
analyzer diagnostics appear alongside regular compilation errors and warnings.

In the future, analyzers may offer code fixes such as suggesting a common base class or type
union when no annotation was added.
