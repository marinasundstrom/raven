# Analyzers

Raven ships with Roslyn-style source code analyzers that inspect syntax and semantics and
report diagnostics. Analyzers run as part of compilation and can surface suggestions or
warnings for code that compiles but might benefit from additional annotations or fixes.

Currently available analyzers:

- **MissingReturnTypeAnnotationAnalyzer** – reports a diagnostic when a function omits an
  explicit return type. By surfacing the inferred type, the analyzer nudges developers to
  weigh the clarity of explicit annotations often favored in object-oriented code against
  the brevity and flexibility of functional style. Teams can then deliberately pick the
  approach that best fits their codebase. When the inferred type is `Unit` (the language's
  `void`), the analyzer suppresses the suggestion.

The `Raven.Compiler` CLI uses `RavenWorkspace` to attach analyzers during compilation. Any
analyzer diagnostics appear alongside regular compilation errors and warnings.

In the future, analyzers may offer code fixes such as suggesting a common base class or type
union when no annotation was added.
