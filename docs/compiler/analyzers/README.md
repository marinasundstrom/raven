# Analyzers

Analyzers are compiler extensions that inspect Raven syntax and semantics and
report diagnostics for code that compiles but should still be surfaced to users. They run
through the workspace and compiler diagnostic pipeline, so analyzer diagnostics participate
in normal severity mapping, suppression, command-line output, and language-server diagnostic
publishing.

This section defines the expected behavior and authoring model for Raven analyzers:

- [Analyzer specification](specification.md) defines analyzer behavior.
- [Authoring analyzers](authoring.md) explains how to implement and test an analyzer.
- [Analyzer API surface](api-surface.md) describes the public types analyzers should use.
- [Analyzer configuration](configuration.md) explains severity, project-file modes, source
  suppression, and language-server reload behavior.

Related docs:

- [Diagnostics catalog](../diagnostics.md) covers compiler diagnostics and source pragmas.
- [Semantic analysis API](../api/semantic-analysis.md) covers semantic queries.
- [Operations API](../api/operations.md) covers operation trees for expression and statement
  analysis.
- [Analyzer verifier](../development/testing/analyzer-verifier.md) covers test helpers.
- [Built-in analyzer inventory](../development/analyzers.md) lists the analyzers shipped
  with Raven.

## Summary

Analyzers should be Roslyn-like:

- Report stable diagnostics with concise messages and stable IDs.
- Prefer semantic APIs, symbols, and operations over ad hoc source text inspection.
- Never throw for ordinary source input; report diagnostics or skip incomplete analysis.
- Treat cancellation as cooperative and immediate.
- Keep analyzer participation separate from diagnostic severity.
- Preserve compiler ownership of semantic state. Analyzers ask `Raven.CodeAnalysis` for
  semantic facts instead of managing binder or cache internals.

## Boundaries

Analyzers are not the home for every editor action. If a change is primarily a stylistic,
reversible rewrite, use the refactoring or suggestion pipeline instead of occupying the
diagnostics list. Diagnostics should represent correctness, policy, likely defects, or
high-signal maintainability guidance.
