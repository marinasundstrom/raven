# Suggestions

Raven currently has two different "suggestion-like" mechanisms:

- analyzer-backed suggestions: diagnostics that carry an optional educational rewrite payload
- context-driven refactorings: code actions that are not backed by any diagnostic

Analyzer-backed suggestions are intended to show "you wrote X, write Y instead" guidance and support
future interactive fix workflows. Context-driven refactorings are for changes that should appear in
the action list without also showing up as errors, warnings, or informational diagnostics.

## Purpose

- Keep suggestion logic in analyzers, not in parser/binder error paths.
- Let the CLI and other hosts render suggestions consistently.
- Keep support open for external analyzers by using shared diagnostic properties.
- Keep purely stylistic or reversible transforms out of the diagnostics list when they are better
  expressed as on-demand refactorings.

## Choosing the mechanism

Use an analyzer-backed suggestion when:

- the user should see a diagnostic in the problems list
- the suggestion is part of a policy or language guidance rule
- the suggestion benefits from `EnableSuggestions` gating or educational rewrite payloads

Use a context-driven refactoring when:

- the action is purely optional and not a problem report
- the transform is reversible and mostly about source shape
- showing a diagnostic would create noise without adding signal

Current built-in refactorings include:

- `Convert if/else to match`
- target-typed union-case rewrites
- expression-body/block-body conversions
- redundant accessor removal
- string concatenation rewrites

These are implemented as standard refactoring providers and no longer depend on the built-in analyzer
diagnostics stream.

In VS Code, context-driven refactorings appear when the caret or selection
intersects an eligible construct and the user opens the refactor/code-action
menu. They do not appear in the Problems list. `Convert if/else to match`
requires an `else` branch and a pattern condition (`if value is pattern` or
`if let pattern = value`); it is not offered for an arbitrary Boolean
condition.

The nullable-to-`Option<T>` migration is intentionally split across the two mechanisms:

- the `RAV9012` code fixes keep the semantic migration on the diagnostic side
  (`Use 'Option<T>'` and `Rewrite nullable flow to Option pattern matching`)
- the general `Convert if/else to match` action is a context-driven refactoring so it can
  be applied to pattern-based `if` statements broadly, not only to `Option<T>` flows

`Convert if/else to match` preserves the original catch-all `else` semantics
with a `_` arm. A refactoring must not infer a complementary union case from a
case-like name alone.

The scoped `RAV9012` nullable-flow rewrite is available for an explicitly
annotated nullable local when its uses are contained by a following `if`
pattern. It is deliberately not offered for inferred locals or when the
nullable local escapes that guarded flow.

## Convention

Suggestion payload is stored in `Diagnostic.Properties` using:

- `raven.suggestion.originalCode`
- `raven.suggestion.rewrittenCode`

Use `/Users/marina/Projects/Raven/src/Raven.CodeAnalysis/Diagnostics/Infrastructure/SuggestionsDiagnosticProperties.cs`:

- `CreateRewriteSuggestion(originalCode, rewrittenCode)`
- `TryGetRewriteSuggestion(diagnostic, out originalCode, out rewrittenCode)`
- `IsSuggestionModeEnabled(compilation)`

## Option Gating

Suggestion behavior is gated by compilation option `EnableSuggestions` (CLI: `--suggestions`).

- Built-in suggestion analyzers should return early when suggestions are disabled.
- Diagnostics that carry the suggestion rewrite properties are filtered out when suggestions are disabled.
- Rendering of suggestion blocks is only enabled when `--suggestions` is set.

## External Analyzer Guidance

External analyzers can participate by:

1. Reporting normal analyzer diagnostics.
2. Attaching rewrite properties via the same keys above.
3. Optionally checking `compilation.Options.EnableSuggestions` before doing expensive suggestion work.

No special compiler hook is required beyond the shared property convention.

Context-driven refactorings use `/Users/marina/Projects/Raven/src/Raven.CodeAnalysis/Workspaces/CodeFixes/CodeRefactoringProvider.cs`
and are surfaced by the workspace/language server without requiring a backing diagnostic.
