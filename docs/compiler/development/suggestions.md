# Suggestions

Raven suggestions are analyzer diagnostics that carry an optional educational rewrite payload.
They are intended to show "you wrote X, write Y instead" guidance and support future interactive
fix workflows.

## Purpose

- Keep suggestion logic in analyzers, not in parser/binder error paths.
- Let the CLI and other hosts render suggestions consistently.
- Keep support open for external analyzers by using shared diagnostic properties.

## Convention

Suggestion payload is stored in `Diagnostic.Properties` using:

- `raven.suggestion.originalCode`
- `raven.suggestion.rewrittenCode`

Use `/Users/robert/Projects/Raven/src/Raven.CodeAnalysis/Diagnostics/Infrastructure/SuggestionsDiagnosticProperties.cs`:

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
