# Code Formatting Guidelines

Raven has two formatting layers for syntax trees:

- `NormalizeWhitespace()` for full-tree normalization of detached or generated
  syntax.
- `Formatter.Format(...)` for targeted formatting driven by
  `Formatter.Annotation` and elastic trivia.

## General rules

- `SyntaxFactory` returns raw structured syntax. Do not assume factory-built
  trees are ready to print without explicit trivia or a formatting pass.
- Red `SyntaxFactory` convenience overloads should stay valid-by-construction.
  If a node needs descriptive or restricted public shapes, define them in
  `Syntax/Factories.xml` instead of exposing a raw full-slot overload.
- Prefer elastic trivia for generated whitespace that should be owned by the
  formatter.
- Preserve concrete comments, directives, and other non-whitespace trivia.
- Keep `NormalizeWhitespace()` and `Formatter.Format(...)` aligned so they share
  the same spacing and line-break rules.

## When adding syntax

- Update `SyntaxNormalizer` so the new syntax kind has stable spacing and line
  breaks.
- Ensure formatter-aware paths honor `Formatter.Annotation` and elastic trivia
  instead of rewriting unrelated parts of the tree.
- Add syntax tests for both full normalization and targeted formatting when the
  feature affects generated or rewritten code.

## Elastic trivia

Use elastic trivia as placeholder spacing:

- `SyntaxFactory.ElasticSpace`
- `SyntaxFactory.ElasticLineFeed`
- `SyntaxFactory.ElasticCarriageReturn`
- `SyntaxFactory.ElasticCarriageReturnLineFeed`
- `SyntaxFactory.ElasticTab`
- `SyntaxFactory.ElasticWhitespace(string)`

Elastic trivia should be treated as formatter-managed whitespace and replaced
with ordinary trivia during formatting.
