# Macro and DSL developer experience

This document defines the intended authoring and tooling experience for Raven
procedural macros, especially macros that embed domain-specific languages.
Implementation sequencing is tracked separately in
[the macro implementation plan](implementation-plan.md).

## Experience goals

A macro DSL should feel like a first-class, contained language region:

* the Raven file parses even when the macro plugin is unavailable
* the authored macro body is preserved exactly
* diagnostics point into the authored body
* completion, highlighting, hover, and navigation can be supplied by the macro
* embedded Raven fragments use Raven parsing and semantic services
* expansion is visible and debuggable
* editing remains responsive and results are tied to one document snapshot
* failures in a macro or its tooling do not crash the compiler or editor

The compiler owns macro semantic truth. The language server schedules requests
and presents compiler results; it does not load or invoke macro plugins
directly.

## Two kinds of syntax structure

### Raven invocation carriers

The Raven syntax tree contains compiler-known carrier nodes at valid grammar
positions. The current expression carrier is
`FreestandingMacroExpressionSyntax`:

```raven
val result = #query {
    from user in users
    select user.Name
}
```

Future carriers may represent statement and member/declaration positions. A
carrier preserves the macro name, delimiters, and lossless raw body. It means
"expand here"; it is not itself the expansion result.

The compiler validates that expansion returns ordinary Raven syntax matching
the carrier category. An expression carrier must produce an `ExpressionSyntax`,
a statement carrier must produce statement syntax, and so on.

### Structured DSL wrappers

A macro may parse its body into a secondary tree of special wrapper nodes for
its own DSL. These wrappers can model DSL expressions, statements,
declarations, clauses, or other concepts, but they do not derive from Raven
`ExpressionSyntax`, `StatementSyntax`, or other ordinary grammar nodes.

Wrapper nodes should follow familiar Raven conventions:

* provider-owned raw kinds
* child nodes and `SyntaxToken` values
* body-relative spans
* diagnostics
* visitors and rewriters where useful

The secondary tree is derived from the raw body and is associated with the
macro invocation plus document snapshot. It supports expansion and tooling.
The macro ultimately lowers it to ordinary Raven syntax before binding.

## Token streams

Macro parsers consume a common token-stream contract whose output primitive is
`SyntaxToken`.

### Standard Raven stream

Raven provides a default stream backed by the normal Raven lexer. This is the
preferred starting point for DSLs that mostly use Raven-compatible lexical
forms.

A macro can install a body-scoped keyword overlay. The standard stream first
lexes a word as an ordinary Raven identifier, then reclassifies it with a
provider-owned `RawKind` and a keyword or reserved-word presentation:

```text
Raven Kind: IdentifierToken
Macro RawKind: QueryKeyword
Text: "query"
```

This does not add a value to `SyntaxKind`, change `SyntaxFacts`, or affect
lexing outside the macro body.

### Custom streams

A DSL with a genuinely different lexical grammar may replace the standard
stream with a custom lexer-backed implementation. Custom tokens use
provider-owned integer raw kinds. Fully custom tokens may use `SyntaxKind.None`
as their Raven kind while retaining text, value, span, and `RawKind`.

The stream/provider owns the mapping from raw kinds to names and editor
classifications. Equal integers from different providers do not imply equal
token kinds.

## Hybrid parsing and embedded Raven

A macro may choose:

* complete custom parsing
* complete Raven fragment parsing
* custom outer parsing with selected spans delegated to Raven

There are two supported lowering paths:

1. **Direct lowering:** consume the standard or custom token stream and
   immediately construct the ordinary Raven syntax returned by expansion.
2. **Structured lowering:** build and retain a secondary DSL wrapper tree for
   diagnostics and editor tooling, then translate that structure into the
   ordinary Raven syntax returned by expansion.

The structured layer is optional. Simple macros should not have to manufacture
a secondary tree, while tooling-rich DSLs should not have to reparse raw text
independently for every editor request. Both paths share the authored raw body,
body-relative spans, token conventions, diagnostics, caching boundary, and
typed Raven output contract.

For example, a query DSL can parse its clauses itself while treating a filter
body as a Raven expression:

```raven
val result = #query {
    from user in users
    where {{ user.IsActive && user.Age >= 21 }}
    select {{ user.Name }}
}
```

All lexer tokens, wrapper nodes, diagnostics, completion edits, and delegated
Raven fragments use one body-relative coordinate system. Compiler APIs map
those spans back to the authored syntax tree.

Raven fragment helpers should eventually return both syntax and mapped parser
diagnostics for expressions, statements, patterns, types, members,
declarations, and compilation units.

## Macro capability hooks

The macro definition is the registration point for optional capabilities:

* expansion/lowering
* standard keyword overlay or custom token-stream provider
* structured DSL parser
* completion provider
* token classification/highlighting provider
* hover and signature-help providers
* definition/navigation provider where the DSL has referable entities

The compiler registry discovers these capabilities together. Compiler services
own caching, cancellation, invalidation, diagnostics, and deterministic result
ordering.

Editor capabilities should share the same cached token and structure snapshot
as expansion. A completion provider should not silently tokenize or parse the
body under different rules from the macro expander.

## Diagnostics

Macro diagnostics should:

* use authored body spans whenever possible
* identify whether an error came from envelope parsing, DSL lexing/parsing,
  embedded Raven parsing, semantic validation, or expansion
* retain the macro invocation as a related location when useful
* remain stable across non-intersecting edits when spans can be translated
* never surface plugin exception stacks as ordinary user diagnostics

An unavailable or failed macro plugin should leave the carrier and raw body
intact so syntax highlighting, navigation around the containing Raven file,
and later recovery remain possible.

## Completion and highlighting

Macro completion has two levels:

1. Raven completes macro names at `#name` and inserts the appropriate carrier
   shape, such as `query { }`.
2. Inside the body, the compiler routes completion to the resolved macro
   capability using the current body-relative position and cached DSL
   structure.

Highlighting uses classifications rather than global `SyntaxKind` additions.
Macro token metadata can mark raw kinds as keyword, reserved word, identifier,
literal, operator, punctuation, comment, or a future custom semantic
classification.

When a macro has no editor provider, the body should still retain neutral,
readable delimiter/text presentation without being misclassified as broken
Raven code.

## Expansion visibility and navigation

Developers should be able to:

* preview the expanded Raven syntax
* compare authored and expanded source
* navigate from the invocation to the macro definition
* see whether a diagnostic belongs to authored DSL input or generated Raven
  output
* inspect macro expansion from CLI/debug captures without an editor

Generated syntax should carry source mappings back to body spans or the
invocation. Navigation and diagnostics must not require consumers to understand
macro caches or generated-tree identities.

## Responsiveness and isolation

Macro tokenization, structure parsing, editor services, and expansion must be
cancellable and versioned with the compilation/document snapshot. Foreground
completion and hover should not wait behind stale project-wide macro work.

The initial in-process plugin model treats macro assemblies as trusted build
extensions. `AssemblyLoadContext` provides dependency isolation, not a security
sandbox. Time/resource limits and an out-of-process execution protocol remain
future hardening work.
