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

The first executable MVP deliberately uses direct lowering:

```raven
let shouldRetry = #guard {
    unless answer == 42
}
```

The macro reads the body-scoped `unless` keyword from Raven's standard token
stream, delegates `answer == 42` back to Raven's expression parser, and returns
an ordinary logical-negation expression. This validates the common path before
introducing retained DSL structure.

The next direct-lowering step uses multiple clause boundaries:

```raven
let verdict = #choose {
    test answer == 42
    then "correct"
    otherwise "wrong"
}
```

Here the clause words are reserved only inside this macro body. Their token
positions define three Raven fragment spans, which are parsed independently
and lowered to one ordinary `if` expression. A LINQ-like DSL follows the same
basic approach with more clause kinds and repeated clauses.

The first LINQ-like MVP applies that model directly:

```raven
let result = #query {
    from item in source
    where item.IsActive
    select item.Name
}
```

The source, predicate, and projection are Raven fragments. The authored `item`
name becomes the parameter of ordinary generated `Where` and `Select` lambdas,
so it scopes over the predicate and projection without leaking outside the
expansion. No hidden macro-generated name is needed in this subset.

Fragment parsing has two API shapes. `ParseExpression(span)` is the concise
syntax-only convenience for macros that deliberately handle invalid input
themselves. `ParseExpressionResult(span)` returns
`MacroSyntaxParseResult<ExpressionSyntax>`, containing the recovered syntax,
native Raven parser diagnostics, and `HasErrors`. Diagnostic locations are
mapped to the invocation's original syntax tree, so a macro can forward them
through `FreestandingMacroExpansionResult.Diagnostics` without translating
generated-tree positions or wrapping them as macro failures.

The parse-result type is generic so later statement, type, pattern, member, and
quote fragment entry points can share the same developer experience. Adding
those category-specific parsers remains incremental work; the generic result
does not make Raven's ordinary syntax hierarchy extensible.

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

Expression fragment helpers return both syntax and mapped parser diagnostics.
The same result shape should extend incrementally to statements, patterns,
types, members, declarations, and compilation units.

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

Retained structure is also the routing map for mixed-language editor services.
A structure snapshot should be able to mark a body-relative region or recovery
slot as expecting a Raven expression, statement, type, pattern, member, or
other supported fragment category. At a completion position, the compiler can
then choose between:

* macro-owned completion for DSL tokens and structure
* ordinary Raven completion for a marked Raven fragment
* a macro-provided semantic bridge when the DSL introduces names visible
  inside that fragment, such as a query range variable

Recovery slots matter as much as successfully parsed child nodes because
completion is commonly requested at an empty or incomplete location. The
compiler-owned snapshot must therefore retain expected categories and spans
even when no valid Raven fragment exists yet.

The exact scope-bridge API remains future work. It must preserve caller scope,
macro hygiene, and compiler-owned semantic caching rather than requiring the
language server to reconstruct an expansion or call the plugin directly.

## Documenting macros

Built-in and plugin macros should use the same task-oriented documentation
shape. Invocation should feel uniform; origin and installation are important
availability facts, not the opening definition of the feature.

A macro reference page should cover, in this order:

1. **Purpose:** the problem it solves and when to choose it.
2. **Quick example:** the smallest realistic invocation and observable result.
3. **Availability:** compiler built-in, SDK/package plugin, or project-local
   plugin; required project references and trust/execution implications.
4. **Input contract:** delimiters, DSL grammar, embedded Raven fragment slots,
   introduced names, and recovery behavior.
5. **Output contract:** expression, statement, declaration, member, or other
   expansion category and any runtime syntax value type.
6. **Composition rules:** scope, hygiene, trivia, splices, repetition, and
   evaluation behavior.
7. **Tooling:** diagnostics, highlighting, completion, hover, navigation, and
   expansion preview currently supported.
8. **Limitations:** unsupported categories, performance constraints, and
   version/runtime dependencies.
9. **Expansion model:** a secondary explanation for debugging and advanced
   users, not the primary statement of purpose.

Documentation for a DSL macro should explicitly mark which regions use custom
grammar and which delegate to ordinary Raven. The same boundary description
should drive highlighting and completion expectations, so the manual does not
promise editor behavior the macro has not registered.

For example, `#quote` should be introduced as a syntax literal: write Raven
code in its readable form and receive the corresponding syntax tree. Its
motivating cases are macro templates, generators, refactorings, and
syntax-oriented tests. The `SyntaxFactory` expansion is useful supporting
detail, but it is not the user-facing purpose.

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

Compiler analyzers must eventually account for references introduced through
expanded syntax. Until that integration exists, a value referenced only inside
a raw macro body can still receive an unused-value diagnostic even though the
expansion binds and executes it.

## Responsiveness and isolation

Macro tokenization, structure parsing, editor services, and expansion must be
cancellable and versioned with the compilation/document snapshot. Foreground
completion and hover should not wait behind stale project-wide macro work.

The initial in-process plugin model treats macro assemblies as trusted build
extensions. `AssemblyLoadContext` provides dependency isolation, not a security
sandbox. Time/resource limits and an out-of-process execution protocol remain
future hardening work.
