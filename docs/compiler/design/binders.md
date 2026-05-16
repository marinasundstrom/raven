# Binders

Binders are the compiler work units that turn syntax into semantic facts.
They answer questions such as:

- which symbol a declaration introduces
- which symbol an expression references
- which type an expression has
- which diagnostics are produced by a local semantic context

The public API should normally go through `SemanticModel`. The language server,
analyzers, hover, completion, and refactorings should ask Roslyn-like APIs such
as `GetDeclaredSymbol`, `GetSymbolInfo`, and `GetTypeInfo`. Those APIs decide
whether they can answer from existing binder state, cached bound nodes, or a
fresh bind. Tooling should not bypass the compiler API to compensate for binder
cost.

## Mental model

Think of a binder as cache-derived state for a syntax node and its parent
semantic context.

```text
syntax node + parent binder + compilation context -> binder
```

If any part of that identity becomes invalid, the binder can be discarded and a
new binder can be created. The binder itself does not decide whether it is
stale. Reuse and invalidation belong to the `SemanticModel` and compilation
layer.

Inside that lifetime, a binder may keep semantic state derived from its syntax.
That is the point of the binder: it avoids recomputing local facts every time a
semantic query asks a nearby question.

## Durable vs transient state

A binder may hold two kinds of state.

Durable binder state is semantic information that belongs to the syntax the
binder owns. It may be reused for the lifetime of that binder:

- local symbols declared by a block
- pattern locals declared by pattern-owning statements and expressions
- local function and local type symbols
- labels
- method or function-expression parameters in the responsible parent binder
- cached bound nodes whose answer is valid for the current binder context

Transient execution state exists only while walking a body:

- the active lexical lookup map at the current statement
- current scope depth
- locals pending disposal for the active scope
- flow facts such as non-null symbols
- statement traversal progress used to seed preceding declarations
- current target type and pattern-binding context

Semantic queries may need to walk part of a body to answer a question. When they
do, they should preserve durable binder state but restore transient execution
state before returning. This lets a query leave useful declaration facts behind
without making later queries observe a half-executed block.

Cached bound statements are still part of a body walk. If a cached statement
introduces declarations, the binder must replay those declaration effects into
the active transient lookup before continuing to later statements. The cached
bound node is the durable result; the active lookup entry is the per-walk state.

## BlockBinder

`BlockBinder` is the main binder for method bodies, function-expression bodies,
block expressions, and top-level statement bodies. It is intentionally heavy:
most statement and expression binding flows through it.

Its responsibilities are:

- bind statements and expressions in execution order
- create local, pattern, local-function, local-type, and label symbols
- resolve local references before delegating lookup to parent binders
- report body-level diagnostics
- keep binder-owned declaration state for the block
- snapshot and restore transient state during semantic queries

The key rule is that declaration-producing syntax should be registered by the
binder that owns that body. Examples:

- `val x = 1` creates a local in the current `BlockBinder`
- `val (key, value) = pair` creates pattern locals in the current `BlockBinder`
- `if val (key, value) = pair { ... }` creates pattern locals owned by the
  current `BlockBinder`, scoped to the `IfPatternStatement`
- `async func (...) { ... }` has parameters owned by the function-expression
  binder, while body locals are owned by that body binder

Querying `GetDeclaredSymbol` for one local should not require binding the whole
method body. The binder should bind the minimal containing construct and any
preceding declarations required for correct lookup.

## Preceding declarations

Symbol lookup inside a body often needs declarations that appear earlier in the
same statement list. `BlockBinder` handles this through an explicit
declaration-seeding path.

When a query asks about a later statement, the binder walks previous sibling
statements and binds only declaration-producing syntax:

- local declarations
- `use` declarations without an inline block
- pattern declaration assignments
- local function statements
- local type declarations

This keeps hover and diagnostics correct after incremental edits without forcing
the full method body to bind eagerly.

## Pattern declarations

Pattern declarations have two related but different owners:

- the binding owner is the syntax that must be rebound to create the pattern
  local
- the lexical scope owner is the syntax that controls where the local is visible

For `if val pattern = value`, both are the `IfPatternStatement`. For
`val pattern = value`, the binding owner is the pattern declaration assignment
statement, while the lexical scope owner is the containing block.

Keep this distinction explicit. It avoids bugs where declared-symbol queries
work, but references in following statements cannot see the locals.

## Incremental compilation

The expected incremental shape is:

1. Syntax changes create a new syntax tree while preserving identity for reused
   green/red nodes where possible.
2. `SemanticModel` decides which binders can be reused for unchanged syntax and
   unchanged parent semantic context.
3. Changed or removed syntax gets fresh binders.
4. Public semantic APIs lazily ask the responsible binder to answer the query.
5. The binder reuses its own durable state when possible and recomputes only the
   missing local facts.

This keeps the compiler deterministic and makes editor scenarios fast. A hover,
type hint, or diagnostic update should not depend on a previous whole-file bind
having happened first.

## Testing guidance

Binder tests should describe durable state and lifecycle behavior, not private
implementation trivia.

Useful assertions:

- querying a declared local stores exactly one local in the owning binder
- repeated queries return the same symbol from the same binder
- removing a cached binder produces a fresh binder and fresh local symbols
- semantic queries restore transient lookup state after returning
- preceding declaration seeding makes later references resolve without eager
  body binding
- pattern locals have the expected lexical scope owner

Avoid tests that depend on a complete bound-tree shape unless the shape itself
is the product contract. Prefer symbol identity, type identity, diagnostics, and
observable semantic API behavior.
