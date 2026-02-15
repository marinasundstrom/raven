# Proposal: Macro System with Token Processing, Syntax Building, and Source Mapping

## Summary

This proposal introduces a macro system for Raven based on:

* **Token processing** (macros read a token stream)
* **Syntax building** (macros emit Raven syntax or tokens)
* **Structured source mapping** (diagnostics map back to original DSL source)

Macros are treated as **syntactic lowering passes** that transform macro invocations into normal Raven syntax before semantic binding.

The system is designed to:

* Preserve Raven’s Roslyn-like stability
* Maintain deterministic parsing
* Provide precise diagnostic attribution
* Support DSLs such as `linq!{}`, `xml!{}`, routing DSLs, etc.

---

# 1. Design Principles

1. Macros are **syntax transformations**, not runtime features.
2. Macro expansion occurs **after parsing** but **before binding**.
3. The original syntax tree (SourceTree) is preserved.
4. The binder operates on a fully expanded syntax tree (ExpandedTree).
5. All diagnostics must map to meaningful source locations.

---

# 2. Macro Invocation Syntax

Macros are explicitly marked and syntactically recognizable:

```raven
name! { ... }
name!( ... )
name![ ... ]
```

The parser recognizes these forms in valid syntactic slots and produces dedicated macro invocation nodes:

* `MacroInvocationExpressionSyntax`
* `MacroInvocationStatementSyntax`
* `MacroInvocationMemberSyntax`

The macro body is captured as a **TokenTree** (or flat token stream).

---

# 3. Macro Targets (Expansion Slots)

Each macro declares a target category:

```raven
macro linq: expr { ... }
macro route: member { ... }
macro sql: stmt { ... }
```

Supported targets (v1):

* `expr` — expands to `ExpressionSyntax`
* `stmt` — expands to `StatementSyntax`
* `member` — expands to `MemberDeclarationSyntax`

Placement is validated during parsing. A macro invocation in the wrong slot is a compile-time error.

---

# 4. Macro Architecture: Processor + Builder

A macro consists of two conceptual phases:

## 4.1 Processor

The processor reads the macro body token stream and builds an intermediate representation (IR).

Macros receive a token cursor:

```text
Peek()
Read()
Expect(kind)
TryRead(kind)
```

The macro context provides helpers:

```text
ParseExpression(cursor, stopCondition)
ParseStatement(cursor, stopCondition)
```

These methods:

* Consume tokens from the same cursor
* Invoke Raven’s real parser
* Preserve original token spans

## 4.2 Builder

The builder constructs the expansion:

* Preferably using `SyntaxFactory`
* Alternatively emitting tokens (advanced mode)
* Must record source mappings

---

# 5. Expansion Pipeline

Compilation pipeline:

1. Parse source → **SourceTree**
2. Identify macro invocations
3. Expand each macro:

   * Process input tokens
   * Build expansion
   * Collect diagnostics
   * Produce source mapping
4. Replace invocation node
5. Produce **ExpandedTree**
6. Binder operates on ExpandedTree

Macro expansion may recurse (bounded by `MaxExpansionDepth`).

---

# 6. Embedded Expression Parsing

When a DSL contains embedded Raven expressions:

```raven
linq! {
    where user.IsActive && user.Age >= 21
}
```

The macro should:

1. Capture the predicate token slice
2. Call `ParseExpression` on those tokens
3. Splice the resulting `ExpressionSyntax` into the expansion

Because original tokens are reused:

* Semantic diagnostics inside the predicate naturally point to DSL source.
* No additional mapping is required for those tokens.

This provides precise source attribution automatically.

---

# 7. Source Mapping Model

## 7.1 Motivation

Generated wrapper code (e.g. `.Where(...)`, lambda wrappers) does not exist in source. Diagnostics in such regions must still map to meaningful locations.

## 7.2 Mapping Structure

Each macro expansion produces a `MacroExpansionMap`:

```text
MacroExpansionMap
  InvocationSpan
  Entries: ExpandedSpan -> SourceSpan
```

Mapping categories:

* `Exact` — precise fragment from macro body
* `Clause` — maps to DSL clause keyword (e.g. `where`)
* `Invocation` — fallback to invocation span

## 7.3 Diagnostic Remapping Algorithm

When a diagnostic occurs in ExpandedTree:

1. If location already refers to original source tokens, use as-is.
2. Otherwise, find the most specific mapping entry.
3. Rewrite diagnostic location to mapped SourceSpan.
4. If no entry found, use InvocationSpan.

This guarantees no diagnostic points to unmapped generated code.

---

# 8. Macro Output Modes

## 8.1 Syntax Expansion (Preferred)

Macro returns:

```text
ExpressionSyntax | StatementSyntax | MemberDeclarationSyntax
```

Built via `SyntaxFactory`.

Benefits:

* Strong typing
* Stable structure
* Easier tooling
* Cleaner mapping

## 8.2 Token Expansion (Advanced)

Macro returns:

```text
TokenTree
```

Compiler reparses tokens using the macro’s target entrypoint.

Token expansions must still provide mapping information.

---

# 9. MacroContext API (Conceptual)

```text
interface MacroContext {
    TokenCursor Cursor
    ExpressionSyntax ParseExpression(...)
    StatementSyntax ParseStatement(...)
    IdentifierToken Gensym(string hint)
    void Report(Diagnostic)
    void Map(ExpandedSpan, SourceSpan)
}
```

---

# 10. Example: LINQ Macro

Input:

```raven
val query = linq! {
    from user in db.Users
    where user.IsActive && user.Age >= 21
    orderby user.Name
    select user.Name
}
```

Expansion:

```raven
val query =
    db.Users
        .Where((user: User) => user.IsActive && user.Age >= 21)
        .OrderBy((user: User) => user.Name)
        .Select((user: User) => user.Name)
```

Mapping behavior:

* Predicate expression → exact DSL span
* Wrapper `.Where(...)` → `where` keyword span
* Entire expansion → invocation span fallback

Semantic errors inside the predicate map precisely to the DSL source.

---

# 11. Stability Guarantees

* SourceTree remains unchanged.
* ExpandedTree is derived.
* Binder never sees macro invocation nodes.
* Diagnostics never point to unmapped generated code.
* Macro invocation syntax is always parseable without expansion.

---

# 12. Future Extensions

* Limited semantic queries inside macros
* Hygienic capture avoidance
* Plugin-based macro assemblies
* IDE “Show Expansion” view
* Incremental macro caching

---

# Conclusion

This macro design:

* Treats macros as syntactic lowering passes
* Preserves Raven’s Roslyn-like architecture
* Provides precise diagnostic mapping
* Enables powerful DSLs without destabilizing the core grammar

It balances power, safety, and tooling stability.