# Raven Macro System

## 1. Goals

Raven macros provide:

* Structured syntactic transformation
* Explicit and contained DSL boundaries
* Precise source diagnostics
* Minimal compiler intrusion
* Optional semantic validation
* Full tooling compatibility

Macros are **compiler-integrated syntax transformers**, not textual preprocessors.

---

# 2. Core Principles

1. **SourceTree is the source of truth**
   All diagnostics are reported in terms of original source spans.

2. **Macros are explicit**
   DSL content only exists within explicit macro invocation syntax.

3. **Parsing is deterministic**
   The file parses fully without macro expansion.

4. **Expansion happens at binding time**
   Macros are expanded lazily when encountered during binding.

5. **Expansion is pure and cached**
   Macro expansion must be deterministic and side-effect free.

6. **Expansion lowers to ordinary Raven syntax**
   Binder and codegen operate only on standard Raven constructs.

---

# 3. Macro Syntax

## Invocation Macros (Rust-style)

```raven
linq! {
    from user in db.Users
    where user.IsActive && user.Age >= 21
    select user.Name
}
```

Characteristics:

* Explicit `!` invocation marker
* Body captured as `TokenTree`
* Must appear in a valid syntactic slot
* No attribute-based macros
* No scope macros

---

# 4. Macro Targets

Each macro declares a target category:

* `expr`
* `stmt`
* `member`

Expansion must match the declared category.

The parser enforces placement correctness.

---

# 5. Macro Processing Model

Macros consist of three conceptual stages:

1. **Token Processing**
2. **Optional Semantic Validation**
3. **Expansion Building**

---

# 6. Token Processing

Macros receive a `MacroContext` with:

### Token Cursor

* `Peek(offset)`
* `Read()`
* `TryRead(kind or text)`
* `Expect(kind or text)`

### Token Remapping (Optional)

Macros may use a local remapping view to interpret identifiers (e.g. `"where"`) as DSL keywords.

Remapping:

* Is contextual
* Does not mutate underlying tokens
* Does not affect the core language

---

# 7. Embedded Raven Parsing

`MacroContext` provides parser entrypoints:

* `ParseExpression(stopCondition)`
* `ParseStatement(stopCondition)`
* (optional) `ParseType`, `ParseMember`

These:

* Consume tokens from the macro cursor
* Use Raven’s real parser
* Preserve original token spans

This ensures precise source mapping for embedded Raven fragments.

---

# 8. Optional Semantic Pass

Macros may perform semantic validation before expansion.

Semantic context may provide:

* `GetOperation(node)`
* `GetTypeInfo(expression)`
* `GetSymbolInfo(node)`
* `LookupSymbols(position)`
* `ExpectedType` at invocation site

### Operations API

The semantic pass uses Raven’s Operations API as the primary semantic entrypoint.

### Constraint

The semantic pass must not require binding the macro’s generated expansion.

Allowed:

* Querying semantics of SourceTree nodes
* Querying semantics of embedded fragments parsed from macro input

Not allowed:

* Binding expansion output during semantic validation

This prevents semantic cycles.

---

# 9. Binding Strategy (Substitution Model)

There is no global expansion phase.

Instead:

When binder encounters a macro invocation node:

1. Retrieve or compute expansion
2. Bind expansion root using the same binder context
3. Remap diagnostics
4. Return bound result

This is a substitution model.

SourceTree remains authoritative.

No merged tree is required.

---

# 10. Macro Expansion Result

Each expansion produces:

```text
MacroExpansionResult {
    ExpansionRootSyntax
    MappingTable
    Diagnostics
}
```

Expansion must be:

* Deterministic
* Pure
* Cached at Compilation level

Cache key includes:

* Invocation span
* Macro name
* Body token hash
* Compilation version

---

# 11. Source Mapping

## Mapping Structure

Each expansion maintains:

```
ExpandedSpan → SourceSpan
```

Categories:

* `Exact` (derived from original tokens)
* `Clause` (maps to DSL clause keyword span)
* `Invocation` (fallback)

## Diagnostic Remapping

When binding emits a diagnostic:

1. If location already points to original source span → keep.
2. Otherwise lookup mapping.
3. Rewrite to mapped source span.
4. If no entry → map to invocation span.

All user-facing diagnostics reference SourceTree coordinates.

---

# 12. Expanded View (Optional)

The compiler may expose:

* Full ExpandedTree
* Pretty-printed expansion
* Mapping information
* Diff view (source vs expansion)

ExpandedTree is not required for binding.

SourceTree remains canonical.

---

# 13. Stability Model

Stable:

* Syntax tree model
* Token model
* Macro host API
* Mapping infrastructure
* Operations API surface
* Diagnostic model

Internal / free to evolve:

* Binder internals
* Lowering passes
* Code generation
* Optimization pipeline

---

# 14. Architectural Positioning

Raven’s macro system combines:

* Rust-style explicit invocation syntax
* Roslyn-style structured syntax trees
* Nemerle-style compile-time transformation
* Compiler-as-a-service discipline

While preserving:

* Deterministic grammar
* Clear DSL boundaries
* Source-accurate diagnostics
* Controlled expansion

---

# 15. Summary

Raven macros are:

* Explicit
* Localized
* Deterministic
* Cached
* Source-mapped
* Tooling-friendly

Expansion occurs lazily during binding via substitution.

SourceTree remains the single source of truth for diagnostics and user-facing locations.