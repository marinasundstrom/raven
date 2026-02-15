# Proposal Extension: Optional Macro Semantic Pass

## 1. Motivation

While macros primarily operate on token streams and syntax, some DSLs benefit from semantic awareness:

* Validate identifiers in scope
* Check type compatibility of embedded expressions
* Validate referenced members exist
* Infer expected types at the macro invocation site

Example:

```raven
linq! {
    where user.Age >= "21"   // semantic type mismatch
}
```

We want:

* Predicate errors to map to DSL source (already handled)
* Macro-level validation (e.g. “missing select clause”) to be supported
* Optional deeper semantic validation before expansion

---

# 2. Macro Processing Model (Extended)

Macros now have **three conceptual stages**:

1. **Token Processing**

   * Read and interpret DSL tokens
   * Produce Macro IR
   * Emit syntactic diagnostics

2. **Optional Semantic Validation**

   * Validate IR against surrounding scope
   * Emit semantic diagnostics
   * May query limited semantic information

3. **Expansion Building**

   * Emit syntax (preferred) or tokens
   * Provide source mapping

---

# 3. Macro Semantic Context

Macros may receive a restricted semantic context:

```text
interface MacroSemanticContext {
    SemanticModel SurroundingModel
    TypeSymbol? ExpectedType
    Symbol? ResolveIdentifier(string name)
    TypeSymbol? ResolveType(string name)
}
```

Important constraints:

* Semantic queries must NOT depend on the macro expansion result.
* Queries must only depend on code outside the macro invocation.
* The macro expansion itself is not yet bound.

This prevents semantic cycles.

---

# 4. Semantic Pass Timing

Compilation pipeline becomes:

1. Parse → SourceTree
2. For each macro invocation:

   * Process tokens → IR
   * Run macro semantic validation (optional)
   * If diagnostics exist and expansion not possible, abort expansion
   * Build expansion
3. Replace invocation
4. Produce ExpandedTree
5. Normal binding

Macro semantic diagnostics are reported before normal binding.

---

# 5. Example: LINQ Semantic Pass

DSL:

```raven
linq! {
    from user in db.Users
    where user.IsActive
    select user.Name
}
```

Semantic pass may:

* Resolve `db` in scope
* Ensure `Users` exists
* Determine element type of `Users`
* Bind `user` symbol within macro IR
* Validate that `user.Name` exists

This allows:

* Better DSL diagnostics
* Earlier feedback
* Cleaner error messages than relying purely on expansion

---

# 6. Diagnostic Mapping in Semantic Pass

Semantic diagnostics produced during macro validation must:

* Point to exact DSL spans
* Use IR node spans recorded during token processing

Example:

```
Error: 'Age' does not exist on type 'User'
    at: user.Age >= 21   // DSL source span
```

Since the IR retains spans from token processing, mapping is direct.

---

# 7. Why This Is Architecturally Clean

Because:

* You are not modifying Raven’s core binder.
* Macro semantic validation is isolated.
* Expansion still produces ordinary Raven syntax.
* Binder still operates on ExpandedTree.
* No circular dependencies are introduced.

This preserves Roslyn-style determinism.

---

# 8. Strict Mode vs Lightweight Mode

Macros may declare:

```raven
macro linq: expr (semantic = true) { ... }
```

If semantic pass is enabled:

* Macro must succeed semantic validation before expansion
* Otherwise compilation fails

If disabled:

* Macro remains syntax-only
* All deeper validation occurs during normal binding

---

# 9. Why This Is More Powerful Than Rust

Rust macros:

* Generally operate purely syntactically
* Have limited semantic awareness at expansion time

Raven macros (with this model):

* Can safely query semantic context
* Can validate DSL structure semantically
* Still preserve precise mapping
* Still maintain deterministic expansion

This gives Raven:

* Stability (Roslyn-like)
* Power (DSL-friendly)
* Toolability (IDE-safe)

---

# 10. Important Safety Rule

Macro semantic pass must NOT:

* Bind the macro expansion
* Modify semantic state
* Depend on macro expansion output

It may only:

* Inspect surrounding context
* Validate IR

This prevents semantic cycles.

---

# Final Architectural Shape

Macros in Raven become:

* Token processor
* Optional semantic validator
* Syntax builder
* Source-mapped expander

All before binding.

That’s extremely powerful — but still disciplined.