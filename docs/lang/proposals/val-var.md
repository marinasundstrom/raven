# Proposal: `val`/`var` Presentation for Bindings and Signatures

## Summary

Raven currently uses `let` for immutable bindings. These bindings conceptually introduce a **value**, not a mutable variable. We want to reflect that concept in how symbols are shown and, optionally, in the surface syntax.

* **Source syntax today**:

  ```raven
  let x: int = 2
  ```
* **Symbol / ToDisplayString** (proposed):

  ```raven
  public val x: int
  ```

Key points:

* `let` is the *binding construct*, not the “kind of storage” in the symbol model.
* In **signatures** (fields, parameters, properties, locals shown via `ToDisplayString`), we normalize to:

  * `val` for immutable storage.
  * `var` for mutable storage.
* For **consistency**, we may allow `val` as an alternative to `let` for local bindings.
* There is an open question whether `let` is still needed in the long term, or if it becomes redundant with `val`.

This mirrors the conceptual split used in F#: a binding construct (`let`) vs. a “value vs variable” notion (`val`/`var`) in signatures and documentation.

---

## Motivation

### 1. Conceptual clarity

Raven’s `let` bindings are immutable:

```raven
let x = 2
```

Semantically, `x` is a **value**:

* You cannot reassign `x`.
* The symbol is effectively a read-only slot.

But when showing symbols in signatures or via `ToDisplayString`, we want a consistent vocabulary that:

* Clearly distinguishes read-only vs mutable:

  ```raven
  val x: int  // read-only
  var y: int  // mutable
  ```
* Does not leak the syntactic choice of how the binding was written (`let` vs `val` in source).

### 2. Uniform symbol display

Fields, parameters, locals, and properties all have a mutability flag in the symbol model. Presenting them as `val` or `var`:

* Gives a single, uniform display form across all declaration kinds.
* Matches user mental models from F#, Kotlin, Swift, etc.
* Makes generated documentation / API browser output much clearer.

Given:

```raven
let x: int = 2
var y: int = 3
```

We want:

```raven
public val x: int
public var y: int
```

regardless of whether the source used `let` or (future) `val`.

### 3. F#-style separation of concerns

F# uses `let` as the binding construct, but `val` as the way to describe values in signatures. Raven can mirror this:

* `let` → how you *introduce* a binding in code.
* `val` / `var` → how the compiler/tooling *describe* the bound symbol (mutability).

---

## Design

### Surface syntax

#### Locals

Current:

```raven
let x = 1  // immutable local
var y = 2  // mutable local (if present)
```

Proposal:

* Keep `let` as the canonical immutable binding keyword **for now**.
* **Optionally** allow `val` as an **alternative** spelling for immutable locals:

  ```raven
  let x = 1  // immutable
  val z = 2  // immutable, equivalent to let
  var y = 3  // mutable
  ```

Semantics:

* `let` and `val` both produce a symbol with `IsReadOnly = true`.
* `var` produces a symbol with `IsReadOnly = false`.

#### Fields and parameters (signatures)

Source today (fields/params may not explicitly use val/var):

```raven
let x: int = 2 // field
func foo(x: int, y: int) -> ()
```

**Display** (via `ToDisplayString`):

```raven
public val x: int

func foo(val x: int, val y: int) -> ()
```

For mutable ones:

```raven
var counter: int = 0
func advance(var state: State) -> ()
```

Display:

```raven
public var counter: int

func advance(var state: State) -> ()
```

In other words:

* `let` as a binding construct never appears in symbol display.
* `val`/`var` are *display-only markers* that reflect mutability.

---

## Symbol model and formatting

### Mutability flag

Ensure all variable-like symbols expose a mutability indicator, e.g.:

```csharp
interface IVariableSymbol : ISymbol
{
    bool IsReadOnly { get; }
}
```

Then:

* `let` local → `IsReadOnly = true`
* `val` local → `IsReadOnly = true`
* `var` local → `IsReadOnly = false`
* Fields, parameters, properties follow the same pattern.

### Display rules

`ToDisplayString` (or equivalent) should:

* Emit `val` when `IsReadOnly == true`.
* Emit `var` when `IsReadOnly == false`.

Examples:

Source:

```raven
let fieldA: int = 2
var fieldB: int = 3

func foo(x: int, var y: int) -> ()
```

Display:

```raven
public val fieldA: int
public var fieldB: int

func foo(val x: int, var y: int) -> ()
```

Even if a local or field was introduced with `let`, the textual representation uses `val`.

---

## `let` vs `val` in source: what should we do?

There is a genuine design question:

> Is `let` still needed if `val` exists?

### Option A: Keep both (`let` and `val`) as synonyms for locals

* `let` remains the primary binding keyword for locals.
* `val` is allowed for consistency and stylistic preference.
* Tooling can standardize to one in fixes/formatting (e.g. always suggest `let` or always suggest `val` based on code style).

Pros:

* Backwards compatible.
* Lets Raven feel familiar to both F# (`let`) and Kotlin/Swift (`val`) users.

Cons:

* Two ways to write the same thing.
* Might feel redundant or confusing if not documented clearly.

### Option B: Gradual migration from `let` → `val`

* In the long run, prefer `val` as the primary immutable binding keyword.
* Keep `let` for:

  * Top-level bindings or module-level constructs.
  * Or as “legacy” syntax behind a language version switch.
* Tooling could:

  * Offer code actions to convert `let` to `val` in certain contexts.
  * Treat `val` as the canonical modern style.

Pros:

* Strong conceptual alignment: `val` in declarations, `val` in signatures.
* `let` can be reserved for more specialized roles if desired (e.g., pattern bindings, module-level definitions).

Cons:

* Language evolution path needs to be clearly documented.
* Might require versioning / compatibility switches.

### Option C: Treat `let` as the binding form, `val` only as display

* Do **not** introduce `val` as a local keyword at all.
* Keep `let` as the only immutable binding keyword.
* Only use `val`/`var` in symbol displays / signatures.

Pros:

* Very simple language surface; existing code unchanged.
* Still get clear `val`/`var` in API views and `ToDisplayString`.

Cons:

* Inconsistency: devs see `public val x: int` but cannot write `val x` in source.
* Might feel surprising or slightly incoherent.

---

## Recommended direction

For now:

1. **Make `val`/`var` the canonical display form** for symbol mutability in signatures and `ToDisplayString`.
2. **Allow `val` as an alternative to `let` for locals**, with identical semantics.
3. Keep `let` for backward compatibility and F#-style “binding construct” semantics.
4. Defer a final decision on deprecating or repurposing `let` to a later language-version proposal, once we see usage patterns.

This achieves:

* Clear, uniform symbol displays (`val`/`var`).
* A consistent story: “immutable bindings are values (`val`), mutable ones are variables (`var`).”
* Minimal disruption to existing code and mental models.
