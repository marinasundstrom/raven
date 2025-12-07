# Proposal: Identifier-Level and Union-Based Nullability in Raven

## Summary

Raven eliminates C#-style nullable types (`T?`) from user-facing syntax.
Nullability is instead expressed through:

1. **Binding-level syntax**

   ```raven
   val x? : int
   Title? : string { get; set; }
   ```

2. **Union types**

   ```raven
   val y : int | null = null
   func getUser() -> User | null
   ```

The binding syntax `name? : T` is **sugar** for `name : T | null`.
`null` is simply a union case; `T | null` is the canonical nullable *type* form in Raven.

The compiler maps these to CLR shapes (`Nullable<T>` and nullable reference metadata), but Raven itself has:

* **no `T?` syntax**,
* **explicit union nullability**,
* **binding contracts**, and
* `Option<T>` as the idiomatic high-level “maybe” type.

This model is simpler, clearer, and more consistent than C#’s model while maintaining perfect interop.

---

# Background: Problems With C#-Style Nullability

The .NET ecosystem carries several overlapping nullability systems:

### 1. Implicitly nullable reference types (pre-NRT)

Reference types were always nullable, but never indicated in code.

### 2. Value vs reference type distinctions

Value types are structurally non-nullable; reference types are inherently nullable.
Code must mentally track which types “allow `null`.”

### 3. Nullable Reference Types (NRT)

C# added `string?` vs `string`, but:

* The system is opt-in,
* Enforced only by warnings,
* Commonly ignored by developers,
* Conceptually treats nullability as a **type modifier**, which is misleading.

### Core conceptual flaw in C#:

> “Nullability is a property of the type.”

This is false.
A **binding** (variable, field, parameter, property) is nullable — not the type itself.

Raven corrects this by:

* Removing nullable type syntax (`T?`),
* Making nullability a **binding contract** (`name? : T`),
* Or a **union shape** (`T | null`).

---

# Design

## 1. Declaration-Level Nullability (`name? : T`)

Binding-level nullable syntax:

```raven
val count? : int
var payload? : Payload
func tryFind(key : string, result? : string) -> bool
Title? : string { get; set; }
```

This is exactly equivalent to:

```raven
count  : int | null
payload: Payload | null
result : string | null
Title  : string | null
```

### Semantics

* Adds `IsNullable = true` to the symbol.
* Underlying type is unchanged (`int`, `string`, `User`).
* CLR mapping:

  * value types → `Nullable<T>`
  * ref types → nullable metadata (`[Nullable]`/`[NullableContext]`)

### Desugaring rule

```text
name? : T
⟶ name : T | null
```

---

## 2. Union-Based Nullability (`T | null`)

Raven treats `null` as just another union case:

```raven
val x : int | null
func h() -> User | null
```

This is the **canonical type form** for nullability.

### Purpose

* Allows nullable types everywhere unions are valid,
* Integrates perfectly with `Option<T>` and algebraic typing,
* Avoids clutter and special-case semantics.

### Relation to binding sugar

```raven
val a? : int        // sugar
val b  : int | null // explicit union
```

These two are identical to the compiler.

---

## 3. Removal of `T?` From User-Facing Syntax

Raven does **not** expose `T?` as a type-level nullability operator.

### Why?

* Avoids C#-style confusion (value vs reference).
* Removes redundant nullable representations:

  * `T?`
  * `T | null`
  * `Option<T>`
* Leaves a single canonical type shape: **`T | null`**.
* Strengthens the binding-first philosophy.

### Interop

Internally:

* `T | null` on value types maps to `Nullable<T>`.
* `T | null` on reference types maps to nullable context attributes.

The Raven developer never writes `T?`, but interop still works seamlessly.

---

## 4. Updated Inference Rules

```raven
val x? = GetNullable()        // OK: binding explicitly nullable
val x  = GetNonNullable()     // OK: inferred non-nullable

val x? : int = GetNullable()  // OK
val x? : int = 5              // OK: sugar for int | null
var x : int | null = 5        // OK: same as above

var x? : int | null = 5       // ERROR: nullable on already-nullable type

val x = GetNullable()         // ERROR: nullable initializer requires `?` or `| null`
val x : int = GetNullable()   // ERROR: nullable → non-nullable
```

### Rule

> **Nullability is never inferred.**
> Only the type may be inferred.
> If the initializer is nullable, the binding must explicitly say so using `?` or `T | null`.

This keeps binding contracts explicit and visible.

---

## 5. Symbol Model

Symbols holding values gain:

* `IsNullable`
* `ReturnIsNullable`

Symbol type is always the “base” shape (`T` or a union) — never `T?`.

In practice:

```raven
val n? : int   // symbol type: int | null, IsNullable = true
val u? : User  // symbol type: User | null, IsNullable = true
```

At the CLR level:

* `int | null` → `Nullable<int>`
* `User | null` → `User` + nullable metadata

---

## 6. Signature Display Rules

There are two useful perspectives:

* **Binding-centric view** (what you *wrote* / what you *see in declarations*),
* **Type-centric view** (what the *type system* reasons about).

Raven’s display rules reflect that.

### 6.1 Default display for signatures (binding-centric)

For locals, parameters, fields, and properties, the **default symbol display** uses the binding sugar form:

```raven
val name? : string
func f(user? : User) -> string?
```

Examples:

Declaration:

```raven
val name? : string
```

Signature display (default):

```raven
val name? : string
```

Declaration:

```raven
func f(a? : User, b : int | null) -> string | null
```

Signature display (default):

```raven
func f(a? : User, b : int | null) -> string | null
```

So:

* **Bindings** (things with names): shown as `name? : T` when nullable.
* **Types** inside other types / return positions: shown as `T | null`.

### 6.2 Type-centric / tool view

When tooling focuses on the *type itself* (e.g. `GetTypeInfo`, type-only displays, debugger “type” column), the canonical form is always:

```raven
T | null
```

So for the same declaration:

```raven
val name? : string
```

* Signature view: `val name? : string`
* TypeInfo view: `string | null`

This makes the ergonomics binding-first for humans, but keeps the type system and tools anchored on the union form.

---

## 7. Flow Analysis

Since nullability is represented as `T | null`, flow analysis is algebraic:

* A union containing `null` is nullable.
* A check like `if x != null` **narrows** the union to remove the `null` case.

Example:

```raven
if user? : User {
    if user != null {
        user.Name   // safe; type narrowed to User
    }
}
```

The narrowing rules apply equally to:

* `name? : T` (desugared to `T | null`)
* `T | null` written explicitly.

---

## 8. Interop With `Option<T>`

Raven’s idiomatic maybe type is:

```raven
Option<T>
```

To interoperate with nullable types (`T | null`), Raven will rely on **user-defined conversions** written in Raven itself once the language supports them:

```raven
// In Raven stdlib, eventually:
implicit func (o : Option<T>) -> T | null { ... }
```

Conceptually:

* `Some(v)` → `v`
* `None` → `null`

This is implemented *in Raven*, not hard-coded into the compiler, since `Option<T>` is just a normal Raven union type.

### No implicit reverse

`T | null → Option<T>` must be explicit to avoid ambiguity.

---

## 9. Note on `&` and `*` in Declarations

Pointer/reference modifiers remain type-level:

```raven
val p : *int
val r : &User
```

Declaration-level forms are rejected:

```raven
val p* = ...
val r& = ...
```

Nullability combines with them through the same union model:

```raven
val r? = &x        // r : &int | null
val p  : *int | null
```

---

# Comparison With Other Languages

## TypeScript

* Uses unions for nullability: `T | null` / `T | undefined`.
* Identifier `?` in object types desugars into union with `undefined`.
* Raven is similar: `name?` is sugar for `T | null`.

## F#

* Idiomatic: `Option<T>`.
* Unions increasingly used: `int | null`.
* Raven follows this closely but adds `name?` for binding clarity and uses `null` as a specific union case for .NET interop.

## Swift

* `T?` is sugar for an enum `Optional<T>`.
* No general union types; nullability is via optionals.
* Raven is more general: supports unions *and* `Option<T>` explicitly.

## Rust

* No nulls; only `Option<T>`.
* Raven must keep `null` for .NET interop but treats it as a union case: `T | null`.

## Kotlin

* Uses type-level `T?`.
* Nullability attached to the type.
* Raven rejects this, attaching nullability to the binding (`name?`) or to the union type (`T | null`) instead.

## ML-family (Haskell, OCaml, Elm)

* Use `Maybe<T>` / `option<T>`.
* No null except at FFI boundaries.
* Raven’s `Option<T>` mirrors this, but unions permit explicit `null` for interop.

### Summary Table

Language   | Nullability Model   | Union? | Option? | Nullability belongs to… |
| ---------- | ------------------- | ------ | ------- | ----------------------- |
| C#         | T?, implicit refs   | ❌      | ❌       | Type + analyzer         |
| TypeScript | T | null            | ✅      | ❌       | Type (union)            |
| F#         | Option + unions     | ✅      | ✅       | Type (union/option)     |
| Swift      | Optional<T>         | ❌      | (enum)  | Type                    |
| Rust       | Option only         | ❌      | ✅       | Binding via Option      |
| Kotlin     | T?                  | ❌      | ❌       | Type                    |
| Raven      | name? : T, T | null | ✅      | ✅       | Binding + union type    |

---

# Conclusion

With C#-style `T?` removed from Raven source, nullability becomes:

* **explicit**,
* **algebraic**,
* **binding-first**,
* and **consistent**.

Raven uses:

1. **Binding sugar** (`name? : T`) for declarations,
2. **Canonical nullable types** (`T | null`) for the type system,
3. **Idiomatic `Option<T>`** for “maybe” semantics,

while preserving flawless .NET interop.

Tooling *analyzes* nullability as `T | null`, and *displays* nullable bindings as `name? : T` by default — giving developers a model that is both mathematically clean and human-friendly.
