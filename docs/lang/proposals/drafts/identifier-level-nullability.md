# Proposal: Identifier-Level Nullability in Raven

## Summary

Raven moves primary nullability from *type syntax* to the *declaration* itself.
Nullability is expressed by placing `?` **after the declared identifier**, not after the type:

```raven
val x? : int
func f(user? : User) -> int
Title? : string { get; set; }
```

This marks the *binding* as nullable. The underlying type (`int`, `User`, …) remains non-nullable in syntax.
The compiler sets symbol-level nullability (`IsNullable`, `ReturnIsNullable`) and maps this to CLR nullability (`Nullable<T>` or reference-type attributes).

Type-level `T?` remains valid Raven syntax — especially for **generic arguments**, **type parameters**, and **return types** — but is not used for ordinary variable, parameter, field, or property declarations.

This model aligns with .NET metadata, simplifies flow analysis, and encourages idiomatic Raven code that uses `Option<T>` for high-level “maybe” semantics.

---

# Background: Problems With C#-Style Nullability

The C# ecosystem accumulated several overlapping nullability systems:

### 1. Reference types (pre-NRT)

All reference types (`string`, `User`, etc.) were implicitly nullable.
The code didn’t show this; it was simply assumed.
Understanding whether `x` can be null requires knowing whether `T` is a reference type.

### 2. Distinction between value types and reference types

Value types are structurally non-nullable and gain nullability through `Nullable<T>` (`int?`).
Reference types are inherently nullable.
Thus the meaning of `T?` depends on whether `T` is a value type or reference type — a source of constant cognitive overhead.

### 3. Nullable Reference Types (NRT)

Microsoft attempted to retrofit “non-nullable by default” onto a runtime where everything was nullable:

* Requires opt-in project settings.
* Adds a major analyzer layer.
* Produces warnings rather than enforced semantics.
* Frequently ignored or suppressed in real-world codebases.

Developers often experience C# nullability as:

> “Sometimes useful, often annoying, frequently ignored.”

And conceptually, the model reinforces a fiction:

> “The *type* is nullable.”

But in reality, the **binding** — the slot that holds the value — is what may or may not contain `null`.
A type like `User` is not inherently nullable or non-nullable; only the declared symbol is.

Raven corrects this misunderstanding by making nullability explicit in the declaration:

```raven
val user? : User
Title? : string { get; set; }
```

Nullability becomes a **contract of the binding**, not a modification of the type.

---

# Design

## 1. Declaration-Level Nullability (`name? : T`)

Nullability is placed directly on the identifier:

```raven
val count? : int
var payload? : Payload
func tryFind(key : string, result? : string) -> bool
Title? : string { get; set; }
```

Meaning:

* The **symbol** is nullable (`IsNullable = true`).
* The **type syntax** remains non-nullable (`int`, `string`, `User`).
* For value types, the effective CLR type becomes `Nullable<T>`.
* For reference types, nullability is encoded using attributes.

### Inference rules

```raven
val x? = GetNullable()        // OK: binding explicitly nullable

val x? : int = GetNullable()  // OK
val x? : int = 5              // OK

val x = GetNonNullable()      // OK: inferred non-nullable
val x : int = GetNonNullable()// OK: explicit non-nullable

val x = GetNullable()         // ERROR: nullable initializer requires explicit `?`
val x : int = GetNullable()   // ERROR: nullable → non-nullable
```

`?` on the declaration **forces** the binding to be nullable regardless of RHS.

If no explicit type is given, only the **type** is inferred; **nullability must always be declared explicitly** on the binding (via `?` or a nullable/union type).
---

## 2. Type-Level Nullability (`T?`) — Still Supported

Raven retains full support for `T?`, `int?`, and `string?` as **type syntax**.

### Allowed and encouraged in:

* **generic type arguments**

  ```raven
  val cache : List<int?>
  val map   : Map<string, List<int?>>
  ```

* **nested type positions**

* **return types**

  ```raven
  func getOrNull() -> int?
  func findUser(id : Guid) -> User?
  func tryLookup() -> Dictionary<string, int?>?
  ```

These are essential for interop and structured nullable shapes.

### Discouraged in simple declarations

```raven
val x? : int       // preferred
val x  : int?      // discouraged (interop-only style)
```

### Illegal combinations

```raven
val x? : int?   // ❌ nullability specified twice
func f(x? : T?) // ❌
```

---

## 3. Union-Based Nullability (`T | null`)

Raven also allows nullability to be expressed as a **true union type**:

```raven
var x : int | null = null
```

Here, the union explicitly includes the `null` case.

This approach is aligned with Raven’s general union philosophy:

```raven
type Result<T> = T | Error
type Maybe<T>  = T | None
```

### Relationship with `x? : T` and `T?`

All three are valid and interoperable:

```raven
var a? : int         // declaration-level nullable
var b  : int?        // type-level nullable (interop)
var c  : int | null  // union with explicit null case
```

* `x? : T` is **binding-level** semantics.
* `T?` is **CLR-shape semantics**.
* `T | null` is **union semantics**.

Flow analysis treats all three as nullable, and `if x != null` refines them appropriately.

### When to use what

* Prefer **`Option<T>`** and unions (`T | None`) for idiomatic “maybe” logic.
* Use **`name? : T`** for explicit binding contracts and properties/parameters.
* Use **`T?`** when you need direct `Nullable<T>` shapes for .NET interop.
* Use **`T | null`** when working inside union-heavy type constructs.

---

## 4. Symbol Model

Symbols that hold values gain:

* `IsNullable`
* `ReturnIsNullable`

The symbol’s **type** remains the base type (`T`).
The **effective CLR type** is computed based on declared nullability.

Examples:

```
val n? : int        → Type=int,  IsNullable=true, CLR=Nullable<int>
val u? : User       → Type=User, IsNullable=true, CLR=User with attributes
func f() -> int?    → ReturnType=int?, ReturnIsNullable=true
```

---

## 5. Flow Analysis

Flow nullability is determined from:

* declared nullability,
* inferred nullability,
* control-flow (conditions, guards, matches).

Example:

```raven
if user? : User {
    if user != null {
        user.Name   // proven safe here
    }
}
```

Raven enforces safety on non-nullable bindings.

---

## 6. Interop With `Option<T>`

Raven’s semantic “maybe” type is **Option<T>**.
Interop with .NET APIs expecting `T?` is enabled through a built-in implicit conversion:

```raven
Option<T> → T?
```

* `Some(v)` → `v`
* `None` → `null` / `default(Nullable<T>)`

Applies in assignments, arguments, return statements, generics.

```raven
func getOrNull() -> int? {
    let opt : Option<int> = compute()
    return opt
}
```

### No implicit `T? → Option<T>`

This conversion is explicit to avoid conflating `null` with meaningful union cases.

---

## 7. Note on `&` and `*` in Declarations

A variant was considered where ref/pointer modifiers would appear on identifiers:

```raven
// Not adopted:
val p* = GetPointer()
val r& = GetRef()
```

Rejected because:

* It interacts poorly with type inference,
* It obscures ref/pointer shape (IL-level detail),
* Nullability is a contract; `&`/`*` are representation details.

Final decision:

* **`?` applies to declarations**,
* **`&` and `*` remain type-level (`&T`, `*T`)**.

---

# Conclusion

Raven’s identifier-level nullability model:

* aligns with actual runtime semantics,
* makes nullability a clear contract of the binding,
* avoids the pitfalls of C#’s type-based approach,
* works cleanly with inference and flow analysis,
* and embraces `Option<T>` and union types for idiomatic “maybe” logic.

Meanwhile, type-level `T?` remains available for interop scenarios, and union types (`T | null`) provide a powerful expressive alternative when needed.

This hybrid model gives Raven the **clarity of functional languages**, the **interoperability of C#**, and a **principled, modern approach** to nullability.
