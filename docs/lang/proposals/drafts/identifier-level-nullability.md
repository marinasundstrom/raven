Nice, this slots really cleanly into your existing grammar. Here’s the **updated proposal**, with a new section that ties the syntax explicitly to the EBNF you pasted.

I’ve only changed wording where it needed to line up with the grammar (e.g. `let/var/const`, `VariableDeclarator`, `LocalVariableDeclarator`, `Parameter`, etc.) and added a **“Grammar Integration”** section at the end.

---

# Proposal: Identifier-Level Nullability

## Summary

Raven moves primary nullability from the **type syntax** to the **declared symbol**, by placing the nullability marker `?` after the identifier rather than after the type. Example:

```raven
let x? : int
```

declares `x` as **nullable**, while the annotated type `int` remains *logically non-nullable*. The compiler attaches nullability information to the declared symbol (`IsNullable`) and adjusts its **effective CLR type** accordingly (e.g. wrapping value types in `System.Nullable<T>`).

Type-level nullability (`int?`) remains legal, **but only inside complex type constructs** (e.g. generic arguments). Using both identifier-level and type-level nullability at the same time is disallowed:

```raven
let x? : int?  // error: nullability must appear only once
```

This model matches .NET metadata, where `NullableAttribute` attaches to **symbols**, while preserving a clean, Raven-friendly syntax.

---

# Motivation

C# projects nullability onto **types** (`string?`), but .NET metadata encodes nullability on **declared symbols** (locals, parameters, fields, properties, return values) via attributes. This mismatch leads to well-known confusion:

* Is `string?` a *different type* than `string`?
* Does nullability belong to the type, or to the variable?
* How do `T?` and `[Nullable]` interplay in generics?

Raven avoids these problems by making nullability:

* **syntactically** tied to the declared symbol (`x?`)
* **semantically** tied to the declared symbol (`IsNullable`)
* **internally** projected onto existing .NET constructs (`Nullable<T>`, `NullableAttribute`)

Benefits:

* A clean mental model: *the binding* (variable/field/param/property) is nullable.
* Flow analysis and exhaustiveness checks become simpler and more intuitive.
* Raven code favors `Option<T>` while still interoperating smoothly with CLR nulls.
* No “double-nullability” scenarios.
* Type syntax remains clean and concise.

---

# Design

## 1. Surface syntax

### Identifier-level nullability (preferred)

Nullability is expressed on the declared identifier:

```raven
// locals
let x? : int
var currentUser? : User

// fields
let value? : Payload

// parameters
func f(x? : User, y : string) -> int

// properties
property Title? : string
```

In statements with type inference, the same form applies:

```raven
let x? = ComputeValue()
var name? = readLine()
```

Here:

* the type flows from the right-hand side (`ComputeValue()`, `readLine()`),
* the `?` on the identifier marks the binding as nullable.

### Type-level nullability (`T?`) kept only for complex type positions

Allowed **only** inside:

* generic type arguments
* nested type syntax
* low-level interoperability cases

Examples:

```raven
let cache : List<int?>
let state : Map<string, List<int?>>
let ptrs : List<*int>         // pointer types are also type-level
```

### Illegal combinations

Declaring nullability in both places is an error:

```raven
let x? : int?           // ❌ double nullability
func f(x? : T?) -> T    // ❌
```

Identifier-level nullability is the canonical Raven style for simple declarations.
Type-level `T?` is not used for plain locals/parameters/fields/properties.

---

## 2. Symbol Model

All declared symbols gain an explicit nullability flag:

* `ILocalSymbol.IsNullable`
* `IParameterSymbol.IsNullable`
* `IFieldSymbol.IsNullable`
* `IPropertySymbol.IsNullable`
* `IMethodSymbol.ReturnIsNullable` (return nullability)

**Symbol semantics:**

* `IsNullable` expresses the declared nullability of the **binding**.
* The underlying type symbol (`ITypeSymbol`) stays non-nullable syntactically.
* The **effective CLR type** of the symbol is computed by the compiler:

  * If underlying type is a non-nullable value type → lifted to `Nullable<T>`.
  * If underlying type is a reference type → remains as is, but metadata attributes reflect nullability (`NullableAttribute` / `NullableContextAttribute`).

---

## 3. Binding Rules

When binding:

```raven
let x? : int
```

the binder:

1. Binds `int` to a non-nullable `ITypeSymbol`.
2. Observes the nullable identifier `x?`.
3. Marks the symbol as `IsNullable = true`.
4. Produces an **effective type**:

   * value type → `NullableTypeSymbol(int)` (i.e. `System.Nullable<int>`),
   * reference type → `User` + `IsNullable = true` on the symbol.
5. Emits diagnostics if both identifier and type attempt to encode nullability:

   ```raven
   let x? : int?    // error: nullability specified on both identifier and type
   ```

Identifier-level nullability applies uniformly across:

* locals
* parameters
* fields
* properties
* pattern variables
* deconstruction variables

---

## 4. Type-Level Nullability Restrictions

`NullableTypeSyntax` (`T?` in the `Type` grammar) is **restricted** by contextual rules (outside the core EBNF):

### Allowed:

* **generic type arguments**:

  ```raven
  let values : List<int?>
  ```

* **nested types**:

  ```raven
  let map : Map<string, List<int?>>
  ```

* **interop constructs** (e.g. APIs that explicitly expose `Nullable<T>` shapes).

### Disallowed:

* **top-level declarations** where identifier-level nullability is available:

  ```raven
  let x : int?        // ❌ use: let x? : int
  func f() -> int?    // ❌ use: Option<int>
  ```

Motivation: Raven prefers `Option<T>` and DU-based modeling for “maybe” semantics in user code. `T?` is treated as a low-level interop/tooling feature and appears mainly inside composite types.

---

## 5. Metadata Representation

### For value types

Identifier-level `x? : int` maps to:

* Effective CLR type: `System.Nullable<int>`
* Debugger and tooling are free to display it as `let x? : int` in Raven source terms.
* The compiler **prevents** constructing nested nullable-of-nullable (`Nullable<Nullable<T>>`) and double-nullability (`x? : int?`).

### For reference types

Identifier-level `x? : User` becomes:

* Type: `User`
* Metadata: `NullableAttribute` (and/or `NullableContextAttribute`) on the symbol representing that it may be null.

### For generics

Raven produces:

* `Dictionary<string, int?>` → `Dictionary<string, System.Nullable<int>>`
* `List<*int>` → `List<int*>`
* `let current? : Dictionary<string, int>` → nullable binding whose type is `Dictionary<string, int>`.

The source-level Raven notation (`let x? : T`) is preserved in tooling and formatting.

---

## 6. Flow Analysis

Flow nullability derives from:

* the declared nullability (`IsNullable`)
* control flow (conditionals, `match`, `when` guards, etc.)
* inferred non-null states after checks:

  ```raven
  if x? : int {
      if x != null {
          // x is non-null in this branch
      }
  }

  match userOpt {
      Some(u) => u.Name  // u is non-null
      None    => ...
  }
  ```

Non-nullable symbols (`IsNullable = false`) cannot be assigned `null` and produce diagnostics on suspicious uses (e.g. dereferencing without checks if analysis concludes a possible null state).

---

## 7. Interop with `Option<T>` and `T?`

Raven’s *idiomatic* “maybe” representation in user code is `Option<T>` (and other unions like `Result<T, E>`). CLR-style `T?` (nullable value types and annotated reference types) is primarily used for:

* .NET interop
* generic shapes that must match existing APIs
* low-level or boundary code

To make it easy to expose Raven APIs to C# and other .NET consumers while keeping Raven internally `Option`-driven, the language defines a **built-in implicit conversion**:

```raven
Option<T>  →  T?
```

### Semantics

For any `T`:

```raven
Some(v)  => v          // value or reference
None     => null       // default(Nullable<T>) / null
```

* For **value types**:

  * `Option<int> -> int?`
  * `Some(42)` becomes `new Nullable<int>(42)`
  * `None` becomes `default(Nullable<int>)`

* For **reference types**:

  * `Option<User> -> User?`
  * `Some(user)` becomes `user`
  * `None` becomes `null`

### Where the conversion applies

The `Option<T> -> T?` implicit conversion is available in any **target-typed** context where the expected type is a nullable type (`T?`):

* assignment:

  ```raven
  let opt : Option<int> = ...
  let x   : int? = opt
  ```

* parameter passing:

  ```raven
  func accept(x : int?) { ... }

  let o : Option<int> = ...
  accept(o)     // Option<int> -> int?
  ```

* returns:

  ```raven
  func getOrNull() -> int? {
      let o : Option<int> = compute()
      return o           // implicit conversion
  }
  ```

* generic instantiations where a `T?` is expected.

This allows Raven code to remain `Option<T>`-centric internally while still exposing familiar `T?` APIs to C# consumers when desired.

### No implicit `T? -> Option<T>`

The reverse conversion is **not implicit**:

```raven
let n : int? = ...
let o : Option<int> = n   // ❌ no implicit conversion
```

`T? -> Option<T>` is intentionally **explicit**, via helpers such as:

```raven
let o = Option.FromNullable(n)
```

Rationale:

* Keeps the “boundary” from CLR-null-land into Raven’s safer `Option<T>` world **visible and intentional**.
* Avoids ambiguity for reference types between `None` and `Some(null)`.
* Makes it clear where external null-heavy APIs are “normalized” into Raven’s union-based style.

---

# Implementation Notes (addendum for interop)

In addition to the earlier notes:

* **Built-in conversion**:
  The compiler (or core library) provides a well-known implicit conversion from `Option<T>` to `T?`:

  * For value types: implemented in terms of `System.Nullable<T>`.
  * For reference types: implemented in terms of the underlying reference type plus `null`.

* **Semantic model**:
  `GetConversion` should report a standard implicit conversion from `Option<T>` to `T?` when the target type is nullable and the source type is `Option<T>`.

* **Display**:
  When showing signatures in Raven, prefer:

  ```raven
  func findUser(id : Guid) -> Option<User>
  func getUserOrNull(id : Guid) -> User?
  ```

  rather than surfacing `Nullable<T>` directly.

That’s the only addition; the rest of the proposal stands as-is.

---

# Examples

### Locals

```raven
let count? : int
let name : string
let active? = getState()   // inferred type + nullable binding
```

### Parameters / returns

```raven
func tryFind(key : string, result? : string) -> bool

func parse(text : string) -> Option<int>
func tryGetToken(input : string, token? : Token) -> bool
```

### Properties / fields

```raven
Title? : string { get; set; }
var payload? : Payload
var id : int         // always non-null
```


> **Note:**
Fields can be bound using `let`, `var`, and `const` for binding statements. The binding keywords `val` (from `let`), `var`, `const` is used in signatures for variables. In the future we might drop `let` for `val`.

### Interplay with generics

```raven
let cache : Dictionary<string, int?>
let current? : Dictionary<string, int>  // nullable binding holding a non-nullable dictionary
let pointers : List<*int>
```

---

# Implementation Notes

## Parser

The existing grammar already introduces identifier-level nullability for locals:

```ebnf
LocalVariableDeclarator  ::= Identifier ('?')? (':' Type)? '=' Expression ;
```

We extend the same pattern to parameters, fields, and properties:

```ebnf
Parameter                ::= ['out'] Identifier ('?')? ':' Type ['=' Expression] ;

VariableDeclarator       ::= Identifier ('?')? (':' Type)? ['=' Expression] ;

PropertyDeclaration      ::= MemberModifiers?
                             ExplicitInterfaceSpecifier? Identifier ('?')? ':' Type AccessorList ;

IndexerDeclaration       ::= MemberModifiers?
                             ExplicitInterfaceSpecifier? Identifier ('?')?
                             BracketedParameterList ':' Type AccessorList ;
```

Optionally, pattern variable designations may also support identifier-level `?` (for pattern-bound nullable locals), e.g.:

```ebnf
VariableDesignationCore  ::= Identifier ('?')? | ParenthesizedVariableDesignation ;
```

This keeps the EBNF structurally simple while pushing context-sensitive validation into the binder (as the grammar comment already notes).

`NullableType` in the type grammar remains:

```ebnf
Type                     ::= ByRefType | PointerType | UnionType ;

ByRefType                ::= '&' Type ;
PointerType              ::= '*' Type ;

UnionType                ::= FunctionType {'|' FunctionType} ;

FunctionType             ::= FunctionParameterClause '->' Type
                           | NullableType ;

FunctionParameterClause  ::= FunctionTypeParameterList | NullableType ;

NullableType             ::= PrimaryType ['?'] ;
```

**But** its *use* is restricted by semantic rules (see Section 4: “Type-Level Nullability Restrictions”).

## Symbols

* Add `IsNullable` (and `ReturnIsNullable`) to relevant symbol interfaces.
* Source symbols get `isNullable` from the syntax (`Identifier ('?')?`).
* Metadata-backed symbols read nullability from `NullableAttribute` / `NullableContextAttribute` and map this onto `IsNullable`.

## Types

* Keep `NullableTypeSymbol` and `PointerTypeSymbol` for CLR correctness and generic instantiation.
* Enforce:

  * no nested `Nullable<Nullable<T>>` in symbol construction,
  * no double-nullability via both identifier and type.

## Diagnostics

* Errors for `let x? : T?` and similar “double nullability” forms.
* Errors (or at least non-idiomatic warnings) for `let x : T?` and `func f() -> T?` when `x? : T` or `Option<T>` is the preferred style.
* Diagnostics for illegal usage of `NullableType` in top-level declaration positions.

## Semantic Model

* `SemanticModel.GetTypeInfo` on a nullable symbol returns the **effective CLR type** (i.e. `Nullable<T>` for value types).
* Symbol display and Quick Info format types using identifier-level notation where possible:

  ```text
  let x? : int
  property Name? : string
  ```

rather than raw `Nullable<T>` / null-annotated reference types.

## Flow Analysis

* Uses `IsNullable` as the declaration’s initial nullability contract.
* Tracks null-state per symbol and per branch.
* Enforces that non-nullable symbols are not assigned `null` and are checked before dereference where analysis cannot prove non-null.

