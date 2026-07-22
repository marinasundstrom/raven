# Fundamental patterns

* `Type` — **type pattern**. Succeeds when the scrutinee can be treated as `Type`.
  If the pattern introduces no designation, it behaves like a type test.

* `Type name` — **typed binding**. Succeeds when the scrutinee can be treated as
  `Type`, then binds the converted value to `name` as an immutable local in the
  success scope.

  When `Type` is an open generic type name written without explicit type arguments
  (for example `Box` where `Box<T>` exists), Raven infers type arguments from the
  scrutinee when possible.

  This inference applies uniformly in:

  * `is` patterns (`if value is Box box { ... }`)
  * `match` expression arms (`match value { Box box => ... }`)
  * `match` statement arms (`match value { Box box => ... }`)

  Example:

  ```raven
  class Box<T> {}
  let value: Box<int> = Box<int>()

  if value is Box box {
      // box : Box<int>
  }
  ```

* `let name` / `var name` / `val name` — **variable pattern**. Always matches and
  introduces a binding. `let`/`val` produce an immutable local; `var` produces a
  mutable one.

  Parenthesized designations such as `let (first, second): (int, string)` bind
  each element positionally.

* **Explicit binding keyword required.** In pattern position, introducing a new
  binding always requires an explicit binding keyword (`let`, `var`, or `val`).
  A bare identifier never introduces a binding; it is interpreted as a *value
  pattern* (constant) or, if applicable, as a type name.

  For example:

  * `Ok(42)` matches the literal value `42`.
  * `Ok(discountedProduct)` matches the runtime value of the in-scope symbol
    `discountedProduct`.
  * `Ok(let n)` binds the payload to a new immutable local `n`.

  The same rule applies uniformly in positional patterns and discriminated-union case
  payloads.

## Discards

* `_` / `Type _` — **discard**. Matches without introducing a binding. The typed
  form asserts the value can be treated as `Type` while still discarding it.
  Because `_` is reserved for discards, writing `_` never creates a binding.

  Discards participate in exhaustiveness: an unguarded `_` arm is a catch-all and
  satisfies any remaining cases (even if earlier arms introduced bindings).

## Constant patterns

* `literal` — **constant pattern**. Matches when the scrutinee equals the literal
  value (`true`, `"on"`, `42`, or `null`).

* `identifier` — **value pattern**. When a bare identifier appears in pattern
  position and resolves to an in-scope value (local, parameter, field, or property),
  the pattern matches when the scrutinee equals the runtime value of that identifier.

  Value patterns are *not* bindings. To introduce a new binding, an explicit
  binding keyword (`let`, `var`, or `val`) is required.

* `Type.Member` — **qualified constant/value pattern**. When the qualified name
  resolves to a static constant-like value, such as `Math.PI` or an enum member
  such as `JsonValueKind.True`, the pattern matches when the scrutinee equals
  that value. Qualified names that resolve to types remain type/declaration
  patterns.

* `.Member` — **target-typed value pattern**. When the scrutinee type is known
  (for example, an enum type or a type with static fields), the leading-dot
  expression resolves against that target type and matches the resulting value.
  Equality expressions also provide a target type from the opposite operand, so
  `value == .Member` is equivalent to `value == EnumType.Member` when `value`
  has the enum/member-bearing type.

> 🧭 **Disambiguation:** A bare identifier in pattern position is context-sensitive. If the
> name resolves to a value symbol, it forms a value pattern. Otherwise, it is
> interpreted as a type name and participates in a type or declaration pattern.
> This disambiguation is performed by the binder, not the grammar.

## Comparison patterns

* `< expr`, `<= expr`, `> expr`, `>= expr`, `== expr`, `!= expr` — **comparison
  pattern**. Matches when the scrutinee compares to the operand using the given
  operator.

  The operand must be a side-effect-free expression (for example, literals,
  constants, or other stable values), ensuring comparison patterns remain predictable
  and optimizable.

  The operand type must match the scrutinee type after nullable/plain-type
  unwrapping. Ordinary implicit numeric widening is not applied inside
  comparison patterns, so matching an `int` scrutinee with `> 0.5` is an error.

  Comparison patterns are commonly used under `not`, `and`, and property patterns,
  e.g. `{ Age: not > 30 }`.

## Range patterns

A **range pattern** matches values that fall within a lower and/or upper bound
using `..` (inclusive upper bound) or `..<` (exclusive upper bound). Range
patterns are valid when the scrutinee type is **orderable** (for example
numeric types, `char`, or other types that support relational comparison).

Like comparison patterns, range bounds must match the scrutinee type after
nullable/plain-type unwrapping; normal implicit numeric conversions are not
applied to range bounds.

Both bounds are optional:

* `lo..hi` — matches values greater than or equal to `lo` and less than or equal to `hi`.
* `lo..<hi` — matches values greater than or equal to `lo` and less than `hi`.
* `..hi` — matches values less than or equal to `hi`.
* `..<hi` — matches values less than `hi`.
* `lo..` — matches values greater than or equal to `lo`.

Bounds are written as expressions, but the binder may restrict them to constant‑like values depending on context.

```raven
let value = 42

let result = match value {
    ..4 => "No"
    40..43 => "Yes"
    _ => "Other"
}
```

Range patterns participate in exhaustiveness and subsumption analysis alongside comparison patterns. They are treated as syntactic sugar for a conjunction of comparison operators (for example `40..43` behaves like `>= 40 and <= 43`).

> 🧭 **Disambiguation:** In pattern position, `..` introduces a range pattern rather than a range expression. The expression parser stops at `..` so the bounds are parsed independently.

## Positional patterns

* `(element1, element2, …)` — **positional pattern**. Matches when the scrutinee
  can be deconstructed positionally with the same arity (either as a tuple or via
  a compatible `Deconstruct` method).

  Positional patterns destructure by position. Each element is itself a pattern and
  may introduce bindings. For example:

  * If the scrutinee type exposes a `Deconstruct` method with matching arity
    (including as an extension method), the positional pattern uses that method
    to obtain positional values.

  * ✅ `(let a, var b)` (explicit mutability)
  * ✅ `(a: int, b: string)` (inline type annotations)
  * ✅ `(int a, string b)` (type-pattern + capture)
  * ✅ `(a: int, _)`
  * ✅ `(let a, == existingValue)` (explicit capture + value pattern)
  * ✅ `(existingA, == existingValue)` (existing-value comparison)

  In freestanding and inline positional patterns, captured variables must use an
  explicit binding keyword (`let`, `var`, or `val`). A bare identifier is
  treated as a value pattern against an existing in-scope value. In assignment
  and declaration deconstruction (`let (a, b) = expr`, `(a, b) = expr`), bare
  identifiers continue to act as deconstruction targets.
  To constrain by type and capture a value in an element, both forms are valid:
  `let name: Type` (Raven-native typed binding style) and `Type name`
  (type-pattern style).
  This is equivalent to introducing a binding and adding a `when` guard that
  compares the bound value, but `== expr` keeps the constraint local to the
  pattern and avoids an additional arm condition.

  In compiler APIs, `== expr` is represented as `ComparisonPatternSyntax`
  (distinct from `ConstantPatternSyntax`) so tools can preserve user intent.

  An element may optionally include a name before the colon (`name: pattern`) to
  bind the element value while still applying a nested pattern.
  When the positional pattern is backed by `Deconstruct` rather than a tuple,
  named elements bind by `Deconstruct` parameter name and may appear in any
  order. Unnamed elements continue to consume the remaining unmatched
  parameters in declaration order. If a supplied name does not match any
  supported deconstruction member/parameter, the pattern is invalid.
