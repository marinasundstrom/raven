# Pattern matching

Patterns test a value's shape or contents and can bind the parts a branch needs.
Use them to make a decision and extract data in one operation:

```raven
let message = response match {
    Ok(let value) => "Received $value"
    Error(let reason) => "Failed: $reason"
}

if input is string text {
    Console.WriteLine(text.Length)
}
```

A successful pattern can introduce new bindings and narrow a value to a more
specific type. Patterns appear in `match`, `is`, conditional pattern bindings,
loops, and deconstruction.

## Matching and deconstruction

Raven uses two related but distinct surfaces:

* **General pattern matching forms** are used in `is`, `match`,
  `if let pattern = expr`, `while let pattern = expr`, `let pattern = expr
  else`, and `for ... in` pattern targets. These support the full pattern vocabulary:
  declaration/type patterns, constants and value patterns, comparison and range
  patterns, positional patterns, sequence patterns, dictionary patterns,
  property patterns, nominal deconstruction patterns, member/case patterns,
  boolean pattern combinators, and whole-pattern designations where the
  construct allows them.
* **Deconstruction forms** are used in declaration/assignment positions such as
  `let (a, b) = expr`, `(a, b) = expr`, `let [a, b] = expr`, `[a, b] = expr`,
  `let ["x": value] = expr`, and `["x": value] = expr`. These are not general
  match statements. They are extraction-oriented and use the
  positional/sequence/dictionary deconstruction subset with nested captures,
  discards, typed designations, explicit value comparisons where supported, and
  recursive composition.

Property patterns, nominal member or case patterns, comparison-only heads,
ranges, and boolean combinators are not valid as declaration or assignment
deconstruction heads.

General patterns can be used in `match` expressions or statements, or as
conditions with `is`:

```raven
let obj: object? = /* ... */

match obj {
    Foo foo => /* Hit Foo case */
    _ => /* Covers remaining cases for object */
}

if obj is Foo foo {
    // foo is assigned, and not null
}

if lookup is ["a": let first, "b": 2] {
    // first is assigned only when both keys exist and "b" maps to 2
}

if let Person { Name: "Ada", Age: age } = obj {
    // age is assigned only when obj is a Person named "Ada"
}
```

## `is` patterns

An `is` expression produces `bool` and has no outer binding keyword. A capture
must therefore be written at its exact extraction point with `let`, `val`, or
`var`. A bare identifier compares against an existing value:

```raven
if person is { Name: name } {       // compare with existing name
    Console.WriteLine("same name")
}

if person is { Name: == name } {    // the same comparison, explicitly
    Console.WriteLine("same name")
}

if person is { Name: let name } {   // capture a new name
    Console.WriteLine(name)
}
```

Literal patterns such as `"Bob"`, `42`, `true`, `false`, and `null` retain
their literal meaning and do not need `==`.

## Conditional pattern binding

Use `if let`, `if val`, or `if var` to match a value and bring its bindings into
the successful branch:

```raven
if let (id, name) = person {
    WriteLine(name)
}
```

This is equivalent to testing the right-hand side with `is` while applying the
outer binding keyword to implicit captures inside the pattern:

```raven
if person is (let id, let name) {
    WriteLine(name)
}
```

The same header can produce a value when used as an expression:

```raven
let name = if let (_, name) = person {
    name
} else {
    "unknown"
}
```

The right-hand side is evaluated once. Pattern bindings are available only in
the successful branch, while the successful and `else` branches determine the
result value and type just as they do for an ordinary `if` expression.

Typed bindings work the same way, which makes nullable narrowing
available in both forms:

```raven
let input: int? = null

if let x: int = input {
    WriteLine(x)
}
```

In a binding-oriented construct, the leading keyword supplies the mode for bare
captures and for an optional whole-pattern designation. Use `== name` when a
bare identifier should compare with an existing value instead of capturing a
new one.

A capture may include a `when` guard. The local is introduced first, then the
guard constrains that captured sub-value:

```raven
if let Person { Age: age when > 20 } = person {
    Console.WriteLine(age)
}

if person is { Age: let age when age > 20 } {
    Console.WriteLine(age)
}
```

## Linear pattern binding with `let ... else`

Use `let pattern = expression else` when following code requires a successful
match:

```raven
let Ok(value) = result else {
    return
}

Console.WriteLine(value)
```

The `else` branch must leave the current control-flow region with `return`,
`throw`, `break`, or `continue`. This guarantees that the pattern bindings are
initialized in the surrounding scope after the declaration.

## Exhaustiveness and closed types

For exhaustiveness, nullable `T?` contributes the `null` case in addition to
the non-null domain of `T`. If `T` is a sealed hierarchy, every permitted leaf
and `null` must be covered. If `T` is open, typed subtype arms plus `null` still
require a base-type or `_` fallback for remaining non-null instances.

A type parameter constrained to a sealed hierarchy uses that constraint as its
closed pattern domain. A `match` over `T where T: Shape` is therefore exhaustive
when it covers every permitted `Shape` leaf; it does not require a `_` fallback
solely because the scrutinee's static type is a type parameter.

The same syntax also works for hierarchy narrowing, just like `if expr is Type name`:

```raven
open class Animal {}
class Dog : Animal {}

if let dog: Dog = animal {
    dog.Bark()
}
```

It can also designate the whole matched value when the pattern succeeds:

```raven
if let (2, > 0.5) point = input {
    WriteLine(point)
}
```

The leading `let` / `val` / `var` is required. A bare `if Pattern = expr` form is
not recognized. When a whole-pattern designation omits its own binding keyword,
it inherits the outer `let` / `val` / `var` binding mode.

## Pattern-binding loops

Statement-form `while` supports the same pattern-binding header:

```raven
while let Ok(value) = Next() {
    WriteLine(value)
}
```

The right-hand expression is evaluated at the start of each iteration. If the
pattern matches, the body executes with the pattern bindings in scope. If the
pattern does not match, the loop exits. A bare `while Pattern = expr` form is not
recognized; the leading `let` / `val` / `var` binding keyword is required.

## Pattern topics

* [Dictionary patterns](dictionary-patterns.md)
* [Match exhaustiveness](match-exhaustiveness.md)
* [Match forms](match-forms.md)
* [Fundamental patterns](fundamental-patterns.md)
* [Sequence and property patterns](sequence-and-property-patterns.md)
* [Deconstruction, member, and union patterns](deconstruction-and-union-patterns.md)
