# Pattern matching

Patterns check the shape or contents of a value and can take out the parts a
branch needs. They appear in `is` predicates and `match` expressions, where
successful checks can also narrow types.

Raven uses two related but distinct surfaces:

* **General pattern matching forms** are used in `is`, `match`,
  `if let pattern = expr`, `while let pattern = expr`, and `for ... in`
  pattern targets. These support the full pattern vocabulary:
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

Patterns can be used in `match` expressions or statements, or as conditions with
`is` patterns:

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

Raven also supports statement-form conditional pattern binding:

```raven
if let (id, name) = person {
    WriteLine(name)
}
```

This form is equivalent to testing the right-hand side with `is` while applying
the outer binding keyword to implicit captures inside the pattern:

```raven
if person is (let id, let name) {
    WriteLine(name)
}
```

Typed implicit bindings work the same way, which makes nullable narrowing
available in statement form:

```raven
let input: int? = null

if let x: int = input {
    WriteLine(x)
}
```

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

## Detailed chapters

* [Dictionary patterns and exhaustiveness](dictionary-patterns-and-exhaustiveness.md)
* [Match forms](match-forms.md)
* [Pattern forms and contexts](pattern-contexts.md)
* [Fundamental patterns](fundamental-patterns.md)
* [Sequence and property patterns](sequence-and-property-patterns.md)
* [Deconstruction, member, and union patterns](deconstruction-and-union-patterns.md)
