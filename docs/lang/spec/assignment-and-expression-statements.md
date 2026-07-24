# Assignment and expression statements

Use an assignment statement to update a value. Use an expression statement
when you want to perform an operation, such as calling a function, without
using its result.

Assignments in statement position produce assignment statements rather than
expression statements.

* The left-hand side may be an assignable expression or a pattern.
* `_ = expr` is a discard assignment; the right-hand side still executes.
* Nullable conditional member assignment is also valid in statement position:
  `receiver?.Member = value` and compound forms like `receiver?.Member += delta`
  evaluate the receiver once and skip the write when the receiver is `null`.

## Expression statements

Most other expressions can appear as statements, in which case their values are
discarded.

* Statement-form `if`, `loop`, `while`, `for`, `match`, and `try` have dedicated
  statement nodes.
* Remaining standalone expressions become expression statements and evaluate to
  `unit`.
* Values produced by expression statements do not become implicit return values.
* Statement-form `match`, and statement-form `if` with an `else` branch, may
  contribute an implicit tail return when they are the final statement in a
  value-returning body.

Statement-form `if` also has a dedicated pattern-binding form:

```raven
if let (id, name) = person {
    WriteLine(name)
}
```

When a failed match should leave the current control-flow region, prefer a
`let ... else` pattern declaration. Successful bindings belong to the
surrounding scope:

```raven
let Some(name) = maybeName else {
    return
}

WriteLine(name)
```

The `else` statement must not complete normally: it must `return`, `throw`,
`break`, or `continue` as appropriate for its context. This guarantees that
every pattern binding is initialized afterward. Use `if let` when the binding
is needed only in a conditional branch, `let ... else` for a linear happy
path, and `match` when several alternatives need handling.

An `is` expression remains available when a pattern test is part of a larger
boolean expression. For binding-oriented control flow, Raven style prefers
`if let` and `let ... else`. A null-coalescing block such as
`let name = candidate ?? { return }` remains valid expression syntax, but is
not preferred when a refutable pattern can establish the binding.

This is sugar for a pattern test against the right-hand side:

```raven
if person is (let id, let name) {
    WriteLine(name)
}
```

The two forms have different binding defaults. An `is` expression is a boolean
test. It has no outer binding keyword, cannot bind the entire pattern result,
and requires each extraction point to be explicit:

```raven
if person is { Name: name } {      // compare against an existing `name`
    WriteLine("same name")
}

if person is { Name: == name } {   // equivalent explicit comparison
    WriteLine("same name")
}

if person is { Name: let name } {  // declare a new binding
    WriteLine(name)
}

if person is { Name: let name when name.Length > 0 } {
    WriteLine(name)
}
```

A dedicated pattern statement can supply a binding mode for the whole pattern.
That ambient mode lets otherwise bare designations inside the pattern destructure
into new locals, and it can also name the entire matched value. When a pattern
statement should compare against an existing local instead of capturing, the
comparison must be explicit with `==`. In practice, `==` is the marker for
"compare with this existing variable or expression" in a place where a bare
identifier would otherwise capture. Literal patterns such as `"Bob"`, `42`,
`true`, `false`, and `null` keep their ordinary literal-matching meaning and do
not need `==`. Qualified constant members, such as `Math.PI` and enum members
such as `JsonValueKind.True`, are also value patterns in `is`/`match` contexts.
Enum patterns may use target-typed member shorthand (`value is .True`) when the
scrutinee supplies the enum type. Equality comparisons can also use the
target-typed shorthand (`value == .True`), while `is` remains the clearer form
when the code is intentionally written as a pattern match:

```raven
if let Person { Name: "Ada", Age: age } matched = input {
    WriteLine(age)
    WriteLine(matched.Name)
}

if let Person { Name: "Bob" } = input {
    WriteLine("literal match")
}

if let Person { Name: == expectedName, Age: age } = input {
    WriteLine(age)
}

if let Person { Name: "Ada", Age: age when > 20 } = input {
    WriteLine(age)
}
```

The outer binding keyword also applies to typed implicit captures, so nullable
checks can be written in the same form:

```raven
let input: int? = null

if let x: int = input {
    WriteLine(x)
}
```

The same typed implicit-capture form applies inside positional, sequence, and
nominal deconstruction patterns when the surrounding construct supplies the
binding mode:

```raven
if let (key: string, value: int) = entry {
    WriteLine(key)
}
```

This also supports ordinary hierarchy narrowing:

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

The leading binding keyword is required. Raven does not accept `if Pattern = expr`
without `let`/`val`/`var`, which keeps the construct distinct from assignment-like
syntax and makes capture intent explicit at the start of the statement.

The outer binding keyword supplies the binding mode for otherwise bare captures
inside the pattern, so `if let Person(1, name, _) = person { ... }` is legal and
equivalent to `if person is Person(1, let name, _) { ... }`. The same ambient
binding mode also applies to an optional trailing whole-pattern designation such
as `point` in the example above. Shadowing and other pattern-binding diagnostics
are the same as for `is` and `match` patterns.

Bindings inside the pattern may also add a nested `when` guard. The guard can be
either another pattern over the same matched sub-value or a boolean expression
that sees the locals introduced by that binding:

```raven
for let (id, amount when > 100) in orders {
    WriteLine(amount)
}

if let (id, amount when amount > 100) = order {
    WriteLine(amount)
}
```

This statement form uses Raven’s **general pattern** surface. It is not limited
to deconstruction-only shapes, so it may use property patterns, nominal
deconstruction patterns, member/case patterns, comparison patterns, and other
match-oriented constructs that are not valid as assignment/deconstruction heads.

```raven
if let Person { Name: "Ada", Age: age } = input {
    WriteLine(age)
}
```

The example above is equivalent in matching behavior to
`if input is Person { Name: "Ada", Age: let age }`, but the statement-form
surface lets the leading binding keyword supply the capture mode for otherwise
bare designations inside the property pattern.

Statement-form `while` supports the same pattern-binding header:

```raven
while let Ok(value) = Next() {
    WriteLine(value)
}
```

The right-hand expression is evaluated at the start of each iteration. If the
pattern matches, any bindings introduced by the pattern are available inside
the loop body for that iteration. If the pattern does not match, the loop exits.
As with `if let`, the leading binding keyword is required and supplies the
binding mode for otherwise bare captures and an optional whole-pattern
designation:

```raven
while let Person(1, name, _) person = NextPerson() {
    WriteLine(person.Name)
    WriteLine(name)
}
```

`while let pattern = expr` uses the same general pattern surface as `is`,
`match`, `if let pattern = expr`, and `for` pattern targets.

Statement-form `loop` is an unconditional loop. It evaluates its body repeatedly
until control leaves through `break`, `return`, `throw`, or another abrupt exit:

```raven
var attempts = 0

loop {
    attempts += 1

    if attempts == 3 {
        break
    }
}
```

The `loop` body is an embedded statement. A block body is the normal spelling for
multi-statement loops, and a non-block embedded statement must begin on the next
line just like `if`, `while`, and `for` bodies.

In value-returning functions, Raven warns when statement-form control flow
produces branch values that are discarded instead of returned:

* `RAV2107` for statement-form `match`.
* `RAV2108` for statement-form `if`.
* `RAV2109` for statement-form `try`.

To address these warnings, either add explicit `return` statements in statement
form, or use expression form in a value context.
