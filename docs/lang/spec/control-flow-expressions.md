# Conditionals and loops

Raven uses familiar conditionals and loops, with expression forms where a
construct needs to produce a value. Blocks introduce a local scope and evaluate
to their final expression, or to `()` when they have no final value.

```raven
let next = {
    let current = 10
    current + 1
}
```

When a block is used as a statement body, any final value is discarded.

## Conditional expressions

An `if` expression evaluates one branch and produces that branch's value.
Branches may be blocks or single expressions:

```raven
let label = if score >= 50 "pass" else "fail"

let adjusted = if score > limit {
    limit
} else {
    score
}
```

When the result is used, an `else` branch is required and the branch values
must have a compatible type. An `if` used only for its effects may omit `else`.

An `if let` header tests a pattern and introduces its bindings in the successful
branch:

```raven
let value = if let Some(number) = option {
    number
} else {
    0
}
```

The value on the right is evaluated once. The leading `let`, `val`, or `var`
sets the binding mode for bare captures and an optional whole-pattern
designation. The bindings are not visible in the `else` branch. See
[Pattern matching](pattern-matching.md) for the complete pattern rules.

## `while`

`while` repeats its body while its condition is `true`:

```raven
var index = 0
while index < items.Length {
    Console.WriteLine(items[index])
    index += 1
}
```

A `while true` loop has no reachable endpoint unless its body contains a
reachable `break`. Other conditions are treated conservatively because they may
be false before the first iteration.

`while let` evaluates and matches a value before each iteration. A successful
match runs the body with the pattern bindings in scope; a failed match ends the
loop.

```raven
while let Ok(value) = next() {
    Console.WriteLine(value)
}
```

As with `if let`, the leading binding keyword is required and controls bare
captures and an optional whole-pattern designation.

## `loop`

`loop` repeats indefinitely until a `break`, `return`, `throw`, or another
abrupt transfer leaves it.

```raven
var attempts = 0

loop {
    attempts += 1
    if attempts == 3 {
        break
    }
}
```

## `for`

`for` evaluates its collection once and visits each element. The target may be
a local name, a discard, an omitted target, or a pattern.

```raven
for item in items {
    Console.WriteLine(item)
}

for _ in items {
    recordVisit()
}

for in items {
    recordVisit()
}
```

A simple name gets its type from the collection's element type. Raven supports
arrays, `IEnumerable<T>`, the enumerator pattern through `Current`, and
non-generic enumeration with `object` elements. `let` and `val` are optional
for a simple target; `var` is not allowed. An explicit annotation constrains the
element type:

```raven
for item: int in values {
    Console.WriteLine(item)
}
```

The collection's element type must be implicitly assignable to the annotation.

### Pattern targets

A pattern target runs the body only for matching elements. Non-matching
elements are skipped.

```raven
for (let x, 0) in points {
    Console.WriteLine(x)
}

for let Person(1, name, _) person in people {
    Console.WriteLine(person.Name)
    Console.WriteLine(name)
}
```

An outer `let`, `val`, or `var` supplies the binding mode for otherwise bare
captures and for an optional whole-pattern designation such as `person`.
Do not combine that outer mode with inline binding keywords in the same target.

### Range loops

A `for` target can iterate over an explicit or from-start range of integral,
floating-point, `char`, or `decimal` values. `..` includes the end; `..<`
excludes it. An omitted start means `0`. The end may not be omitted or use a
from-end bound.

```raven
for x in 0..10 { }
for x in ..<10 { }
for x in 0..<10 by 2 { }
for x in 10..0 by -3 { }
for x in 0..10.0 by 0.1 { }
```

The optional `by` step must be non-zero and is only valid for a range loop. A
positive step proceeds toward a larger end; a negative step proceeds toward a
smaller end. Inclusive ranges compare with `<=` or `>=`, while exclusive ranges
use `<` or `>`.

Range-loop diagnostics identify each invalid shape: a missing end reports
`RAV2602`, a from-end bound reports `RAV2603`, a zero step reports `RAV2604`,
and a `by` clause on a non-range collection reports `RAV2605`.

### Async enumeration

Use `await for` in an async context to enumerate a value that provides
`GetAsyncEnumerator`, `MoveNextAsync`, and `Current`:

```raven
async func process(values: IAsyncEnumerable<int>) -> Task {
    await for value in values {
        Console.WriteLine(value)
    }
}
```

## Detailed flow rules

Use `break` and `continue` to control loop execution, optionally targeting an
enclosing labeled loop. See [Jumps and labels](jumps-and-labels.md). Use
[Return and yield](returns-and-yield.md) for callable and iterator exits, and
[Exceptions and structured handling](error-handling.md) for `throw`, `try`, and
typed failures.

Statements after an unconditional transfer are unreachable. Raven reports
`RAV0162` on each unreachable statement so dead paths remain visible.
