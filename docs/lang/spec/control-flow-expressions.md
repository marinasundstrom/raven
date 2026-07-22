# Control-flow expressions and statements

A block is an expression; its value is the value of its last expression
(or `()` if none). Each block introduces a new scope for local declarations.

```raven
{
    val x = 10
    x + 1
}
```

When the same brace form appears in statement position (for example, as the body
of an `if` statement or loop, or as a standalone scoped `{ ... }` statement), it is
bound as a block statement and its expression results are discarded.

## `if` expression

`if` expressions evaluate the condition and execute exactly one branch. The
value of the overall expression is the value produced by the executed branch. If
both branches produce values, the result participates in type inference as
described in [Type inference](expressions-and-inference.md#type-inference).

Branches may be written either as block expressions (`{ ... }`) or as a single
expression without braces:

```raven
val a = if cond { 10 } else { 20 }
val b = if cond 10 else 20
```

```raven
val res =
    if cond {
        10
    } else {
        20
    }
```

When an `if` expression is used purely for its effects, omitting the `else`
branch is permitted and the value is ignored. In value contexts, provide an
`else` branch so that both outcomes yield a value.

## `while` statement

`while` repeatedly executes its body while the condition evaluates to `true`.

```raven
var i = 0
while i < list.Length {
    val item = list[i]
    Console.WriteLine(item)
    i = i + 1
}
```

`while` also supports a pattern-binding condition:

```raven
while let .Ok(value) = Next() {
    Console.WriteLine(value)
}
```

The right-hand expression is evaluated before each iteration. If the pattern
matches, the body executes and pattern bindings are in scope for that iteration.
If the pattern does not match, the loop exits. The leading `let` / `val` / `var`
binding keyword is required and supplies the binding mode for otherwise bare
captures inside the pattern and for an optional whole-pattern designation:

```raven
while let Person(1, name, _) person = NextPerson() {
    Console.WriteLine(person.Name)
    Console.WriteLine(name)
}
```

## `loop` statement

`loop` repeatedly executes its body until control leaves through `break`,
`return`, `throw`, or another abrupt exit.

```raven
var attempts = 0

loop {
    attempts += 1

    if attempts == 3 {
        break
    }
}
```

## `for` statement

Iterates over each element of a collection. The iteration target may bind a fresh
local, discard the element, omit the target entirely, or match a pattern
against each iterated value.

```raven
for item in items {
    Console.WriteLine(item)
}
```

Pattern targets use the same pattern syntax as `is` and `match`. Matching
elements execute the body; non-matching elements are skipped.

```raven
for (let x, 0) in points {
    Console.WriteLine(x)
}
```

An optional outer binding keyword may appear before the iteration target:

```raven
for let item in items {
    Console.WriteLine(item)
}

for item: int in items {
    Console.WriteLine(item)
}

for let item: int in items {
    Console.WriteLine(item)
}

for let _ in items {
    log("processing")
}

for let Person(1, name, _) in persons {
    Console.WriteLine(name)
}

for let Person(1, name, _) person in persons {
    Console.WriteLine(person.Name)
}
```

For pattern targets, the outer binding keyword uses the same shorthand rule as
pattern/deconstruction assignment: it supplies the binding mode for otherwise
bare captures inside the pattern. The same ambient binding mode also applies to
an optional trailing whole-pattern designation, so `person` in the second
example above is introduced as an immutable local. Mixing an outer binding
keyword with inline pattern binding keywords in the same target is an error.

`for` evaluates the collection once, then executes the body for every element.
Simple identifier targets resolve their element type from arrays,
`IEnumerable<T>`, and enumerator-pattern `Current` members; non-generic
fallbacks use `object`. For simple identifier targets, `val` and `let` are
allowed and optional; `var` is rejected. A simple identifier target may include
an explicit type annotation, written after the target name. The annotation is
part of the iteration target, so `for item: int in items` declares `item` as
`int`, and the collection element type must be implicitly assignable to the
annotated type. This form is distinct from type-pattern syntax with a
designation, which keeps the pattern shape `Type designation`.
If the element value is unused, the
iteration target may be written as `_` or omitted entirely:

```raven
for _ in items {
    log("processing")
}

for in items {
    log("processing")
}
```

Both forms still enumerate the collection but do not introduce a new binding.
Pattern targets are lowered as a per-element `is` check around the loop body.

Async enumeration uses `await for`:

```raven
async func Process(values: IAsyncEnumerable<int>) -> Task {
    await for value in values {
        Console.WriteLine(value)
    }
}
```

`await for` requires an async context and an async-enumerator pattern
(`GetAsyncEnumerator`, `MoveNextAsync`, and `Current`). The loop is lowered
before async state-machine rewriting so it works in both classic async lowering
and runtime-async mode.
When the collection is a range with explicit, from-start bounds, the loop
iterates over integral, floating-point, `char`, or `decimal` values beginning
at the range's lower bound and continuing through the upper bound. `..` uses an
inclusive upper bound, while `..<` uses an exclusive upper bound.
Omitting the start defaults it to `0`, while omitting the end or using
from-end bounds in a `for` statement reports a diagnostic.

Range-based `for` statements may include an optional `by` clause:

```raven
for x in 0..10 by 2 { }
for x in 0..<10 by 2 { }
for x in 10..0 by -3 { }
for x in 0..10.0 by 0.1 { }
```

The step must be non-zero. With `..`, if the step is positive iteration
continues while the current value is less than or equal to the end bound; if
the step is negative, iteration continues while the current value is greater
than or equal to the end bound. With `..<`, the comparisons are strict (`<` and
`>` respectively). The `by` clause is only valid for range-based `for` loops.

## `break` and `continue`

`break` immediately exits the innermost loop (`loop`, `while`, or `for`).
`continue` skips the remainder of the current iteration and jumps to the next
loop pass. `break label` and `continue label` target the enclosing labeled loop
with that name, allowing nested loops to exit or continue an outer loop directly.
Both keywords are only valid inside loops; using them elsewhere produces
diagnostics (`RAV2600` for `break`, `RAV2601` for `continue`). They are
statements, not expressions, so placing them in an expression context also
triggers an error.

```raven
while cond {
    if shouldStop {
        break
    }

    if shouldSkip {
        continue
    }

    work()
}
```

A labeled `break` or `continue` must name an enclosing labeled loop. A label on
an ordinary statement is still a valid `goto` target, but it is not a loop target;
using it with `break` or `continue` reports `RAV2606`.

```raven
outer: loop {
    for value in values {
        if shouldStop(value) {
            break outer
        }
    }
}
```

## Unreachable statements

Flow analysis tracks whether each statement in a block can execute. When a
previous statement always transfers control—such as `return`, `throw`, `goto`,
`yield break`, or a loop control transfer like `break`/`continue`—any following
statement in the same block becomes unreachable. The compiler reports
`RAV0162` (warning severity) on each such statement to highlight dead code
paths.

```raven
func demo(flag: bool) {
    if flag {
        return
    }

    throw System.InvalidOperationException()
    var value = 1      // RAV0162: Unreachable code detected
}
```

## Labeled statements and `goto`

A label is written as `identifier:` ahead of a statement. Labels share a single
scope within the surrounding function body: redeclaring the same label name, or
using a reserved keyword as the label, produces a diagnostic. Escaped
identifiers such as `@loop:` use the identifier's logical name for lookup, so
`goto @loop` and `@loop:` refer to the same target. 【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Statements.cs†L560-L609】【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Statements.cs†L702-L733】

`goto name` transfers control to the labeled statement with the given name.
Targets must be labels declared in the same function; unresolved names, missing
identifiers, or reserved words report diagnostics (`RAV2500`–`RAV2502`). `goto`
statements are likewise disallowed in expression contexts. 【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Statements.cs†L572-L609】【F:src/Raven.CodeAnalysis/DiagnosticDescriptors.xml†L337-L345】

```raven
func retryingWork() {
start:
    val ok = tryOnce()
    if not ok {
        goto start
    }
}
```

## `throw` statement

`throw expression` aborts the current control path by raising an exception. The
expression must be implicitly convertible to `System.Exception`; otherwise the
compiler reports `RAV1020`. Statement `throw` is allowed inside block
expressions used as scoped early-exit regions, but it remains disallowed in
inline expression arms (for example, `if`/`match` expression arms), where it
reports `RAV1907`. Use `throw` expression form (`?? throw ...`) when you need
inline expression-level control flow.
【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Statements.cs†L529-L557】【F:src/Raven.CodeAnalysis/DiagnosticDescriptors.xml†L231-L240】

```raven
func parse(text: string) -> int {
    if text.Length == 0 {
        throw System.ArgumentException("Value is required")
    }

    return Convert.ToInt32(text)
}
```

## Iterator statements (`yield`, `yield return`, `yield break`)

Iterator methods and function expressions produce lazily-evaluated sequences by
using `yield` statements.
`yield expression` and `yield return expression` publish the next element of the
sequence; the expression is converted to the iterator's element type before
emission.
`yield break` terminates the sequence early. Both forms may only appear in
methods or function expressions whose return type implements
`System.Collections.Generic.IEnumerable<T>`,
`System.Collections.Generic.IEnumerator<T>`,
`System.Collections.Generic.IAsyncEnumerable<T>`,
`System.Collections.Generic.IAsyncEnumerator<T>`, or their non-generic
counterparts. When such a method or function expression contains `yield`, the
compiler rewrites it into a state machine that implements the appropriate
enumerator pattern. 【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Statements.cs†L489-L527】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/IteratorLowerer.cs†L25-L58】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/IteratorLowerer.cs†L302-L340】

For function expressions, the iterator return type may be written explicitly or
inferred from the presence of `yield`. Unannotated synchronous iterator lambdas
infer `IEnumerable<T>`; unannotated async iterator lambdas infer
`IAsyncEnumerable<T>`.

```raven
import System.Collections.Generic.*

class Counter {
    func Numbers(max: int) -> IEnumerable<int> {
        var current = 0
        while current < max {
            yield current
            current = current + 1
        }

        yield break
    }
}
```

```raven
import System.*
import System.Collections.Generic.*

class Counter {
    func Numbers() -> Func<IEnumerable<int>> {
        val factory: Func<IEnumerable<int>> = () => {
            yield 1
            yield 2
        }

        factory
    }
}
```

The generated state machine preserves captured locals and surfaces the expected
metadata shape for the declared iterator kind. Synchronous iterators expose
`Current`, `MoveNext`, `Dispose`, and `GetEnumerator`; async iterators expose
`Current`, `MoveNextAsync`, `DisposeAsync`, and `GetAsyncEnumerator`.
Each `yield` / `yield return` resumes exactly where it left off on the next move
call.
For async iterators, a `CancellationToken` parameter only receives the token
passed to `GetAsyncEnumerator(...)` when that parameter is marked with
`[EnumeratorCancellation]`. If an async iterator has `CancellationToken`
parameters but none is marked, Raven warns that the enumerator token will be
ignored. If both a direct method argument token and a `GetAsyncEnumerator(...)`
token are present for the marked parameter, Raven combines them so cancellation
from either source is observed inside the iterator body.
【F:src/Raven.CodeAnalysis/BoundTree/Lowering/IteratorLowerer.cs†L46-L128】【F:src/Raven.CodeAnalysis/Binder/MethodBodyBinder.cs†L255-L280】【F:src/Raven.CodeAnalysis/Binder/BlockBinder.FunctionExpressions.cs†L2140-L2165】

```raven
import System.Collections.Generic.*
import System.Runtime.CompilerServices.*
import System.Threading.*
import System.Threading.Tasks.*

class Streams {
    async func People([EnumeratorCancellation] cancellationToken: CancellationToken) -> IAsyncEnumerable<Person> {
        yield Person("Bob", 30)

        await Task.Delay(1000, cancellationToken)
        yield Person("Alice", 20)
    }
}
```
