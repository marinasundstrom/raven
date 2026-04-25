# Control flow

Raven is expression-oriented, but it also supports statement-form control flow
for sequencing, early exits, and interop-friendly imperative code.

## Expression and statement context

Every occurrence of code appears in either an expression context or a statement
context.

### Concept

* An expression context expects a value.
* A statement context expects an effect.
* Expressions are valid in statement context; their values are discarded unless
  captured.
* Some constructs are statement-only because they do not yield a value.

### Example

```raven
val value = if flag { 1 } else { 2 }

if flag {
    Log("side effect")
}
```

### Rules

* Expression contexts include assignment right-hand sides, invocation
  arguments, `match` scrutinees and arms, `if` expression branches, and the
  final position of a block expression.
* Statement contexts include block bodies before the final expression, method
  bodies, accessor bodies, and any location terminated by a newline or
  semicolon.
* Statement-form `return`, `yield return`, `yield break`, `break`, `continue`,
  statement-form `throw`, and statement-form `try` are valid only in statement
  context.
* Statements are terminated by a newline, an optional semicolon, `}`, or a
  construct-closing keyword such as `else`, `catch`, or `finally`.
* Newlines inside parentheses, brackets, or braces do not terminate
  statements.
* End-of-file fulfills the same terminating role as a newline.

When extra tokens remain on the same line after a statement has already
completed, the parser reports `RAV1019: Expected newline or ';' to terminate the
statement.`【F:src/Raven.CodeAnalysis/DiagnosticDescriptors.xml†L256-L262】 Insert
semicolons to separate statements intentionally written on one line or move the
remaining tokens to the following line.

```raven
System.Console.WriteLine("Examples") 42 // RAV1019
System.Console.WriteLine("Examples") ff; // RAV1019
var x = 2 test // RAV1019
```

## Line continuations

Line continuation is layout-sensitive and uses at most one newline.

### Concept

When the parser still expects more of the current expression, one newline is
treated as continuation trivia instead of as a statement terminator.

### Example

```raven
val sum =
    1
    + offset

val labelled = 42 // comment stays with the literal
val next =
    labelled
```

```raven
val chain = source
    .Child
    .SubChild

val first = source.Call()

.Ok // starts a new target-typed expression (blank line breaks continuation)
```

```raven
x
+ 2     // same expression

x
.Foo()  // same expression

x

+ 2     // new statement

x

.Foo()  // new statement (member binding / target-typed form)
```

```raven
val a = 42
val b = 1; b = 3
```

### Rules

* One newline may continue the current expression.
* Two or more consecutive newlines always terminate the current expression
  statement.
* Continuation commonly occurs after assignment operators, before binary
  operators, and before member access (`.` or `->`).
* Indentation on the continued line remains trivia and does not change the
  semantics of the continued expression.

## Assignment statements

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

* Statement-form `if`, `while`, `for`, `match`, and `try` have dedicated
  statement nodes.
* Remaining standalone expressions become expression statements and evaluate to
  `unit`.
* Values produced by expression statements do not become implicit return values.
* Statement-form `match`, and statement-form `if` with an `else` branch, may
  contribute an implicit tail return when they are the final statement in a
  value-returning body.

Statement-form `if` also has a dedicated pattern-binding form:

```raven
if val (id, name) = person {
    WriteLine(name)
}
```

This is sugar for a pattern test against the right-hand side:

```raven
if person is (val id, val name) {
    WriteLine(name)
}
```

The outer binding keyword also applies to typed implicit captures, so nullable
checks can be written in the same form:

```raven
val input: int? = null

if val x: int = input {
    WriteLine(x)
}
```

This also supports ordinary hierarchy narrowing:

```raven
open class Animal {}
class Dog : Animal {}

if val dog: Dog = animal {
    dog.Bark()
}
```

It can also designate the whole matched value when the pattern succeeds:

```raven
if val (2, > 0.5) point = input {
    WriteLine(point)
}
```

The leading binding keyword is required. Raven does not accept `if Pattern = expr`
without `let`/`val`/`var`, which keeps the construct distinct from assignment-like
syntax and makes capture intent explicit at the start of the statement.

The outer binding keyword supplies the binding mode for otherwise bare captures
inside the pattern, so `if val Person(1, name, _) = person { ... }` is legal and
equivalent to `if person is Person(1, val name, _) { ... }`. The same ambient
binding mode also applies to an optional trailing whole-pattern designation such
as `point` in the example above. Shadowing and other pattern-binding diagnostics
are the same as for `is` and `match` patterns.

Bindings inside the pattern may also add a nested `when` guard. The guard can be
either another pattern over the same matched sub-value or a boolean expression
that sees the locals introduced by that binding:

```raven
for val (id, amount when > 100) in orders {
    WriteLine(amount)
}

if val (id, amount when amount > 100) = order {
    WriteLine(amount)
}
```

This statement form uses Raven’s **general pattern** surface. It is not limited
to deconstruction-only shapes, so it may use property patterns, nominal
deconstruction patterns, member/case patterns, comparison patterns, and other
match-oriented constructs that are not valid as assignment/deconstruction heads.

```raven
if val Person { Name: "Ada", Age: age } = input {
    WriteLine(age)
}
```

The example above is equivalent in matching behavior to
`if input is Person { Name: "Ada", Age: val age }`, but the statement-form
surface lets the leading binding keyword supply the capture mode for otherwise
bare designations inside the property pattern.

Statement-form `while` supports the same pattern-binding header:

```raven
while val .Ok(value) = Next() {
    WriteLine(value)
}
```

The right-hand expression is evaluated at the start of each iteration. If the
pattern matches, any bindings introduced by the pattern are available inside
the loop body for that iteration. If the pattern does not match, the loop exits.
As with `if val`, the leading binding keyword is required and supplies the
binding mode for otherwise bare captures and an optional whole-pattern
designation:

```raven
while val Person(1, name, _) person = NextPerson() {
    WriteLine(person.Name)
    WriteLine(name)
}
```

`while val pattern = expr` uses the same general pattern surface as `is`,
`match`, `if val pattern = expr`, and `for` pattern targets.

In value-returning functions, Raven warns when statement-form control flow
produces branch values that are discarded instead of returned:

* `RAV2107` for statement-form `match`.
* `RAV2108` for statement-form `if`.
* `RAV2109` for statement-form `try`.

To address these warnings, either add explicit `return` statements in statement
form, or use expression form in a value context.

## Match statement

Statement-form `match` (`match expr { ... }`) is a control-flow statement. Its
arms are evaluated for effects, and arm values are discarded by default. When a
statement-form `match` is the final statement in a value-returning member, it
is treated as an implicit tail return.

Statement-form `if` follows the same tail rule when it has an `else` branch:
if it is the final statement in a value-returning member (including a
function-expression block body), branch values are treated as an implicit tail
return.

When a statement-form `match` produces values but is not in implicit-return
position, the compiler reports warning `RAV2107`.

## Return statements and expressions

The `return` keyword exits a function, lambda, or property accessor. Because
Raven supports both statement and expression forms:

* Statement form: `return` or `return <expression>` in statement position.
* Expression form: `return <expression>` in expression contexts where early exit
  is valid (for example, `name ?? return -1`).

`return` statements remain statement-only. The compiler reports `RAV1900` when a
statement `return` is used in inline expression contexts (for example, expression
`if`/`match` arms).

`return` expression form is intentionally narrow. It models an abrupt exit from
the enclosing callable while still fitting expression-oriented operators like
`??`, `if` arms, and similar value positions. It does not turn expression blocks
into statement-flow regions. In other words:

* `let b = if (a) e else return -1` is valid (`else return -1` is a return expression).
* `let b = if (a) e else { return -1 }` is invalid because `{ ... }` here is a
  block expression, and `return` inside that block is parsed as a statement.

A `return` statement may omit its expression when the surrounding function or
accessor returns `unit`. See [implementation notes](dotnet-implementation.md#return-statements)
for how such returns are emitted. This is equivalent to returning the `()` value
explicitly.

```raven
return            // equivalent to returning ()
return ()         // explicit unit return
```

Within a method-like body, each `return` is validated against the declared
return type of that body. A `return` without an expression is treated as
returning `unit`. If the returned expression's type is not assignable to the
declared return type, the compiler emits a conversion diagnostic.

Return expressions follow the same return-type validation as return statements:
the payload must be assignable to the enclosing function/lambda/accessor return
type.

Implicit return inference for methods/functions/lambdas uses explicit `return`
statements and the outer body tail expression only. Tail expressions inside nested
statement blocks are not treated as implicit returns for the enclosing member.

```raven
func DoOperation(a: int, b: int) -> int {
    if a > b {
        return -1
    }
    return a + b
}

func DoOperation2(a: int, b: int) -> Result<int, bool> {
    if a > b {
        return .Error(false)
    }
    return .Ok(a + b)
}
```

Property accessors follow the same rule: each explicit `return` must produce a
value compatible with the property's declared type.

```raven
func choose(flag: bool) -> Result<int, ()> {
    return if flag {
        .Ok(42)
    } else {
        .Error(())
    }
}

if flag {
    return 42          // allowed: expression used as a statement
}

func log(msg: string) {
    Console.WriteLine(msg)
    return            // equivalent to returning ()
}

func firstCharOrFail(name: string?) -> Result<int, string> {
    val required = name ?? return .Error("Missing name")
    return .Ok(required.Length)
}
```

## `yield` and `yield return` statements

Iterator-like members may suspend execution with either `yield expression` or
`yield return expression`. Each form produces the next element in the
enumerator sequence while preserving the generator's state so execution can
resume on the next iteration. Async iterators returning `IAsyncEnumerable<T>`
or `IAsyncEnumerator<T>` also participate in enumeration cancellation through
`GetAsyncEnumerator(CancellationToken)`. To make that enumerator token visible
inside the iterator body, mark the intended `CancellationToken` parameter with
`[EnumeratorCancellation]`.

```raven
func numbers() -> IEnumerable<int> {
    var i = 0
    while i < 3 {
        yield i
        i += 1
    }
}
```

```raven
import System.Collections.Generic.*
import System.Runtime.CompilerServices.*
import System.Threading.*
import System.Threading.Tasks.*

async func numbers([EnumeratorCancellation] cancellationToken: CancellationToken) -> IAsyncEnumerable<int> {
    yield 1
    await Task.Delay(1000, cancellationToken)
    yield 2
}
```

`yield expression` is shorthand for `yield return expression`. Raven continues
to support the explicit `yield return` spelling as well. Like explicit `return`
statements, both yield-value forms are limited to statement positions and
cannot appear in expression context.

## `yield break` statements

`yield break` terminates an iterator early. Any remaining statements in the
member are skipped, and the enumerator completes without producing additional
values.

```raven
func firstOrNone(values: IEnumerable<int>) -> IEnumerable<int> {
    for value in values {
        yield value
        yield break
    }

    yield break
}
```

The `yield break` form follows the same placement rules as `yield return` and
`break`—it must appear in statement position and is illegal outside iterator
bodies.

## `break` statements

`break` exits the innermost enclosing loop statement immediately. Execution
resumes at the statement following that loop.

```raven
var i = 0
while true {
    if i == 10 {
        break
    }

    i += 1
}
```

A `break` statement must appear within a `while` or `for` *statement*. Placing
`break` in any expression context, including the bodies of `if`, `while`, or `for`
expressions, produces diagnostic `RAV1902`. Using `break` outside a loop reports
diagnostic `RAV2600`.

## `continue` statements

`continue` skips the remainder of the current loop iteration and jumps to the
loop's re-check point.

```raven
for value in values {
    if value.isOdd {
        continue
    }

    print(value)
}
```

`continue` follows the same placement rules as `break`: it may only appear inside
`while` or `for` statements. Using it from an expression context results in
diagnostic `RAV1903`, and placing it outside a loop reports diagnostic `RAV2601`.

## Labeled statements

A **labeled statement** prefixes another statement with an identifier followed by
a colon:

```raven
start:
print("running")
```

Labels introduce symbolic targets that `goto` statements can reference. A label
applies to the next statement in the source. When a newline immediately follows
the colon, the compiler synthesizes an empty statement so the label remains a
valid target even without an explicit body. Multiple labels may precede the same
statement.

Label names are scoped to the containing function body. Declaring the same label
more than once in the same body is an error (`RAV2500`). Labels participate in
semantic lookup just like other declarations, so tools can navigate to them.

Like all statements, a label may only appear where the grammar permits
statements. Attempting to nest a labeled statement inside an expression body (for
example, inside the branches of an `if` expression) reports diagnostic `RAV1905`.

## `goto` statements

The `goto` statement jumps to a labeled statement within the same function-like
body:

```raven
start:
print("loop")
goto start
```

Evaluation of a `goto` statement ends the current statement immediately and
transfers control to the target label. Targets may appear before or after the
`goto`; backward jumps form loops while forward jumps skip ahead in the block.
Jumping to a nonexistent label produces diagnostic `RAV2501`.

Gotos cannot escape the body they are declared in. A `goto` inside a function,
lambda, or accessor may only refer to labels declared in that same body. The
compiler ensures any scopes exited by the jump are correctly unwound before
branching.

`goto` statements follow the same placement rules as other control-flow
statements: they are only legal in statement contexts. Embedding a `goto` inside
an expression context (such as the body of an `if` expression) produces
diagnostic `RAV1904`.
