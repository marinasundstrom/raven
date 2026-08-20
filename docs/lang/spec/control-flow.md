# Control flow

Raven is expression-oriented, but it also supports statement-form control flow
for running code in order, repeating work, and leaving a block early.

## Expression and statement context

Raven has both value-producing expressions and action-oriented statements. An
expression context expects a value; a statement context runs an action. The
same control-flow construct can sometimes be used in either form:

```raven
let value = if flag { 1 } else { 2 }

if flag {
    Log("side effect")
}

if flag then Log("side effect")
```

The first `if` produces the value assigned to `value`. The second is used only
for its effect. `then` is generally used with expression-form `if`. A
statement-form `if` normally uses blocks, though Raven also provides unbraced
single-statement bodies. For symmetry, statement-form `if` accepts `then` as
well; when present, an unbraced body may begin on the same line.

Expression contexts include assignment right-hand sides, call arguments,
`match` scrutinees and arms, `if` expression branches, and the final position of
a block expression. Statements appear in bodies and other positions that do not
pass a value outward.

`return`, `yield return`, `yield break`, `break`, `continue`, statement-form
`throw`, and statement-form `try` are valid only in statement context. Other
constructs, including expressions, may also be used as statements when their
result is not needed.

### Statement termination

A newline normally ends a statement. A semicolon can end one explicitly, and
is useful when placing more than one statement on a line. A statement also ends
at `}`, at end of file, or before a construct-closing keyword such as `else`,
`catch`, or `finally`. Newlines inside parentheses, brackets, or braces do not
end a statement.

When extra tokens remain on the same line after a statement has completed, the
compiler reports `RAV1019: Expected newline or ';' to terminate the statement.`
Move the remaining tokens to the next line or separate two intentional
statements with a semicolon.

```raven
System.Console.WriteLine("Examples") 42 // RAV1019
System.Console.WriteLine("Examples") ff; // RAV1019
var x = 2 test // RAV1019
```

## Line continuations

When an expression clearly continues, a single newline is treated as whitespace
instead of a statement terminator:

```raven
let sum =
    1
    + offset

let labelled = 42 // comment stays with the literal
let next =
    labelled
```

```raven
let chain = source
    .Child
    .SubChild

let first = source.Call()

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
let a = 42
let b = 1; b = 3
```

One newline may continue an expression after an assignment operator, before a
binary operator, or before member access with `.` or `->`. Two or more
consecutive newlines always end the current expression statement. Indentation
on the continued line is whitespace and does not change its meaning.

## Control-flow topics

* [Assignment and expression statements](assignment-and-expression-statements.md)
* [Match forms](match-forms.md)
* [Return and yield](returns-and-yield.md)
* [Jumps and labels](jumps-and-labels.md)
