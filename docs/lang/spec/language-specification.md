> ⚠️ This is a living document that is subject to change.

# Language specification

Implementation details describing how Raven projects map to .NET are documented in [dotnet-implementation.md](dotnet-implementation.md).

An overview of available types, literal semantics, and conversions can be found in the [type system](type-system.md).

## Document conventions

* **Normative requirements** use key words such as “must”, “may”, and “should” to describe observable language behaviour.
* Notes and tips highlight rationale, examples, or implementation remarks. They are informative rather than normative.
* Code snippets use the `.rav` file extension and omit surrounding boilerplate unless it is essential to the rule being described.
* When behaviour is intentionally unspecified or still under design, this specification calls it out explicitly and, where possible, links to suggested follow-up work.

## Code samples

Representative sample programs live in the repository's [`samples/`](../../../samples/) directory alongside runnable CLI demos.
Each sample intentionally exercises language features such as pattern matching, pipe operators, tuple flow, and .NET interop so
changes to the language can be validated with real code, not just unit tests. The top-level `samples/README.md` explains how to
run them with the Raven CLI.

## Proposals

You find proposals for future language features [here](../proposals/).

## File extension

The file extension for source code files is: `.rav`.

## Grammar

An accompanying [EBNF grammar](grammar.ebnf) describes the structural
syntax of Raven. **It is non-normative**: it does not encode contextual rules,
disambiguation, or the full parsing process; those details are specified
throughout this language specification.

### Identifiers

Identifiers name values, members, and types. They may begin with `_`, `$`, or
any Unicode character classified as a letter (including letter numbers such as
Roman numerals). ASCII letters therefore continue to work as before. Subsequent
characters may also include decimal digits, connector punctuation (such as
`_`), combining marks, and format characters. These rules mirror .NET identifier
support so Raven source can interoperate with existing APIs. Reserved keywords
cannot be used as identifiers.

### Keyword summary

The lexer defines the complete keyword set in [`Tokens.xml`](../../../src/Raven.CodeAnalysis/Syntax/Tokens.xml), which
classifies each keyword as either reserved or contextual.

| Kind | Keywords |
| --- | --- |
| Reserved | `and`, `as`, `await`, `base`, `bool`, `break`, `byte`, `catch`, `char`, `class`, `const`, `continue`, `decimal`, `default`, `double`, `each`, `else`, `enum`, `false`, `finally`, `float`, `for`, `func`, `goto`, `if`, `int`, `interface`, `is`, `let`, `long`, `match`, `new`, `nint`, `not`, `null`, `nuint`, `object`, `or`, `return`, `sbyte`, `self`, `short`, `sizeof`, `string`, `struct`, `throw`, `true`, `try`, `typeof`, `uint`, `ulong`, `ushort`, `var`, `when`, `while`, `yield` |
| Contextual | `abstract`, `alias`, `explicit`, `final`, `get`, `implicit`, `import`, `in`, `init`, `internal`, `namespace`, `open`, `operator`, `partial`, `out`, `override`, `private`, `protected`, `public`, `ref`, `sealed`, `set`, `static`, `unit`, `use`, `val`, `virtual` |

Reserved keywords are always treated as keywords and therefore unavailable for use as identifiers—even when a construct makes
their presence optional (for example, omitting `each` in a `for` expression). Contextual keywords behave like ordinary
identifiers except in the syntactic positions that demand their special meaning—for example, accessibility modifiers
(`public`, `internal`, `protected`, `private`) or accessor modifiers (`get`, `set`). The `partial` keyword is only recognised
when declaring types and controls whether multiple declarations of the same class merge; see [Partial classes](classes-and-members.md#partial-classes).

To use a reserved keyword as an identifier, prefix it with `@`. The lexer produces an identifier token whose `Text` still
includes the `@` escape, while the token's `ValueText` omits it. All bound symbols expose the unescaped name, mirroring C#'s
behaviour and ensuring metadata and semantic lookups use the identifier's logical name instead of its escaped form.

```raven
class @int {}

static @match(@return: int) -> int
{
    val @and = @return;
    return @and;
}
```

The single-character `_` token is reserved for discards. When a pattern,
deconstruction, or other declaration spells its designation as `_` (optionally
with a type annotation), the compiler suppresses the binding and treats the
designation as a discard instead. Longer identifiers may still contain
underscores, and `$` is available for interop- or DSL-oriented naming schemes.
Because `_` never produces a value, using it as an expression—for example
in `_ + 2`—is rejected as an error.

```raven
val $ffiResult = call()
val value_1 = value0
val 数据 = call()
val сумма = total1 + total2
```

### Comments

Comments provide source-level annotations that the compiler ignores during
semantic analysis. They may appear anywhere whitespace is permitted, including
between tokens and at the end of a line. Comments never contribute tokens to
the syntax tree; they are attached as trivia instead.

Two forms of comments are supported:

* **Single-line comments** start with `//` and continue until the next newline
  or the end of the file. The terminating newline is not part of the comment.
* **Multi-line comments** start with `/*` and end with the next `*/`. They may
  span multiple lines but **do not nest**—a `/*` encountered inside a
  multi-line comment is treated as ordinary text. If the end of the file is
  reached before `*/`, the comment consumes the remainder of the file.

Comment contents are treated as uninterpreted Unicode text. Any Unicode scalar
value may appear inside a comment without escaping, including characters that
would otherwise form tokens. This includes emoji and other symbols outside the
Basic Multilingual Plane as long as the source file's encoding can represent
them. The lexer preserves the original spelling (other than omitting the
terminator), so encodings such as UTF-8 or UTF-16 must supply valid code units
for the desired characters.

```raven
val answer = 42  // the ultimate answer
val greeting = "hello"  // 😀 emoji and other symbols are fine

/*
 Multi-line comments can document larger blocks of code.
 The first */ encountered closes the comment.
*/
```

### Documentation comments

Documentation comments describe publicly consumable APIs and attach to the
next declaration when only whitespace and newlines appear in between. Placing a
documentation comment elsewhere produces a documentation warning and the
comment is ignored for that declaration.

Two documentation spellings are supported:

* **Single-line documentation comments** start with `///` and may be stacked.
  Each line contributes to the same documentation block.
* **Multi-line documentation comments** start with `/**` and end with the next
  `*/`. Leading `*` characters are stripped from each line to simplify
  indentation.

The documentation **format** is selected per syntax tree. Markdown is the
default authoring format; XML can be requested through parser/compiler options
when compatibility with XML doc tools is required. The selected format is
preserved so consumers can render Markdown directly or process XML with tag
awareness.

Regardless of format, the following logical sections are recognised:

* `summary` — a short description of the declaration.
* `param` / parameter entry — one per parameter, aligned by name.
* `typeparam` — one per type parameter, aligned by name.
* `returns` — the return value description for non-`unit` members.
* `remarks` — optional long-form notes, examples, and links.

XML documentation must be well-formed; unterminated or mismatched tags are
reported as documentation warnings. Markdown content should follow CommonMark
conventions, including balanced fenced code blocks. These diagnostics are
suppressed when documentation mode is disabled.

## Syntax node model

The syntax node model defines the **logical structure** of Raven programs.
It is specified in [Model.xml](../../../src/Raven.CodeAnalysis/Syntax/Model.xml) and
drives code generation of the immutable syntax tree API.

The model describes the set of node kinds and their children.
The **parser** applies the grammar and contextual rules to construct these nodes.

In short: the model defines the shape; the parser defines the rules,
as outlined in this specification.

## Unit type

Raven has no `void` type. The absence of a meaningful value is represented by the
`unit` type, which has exactly one value written `()`. The type itself may be
spelled `unit` or `()`. Functions without an explicit return type implicitly
return `unit`. In .NET, `unit` corresponds to `void` (see [implementation notes](dotnet-implementation.md#unit-type)). The `unit` type participates in generics, tuples, and unions like any other type.

## Statements

Raven is primarily **expression-oriented**: most constructs yield values and can
appear wherever an expression is expected. For details on statement forms,
terminators, and control-flow constructs, see [Control flow](control-flow.md).
Structured exception handling is covered in [Error handling](error-handling.md).

### Variable bindings

`val` introduces an immutable binding, `var` introduces a mutable one, and `const`
produces an immutable binding whose value is baked in at compile time. A binding may
declare its type explicitly or rely on the compiler to infer it from the initializer
expression.

```raven
val answer = 42         // inferred int
let answer = 42         // alias for 'val': inferred int

var name = "Alice"    // inferred string, mutable

const greeting = "Hi"  // inferred string constant

val count: long = 0     // explicit type
```

> **Note:** We should decide upon whether to prefer `val` over `let`.

If the type annotation is omitted, an initializer is required so the compiler can
determine the variable's type. Const bindings always require an initializer, even when
annotated, and the expression must be a .NET compile-time constant (numeric and
character literals, `true`/`false`, strings, or `null`).

Control-flow constructs such as `if`, `while`, and `for` are expressions whose
statement forms are described in [Control flow](control-flow.md).

Later declarations in the same scope may **shadow** earlier bindings. Each declaration
introduces a new symbol; code that follows binds to the most recent declaration.
Shadowing is permitted for both `val` and `var` bindings, but it produces the
warning diagnostic `RAV0168` to help catch unintentional redeclarations. Parameters of
the enclosing function count as previous declarations for this purpose, so a local that
reuses a parameter name both shadows it and triggers the same warning.

```raven
val answer = 41
val answer = answer + 1 // RAV0168 (warning)
```

### File-scope code

File-scope code is supported—no `Main` function is required.

```raven
import System.*
alias print = System.Console.WriteLine

sayHello()

func sayHello() {
    print("Hello, World!")
}
```

## Expressions

### Target typing


Many expressions rely on the type expected by their context, called the **target type**.
For example, the enum shorthand `.B` in `var grade: Grades = .B` uses the declared type `Grades` to resolve the member.

#### Discriminated union case sugar for `unit`

When a discriminated union case carries exactly one payload of type `unit`, Raven permits the case to be written *without* an argument list in expression position. In such contexts, a bare case name is sugar for supplying the sole `unit` value `()`.

```raven
func Save() -> Result<(), Error> {
    return .Ok       // sugar for `.Ok(())`
}
```

This rule applies uniformly to **all** discriminated unions, not only `Result` or `Option`. The case must declare exactly one constructor parameter whose type is `unit`; cases with additional parameters or non-`unit` payloads still require an explicit argument list.

This mirrors the existing pattern-matching rule where a bare case such as `.Ok` matches `.Ok(())` when the payload is `unit`, ensuring consistency between expression construction and pattern matching.

### Type inference

When an expression or declaration omits an explicit type, Raven infers one from
the expression. If multiple different types can flow to a location—through
conditional branches or early `return` statements—the inferred result becomes a
**union** of those types. The compiler does not collapse distinct types to their
nearest common base; returning `Dog` and `Cat` infers `Dog | Cat`, not `Animal`.
This specification refers to such results as **inferred union types**.
Inferred union types (`A | B`) are distinct from declared discriminated/tagged
unions introduced with the `union` keyword.

```raven
val pet = if flag { Dog() } else { Cat() }
// pet has type: Dog | Cat
```

Literal expressions infer the underlying primitive type when used to initialize
`val` or `var` bindings. Literal types are subset types of their underlying
primitive, so a literal like `1` can be used wherever an `int` is expected.
When inference gathers multiple results—such as the branches of an `if`
expression—it normalizes the union before exposing it. Literal members collapse
to their underlying type when a non-literal of the same type also flows to the
branch, so the union reflects only the distinct possibilities that can escape.
To retain a literal's singleton type for a single value, an explicit annotation
is required.

```raven
var i = 0       // i : int
val j = 0       // j : int
var k: 1 = 1    // k : 1
```

Control-flow expressions participate in the same inference. An `if` expression
whose branches produce different types infers a union of those results. The
union is normalized so redundant members collapse away while distinct literal
branches remain precise:

#### Collapsing branch unions

```raven
val x: int = 3
val value = if x > 2 { 42 } else { x }
// value : int          (literal 42 collapses into the `int` branch)

val digits = if x > 2 { 1 } else { 2 }
// digits : 1 | 2       (distinct literal branches remain literal)

val other: long = 0
val ambiguous = if x > 2 { other } else { 42 }
// ambiguous : int | long
// val target: int = ambiguous  // error: not every branch converts to int
```

Each branch contributes its inferred type to the union. Because the `else`
branch in the first example is the literal `42`, and the other branch produces
an `int`, the literal collapses into the `int` branch. When all branches are
distinct literals, the union records them individually so downstream pattern
matching can reason about the precise set of constants. When inference observes
multiple primitive types, the union retains each one. Assigning that union to a
single numeric type triggers a diagnostic because not every branch converts to
the target.

Numeric literals choose an underlying primitive type according to their form
and optional suffix. The default rules are designed to be predictable while
still allowing safe narrowing through implicit *constant* conversions.

#### Integer literals

* **Unsuffixed integer literals** default to `int`.
  * If the value does not fit in `int`, the literal is typed as `long`.
* The following suffixes override the default:
  * `b` / `B` — `byte`
  * `l` / `L` — `long`

Unsuffixed integer literals may still convert implicitly to smaller integral
types (such as `byte` or `char`) **when used as constant expressions** and the
value fits in the target type. This mirrors C#’s constant conversion rules and
avoids accidental narrowing for non-constant values.

```raven
val a = 42        // int
val b = 4_000_000_000 // long

val x: byte = 12 // OK: constant int fits in byte
val y: byte = 300 // error: constant out of range

val z = 32b      // explicit byte literal
val n = 10L      // explicit long literal
```

#### Floating-point literals

* **Unsuffixed floating-point literals** (those containing a decimal point or
  exponent) default to `double`.
* The following suffixes override the default:
  * `f` / `F` — `float`
  * `d` / `D` — `double`
  * `m` / `M` — `decimal`

```raven
val d = 3.14     // double
val f = 3.14f    // float
val m = 9.99m    // decimal
val e = 1e3      // double
```

Decimal literals do not support exponent notation. Attempting to combine an
exponent with the `m`/`M` suffix produces a diagnostic.

Literal values participate in overload resolution and type inference using
their underlying primitive type. When a literal appears in a context that
supplies a target type, the compiler may apply implicit constant conversions
before considering explicit casts.

Overload resolution applies the same rule: a literal argument converts to its
underlying type when selecting among method overloads. For example,
`Console.WriteLine(1)` binds to `Console.WriteLine(int)` if such an overload
exists, and `Console.WriteLine("test")` chooses `Console.WriteLine(string)`.

Functions and lambdas without an annotated return type infer their result by
collecting:

1. the types of all explicit `return` statements, and
2. the final expression of the outer body when that body has a value-producing tail expression.

Expression statements in nested statement blocks (for example, inside `if`/`while`/`for`
statement bodies) do not participate in return-type inference. If no value-returning
path exists, the type defaults to `unit`.

```raven
func example(x: int) -> {
    if x > 0 { return x }
    "neg"
}
// inferred return type: int | string
```

When a lambda expression is assigned to a binding without an explicit type, Raven
still materialises a concrete delegate. The compiler synthesises an appropriate
`System.Func`/`System.Action` definition using the lambda's parameter types and
the inferred return type (treating `unit` results as actions). Captured
variables participate in the enclosing flow analysis before the delegate type is
constructed, so the lambda observes the same declared type as any other use of
the variable.

```raven
val a = 42
val makeAdder = () => a + 3

makeAdder() // returns 45, makeAdder : System.Func<int>
```

Async lambda expressions mirror async functions: placing `async` before the parameter
clause permits `await` inside the body. When the lambda's return type is not annotated
and no delegate supplies one, the compiler wraps the inferred result in
`System.Threading.Tasks.Task<T>` (or `Task` when the body produces `unit`). A delegate
annotation or target type may still specify a concrete `Task` shape, in which case the
lambda body must evaluate to the awaited result type rather than the task itself.
Annotating an async lambda with a non-`Task` return type is an error.

### Additional type inference rules (normative)

The following clarifications extend the type inference model:

* **Contextual inference**: Raven computes a contextual type based on both expression shape and target type. Inference is bidirectional.
* **Union growth**: To prevent type explosion, implementations may limit union width. Public APIs that infer wide unions should prefer explicit annotations.
* **Literal arithmetic**: Non-constant operations widen literals to their base type unless constant-folded.
* **Overload resolution with unions**: A candidate overload is viable only if every alternative in a union converts to its parameter type. Ambiguities require explicit casts.
* **Generic inference**: Type argument inference from union arguments requires a single consistent set of type arguments that satisfy all alternatives. Constraints must hold for each alternative.
* **Nullability**: `T | null` with a single `T` is equivalent to `T?`. `T? | U` is invalid. Safe navigation over unions yields nullable results.
* **Pattern narrowing**: See [Pattern matching](#pattern-matching) for how `is` and `match` refine variables; inferred result types for `match` are the union of arm results.
* **Tuples**: Tuple element names do not affect type identity. Conditional tuples union element-wise.
* **Ref/out parameters**: `ref` requires exact type match; `out` contributes to inference of the parameter type.
* **Flow stability**: Variable declarations have a fixed declared type. Narrowings are ephemeral and do not change declared type. Captured variables use the join of all flows.
* **Diagnostics**: When conversion fails, diagnostics must identify which union member(s) failed and why. For overloads, diagnostics must explain alternative selections.

### Union conversions

Assigning or returning a union to an explicitly typed target succeeds only when
**every** member of the source union can convert to the target type. The compiler
checks each constituent individually.

```raven
val maybe = if flag { 0 } else { 1.0 } // int | float

val n: int = maybe    // error: float not assignable to int
val o: object = maybe // ok: both int and float convert to object

val pet = if flag { Dog() } else { Cat() } // Dog | Cat
val a: Animal = pet   // ok: Dog and Cat derive from Animal
val s: string = pet   // error: neither member converts to string
```

### Await expressions

The `await` keyword introduces a unary expression with the grammar `await`
*expression*. Await expressions participate in the same precedence as other
prefix unary operators. Because `await` is reserved, the identifier form must be
escaped as `@await` when used outside this construct.

`await` may only appear inside an async function or lambda. File-scope
statements synthesize a synchronous `Program.Main` plus an async
`Program.MainAsync`, so top-level awaits run inside the async method while the
bridge awaits `MainAsync(args)` before returning to the host. The awaited
expression must expose an instance method `GetAwaiter()` whose return type
provides an accessible `bool IsCompleted { get; }` property and a parameterless
`GetResult()` method. If the awaiter’s `GetResult` produces no value, the await
expression’s type is `unit`; otherwise it matches the `GetResult` return type.

Failing any of these requirements produces a compile-time diagnostic identifying
the missing member. The compiler also reports an error when `await` appears
outside an async context.

Evaluation first computes the operand value and calls `GetAwaiter()` to obtain
the awaiter. If `IsCompleted` is `true`, `GetResult()` is invoked immediately
and the await expression yields its value. Otherwise execution is suspended and
later resumed when the awaiter signals completion; resumption continues after
the `await` with the result of `GetResult()`.

### Try expressions

`try expression` evaluates its operand and captures either the resulting value
or an exception into a `System.Result<T, System.Exception>`. The operand must be
any expression that is valid in the current context. The compiler assigns the
`try` expression the `Result` type whose `T` is the operand type (or `unit` when
the operand produces no value), enabling pattern matching on successful results
versus failures. Nested `try` expressions are disallowed and produce `RAV1906`.

`try? expression` combines `try` with the propagation operator and is sugar for
`(try expression)?`. It evaluates the operand into a `Result<T, Exception>` and
then propagates errors to the enclosing `Result`/`Option` return type, producing
the success payload as the expression value. A trailing `match` is not permitted
after `try?`; use `try expression` when matching is required. The compiler
reports `RAV1908` if a `match` suffix follows a `try?` expression.

Execution enters a `try`/`catch` block that stores the operand’s value in a
temporary. If evaluation completes without throwing, the temporary value is
wrapped in `.Ok(...)` and becomes the expression’s final value. If evaluation
throws an exception, the runtime catches the `System.Exception` instance and
wraps it in `.Error(...)` instead. When the operand’s value type is `unit`, the
success case is still `.Ok(())`. 【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L1565-L1595】【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L3672-L3759】

When matching, a bare case like `.Ok` is sugar for `.Ok(())` if the case carries
a single payload. This makes unit-typed results concise to handle.

`await` may appear inside a `try` expression when used in an async method or
lambda. The awaited result still flows to the success case, and exceptions
propagate to the `Result`'s error case without altering the `try` expression's
shape. 【F:test/Raven.CodeAnalysis.Tests/Semantics/ExceptionHandlingTests.cs†L89-L123】

#### Examples

The `try` keyword produces a `Result<T, Exception>` value that can be consumed with `match` or `is` patterns.

##### Matching success vs failure

```raven
import System.*
import System.Console.*

func ParseInt(text: string) -> string {
    return try int.Parse(text) match {
        .Ok(val value) => "Parsed: $value"
        .Error(FormatException ex) => "Invalid format: ${ex.Message}"
        .Error(_) => "Unknown error"
    }
}

WriteLine(ParseInt("123"))
WriteLine(ParseInt("foo"))
```

##### Propagating with `try?`

```raven
func ParseInt(text: string) -> Result<int, string> {
    val value = try? int.Parse(text)
    return .Ok(value)
}
```

##### `try` expression returning `unit`

When the operand produces no value, the success case is `.Ok(())`. A bare `.Ok`
arm is sugar for `.Ok(())`.

```raven
import System.*
import System.Console.*

func SaveFile(path: string, text: string) -> string {
    return try System.IO.File.WriteAllText(path, text) match {
        .Ok => "Saved!"
        .Error(UnauthorizedAccessException ex) => "Access denied: ${ex.Message}"
        .Error(IOException ex) => "I/O error: ${ex.Message}"
        .Error(_) => "Unknown error"
    }
}

WriteLine(SaveFile("out.txt", "Hello"))
```

##### Using `try` with `await`

```raven
import System.*
import System.Console.*
import System.Net.Http.*

func Download(url: string) -> string {
    val http = HttpClient()

    return try await http.GetStringAsync(url) match {
        .Ok(val text) => text
        .Error(HttpRequestException ex) => "Network error: ${ex.Message}"
        .Error(_) => "Unknown error"
    }
}
```

##### Handling Result values with `is` patterns

### Result and Option propagation (`?`)

The postfix `?` operator unwraps a `Result<T, E>` or `Option<T>` value by extracting its success payload (`T`) or short-circuiting the current function by returning the error/none case.

`expr?` is only valid inside a function or lambda whose return type matches the propagated shape:

* For `Result`: the operand expression must have type `Result<T, E>`, and the enclosing return type must be `Result<_, _>`. When the operand evaluates to `.Ok(value)`, the propagation expression yields `value`. When the operand evaluates to `.Error(error)`, the propagation expression performs an early return of `.Error(error)` from the enclosing function.
* For `Option`: the operand expression must have type `Option<T>`, and the enclosing return type must be `Option<_>`. When the operand evaluates to `.Some(value)`, the propagation expression yields `value`. When the operand evaluates to `.None`, the propagation expression performs an early return of `.None` from the enclosing function.

The `?` operator is **not** a general control-flow expression: it does not permit `return` statements inside `match` arms. Instead, propagation introduces an implicit control-flow split that is lowered as branching when emitting code.

Propagation is context-sensitive with conditional access. In postfix position:

* `expr?.Member`, `expr?(...)`, and `expr?[...]` form conditional access and do not perform Result propagation.
* `expr?` with no following trailer performs Result propagation.

#### Examples

##### Unwrapping a Result

```raven
func ReadValue() -> Result<int, Exception> {
    val value = ParseInt("123")?
    return .Ok(value)
}
```

##### Propagating errors from chained calls

```raven
func LoadAndParse(path: string) -> Result<int, Exception> {
    val text = ReadAllText(path)?
    val value = ParseInt(text)?
    return .Ok(value)
}
```

##### Propagating into a different error type with an extension implicit converter

```raven
public class ParserError { }
public class DomainError { }

public extension ErrorConverters for ParserError {
    public static implicit operator(value: ParserError) -> DomainError {
        return DomainError()
    }
}

public func Parse(text: string) -> Result<int, ParserError> {
    return .Error(ParserError())
}

public func Execute(text: string) -> Result<int, DomainError> {
    val value = Parse(text)?
    return .Ok(value)
}
```

When propagation uses a user-defined implicit conversion on the error channel,
the compiler reports informational diagnostic `RAV1506` (at `?`, or at `?.` for
direct-return carrier conditional access).

##### Unwrapping Option values

```raven
func GetEven(values: int[]) -> Option<int> {
    val value = FindFirstEven(values)?
    return .Some(value)
}
```

##### Using propagation in larger expressions

Propagation may appear inside larger expressions. The compiler evaluates the operand exactly once and introduces temporaries as needed.

```raven
func Describe(text: string) -> Result<string, Exception> {
    return .Ok("Value: ${ParseInt(text)?}")
}
```

```raven
import System.*
import System.Console.*

func Describe(text: string) -> string {
    val result = try int.Parse(text)

    if result is .Ok(val value) {
        return "Ok: $value"
    }

    if result is .Error(FormatException ex) {
        return "Format invalid: ${ex.Message}"
    }

    return "Other error"
}

WriteLine(Describe("10"))
WriteLine(Describe("abc"))
```

## Carrier conditional access for `Result` and `Option`

Raven supports *carrier-aware* conditional member access using `?.` for `Result<T, E>` and `Option<T>`.

This is distinct from ordinary conditional access:

* For nullable/reference conditional access, `?.`, `?[...]`, and `?(...)` behave as conditional access on the underlying value (when supported).
* For **carrier types** (`Result` / `Option`), **only** the member form `?.Member` is supported.

  * `?[...]` and `?(...)` **do not** apply to `Result` or `Option` and are not lifted.

### `Result<T, E>?.Member`

If the receiver has type `Result<T, E>`, then `receiver?.Member` is evaluated as:

* If `receiver` is `Ok(payload)`: evaluate `payload.Member` and wrap the result back into `Result<…, E>`.
* If `receiver` is `Error(error)`: do **not** evaluate `Member`; the result is `Error(error)`.

In other words, `?.` maps over `Ok` while preserving the original error type `E`.

### `Option<T>?.Member`

If the receiver has type `Option<T>`, then `receiver?.Member` is evaluated as:

* If `receiver` is `Some(payload)`: evaluate `payload.Member` and wrap the result back into `Option<…>`.
* If `receiver` is `None`: do **not** evaluate `Member`; the result is `None`.

In other words, `?.` maps over `Some` while preserving `None`.

### Interaction with the propagation operator `?`

The postfix propagation operator `?` unwraps a carrier value and propagates the “empty/error” case to the nearest enclosing carrier-returning context.

This allows chaining carrier-aware `?.` and then unwrapping at the end:

```raven
func f() -> Result<int, Err> {
    // GetUser() : Result<User, Err>
    // ?.Name   : Result<string, Err>
    // ?.Length : Result<int, Err>
    // trailing ? unwraps Result<int, Err> -> int and propagates Error
    val len = GetUser()?.Name?.Length?

    return .Ok(len + 1)
}

func GetUser() -> Result<User, Err> {
    return .Error(Err.MissingUser)
}

record class User(Name: string)

union Err {
    MissingUser
    MissingName
}

```

Example where the payload itself is an `Option`:

```raven
func GetItem() -> Result<string, Err> {
    // GetUser() : Result<User, Err>
    // ?.Item    : Result<Option<Item>, Err>
    // trailing ? unwraps the Result and propagates Error, leaving Option<Item>
    val maybeItem = GetUser()?.Item?

    return maybeItem match {
        .Some(val item) => .Ok(item.Name)
        .None           => .Error(Err.MissingName)
    }
}

record class Item(Name: string)
```

### Not supported for carriers

For `Result` and `Option`, the following conditional forms are not lifted and are therefore not supported:

* `receiver?[index]`
* `receiver?(args)`

If you need indexing or invocation, unwrap first (with `?`, `UnwrapOrDefault`, or pattern matching), then apply indexing/invocation on the unwrapped value.

## Standard carrier helper APIs (Raven.Core)

Raven ships `Option<T>`, `Result<T, E>`, and related extension helpers in
`Raven.Core`. These are library APIs (not syntax), but they are part of the
standard language experience and are expected by diagnostics, samples, and
tooling.

### `Option<T>` helpers

- State checks: `HasSome`, `HasNone`
- Mapping/composition: `Map`, `Then`, `Where`, `Filter`, `OrElse`
- Result bridge: `ThenResult`, `MapResult`, `IsOkOr(error)`,
  `IsOkOr(errorFactory)`
- Pattern/value helpers: `Match`, `Tap`, `TapNone`
- Unwrap helpers: `UnwrapOrElse`, `UnwrapOrDefault`, `UnwrapOrThrow`,
  `UnwrapOr(defaultValue)`
- Enumeration helpers: `ToEnumerable`, `GetEnumerator`
- Nested carrier helper: `Flatten` for `Option<Option<T>>`
- Nullable conversions:
  - `Option<T : class> <-> T?`
  - `Option<T : struct> <-> T?`

### `Result<T, E>` helpers

- State checks: `HasOk`, `HasError`
- Channel projection: `IsOk`, `IsError`
- Mapping/composition: `Map`, `Then`, `MapError`, `OrElse`
- Pattern/value helpers: `Match`, `Tap`, `TapError`
- Unwrap helpers: `UnwrapOrElse`, `UnwrapOrDefault`, `UnwrapOrThrow`,
  `UnwrapOr(defaultValue)`, `UnwrapError`
- Enumeration helpers: `ToEnumerable`, `GetEnumerator`

### Carrier LINQ extensions on `IEnumerable<T>` (`System.Linq`)

- Option-returning:
  - `FirstOrNone()`, `FirstOrNone(predicate)`
  - `LastOrNone()`, `LastOrNone(predicate)`
  - `SingleOrNone()`, `SingleOrNone(predicate)`
  - `ElementAtOrNone(index)`
- Result-returning with custom errors:
  - `FirstOrError(errorFactory)`, `FirstOrError(predicate, errorFactory)`
  - `LastOrError(errorFactory)`, `LastOrError(predicate, errorFactory)`
  - `SingleOrError(errorFactory)`, `SingleOrError(predicate, errorFactory)`
  - `SingleOrError(errorIfNone, errorIfMany)`
  - `SingleOrError(predicate, errorIfNone, errorIfMany)`
  - `ElementAtOrError(index, errorFactory)`
- Result-returning with captured exceptions:
  - `ToArrayOrException() -> Result<T[], Exception>`
  - `ToListOrException() -> Result<List<T>, Exception>`
  - `ToHashSetOrException() -> Result<HashSet<T>, Exception>`
  - `ToDictionaryOrException(keySelector) -> Result<Dictionary<TKey, T>, Exception>`
  - `ToDictionaryOrException(keySelector, elementSelector) -> Result<Dictionary<TKey, TValue>, Exception>`
- Result-returning with mapped errors:
  - `ToArrayOrError(errorFactory: Exception -> E) -> Result<T[], E>`
  - `ToListOrError(errorFactory: Exception -> E) -> Result<List<T>, E>`
  - `ToHashSetOrError(errorFactory: Exception -> E) -> Result<HashSet<T>, E>`
  - `ToDictionaryOrError(keySelector, errorFactory) -> Result<Dictionary<TKey, T>, E>`
  - `ToDictionaryOrError(keySelector, elementSelector, errorFactory) -> Result<Dictionary<TKey, TValue>, E>`

### Cast expressions

Explicit casts request a conversion to a specific type and use C# syntax.

```raven
val d = (double)1
val i = (int)3.14  // numeric narrowing
val s = obj as string
```

`(T)expr` performs a runtime check and throws an `InvalidCastException` when the value cannot convert to `T`. Use this form for downcasts, numeric narrowing, or unboxing scenarios.
`expr as T` attempts the conversion and returns `null` (or a nullable value type) instead of throwing on failure.

### `typeof` expressions

The `typeof` operator produces the runtime [`System.Type`](https://learn.microsoft.com/dotnet/api/system.type)
for a compile-time type. The operand must be a type syntax—predefined, user-defined,
tuple, nullable, or union—and is not evaluated. The expression always has type
`System.Type`. Using a namespace or otherwise invalid type yields a binding
diagnostic.

```raven
val textType = typeof(string)
val listType = typeof(System.Collections.Generic.List<int>)
```

`typeof` is useful when reflecting over metadata or when passing type objects to
APIs such as `Activator.CreateInstance`.

### `sizeof` expressions

The `sizeof` operator produces the size, in bytes, of a compile-time type. The
operand must be a type syntax and is not evaluated. The expression always has
type `int`.

```raven
val intSize = sizeof(int)       // 4
val charSize = sizeof(char)     // 2
```

### `nameof` expressions

The `nameof` operator produces the **unqualified source name** of a symbol as a `string` at compile time. The operand is **not evaluated**; instead, the compiler validates the operand syntactically and semantically and substitutes the referenced symbol’s name.

The operand may refer to:

* a local variable or parameter
* a field or property
* a method, function, or event
* a type or type member
* a qualified or member-accessed symbol

The result of a `nameof` expression is always of type `string` and is a compile-time constant.

```raven
val x = 42
val name = nameof(x)          // "x"

val text = nameof(System.Console.WriteLine)
```

When applied to a member access, only the **final identifier** is returned:

```raven
nameof(System.Collections.Generic.List<int>)   // "List"
nameof(Console.WriteLine)                      // "WriteLine"
```

Using `nameof` with an invalid operand (such as a literal, invocation, or arbitrary expression) produces a binding diagnostic. Because `nameof` is evaluated at compile time, it cannot observe runtime state and has no side effects.

`nameof` is commonly used for diagnostics, argument validation, logging, and reflection-friendly APIs where symbol names must remain stable under refactoring.

### Default expressions

`default` produces the zero-initialized value for a type. Use the explicit form
`default(T)` to request the default value for a known type `T`. The target-typed
literal `default` instead relies on the surrounding context—such as a variable
annotation, a return type, or an argument position—to supply the type. When no
target type is available, the compiler reports `RAV2011` because the literal
cannot be inferred.

```raven
val zero = default(int)
val emptyText: string = default
```

### String literals

```raven
val hello = "Hello, "
Console.WriteLine(hello + "World!")
Console.WriteLine("Hello, " + 2)
```

String literals recognize the standard escape sequences shown below as well as
Unicode escapes. Use `\uXXXX` or `\UXXXXXXXX` for fixed-width hexadecimal
escapes, or `\u{...}` for variable-length scalars up to `0x10_FFFF`. Each escape
expands to the corresponding UTF-16 sequence, so `"\u{1F600}"` produces the 😀
emoji.

| Escape | Meaning |
| --- | --- |
| `\\0` | Null character |
| `\\a` | Alert (BEL) |
| `\\b` | Backspace |
| `\\t` | Horizontal tab |
| `\\n` | Line feed |
| `\\v` | Vertical tab |
| `\\f` | Form feed |
| `\\r` | Carriage return |
| `\\"` | Double quote |
| `\\'` | Single quote |
| `\\\\` | Backslash |
| `\\$` | Literal dollar sign in interpolated strings |

#### Multiline string literals

A multiline string literal is written using triple double quotes `"""` and spans zero or more lines.

```raven
val text = """
    Line one
    Line two
"""
```

Multiline literals are **raw** with respect to escape processing: their contents are taken exactly as written between the delimiters and are **not subject to escape‑sequence decoding**. Unlike earlier revisions, multiline literals **may contain interpolation** using the same forms as ordinary strings (`$identifier` and `${ Expression }`). The parser preserves the raw text and source spans, while the binder interprets interpolation segments and produces the final value.

Indentation trimming rules are applied to both plain and interpolated multiline strings. For interpolated forms the trimming is performed during binding so that interpolation segments remain span‑correct in the syntax tree while the resulting value matches the behavior of non‑interpolated multiline literals.

The lexer produces a token of kind **`MultilineStringLiteralToken`** whose value text contains the literal characters between the delimiters after applying indentation trimming rules. No escape processing is performed, so sequences such as `\n` and `\t` represent backslash characters followed by letters rather than control characters.

The delimiters must appear on their own lines or immediately adjacent to content; the closing `"""` terminates the literal at the first matching sequence. If the end of file is reached before a closing delimiter, the lexer reports an unterminated-string diagnostic and produces a `MultilineStringLiteralToken` containing the available content.

Trivia and comments may appear adjacent to the delimiters but are not part of the literal value.

### String interpolation

Embed expressions directly into strings using `${...}` without requiring a prefix.
For simple identifiers, a shorthand `$identifier` form avoids the braces.

```raven
val name = "Alice"
val age = 30
val greeting = "Hello $name!"
val msg = "Name: ${name}, Age: ${age}"
Console.WriteLine(msg)
```

Escapes inside the literal portions of an interpolated string follow the same
rules as ordinary string literals, ensuring Unicode escapes work uniformly in
both forms. Use `\$` to emit a literal dollar sign when the following characters
would otherwise start an interpolation. For instance, `"Price: \$${amount}"`
produces `Price: $` followed by the `amount` value instead of treating the
escaped dollar as the start of its own interpolation.

Interpolated strings preserve Unicode content from both left-to-right and
right-to-left scripts. Literal segments keep their original characters so that
text such as Japanese kana or Arabic phrases stay intact around the embedded
expressions.

```raven
val name = "ليلى"
val city = "دبي"
val welcome = "\u200Fمرحبا ${name}! أهلا بك في ${city}"
```

The `\u200F` right-to-left mark keeps the greeting flowing correctly even when
mixing scripts in the same interpolated string.

### Collection expressions

Collection expressions use bracket syntax `[element0, element1, ...]` (with an optional
trailing comma) to build arrays and other collection types. Elements are evaluated
from left to right. In addition to ordinary expressions, an element may be written as
`..expression`—called a *spread*. Spreads enumerate the runtime value and insert each
item into the resulting collection in order. The spread source must be convertible to
`System.Collections.IEnumerable` (including arrays and `IEnumerable<T>` implementations);
otherwise diagnostic `RAV2022` is reported. 【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L3620-L3670】【F:src/Raven.CodeAnalysis/DiagnosticDescriptors.xml†L260-L266】

Collection expressions are target-typed:

* **Array targets** — When the expected type is an array `T[]`, the expression allocates a
  new array of that element type. Each item is implicitly converted to `T` before storage,
  and spreads must enumerate values assignable to `T`. 【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L3672-L3738】【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L950-L1016】
* **Collection targets** — When the expected type is a non-array type with an accessible
  parameterless constructor and an instance `Add` method, the compiler constructs the
  target and calls `Add` for every element. The `Add` parameter determines the element
  conversions, and spread entries must supply compatible values. 【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L3738-L3776】【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L1016-L1096】
* **No target type** — Without an expected type, Raven infers a best common element type
  by merging the contributions of each element (spreads use their enumerated element type).
  The expression then produces an array of that inferred element type, defaulting to
  `object` when no more precise choice is available. 【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L3776-L3861】

An empty collection expression `[]` must be used in a context that supplies a target type;
otherwise its type cannot be inferred. When a target type is available, the compiler
produces an empty instance of that type (an empty array or an initialized collection).
【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L3620-L3651】【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L1170-L1192】

```raven
val numbers: int[] = [1, 2, 3]
val combined = [0, ..numbers, 4]

val names: List<string> = ["a", "b"]
val inferred = [1, 2.0]  // inferred as object[]
```

#### Element access

```raven
val list = [1, 42, 3]
val a = list[1]
```

### Function invocation

```raven
Foo(1, 2)
Console.WriteLine("Test")
```

The `()` call operator invokes a function-valued expression. If the target
expression's type defines an invocation operator via a `self` method, that
member is invoked instead; see [Invocation operator](#invocation-operator).
Invocation operators can be declared on classes or interfaces. Class
declarations may mark them `virtual` or `abstract` to support overrides.

When the target has optional parameters, omitted trailing arguments are filled
in using the defaults declared on the parameter list. The supplied arguments are
matched positionally before defaults are considered.

Arguments may also be written with an explicit name using the `name: expression`
syntax. Named arguments may appear in any order, and each one must match a
parameter declared by the target. After a named argument is used, any remaining
positional arguments must correspond to parameters that have not already been
specified by name and that occur after the right-most named argument. Duplicate
or unknown names cause overload resolution to reject the candidate. Named
arguments are supported in function invocations, object creation, and
constructor initializers. Attribute argument lists also support named
arguments, but they use the same `name: expression` form only; the legacy
`name = expression` C# style is not valid Raven syntax.

```raven
func makePoint(x: int, y: int, label: string = "origin") -> string
{
    return $"{label}: ({x}, {y})";
}

val swapped = makePoint(y: 2, x: 1);
val mixed = makePoint(3, label: "axis", y: 0);
val invalid = makePoint(x: 1, 2);  // error: positional argument cannot follow `x:`
```

The compiler binds each named argument to its declared parameter. The call to
`makePoint` named `mixed` demonstrates that positional arguments may precede the
first named argument, while the `invalid` call is rejected because it attempts
to supply a positional argument (`2`) after specifying `x` by name.

#### Object initializer trailers

Any invocation expression may be followed by an **object initializer** block written with braces. The initializer is a postfix *trailer* and is evaluated immediately after the invocation target is constructed or returned.

```raven
val window = Window() {
    Title = "Main"
    Width = 800
    Height = 600
}
```

Init-only accessors are treated as initializer-only members, so they may be assigned in object initializers:

```raven
class Settings
{
    public Theme: string { get; init; }
    public FontSize: int { get; init; }
}

val settings = Settings {
    Theme = "Dark"
    FontSize = 14
}
```

Initializer bodies consist of a sequence of **entries**. Entries may be mixed freely and appear in source order:

* **Property entries** assign to a writable property or field using `Name = Expression`.
* **Content entries** are standalone expressions, typically nested object constructions such as `Button { ... }`.

```raven
val window = Window {
    Title = "Main"

    Button { Text = "OK" }

    Width = 800
    Height = 600

    Button { Text = "Cancel" }
}
```

Property entries are applied to the newly created instance in source order.

Content entries use one of the following binding rules:

* **Content property convention** — If the initialized type has an accessible, settable instance property named `Content`, Raven treats the first content entry as an assignment to that property (as if it were written `Content = <expr>`). When this convention applies, at most one content entry is permitted.
* **Add method convention** — Otherwise, each content entry is forwarded to the initialized instance by binding it as an `Add(<expr>)` call during lowering. The `Add` method must be an accessible instance method that takes exactly one parameter whose type is compatible with the content entry expression.

If more than one content entry is provided for a type that uses the Content property convention, the compiler reports `RAV1505`.


> **Note:** The grammar permits an initializer trailer after any invocation, but in statement headers such as `if`, `while`, and `for`, the `{` token begins the statement body and is not parsed as an object initializer trailer. This is a context-sensitive parsing rule.

> **Note:** The Content property convention is intended to support DSL-style UI composition (for example SwiftUI/Flutter-like syntax). Container types that accept multiple children should expose a collection-like API (for example `Add(TChild)` or a `Children` collection) instead of `Content`.


#### Required members and init semantics

Members marked as **required** must be definitely assigned during object construction. A required member may be a field or property. Required members participate in object initializer checking and influence which constructors are considered complete.

```raven
class Person
{
    public required Name: string { get; init; }
    public required Age: int { get; init; }
}

val p = Person { Name = "Ada", Age = 36 }   // ok
val q = Person { Name = "Ada" }            // error: Age must be set
```

##### Declaration rules

* A required **field** must be **mutable**. Declaring `required` on an immutable (`let`) field is an error.
* A required **property** must have an accessible **init** or **set** accessor so that it can be assigned during initialization.
* `required` is not permitted on `const`, `static`, or read‑only members.

##### Constructors and `SetsRequiredMembers`

A constructor may satisfy required members directly. Such constructors are annotated with the attribute `System.Diagnostics.CodeAnalysis.SetsRequiredMembersAttribute`. When a constructor carries this attribute, the compiler considers all required members to be assigned by that constructor and does not require an object initializer.

```raven
record Person(Name: string, Age: int)
// primary record constructor is treated as [SetsRequiredMembers]

val p = Person("Ada", 36)   // ok
```

`record Name(...)` is shorthand for `record class Name(...)`. Use
`record struct Name(...)` for a value-type record.

For constructors **without** this attribute, all required members must be provided by an object initializer at each creation site.

##### Object initializer checking

When binding an object creation expression:

1. Collect the set of required members declared on the type and its base types.
2. If the selected constructor has `SetsRequiredMembers`, no further checks are performed.
3. Otherwise, an object initializer must assign **all** required members.
4. Omitting any required member produces a compile‑time error.

Assignments in the initializer may target fields or properties with `init`/`set` accessors. Nested initializers and `with` expressions are treated as initializer contexts.

##### Inheritance

Required members declared on base types are inherited by derived types and must also be satisfied during construction of the derived type, unless a base constructor is marked with `SetsRequiredMembers`.

##### Interaction with `with` expressions

`with` expressions operate in initializer context. Required members may be assigned within a `with` initializer just as in object initializers:

```raven
val p2 = p with { Age = 37 }
```

#### With expressions

A **with expression** creates a copy of a value and applies a list of member assignments without mutating the original instance. The syntax is:

```raven
val updated = point with {
    X = 10
    Y = 20
}
```

The receiver expression is evaluated exactly once. Each assignment expression is evaluated left-to-right in source order. If a member is listed more than once, the compiler reports `RAV0241`.

Assignments in a with initializer must target writable instance fields or properties. `init` accessors are permitted because with initializers are treated as initializer contexts (matching object initializer semantics).

With expressions are the preferred way to update record values, producing a new record instance while preserving the original:

```raven
record Point(X: int, Y: int)

val origin = Point { X = 0, Y = 0 }
val moved = origin with { X = 10 }
```

When binding a with expression, the compiler selects the first applicable strategy in the following order:

1. **Record clone** — Record types use their copy constructor (synthesized if needed) to create a clone, then apply assignments as initializer-style member assignments.
2. **`Update(...)` convention** — An instance method named `Update` whose parameter names correspond to readable members on the receiver. Each parameter receives either the provided assignment value (if present) or the receiver's current member value.
3. **`With(...)` convention** — Same as `Update`, but with an instance method named `With`.
4. **`WithX(...)` chaining** — For each assignment `X = expr`, invoke a single-parameter method named `WithX`. Methods are invoked in source order, and each invocation receives the assignment's value expression.
5. **Clone/copy fallback** — Use a parameterless `Clone()` method or a copy constructor (one parameter of the receiver type) to create a copy, then apply assignments as initializer-style member assignments.

If none of these conventions apply, the compiler reports `RAV0240` to indicate the type does not support `with` expressions.

### Extensions (Traits)

Extensions provide helper members for an existing receiver type without
modifying the original declaration. The `extension` keyword declares a
namespace-scoped container that targets a specific type via a `for` clause.
Importing the container (for example, `import MyApp.StringExt.*`) brings its
members into scope for lookup.


Extensions may be declared using either the `extension` keyword or the `trait` keyword. Both forms declare the same construct and are semantically equivalent in the current version of Raven; the choice of keyword is intended to communicate author intent. The `trait` keyword emphasizes behavioral composition, while `extension` emphasizes convenience and interoperability.

An extension/trait declaration may omit its identifier, in which case the compiler synthesizes a private, mangled name for the generated container type. **Public** extensions/traits (those intended to be consumed across assemblies) must declare an explicit identifier so the container can be referenced and imported by name. Unnamed extensions are intended for internal, assembly-local augmentation.


Extensions and traits may declare type parameters and generic constraints (`where` clauses). These constraints participate in extension resolution: an extension is considered applicable only when its type parameters can be inferred and all declared constraints are satisfied for the receiver type. Extensions whose constraints are not met are ignored during member lookup.

Examples:

```raven
// Named extension: can be imported and referenced by name.
public extension StringExt for string
{
    ToSlug() -> string => self.Trim().ToLowerInvariant().Replace(" ", "-")
}

import MyApp.StringExt.*
val slug = " Hello World ".ToSlug()
```

```raven
// Unnamed extension: identifier omitted; the compiler synthesizes a mangled container name.
// Intended for internal / assembly-local augmentation (and cannot be imported by name).
extension for string
{
    IsNullOrWhiteSpace() -> bool => self.Trim().Length == 0
}

// In the same compilation unit where it is visible:
val empty = "   ".IsNullOrWhiteSpace()
```

```raven
// Constrained extension: applicable only when the receiver satisfies the constraints.
public trait ValueSequenceExt<T> for System.Collections.Generic.IEnumerable<T>
    where T: struct
{
    Sum() -> T { /* ... */ }
}

// If the receiver's element type is not a struct, this extension is ignored during lookup.
```

```raven
// Extension property: participates in member lookup like an instance property.
extension ListIntExt for System.Collections.Generic.List<int>
{
    CountPlusOne: int
    {
        get => self.Count + 1
        set => self.Add(value)
    }
}

import System.Collections.Generic.*
import MyApp.ListIntExt.*

val items = List<int>()
items.Add(1)

val c = items.CountPlusOne      // invokes getter
items.CountPlusOne = 5          // invokes setter
```

```raven
// Static extension members: accessed through the receiver type when no real static member matches.
public extension ListStatics for System.Collections.Generic.List<int>
{
    public static Empty() -> System.Collections.Generic.List<int> => System.Collections.Generic.List<int>()
    public static DefaultCapacity: int { get => 4 }
}

import System.Collections.Generic.*
import MyApp.ListStatics.*

val empty = List<int>.Empty()
val cap = List<int>.DefaultCapacity
```

> **Note:** Extensions are trait-like: they group behavior that is treated as part of the target type for member lookup and overload resolution, without introducing storage or inheritance. In the current version of Raven, extensions and traits are applied implicitly based on scope. The design is still evolving, and future versions may introduce explicitly applied traits.

Each member inside the body is implicitly an extension member for the receiver
type. Members may be function declarations or computed properties. The compiler
synthesizes a `self` parameter whose type matches the receiver and passes it as
the first argument whenever the member is invoked. The `self` parameter behaves
like a `val` binding: it cannot be reassigned but may be used to access members
or forwarded to other calls. Extension members default to `public`
accessibility and may be marked `internal` to restrict their visibility; other
modifiers are rejected. As a result, extensions cannot declare `protected` or
`private` members.

#### Extension methods


Extension methods add callable helpers to the receiver type. They are declared
inside an `extension` container as function members:

```raven
extension StringExt for string
{
    ToSlug() -> string
    {
        // inside the body, `self` is a synthesized parameter of type string
        return self.Trim().ToLowerInvariant().Replace(" ", "-")
    }
}
```

The receiver parameter determines which expressions may invoke the extension.
Additional parameters follow the ordinary parameter rules: they may be generic,
optional, `params`, or accept lambdas. Generic receiver parameters are
substituted during method type inference, so helpers like `Where<T>(this
IEnumerable<T>, Func<T, bool>)` become available to Raven code as soon as the
appropriate namespace is imported.

##### Partial explicit type arguments

When calling a generic method (including extension methods), Raven permits supplying **only the trailing type arguments** that cannot be inferred.

* The receiver's type is inferred from the call-site receiver expression.
* Any remaining type parameters are inferred from the provided arguments.
* Explicit type arguments provided at the call site are **right-aligned** with the method's type parameter list.

This mirrors the common C# ergonomics for extension calls where the receiver type is already known.

```raven
import System.Console.*
import System.Collections.Generic.*

val items = List<int>()
items.Add(1)
items.Add(2)

public extension MyEnumerableExt<T> for System.Collections.Generic.IEnumerable<T> {
    public CountItems<B>(arg: T) -> B {
        return default(B)
    }
}

// T is inferred from the receiver (`IEnumerable<int>`), while B is specified explicitly.
val count2 = items.CountItems<double>(2)
WriteLine(count2)

// The fully specified form is still valid.
val count3 = items.CountItems<int, double>(2)
WriteLine(count3)
```

If the supplied explicit type arguments do not match the method's trailing type parameters (or if inference cannot determine the remaining parameters), overload resolution fails and a diagnostic is reported.

Extension methods participate in lookup through the same `import` mechanism used
for types. Importing a namespace brings every extension method declared on the
static types within that namespace into scope. Importing a specific static type
exposes only the extensions declared on that type. Metadata extensions contained
in referenced assemblies, such as `System.Linq.Enumerable`, and Raven-authored
extensions are surfaced uniformly by binding, so source and metadata callers see
the same candidates.【F:src/Raven.CodeAnalysis/Binder/NamespaceBinder.cs†L33-L61】

Invoking an extension uses method-style member access. When `expr.Member(...)`
fails to resolve to an instance member, Raven gathers the in-scope extension
methods whose receiver parameter is compatible with `expr`. Eligible extensions
join overload resolution alongside instance members; if both an instance member
and an extension are applicable, the instance member wins. Once overload
resolution selects an extension, the compiler rewrites the invocation to pass the
receiver as the leading argument to the static method before lowering, so the
generated IL matches the direct static-call form.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L1946-L2001】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/Lowerer.Invocation.cs†L8-L29】

Because extension methods are ordinary `static` methods, accessibility rules and
diagnostics mirror those for other members. Missing imports or inaccessible
extensions produce the same `RAV1xxx` diagnostics emitted for ordinary method
lookups. Lambdas supplied to extension method parameters participate in the same
delegate inference as other calls; Raven replays the lambda for each candidate
signature until one succeeds.

#### Extension properties

Extension properties participate in member lookup alongside methods. When a
property access such as `expr.Member` fails to bind to an instance property,
Raven considers imported extension properties whose synthesized `self`
parameter can accept the receiver. Successful binding rewrites the access to
call the accessor extension method (for example, `get_Member(self)`), and
assignments translate to setter invocations that pass both `self` and the
assigned value. Overload resolution prefers instance properties over
extensions, mirroring the method rules. Extension properties are accessor-only:
they cannot declare backing storage, and both accessors must be implemented
with bodies or expression clauses.

```raven
extension ListExt for List<int>
{
    CountPlusOne: int
    {
        get => self.Count + 1
        set => self.Add(value)
    }
}
```

#### Static extension members

Extensions may also declare `static` methods and properties. Static extension
members are associated with the receiver type and are accessed through static
member lookup (`Type.Member`) or by importing the target type (`import
Type.*`). When binding a static member access, the compiler first resolves real
static members on the type; if no match is found, it searches in-scope extension
containers whose receiver type is compatible with the target type. Receiver
compatibility follows the same implicit conversion and nullability rules used
for extension methods, so constructed and nested types participate normally.
Static extension members do not synthesize `self` and are emitted as ordinary
static members on the extension container.

#### Pipe operator

Raven also supports a pipeline form that feeds the left-hand value into a call
on the right. The operator has the lowest precedence among binary operators and
associates left-to-right, so a chain such as `source |> First() |> Second()`
evaluates `source`, passes it to `First`, then pipes the result into `Second`.

```raven
val result = 5 |> Square() |> AddOne()

val result = AddOne(Square(5))
```

When the pipeline targets an invocation, the syntax mirrors a regular call:

```raven
val result = 5 |> MathHelpers.Increment(2)

public static class MathHelpers {
    public static Increment(x: int, amount: int) -> int {
        return x + amount
    }
}
```

Pipe chains also allow an implicit invocation form for method targets with no
explicit argument list. In that form, `value |> Method` is treated as
`value |> Method()`:

```raven
func Inc(x: int, n: int = 1) -> int {
    return x + n
}

val a = 5 |> Inc
val b = 5 |> Inc()
val c = 5 |> Inc(2)
```

All three bindings above call `Inc`; `a` and `b` use the default value for `n`,
while `c` supplies `n` explicitly.

The pipe operator accepts an invocation target (explicit or implicit) or a
property access with a setter on the right-hand side. If the syntax does not
form either shape, diagnostic `RAV2800` is issued.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L2690-L2766】【F:src/Raven.CodeAnalysis/DiagnosticDescriptors.xml†L19-L23】

If the pipeline targets a property, Raven assigns the left expression to that property through its setter before producing the property's type as the result of the pipe expression. Both instance and static properties are supported:

```raven
val container = Container()
val _ = 42 |> container.Value
val _ = 42 |> Container.Count

public class Container {
    public Value: int { get; set; }
    public static Count: int { get; set; }
}
```

When the invocation resolves to an extension method, the left expression becomes
the extension receiver, mirroring `value.Extension()` syntax. Otherwise the
compiler prepends the piped value as the first argument before overload
resolution runs, so ordinary static helpers that expect a leading value parameter
remain callable through pipelines.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L2698-L2768】

Pipeline targets participate in normal name lookup, so the operator can call
members brought into scope by `import` directives (including static imports) as
well as top-level `func` declarations. Because overload resolution still sees
the piped value as the first argument, generic methods can infer type arguments
from that value without any additional annotations.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L2724-L2768】【F:test/Raven.CodeAnalysis.Tests/Semantics/ExtensionMethodSemanticTests.cs†L1396-L1507】

### Index expressions

Prefixing an expression with `^` produces a `System.Index` value that counts
from the end of a sequence. The operand must be implicitly convertible to
`int`, and the result keeps its `Index` type even when not target-typed, so
`val offset = ^2` is valid without annotations. When indexing arrays, from-end
indices are computed using the array's length and are evaluated exactly once
alongside the receiver.

`^` is parsed as a tight prefix form: `^expr`. Trivia between `^` and the
operand is rejected, so `^1` is valid while `^ 1` is not.

### Range expressions

`..` produces a `Range` value that can be stored, passed to APIs, or used for
slicing receivers that understand .NET ranges. Both endpoints are optional, and
either endpoint may be written as a **from-end** index by prefixing it with `^`.

```raven
val r = 3..^5
val head = ..3
val tail = 3..
val all  = ..
```

Forming a range evaluates each supplied boundary exactly once, left-to-right.
Each boundary expression must be implicitly convertible to `int`; the `^` prefix
indicates the operand counts from the end of the receiver instead of from the
start. The resulting `Range` uses `Index.FromStart` for ordinary boundaries and
`Index.FromEnd` for prefixed ones, and omitting a boundary produces
`Range.StartAt`, `Range.EndAt`, or `Range.All` accordingly. A range expression
retains its `Range` type even when no target type is provided, enabling
declarations like `val r = 3..^5` without additional annotations.

Element access applies these types directly:
- One-dimensional arrays accept a single `Range` argument (`array[range]`) and
  produce a sliced array.
- Non-array receivers resolve `[]` using argument-type matching, so indexers
  that differ by `Index` vs `Range` can coexist.

### Bitwise operators

Raven supports unary bitwise-not `~` plus binary `&`, `|`, `^`, `<<`, and `>>`.

- `~` is defined for `int` and `long`.
- `&`, `|`, and `^` are defined for `int`, `long`, `bool`, and matching enum operands.
- `<<` and `>>` are defined for `int` and `long` left operands with an `int` shift count.

When both operands are `bool`, `&`, `|`, and `^` evaluate **without** short-circuiting and return `bool`, allowing direct use in non-conditional contexts or within compound assignments. Operands must share the same enum type when applied to enums; the result has that enum type.

Compound assignments `&=`, `|=`, and `^=` are available and apply the corresponding binary operator after evaluating the left-hand side once. These operators share left-to-right associativity with other Raven binary operators, and their precedence sits between the logical (`||`, `&&`) and equality operators.

Enum member accesses support **leading-dot** syntax when a target type is already known, including inside bitwise combinations and argument lists:

```raven
val flags: BindingFlags = .NonPublic | .Static

func WithBinding(flags: BindingFlags) { /* ... */ }

WithBinding(.Public | .Instance)
```

### Object creation

Objects are created by **calling the type name** directly, just like any
other method.

```raven
val sb = StringBuilder()
sb.AppendLine("Foo")
```

Generic types work the same way:

```raven
val list = List<int>()
list.Add(2)
```

Raven also supports the `new` keyword for **backwards compatibility** and
for cases where you want to be explicit about creating an object:

```raven
val sb = new StringBuilder()
val list = new List<int>()
```

This way it’s clear that *constructor-as-call* is the default, and `new` is optional/explicit.  

When an object initializer trailer is present, the parameter list may be omitted for parameterless construction:

```raven
val window = Window {
    Title = "Main"
}
```

This form is equivalent to calling `Window()` and then applying the initializer entries in order.

### Tuple expressions and access

Tuples can be **named** or **positional**. Both projections are available.

```raven
val tuple = (a: 42, b: 2)
Console.WriteLine(tuple.a)      // named
Console.WriteLine(tuple.Item1)  // positional
```

### Block expression

A block is an expression; its value is the value of its last expression
(or `()` if none). Each block introduces a new scope for local declarations.

```raven
{
    val x = 10
    x + 1
}
```

When the same brace form appears in statement position (for example, as the body
of an `if` statement or loop), it is bound as a block statement and its expression
results are discarded.

### `if` expression

`if` expressions evaluate the condition and execute exactly one branch. The
value of the overall expression is the value produced by the executed branch. If
both branches produce values, the result participates in type inference as
described in [Type inference](#type-inference).

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
`else` branch so that both outcomes yield a value. (See [Outstanding
questions](#outstanding-questions-and-suggested-follow-ups) for current gaps and
suggested fixes.)

### `while` expression

`while` repeatedly executes its body while the condition evaluates to `true`.
Because loops are expressions, the overall value of a `while` expression is `()`.

```raven
var i = 0
while i < list.Length {
    val item = list[i]
    Console.WriteLine(item)
    i = i + 1
}
```

### `for` expression

Iterates over each element of a collection, binding it to a fresh local. The optional
`each` keyword improves readability.

```raven
for each item in items {
    Console.WriteLine(item)
}
```

The `each` keyword may be omitted:

```raven
for item in items {
    doSomething(item)
}
```

`for` evaluates the collection once, then executes the body for every element.
When iterating over arrays, the element type comes from the array's element
type. Other collections are currently treated as `System.Collections.IEnumerable`
and default the iteration variable to `object`. If the element value is unused, the
iteration variable may be written as `_` or omitted entirely:

```raven
for each _ in items {
    log("processing")
}

for each in items {
    log("processing")
}
```

Both forms still enumerate the collection but do not introduce a new binding.
Like other looping constructs, a `for` expression evaluates to `()`.

When the collection is a range with explicit, from-start bounds, the loop
iterates over `int` values beginning at the range's lower bound and stopping
before the upper bound. Omitting the start defaults it to `0`, while omitting
the end or using from-end bounds in a `for` expression reports a diagnostic.

### `break` and `continue`

`break` immediately exits the innermost loop (`while`, `for`, or other
expression-oriented loop construct). `continue` skips the remainder of the
current iteration and evaluates the loop condition for the next pass. Both
keywords are only valid inside loops; using them elsewhere produces diagnostics
(`RAV2600` for `break`, `RAV2601` for `continue`). They are statements, not
expressions, so placing them in an expression context—such as inside an
expression-bodied lambda—also triggers an error. 【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Statements.cs†L614-L648】【F:src/Raven.CodeAnalysis/DiagnosticDescriptors.xml†L346-L351】

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

### Unreachable statements

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

### Labeled statements and `goto`

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

### `throw` statement

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

    return int.Parse(text)
}
```

### Iterator statements (`yield return`, `yield break`)

Iterator methods produce lazily-evaluated sequences by using `yield` statements.
`yield return expression` publishes the next element of the sequence; the
expression is converted to the iterator's element type before emission.
`yield break` terminates the sequence early. Both forms may only appear in
methods whose return type implements `System.Collections.Generic.IEnumerable<T>`,
`System.Collections.Generic.IEnumerator<T>`, or their non-generic counterparts.
When such a method contains `yield`, the compiler rewrites it into a state
machine that implements the appropriate enumerator pattern. 【F:src/Raven.CodeAnalysis/Binder/BlockBinder.Statements.cs†L489-L527】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/IteratorLowerer.cs†L25-L58】【F:src/Raven.CodeAnalysis/BoundTree/Lowering/IteratorLowerer.cs†L302-L340】

```raven
import System.Collections.Generic.*

class Counter {
    Numbers(max: int) -> IEnumerable<int> {
        var current = 0
        while current < max {
            yield return current
            current = current + 1
        }

        yield break
    }
}
```

The generated state machine preserves captured locals and surfaces the same
metadata shape (`Current`, `MoveNext`, `Dispose`, and `GetEnumerator`) as .NET
iterators. Each `yield return` resumes exactly where it left off on the next
`MoveNext` call, allowing Raven iterators to interoperate seamlessly with .NET's
enumeration APIs. 【F:src/Raven.CodeAnalysis/BoundTree/Lowering/IteratorLowerer.cs†L46-L128】

## Pattern matching

Patterns let you inspect values using concise, algebraic syntax. They appear in `is`
predicates and in `match` expressions and participate in Raven’s flow-sensitive
type analysis.

### `match` forms

Raven supports both expression and statement-position `match` syntax:

* **Expression form**: `scrutinee match { ... }`
* **Statement form**: `match scrutinee { ... }`

The parser represents these as distinct syntax nodes: expression form produces
`MatchExpressionSyntax`, while statement form produces `MatchStatementSyntax`.

Both forms use the same pattern binder and diagnostics, including exhaustiveness
checking and unreachable-arm detection.

In expression form, the `match` result is the selected arm expression value. In
statement form, the selected arm expression is evaluated and its resulting value
is discarded.
Statement-form `match` participates only in statement control flow and never
contributes an implicit return value to the enclosing function.

In statement form, arm block expressions are interpreted in statement context.
That means explicit `return`/`throw` statements inside those arm blocks are
valid. `return` exits the enclosing function/method and `throw` raises an
exception. In expression form, statement `return`/`throw` remains disallowed in
arm expressions and reports `RAV1900`/`RAV1907`.

Arm bodies accept any expression, including block expressions:

```raven
val label = value match {
    0 => { "zero" }
    _ => {
        val text = "other"
        text
    }
}

match value {
    0 => { Console.WriteLine("zero") }
    _ => {
        Console.WriteLine("other")
        ()
    }
}
```

Statement-form `match` with block arms may use explicit `return`:

```raven
class Evaluator {
    Eval(scrutinee: bool) -> bool {
        match scrutinee {
            true => {
                return true
            }
            false => {
                return false
            }
        }
    }
}
```

### Pattern forms

Patterns compose from the following primitives.

#### Type and binding patterns

* `Type` — **type pattern**. Succeeds when the scrutinee can be treated as `Type`.
  If the pattern introduces no designation, it behaves like a type test.

* `Type name` — **typed binding**. Succeeds when the scrutinee can be treated as
  `Type`, then binds the converted value to `name` as an immutable local in the
  success scope.

* `val name` / `var name` / `let name` — **variable pattern**. Always matches and
  introduces a binding. `val`/`let` produce an immutable local; `var` produces a
  mutable one.

  Parenthesized designations such as `val (first, second): (int, string)` bind
  each element positionally.

* **Explicit binding keyword required.** In pattern position, introducing a new
  binding always requires an explicit binding keyword (`val`, or `var`).
  A bare identifier never introduces a binding; it is interpreted as a *value
  pattern* (constant) or, if applicable, as a type name.

  For example:

  * `.Ok(42)` matches the literal value `42`.
  * `.Ok(discountedProduct)` matches the runtime value of the in-scope symbol
    `discountedProduct`.
  * `.Ok(val n)` binds the payload to a new immutable local `n`.

  The same rule applies uniformly in positional patterns and discriminated-union case
  payloads.

#### Discards

* `_` / `Type _` — **discard**. Matches without introducing a binding. The typed
  form asserts the value can be treated as `Type` while still discarding it.
  Because `_` is reserved for discards, writing `_` never creates a binding.

  Discards participate in exhaustiveness: an unguarded `_` arm is a catch-all and
  satisfies any remaining cases (even if earlier arms introduced bindings).

#### Constant patterns

* `literal` — **constant pattern**. Matches when the scrutinee equals the literal
  value (`true`, `"on"`, `42`, or `null`). Literal patterns piggyback on Raven’s
  literal types and participate precisely in union narrowing.

* `identifier` — **value pattern**. When a bare identifier appears in pattern
  position and resolves to an in-scope value (local, parameter, field, or property),
  the pattern matches when the scrutinee equals the runtime value of that identifier.

  Value patterns are *not* bindings. To introduce a new binding, an explicit
  binding keyword (`val`, or `var`) is required.

* `.Member` — **target-typed value pattern**. When the scrutinee type is known
  (for example, an enum type or a type with static fields), the leading-dot
  expression resolves against that target type and matches the resulting value.

> **Note:** A bare identifier in pattern position is context-sensitive. If the
> name resolves to a value symbol, it forms a value pattern. Otherwise, it is
> interpreted as a type name and participates in a type or declaration pattern.
> This disambiguation is performed by the binder, not the grammar.

#### Relational patterns

* `< expr`, `<= expr`, `> expr`, `>= expr`, `== expr`, `!= expr` — **relational
  pattern**. Matches when the scrutinee compares to the operand using the given
  operator.

  The operand must be a side-effect-free expression (for example, literals,
  consts, or other stable values), ensuring relational patterns remain predictable
  and optimizable.

  Relational patterns are commonly used under `not`, `and`, and property patterns,
  e.g. `{ Age: not > 30 }`.

#### Positional patterns

* `(element1, element2, …)` — **positional pattern**. Matches when the scrutinee
  can be deconstructed positionally with the same arity (either as a tuple or via
  a compatible `Deconstruct` method).

  Positional patterns destructure by position. Each element is itself a pattern and
  may introduce bindings. For example:

  * If the scrutinee type exposes a `Deconstruct` method with matching arity
    (including as an extension method), the positional pattern uses that method
    to obtain positional values.

  * ✅ `(val a, val b)`
  * ✅ `(val a: int, val b: string)`
  * ✅ `(val a: int, _)`
  * ❌ `(a, b)` — does **not** introduce bindings; both elements are treated as
    value patterns and must resolve to in-scope values.

  A positional element introduces a new binding **only** when an explicit binding
  keyword (`val`, or `var`) is present.

  An element may optionally include a name before the colon (`name: pattern`) to
  bind the element value while still applying a nested pattern.

#### Collection patterns

* `[pattern1, pattern2, …]` — **collection pattern**. Matches when the scrutinee
  is an array with the same length and each element pattern matches the
  corresponding array element.

  * Collection patterns are currently supported for array scrutinees (`T[]`).
  * Each element is a full pattern; bindings still require `val`/`var`.
  * Length must match exactly.

#### Property patterns

* `Type { member1: pattern1, member2: pattern2, … }` — **property pattern**.
  Matches when the scrutinee is not `null` and can be treated as `Type`, then
  evaluates each listed member subpattern against the corresponding instance
  member.

  * Each `member: pattern` targets an accessible instance field or readable
    instance property.
  * Nested patterns are type-checked against the member’s type.
  * Member subpatterns are evaluated left-to-right; bindings from earlier entries
    are in scope for later ones.
  * The empty pattern `Type { }` matches any non-`null` value that can be treated
    as `Type`.

* `Type { … } designation` — **property pattern with designation**. Like
  `Type { … }`, but also introduces a designation for the matched receiver.

  * The designation must include an explicit binding keyword (`val`, or
    `var`).
  * Writing `var p` produces a mutable binding.
  * The designation is introduced only if the entire property pattern succeeds.
  * Writing `_` discards the receiver value while still enforcing the pattern.

* `{ member1: pattern1, member2: pattern2, … }` — **inferred property pattern**.
  Like `Type { … }`, but the receiver type is inferred from the scrutinee’s static
  type.

  * If the receiver type cannot be inferred, the pattern is invalid and requires
    an explicit type.
  * `{ }` acts as a non-null test.

* `{ … } designation` — **inferred property pattern with designation**.

  * The designation must include an explicit binding keyword (`val`, or
    `var`).
  * Writing `var p` produces a mutable binding.

#### Record patterns

* `RecordType(pattern1, pattern2, …)` — **record pattern**. Matches when the
  scrutinee can be treated as `RecordType` and each positional subpattern matches
  the corresponding record property in primary-constructor order.

  * Record patterns are only valid on `record` types.
  * Record patterns use the record’s `Deconstruct` method to obtain positional
    values.
  * Each positional element is a pattern, so bindings still require `val`/`var`.
  * The number of positional elements must match the record’s primary-constructor
    parameters; mismatches are errors.

#### Member patterns

* `.Case` / `Type.Case` — **member pattern**. Matches a named member by name,
  including discriminated union cases, constants, or nested types.

  * The leading `.` resolves against the current scrutinee.
  * Member payloads may supply nested subpatterns matching the member’s
    parameter list, e.g. `.Identifier(text)` or `Result<int, string>.Error(val message)`.
  * Parentheses are optional for parameterless members.
  * Payload arity must match the declared parameters.
  * Each nested subpattern is typed to the corresponding member parameter.
  * A payload element introduces a new binding **only** when it uses
    `val`/`var`. A bare identifier is a value pattern that matches an
    existing in-scope symbol.

    * Example: `.Case(val a, b)` binds `a` and matches the second payload against
      the runtime value of in-scope `b`.
  * Use `_` to explicitly discard a payload.

#### Pattern combinators

* `pattern1 and pattern2` — **conjunction**. Succeeds only when both operands
  match. Bindings from either operand are available after the conjunction.

* `pattern1 or pattern2` — **alternative**. Matches when either operand matches.

* `not pattern` — **complement**. Succeeds when the operand fails. `not` never
  introduces bindings.

Precedence: `not` > `and` > `or`. `or` associates left-to-right. Parentheses
override precedence.

## Namespace declaration

Each file may define a namespace:

```raven
namespace Foo

// Members here
```

### Import directive

```raven
namespace Foo

import System.*
// or import System.Collections.*

// Members here
```

The wildcard may also be applied to a type name to bring its static members
and nested types into scope:

```raven
import System.Math.*

val pi = PI
```

Extension methods defined on imported types are also brought into scope. This
enables consuming .NET helpers such as `System.Linq.Enumerable.Where` or
`System.MemoryExtensions.AsSpan` directly from Raven source:

```raven
import System.Collections.Generic.*
import System.Linq.*

val odds = List<int>()
odds.Add(1)
odds.Add(3)
val filtered = odds.Where(value => value % 2 == 1)
```

The compiler treats `Where` as an instance-style invocation even though it is
declared as a static C# extension method, inserting the receiver as the first
argument when emitting IL.

Import directives appear at the beginning of a compilation unit or namespace and
simply make existing namespaces or types available. They do not introduce new
names. To bind a custom name, use an `alias` directive. All imports for a given
scope must come before any alias directives or member declarations. Placing an
import directive after an alias or member is a compile-time error (`RAV1005`).

### Alias directive

The `alias` directive assigns an alternative name to a fully qualified
**namespace**, type, static member, or to any type expression such as tuples
and type unions.

```raven
alias IO = System.IO
alias SB = System.Text.StringBuilder
alias PrintLine = System.Console.WriteLine
alias Pair = (x: int, y: int)
alias Number = int | string
alias Flag = bool
alias Text = string
alias Five = 5

val sb = SB()
PrintLine("Hi")
val tmp = IO.Path.GetTempPath()
```

Aliasing a method binds a specific overload. Multiple directives using the
same alias name may appear to alias additional overloads, forming an overload
set.

Predefined and literal types may be aliased directly. The supported built-in alias targets are `bool`, `char`, `int`, `long`, `float`, `double`, `string`, and `unit` (spelled `unit` or `()`), and any literal value. Raven has no `void`; the `unit` type is used instead (see [implementation notes](dotnet-implementation.md#unit-type)). If the alias target is invalid, the compiler emits diagnostic `RAV2020`, which lists the supported targets such as types, namespaces, unions, tuples, these predefined types, and literal values.

Aliases require fully qualified names for namespaces, types, and members to
avoid ambiguity; type expressions are written directly. Alias directives may
appear at the top of a file or inside a namespace but must follow all import
directives and precede any declarations or statements. An alias directive that
appears after a member declaration produces diagnostic `RAV1006`.

### Scoped namespaces

You may define multiple namespaces (including nested) in one file using
block scopes:

```raven
// Members in the global namespace

namespace A1
{
    import System.*
    import System.IO.*

    // Members here

    namespace B1
    {
        // Members here
    }
}

namespace A.B
{
    // Members here
}
```

The outermost undeclared namespace is the **global namespace**.

## Entry points

## Enum declarations

An `enum` declaration defines a set of **named constants** backed by an integral
underlying type. Enums model simple symbolic values and are primarily intended
for interoperability and compatibility with existing .NET APIs.

```raven
enum Color {
    Red
    Green
    Blue
}
```

> **Design note**
>
> Enums in Raven represent *named constants only*. They do **not** support
> attaching additional data to individual members, and they are treated as
> **non-exhaustive** in pattern matching. This reflects their CLR representation
> and preserves compatibility with existing .NET libraries.
>
> Because enum values are not closed over time (new values may appear via casts,
> interop, or future versions), `match` expressions over enums are not required
> to be exhaustive, and the compiler does not enforce completeness.

### Underlying type

An enum may optionally specify an explicit underlying type using a base list after
the enum name:

```raven
enum Status : byte {
    Ok = 1
    Error = 2
}
```

If no underlying type is specified, the underlying type defaults to `int`.

Only a single underlying type may be specified. The underlying type must be a
non-nullable integral primitive type (`byte`, `sbyte`, `short`, `ushort`, `int`,
`uint`, `long`, `ulong`, or `char`). Specifying any other type is a compile-time error.

### Enum members

Each enum member introduces a public constant whose type is the enclosing enum.

Enum members carry no associated payload or structure beyond their constant
value. They cannot declare fields, parameters, or additional data.

An enum member may optionally declare an explicit value using `=` followed by a
constant expression that is convertible to the enum’s underlying type:

```raven
enum ErrorCode : int {
    None = 0
    NotFound = 404
    Timeout = 405
}
```

If an enum member does not specify a value, its value is implicitly defined as one
greater than the previous member. The first member defaults to zero when no
explicit initializer is present.

Enum member initializers must be constant expressions. They may reference previously
declared enum members. References to non-constant values are invalid, and values
that cannot be represented in the underlying type are compile-time errors.

### Conversions

An explicit conversion exists from an enum type to its underlying type:

```raven
val code: int = (int)ErrorCode.NotFound
```

The reverse conversion—from the underlying type to the enum type—requires an
explicit cast and is not validated for named membership at compile time.

### Enums vs. discriminated unions

When modeling a *closed* set of alternatives where:

* every case must be handled exhaustively, or
* individual cases need to carry associated data,

**discriminated unions** should be used instead of enums.

Discriminated unions provide:

* compile-time exhaustiveness checking in `match` expressions,
* strongly typed payloads attached to each case, and
* safer evolution as new cases are added.

Enums remain appropriate for simple symbolic values, flags, and interop
scenarios where CLR compatibility is required.

### Runtime representation

At runtime, enums are emitted as CLR enum types. The compiler emits a special
instance field named `value__` whose type is the enum’s underlying type. Each
enum member is emitted as a public static literal field whose constant value is
stored using the underlying type.

### Supported entry-point forms

Console applications may start in any of the following shapes, all of which obey
the same signature rules:

* **File-scope code (global statements):** executable statements at the top of a
  file. These coexist with other declarations (namespaces, types, functions) in
  the same compilation unit.
* **Top-level function:** a global `func Main` declaration that can appear next
  to other top-level members. When present, no other file-scope statements may
  appear alongside it.
* **Classic static method:** a `Main` method declared on a type such as
  `Program.Main`.

### File-scope code rules

Files may start with executable statements that aren't enclosed in a function or
type. This file-scope code forms the application's entry point and is translated
into a synchronous `Program.Main` plus an async `Program.MainAsync` that returns
`Task` or `Task<int>` depending on whether the script returns a value. Only
console applications may include file-scope code, and
it may appear in at most one file per compilation. When present, these
statements must come before any other declarations in the file or its
file-scoped namespace.

Function declarations (local function statements) within file-scope code are
hoisted and may be referenced from anywhere in that file-scoped region,
regardless of their order. When file-scope code contains *only* function
declarations, the compiler skips synthesizing the implicit `Program.Main`
bridge; entry-point discovery falls back to user-defined candidates such as a
top-level `func Main` alongside other global declarations.

Defining a top-level `func Main` suppresses additional file-scope statements.
Any other file-scope statement (including variable declarations or
expressions) in the same compilation unit causes the compiler to emit
`RAV1021` *Top-level statements are not allowed when 'Main' is declared as a
top-level function*.

### Entry point resolution

Console applications begin executing at the synthesized `Program.Main` bridge
that forwards to the async `Program.MainAsync` backing file-scope code. When a
project does not contain runnable file-scope statements, the compiler instead
looks for a user-defined entry point. Any
static method named `Main` qualifies when it meets the following requirements:

* The method returns one of:
  `unit`, `int`, `Task`, `Task<int>`, `Result<int, E>`, `Result<(), E>`,
  `Task<Result<int, E>>`, or `Task<Result<(), E>>`.
* It has no type parameters.
* It declares either no parameters or a single parameter of type `string[]`
  (representing the command-line arguments).

If exactly one method satisfies these conditions, it becomes the entry point for
the compilation. When no method qualifies, the compiler reports
`RAV1014` *Entry point 'Main' not found*. Declaring more than one valid
`Main` (including mixing top-level statements with a matching method) causes the
compiler to emit `RAV1017` *Program has more than one entry point defined*.

When the selected entry point returns `Task`, `Task<int>`, `Result<int, E>`,
`Result<(), E>`, `Task<Result<int, E>>`, or `Task<Result<(), E>>`, the compiler emits
an implicit synchronous bridge method in the entry point's containing type. For
file-scope code the bridge is `Program.Main`, while user-defined async entries
receive a synthesized `<Main>_EntryPoint` neighbor unless a synchronous `Main`
already exists. The bridge calls into the async implementation, awaits it via
`GetAwaiter().GetResult()`, and forwards the resulting value (if any) to the host
environment. A `Task`-returning entry point produces a bridge whose CLR
signature omits a return value; the helper awaits the async body, discards the
awaited `Unit` value, and only returns after the async work (such as console writes)
completes. Exceptions thrown from the async body bubble through the same
`GetResult()` call so the process exits with the same failure semantics as a
purely synchronous entry point. 【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L352-L403】【F:test/Raven.CodeAnalysis.Tests/CodeGen/CodeGeneratorTests.cs†L88-L144】

Entry points that return `Task<int>` produce a bridge that awaits the async body
and returns the awaited integer as the process exit code. Entry points that
return `Result<int, E>` or `Task<Result<int, E>>` use `Ok(value)` as the process
exit code. Entry points that return `Result<(), E>` or `Task<Result<(), E>>`
produce no output on `Ok`, while `Error` payload data is printed to standard
error.
The bridge also leaves console writes intact so the awaited value can be
observed by both the caller and the host operating system.
【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L405-L476】

Library and script output kinds ignore the entry point search; they never report
missing or ambiguous entry-point diagnostics.

## Delegate declarations

Delegate declarations introduce named callable types. The declaration lists a parameter signature and optional return type, and the compiler synthesizes the corresponding delegate members.

```raven
delegate Transformer(value: int) -> string

class Pipeline
{
    public delegate Stage<T>(ref value: T) -> bool
}
```

Delegates are emitted as sealed, abstract types that inherit from `System.MulticastDelegate`. For every delegate declaration, the compiler synthesizes:

* A constructor `.ctor(object, IntPtr)` that binds a target and method pointer.
* An `Invoke` method whose parameters (including any `ref`/`out`/`in` modifiers) and return type match the declaration.

If the return type clause is omitted, the delegate returns `unit`. Delegate declarations support generic type parameters and constraints using the same `where` clause rules as other type declarations. Accessibility defaults follow the standard type rules: top-level delegates are `internal` unless marked `public`, and nested delegates default to `private` except when declared inside interfaces.

## Functions

```raven
func Foo(a: int, b: int) -> int
{
    a + b
}
```

Arrow bodies are allowed:

```raven
func add(a: int, b: int) -> int => a + b
```

Function declarations (including local `func` statements) may carry declaration
attributes with the same bracket syntax as methods:

```raven
[Trace]
func compute(x: int) -> int => x * 2
```

Return-targeted attribute lists are also supported on functions:

```raven
[return: MaybeNull]
func find(name: string) -> string { /* ... */ }
```

### Parameters

Function, method, and accessor parameters use the `name: Type` syntax. Parameter
names are required and participate in overload resolution alongside their types
and any `ref`/`out` modifiers.

Lambda parameter lists use the same parameter form and default-value rules.
Parameters in function-like declarations may include attribute lists. This
applies to functions, methods, constructors, delegates, accessors, and lambdas.

Parameters may provide a default value using `= expression` after the type. A
parameter with a default value is optional when invoking the function: callers
can omit that argument and the compiler supplies the stored constant instead.
Only trailing parameters may be optional; omitting an argument fixes the default
for that position and all following parameters must also declare defaults.

```raven
func greet(name: string, punctuation: string = "!")
{
    Console.WriteLine($"Hello, ${name}${punctuation}")
}

greet("Raven")          // prints "Hello, Raven!"
greet("Raven", "!!!")    // caller-provided punctuation wins
```

Default expressions must be compile-time constants: literals (including `null`),
parenthesized literals, and unary `+`/`-` applied to numeric literals. The value
must convert to the parameter type using an implicit conversion. Nullable value
types accept `null` defaults; other value types require a literal of the
underlying type. Reference-type parameters accept `null` defaults.

When importing methods from other assemblies, Raven also recognizes optional
parameters surfaced through metadata. Parameters marked optional with a stored
default constant or with `System.Runtime.InteropServices.DefaultParameterValueAttribute`
and `System.Runtime.InteropServices.OptionalAttribute` participate in overload
resolution just like source-declared defaults. Omitted positional and named
arguments are synthesized from those metadata-provided constants, after applying
the same constant-conversion rules as source defaults. If only
`System.Runtime.InteropServices.OptionalAttribute` is present, Raven will use the
parameter type’s CLR default value when materializing an omitted argument,
emitting `default(T)` for value types that lack a literal representation.

### Generic functions and methods

### Type parameters and constraints

Functions, methods, types, and local functions may declare one or more **type parameters**
using a type parameter list written after the declaration name:

```raven
func identity<T>(value: T) -> T { value }
class Box<T> { value: T }
```

Each type parameter introduces a distinct generic placeholder type. Type parameter names
must be unique within the same type parameter list. A type parameter is in scope within:

* the remainder of the type parameter list
* parameter types
* return types
* constraint clauses
* the declaration body

#### Variance

Where permitted by the enclosing declaration, a type parameter may be annotated with
variance:

* `out T` — covariant
* `in T` — contravariant

Variance annotations affect assignability of constructed generic types and are validated
by the compiler according to the rules of the enclosing declaration kind.

#### Inline constraints

A type parameter may declare **inline constraints** using a colon immediately following
its name:

```raven
func inner<T: struct>(value: T) -> T { value }
func map<T: class, U>(value: T) -> U { /* ... */ }
```

Inline constraints are syntactic sugar for an equivalent `where` clause on the same type
parameter:

```raven
func inner<T>(value: T) -> T where T: struct { value }
```

The compiler normalizes inline constraints and `where` clauses into the same internal
constraint representation.

#### Constraint source rule

For a given type parameter, constraints must be specified using **exactly one** of:

* inline constraints (`T: ...`)
* one or more `where` clauses targeting that parameter

Specifying constraints for the same type parameter using both forms is a compile-time
error.

#### Constraint forms

Each constraint in a constraint list must be one of the following:

* `class` — reference type constraint
* `struct` — non-nullable value type constraint
* `notnull` — non-null constraint
* `unmanaged` — unmanaged value type constraint
* a base class type
* an interface type
* `new()` — public parameterless constructor constraint

Constraints are **conjunctive**: all listed constraints must be satisfied.

The following restrictions apply:

* At most one of `class` or `struct` may appear.
* At most one base class constraint may appear.
* Any number of interface constraints may appear.
* `new()` may appear at most once.
* Duplicate constraints are not permitted.

Violating any of these rules produces a compile-time diagnostic.

#### Constraint ordering

When written, constraints should appear in the following order:

1. `class` or `struct`
2. base class
3. interfaces
4. `new()`

The compiler may diagnose violations of this ordering for consistency.

Functions—including methods declared inside types—may introduce type parameters
by placing `<...>` after the function name. Each type parameter can be used in
the parameter list, return type, and body just like any other type annotation.

```raven
func identity<T>(value: T) -> T { value }

val number = identity(42)         // inferred T = int
val text = identity<string>("hi")
```

Call sites may omit explicit type arguments when inference can determine a
unique solution from the arguments and expected return type. When inference
fails—for example, because multiple type choices satisfy the call—the type
arguments must be provided explicitly.

Method declarations use the same syntax, and local functions follow the exact
rules when they introduce type parameters inside another body:

```raven
class Cache
{
    static store<T: class>(value: T) { /* ... */ }
}

Cache.store(System.Text.StringBuilder())   // inference picks T = System.Text.StringBuilder
Cache.store<string>(null)                  // explicit type argument when passing null
```

Type parameter constraints mirror those on generic types. After the colon, list
`class`, `struct`, or specific base/interface types that each argument must
implement. Constraints are conjunctive: every listed requirement must be
satisfied. The `struct` constraint excludes nullable value types, while `class`
admits reference types (including nullable references). Violating a constraint
produces a diagnostic identifying the failing type argument and the unmet
requirement.

### Nested functions

Functions may be declared inside other functions. Such a function is
scoped to its containing body and can capture local variables. Local functions
support the same generic syntax and constraints as file-scoped functions: place
an optional type parameter list after the function name and declare constraints
using the `:` syntax when needed.

```raven
func outer() {
    func inner<T: struct>(value: T) -> T { value }

    val y = inner(2)
    val point = inner((x: 1, y: 2))
}
```

### Async functions

The `async` modifier may appear on free functions, methods, and nested
functions. An async declaration opts the body into asynchronous control flow so
`await` expressions can suspend and resume execution. When no return type is
annotated, the compiler infers `System.Threading.Tasks.Task` for bodies that
complete without a value and `System.Threading.Tasks.Task<T>` when the body
produces a value of type `T`.

Async functions with an explicit return type must annotate one of the supported
task shapes: `System.Threading.Tasks.Task` or `System.Threading.Tasks.Task<T>`.
Annotating any other type produces a diagnostic, and the compiler continues
analysis as though the return type were `Task`. This rule applies uniformly to
methods, file-scoped functions, and local functions declared inside other
bodies. Property and indexer accessors may also carry `async`; getters must
expose a task-shaped return type to remain valid, while setters may await
asynchronous work before storing values.

Async declarations support both block bodies and expression bodies. Every
`return` inside an async declaration completes the task produced by the method.
For `async Task<T>` block bodies, a trailing expression statement is treated as
an implicit return value and must convert to `T`.
For `async Task` members each `return` statement must omit the expression;
falling off the end of the body is equivalent to `return;`. For `async Task<T>`
members, return expressions must convert to `T`.

Returning an existing task instance such as `Task.CompletedTask` is not
permitted inside an `async Task` body. Authors must `await` the task to observe
its completion instead of returning it directly. Attempting to return an
expression from an `async Task` member produces a diagnostic that mirrors the
behavior of C# (error RAV2705). Exceptions that escape before the first `await`
propagate directly to the caller. Once asynchronous execution begins, `await`
unwrapped exceptions rethrow when the task is awaited, matching .NET's
observable behaviour.

### Lambda expressions and captured variables

Lambda expressions start with either a parenthesized parameter list or a single
identifier, optionally followed by a return-type arrow, and then the `=>` token
with either an expression or block body. Lambdas may appear wherever a function
value is expected. When a lambda references a local defined in an
outer scope, the compiler lifts that local into shared closure storage so both
the outer scope and the lambda observe the same value. Each capturing lambda
materializes a synthesized closure class that stores the lambda body as an
instance method and exposes fields for every captured symbol. Reads and writes
in any scope access those fields directly, so mutating a `var` binding after
creating a lambda immediately affects all delegates that captured it. Capturing
`self` produces a reference to the enclosing instance, and capturing parameters
preserves the argument value from the invoking scope. Nested lambdas reuse the
closure instances produced by their enclosing scopes so that captures shared
across multiple lambda layers continue to reference the same storage locations.

Parenthesized lambdas may place attribute lists immediately before the
parameter list as shorthand. Leading lists are applied contextually:
non-targeted attributes are applied to the first parameter, while
`[return: ...]` lists are applied to the lambda return type.

```raven
val parse = [FromBody](content: string) => content
```

Lambda parameters may also declare default values using the same trailing
optional-parameter rules as functions and methods.

```raven
val format = (name: string, age: int = 1) => $"{name}:${age}"
```

Lambda parameter types are optional when the expression is converted to a known
delegate type. The compiler infers the parameter types (and any `ref`/`out`
modifiers) from the delegate's `Invoke` signature and converts the body to the
delegate's return type. If no delegate context is available, diagnostic
`RAV2200` is reported and explicit parameter annotations are required.

Lambdas are target-typed: the same lambda expression may be assigned to, passed
to, or returned as any compatible delegate type. Compatibility is determined
solely by the delegate's `Invoke` signature (parameter types, `ref`/`out`
modifiers, and return type). Delegate types themselves are **not** implicitly
convertible between one another even when their signatures match; preserving
delegate identity requires an explicit cast when converting from one delegate
type to another.

### Function values and method references

Functions and methods are first-class values. Referencing a function or method
name without invoking it produces a delegate that can be stored, passed around,
or invoked later. The compiler picks an appropriate delegate type using the
same target-typing rules that guide overload resolution. In a binding such as
`val` or `var`, the initializer (or an explicit type annotation) supplies that
context so the delegate type—and corresponding overload—can be determined.
When no delegate context is available, diagnostic `RAV2201` is reported and the
method must either be invoked directly or annotated with a delegate type.

```raven
val writeLine: (string) -> () = Console.WriteLine
writeLine("Hello from Raven!")
```

If the referenced member has no overloads, the compiler may omit the
annotation and still infer the delegate type from that unique signature.

When the referenced method defines multiple overloads, Raven does **not** allow
an unannotated binding to rely solely on type inference; such declarations are
ambiguous and produce diagnostic `RAV2202`. To disambiguate, provide the
delegate type explicitly or use another context with a well-defined target
type.

```raven
val writeLine = Console.WriteLine             // error: overloaded method group
val writeLine: (string) -> () = Console.WriteLine // ok
```

Passing `Console.WriteLine` as an argument to a parameter of function type `(string) -> ()`
(equivalent to `System.Action<string>`) likewise selects the `string` overload without requiring
an explicit annotation at the call site. If no overload matches the target
delegate's signature, diagnostic `RAV2203` is produced.

If no compatible delegate type exists in the current context, the compiler
generates one whose signature matches the referenced function or method. The
generated delegate observes the same parameter list (including `ref`/`out`
modifiers) and return type as the source symbol so the resulting value behaves
identically to directly invoking that member. Subsequent uses of the same
signature within the compilation reuse the synthesized delegate.

Instance method references capture their receiver automatically. Evaluating
`self.Member` as a value stores the current instance alongside the referenced
method so later invocations execute against the same object:

```raven
class Counter {
    value: int = 3

    Increment(delta: int) -> int { self.value + delta }

    Run() -> int {
        val increment = self.Increment
        increment(7) // returns 10
    }
}
```

Method references may be passed directly to parameters of delegate type. The
overload chosen for the receiving method is the one whose delegate parameter
matches the referenced method's signature:

```raven
func Run(action: System.Action<string>) { action("ready") }
func Run(value: string) { Console.WriteLine(value) }

Run(Console.WriteLine) // selects the Action<string> overload
```

When a referenced method's signature requires a delegate that doesn't already
exist—such as one with `ref`/`out` parameters—Raven synthesizes an internal
delegate type and uses it as the expression's type. These delegates behave like
framework-provided types and faithfully propagate modifiers:

```raven
class Accumulator {
    static TryAccumulate(state: &int, out doubled: &int) -> bool {
        state = state + 1
        doubled = state * 2
        true
    }

    static Execute(value: int) -> int {
        val callback = Accumulator.TryAccumulate
        var current = value
        var doubled = 0

        callback(&current, &doubled)
        current + doubled // evaluates to 12 when value is 3
    }
}
```

### Pointer types

The `*Type` form declares a native pointer to `Type`. Pointer types are
distinct from by-reference types but interoperate with address-of
expressions: taking the address of a local or field produces an address
handle that implicitly converts to the matching pointer type.

Pointer declarations and pointer-type usage require unsafe mode. Enable
unsafe mode with the compiler's `--unsafe` option; otherwise pointer type
syntax is rejected.

Unsafe context can be enabled either globally (compiler option `--unsafe`)
or locally with `unsafe` contexts:

* `unsafe func Name(...) { ... }` for function/method scope
* `unsafe { ... }` for block scope

```raven
val value = 42
val pointer: *int = &value
```

### Pointer operations

Pointer operations are also gated by unsafe mode. In unsafe mode:

* `*ptr` dereferences a pointer (or by-reference value) and reads the pointed value.
* `*ptr = value` writes through the pointer.
* `ptr->Member` accesses a member on the pointed-at type and is equivalent to
  `(*ptr).Member`, while preserving direct storage semantics (no implicit struct copy for
  instance member calls/field writes).
* `ptr + n` / `n + ptr` advances a pointer by `n` elements.
* `ptr - n` rewinds a pointer by `n` elements.
* `ptr1 - ptr2` returns the element-distance as `nint` when both pointers share the same element type.
  Arithmetic offsets are scaled by `sizeof(element-type)`.

```raven
var value = 41
let pointer: *int = &value

*pointer = 42
val result = *pointer // 42
```

```raven
struct Holder {
    public var Foo: int = 0
}

unsafe func assignThroughPointer() -> int {
    var holder = Holder()
    let pointer: *Holder = &holder
    pointer->Foo = 2
    holder.Foo // 2
}
```

### Extern declarations

Methods and function statements can be marked `extern` to declare that their
implementation is provided externally (for example via P/Invoke). Extern
declarations do not include a body.

Rules:

* Type members marked `extern` must also be `static`.
* `extern` members/functions cannot declare a block body or expression body.
* P/Invoke declarations are expressed with `[DllImport(...)]` on static extern methods.

```raven
import System.Runtime.InteropServices.*

class Native {
    [DllImport("kernel32", EntryPoint: "GetTickCount")]
    public extern static GetTickCount() -> uint;
}
```

### Pass by reference

By-reference types can annotate locals and return values. A local
declared with `&Type` acts as an alias to the underlying storage, so
assignments flow through to the referenced location. Functions may
return by-reference values to expose existing storage to the caller. If
you plan to reassign the alias, declare it with `var` so the reference
itself remains mutable.

Taking the address of a value with `&expression` implicitly produces a
by-reference type when the target binding has no explicit annotation.
Use a pointer annotation to force the result into a native pointer
instead of a managed alias.

```raven
func headSlot(values: int[]) -> &int {
    return &values[0]
}

var numbers: int[] = [10, 20, 30]
var slot = headSlot(numbers)
slot = 42 // numbers[0] is now 42

val value = 0
val alias = &value      // alias : &int
val raw: *int = &value  // raw : *int
```

By-reference returns must point to storage that outlives the callee. Returning
`&local` or `&valueParameter` is rejected because those addresses become invalid
after the function returns.

### `ref`/`out` arguments

Parameters can also be declared by reference using `&Type`. When a
by-reference parameter is passed **into** a function, it behaves just
like a by-reference local: the callee receives an alias to the caller's
storage and can both read and write through that reference. To mark a
parameter that must be assigned by the callee before returning, place
`out` before the parameter name. Parameters are immutable by default, so
add the `var` modifier when you need to reassign the alias—for example
to satisfy an `out` contract or to reuse a ref parameter as scratch
storage. At call sites, pass the argument with the address operator
`&`. (Exact rules are contextual; the binder enforces that the target is
assignable.)

By-reference locals and fields never use the `out` modifier—`out` is
only meaningful at the call boundary to signal definite assignment
responsibilities between caller and callee. Declaring a local with
`&Type` produces a reference variable that immediately aliases an
existing storage location; invoking a member with an `out &Type`
parameter transfers that aliasing requirement to the parameter for the
duration of the call.

```raven
func TryParse(text: string, out var result: &int) -> bool { /* ... */ }

var total = 0
if !TryParse(arg, &total) {
    Console.WriteLine("Expected number")
}
```

## Local declarations

### Value binding (`val`)

A `val` binding is **immutable** (not reassignable). Types are inferred
unless annotated.

```raven
val x = "Foo"
val y: int = 2
val a: int = 2, b: string = ""
```

Raven treats `let` as an alias for `val`.

### Variable binding (`var`)

A `var` binding is **mutable** (reassignable).

```raven
var x = "Foo"
x = "Bar"

var y: int = 2
y = 3
```

### Constant binding (`const`)

A `const` binding is immutable like `val` but additionally requires a compile-time
constant initializer. The compiler embeds the resulting value directly into the
generated IL so the symbol can be referenced from other assemblies without
executing the initializer.

```raven
const pi: double = 3.141592653589793
const banner = "Ready"      // inferred string constant
```

Const bindings support the primitive constant forms recognized by .NET: numeric and
character literals (including the appropriate suffixes), `true`/`false`, strings, and
`null` for reference types. Type inference works the same way as `val`; the initializer
must still be a compile-time constant.

Const applies to both local bindings and fields. Member declarations treat `const`
fields as implicitly `static`; the value is emitted as metadata so other assemblies can
import it without running an initializer.

Positional deconstruction lets you bind or assign multiple values at once. The outer
`val`/`var` controls the binding’s mutability, while each element uses a
designation (possibly nested) to capture or discard the corresponding value.
Elements may include inline type annotations. Positional deconstruction works with
tuples and with any type that exposes a compatible `Deconstruct` method (including
as an extension method).

```raven
val (first, second, _) = (1, 2, 3)
var (head, tail: double, _) = numbers()
(first, second, _) = next()
(val lhs, var rhs: double, _) = evaluate()
```

Existing locals can participate in positional assignments alongside new
bindings. Mixed `val`/`var` designations and inline type annotations are
supported in both declarations and assignments:

```raven
var first = 0
var second = 0

(first, second, _) = (1, 2, 3)
val (third, fourth: double, _) = toTuple()
var (val fifth, var sixth: double, _) = project()
```

Use `_` to discard unwanted elements. Nested positional patterns work the same way:

```raven
var ((x, y), val magnitude, _) = samples()
```

The discard identifier also appears in ordinary assignment statements. Writing
`_ = Compute()` produces a discard assignment statement whose left-hand side is a
dedicated discard expression. The assignment still evaluates the right-hand
expression, but the result is ignored. Discard assignments follow the same rules
as positional assignment: they never declare a binding and may carry a type
annotation when overload resolution needs guidance. `AssignmentStatementSyntax`
exposes an `IsDiscard` helper when analyzers need to detect this pattern.

### Resource declarations (`use`)

A `use` declaration introduces a **scoped disposable resource**.  
The declaration resembles a local variable binding and **must include an initializer**.

The initializer’s type — and any explicit type annotation — must be convertible to `System.IDisposable`. If the conversion fails, Raven reports the same diagnostic used for other implicit conversions. 【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L188-L224】

Resources created with `use` behave like ordinary locals: they remain in scope for the enclosing block and participate in definite-assignment rules. When control leaves the block, the resource is **automatically disposed**. Disposal occurs in **reverse declaration order**, ensuring that later resources observe earlier ones still alive.

File-scope `use` declarations participate as well: they are disposed after the file’s top-level statements finish executing. 【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L222-L282】【F:src/Raven.CodeAnalysis/CodeGen/Generators/Generator.cs†L54-L87】【F:src/Raven.CodeAnalysis/CodeGen/MethodBodyGenerator.cs†L114-L148】

```raven
use stream = System.IO.File.OpenRead(path)
use reader = System.IO.StreamReader(stream)

val text = reader.ReadToEnd()

// reader.Dispose() and stream.Dispose() run automatically when the scope ends
````

## Types

### Type annotations

Use type annotations where inference is insufficient (e.g., for
parameters, some bindings, or return types):

```raven
val a = 2
val b: int = 2

func add(a: int, b: int) -> int { a + b }
```

### Tuple types

Tuple types use parentheses with comma-separated element types and map to
`System.ValueTuple`:

```raven
val pair: (int, string) = (42, "answer")
```

Elements may optionally be named with a `name: Type` pair. Names exist only for
developer clarity and do not participate in type identity or assignment:

```raven
val tuple2: (id: int, name: string) = (no: 42, identifier: "Bar")
```

When a tuple expression is assigned to an explicitly annotated tuple type, each
element is validated against the corresponding element type. Elements are
accessed positionally (e.g. `Item1`, `Item2`). Tuple types may nest or
participate in other type constructs such as unions or nullability.

### Function types

Function types describe callable delegates directly in a type annotation. The
syntax mirrors a lambda signature: a comma-separated parameter list enclosed in
parentheses followed by `->` and the return type.

```raven
val applyTwice: (int -> int, int) -> int
val thunk: () -> unit
val comparer: (string, string) -> bool
```

Single-parameter functions may omit the surrounding parentheses:

```raven
val increment: int -> int
```

The return portion may itself be any Raven type, including unions. For example
`string -> int | null` represents a delegate that returns either an `int` or
`null`. Nested arrows associate to the right, so `int -> string -> bool` is
parsed as `int -> (string -> bool)`.

Function annotations are sugar over delegates. When the parameter and return
types match an existing declaration (including the built-in `Func`/`Action`
families), the compiler binds to that delegate. Otherwise it synthesizes an
internal delegate with the appropriate signature so interop with .NET remains
transparent. Parameter modifiers and names are not permitted inside a function
type; specify only the types that flow into and out of the delegate. A `unit`
return represents an action with no meaningful result.

### Union types

Unions express multiple possible types (e.g., `int | string`). A union’s members are **normalized**: nested unions flatten, duplicates are removed, and order is irrelevant. For example, `int | (string | int)` simplifies to `int | string`.
These are type-level unions, separate from `union Name { ... }` declarations.

#### When to use type unions

Use a type union when a value is intentionally one of several concrete types and
you want that set to remain explicit in the type system. This is especially
useful when alternatives have no meaningful shared base type beyond `object`.
Without a union annotation or inference context, those values would otherwise
often collapse to `object` and lose branch-specific intent.

```raven
val a: int | string
val b = if flag { 1 } else { "one" }  // b : int | string
```

#### Literal types in unions

A **literal type** represents a single constant value of a primitive, string, bool, or enum type. Literal types may appear anywhere a type is expected and compose naturally with unions.

```raven
val n: 1 | 2 | 3
val s: "on" | "off" | "auto"
val e: Grades.A | Grades.B
```

**Normalization with literals**:

* A literal type `L(c : B)` is a subtype of its base `B`. If a union contains both `B` and any of its literals, the literal members **collapse into** `B` *unless* the union has only literals of that base (a finite domain).

  * `int | 42` → `int`
  * `1 | 2` stays as `1 | 2`
* Literal types of **different bases** remain distinct: `3 | 2L` (assuming `long`) stays as `int | long`.

#### Nullability and `null`

Nullability is **explicit** in Raven. Reference types are non-nullable by
default, and `null` can only flow through nullable annotations (`T?`) or unions
that include `null`. The same rules apply uniformly to reference and value
types; the distinction only affects runtime representation, not the surface
type rules.

`null` may appear as a union member:

```raven
val maybe = if flag { 1 } else { null }  // int | null
```

If a union contains `null` and exactly one non-nullable type `T`, it implicitly converts to `T?` (both in inference and explicit annotations):

```raven
val x: int? = maybe         // ok
val y: string? | int        // error: explicit nullable types must not be unioned
```

To model absence explicitly, Raven recommends the **Option union** defined in
`src/Raven.Core/Option.rav` (`System.Option<T>`). It behaves like a
`T | null` union for both reference and value types and includes an implicit
conversion to the nullable form (`T?` / `Nullable<T>`) when interacting with
existing .NET APIs that expect nullable types.

#### Assignability and conversions

val `U = T1 | … | Tn` be a source union.

* **Value → union**: An expression of type `S` may be assigned to `U` if `S` is assignable to **at least one** member `Ti`.
* **Union → value**: `U` may be assigned to `S` only if **every** member `Ti` is assignable to `S`.
* **Literal checking**: Assigning to a **finite literal union** requires the value to be a compile-time constant equal to one of the listed literals.
* **Variant interfaces**: When `Ti` or `S` is a generic interface or delegate, assignability follows the CLR's variance annotations. Covariant parameters permit `T<Derived>` to flow to `T<Base>`, and contravariant parameters accept `T<Base>` where `T<Derived>` is expected. Raven interface declarations expose the same behaviour with `out` and `in` modifiers on their type parameters, so source and metadata symbols participate in the same set of conversions.

```raven
val a: "true" | 1 = 1      // ok
val b: "true" | 1 = 2      // error: 2 not permitted by '"true" | 1'
val c: "yes" | "no" = "yes"  // ok
```

When a union value is assigned or returned to an explicit target type, the compiler checks each constituent individually:

```raven
val u = if flag { 0 } else { 1.0 }  // int | float

val i: int = u       // error: float not assignable to int
val o: object = u    // ok: both convert to object
```

#### Type inference with control flow

Control-flow expressions that produce different types infer a union of those results. The union is normalized; literal branches collapse into their base if a non-literal of that base also flows.

```raven
val x = if flag { 42 } else { 3 }        // x : 42 | 3
val y = if flag { 42 } else { parseInt() } // y : int   (42 collapses into int)
```

#### Pattern matching and flow-sensitive narrowing

See [Pattern matching](#pattern-matching) for the full set of pattern forms and
for how `is` and `match` narrow union members. Literal unions stay precise so
long as every literal appears in a branch, and open-ended unions require a
catch-all arm or type pattern to remain exhaustive. Guards participate only in
the arm that defines them and do not relax the exhaustiveness checks on the
outer pattern.

#### Member access on unions (nominal CLR members only)

A member access `u.M(...)` on `u : T1 | … | Tn` is permitted **only** when all element types share the **same CLR member origin** via a common base class or interface (same original definition/slot). The intersection remains purely **nominal**: only members declared on the hierarchy can satisfy the rule. Extension methods may still be invoked when in scope, but they do not cause a member to be considered common to the union.

* **What counts as common**: a method/property/indexer/event declared on the nearest common base class or on an interface implemented by **all** `Ti`, where each `Ti` inherits/overrides/implements that same original member.
* **Overloads**: intersect overload sets by original definition; only the common overloads remain available.
* **Properties**: a setter is available only if all elements have a setter; a getter only if all have a getter.

Typing and lowering:

```raven
val msg = u.ToString()   // treat receiver as the ancestor that declares ToString
// lower: ((Ancestor)u).ToString()
```

If any element **hides** the base member with `new` (different original), or an overload isn’t present on some element, the access is rejected with a diagnostic suggesting a cast or pattern match.

Examples:

```raven
open class Animal { virtual Speak() -> string }
class Dog : Animal { override Speak() -> "woof" }
class Cat : Animal { override Speak() -> "meow" }

val a: Dog | Cat
val sound = a.Speak()    // ok via Animal.Speak
```

```raven
open class Base { virtual P: int { get; set; } }
class D1 : Base { override P : int { get; set; } }
class D2 : Base { override P : int { get; } }

val u: D1 | D2 = D1()
val x = u.P      // ok (getter common)
u.P = 3          // error (setter not common)
```

#### Operations on literal types

Operations generally **widen** literals to their base types unless constant-folding applies:

```raven
val k: 2 | 42 = 2
val z = k + 1       // z : int
```

Enum literals retain the enum type identity; matching respects the enum, not just underlying integral values.

#### Summary of rules (normative)

* Union normalization:

  * Flatten, deduplicate; literal collapse into base only in the presence of that base.
  * `null | T` with a single `T` converts to `T?`; explicit `T? | U` is invalid.
* Assignability:

  * `S → (T1 | … | Tn)` if `S → Ti` for some `i`.
  * `(T1 | … | Tn) → S` if `Ti → S` for **all** `i`.
  * Finite literal unions accept only matching constants.
* Flow narrowing:

  * `is`/`==` with literals narrows to the accept set in the then-branch and subtracts it in the else-branch; `match` arms behave analogously.
* Member access:

  * Allowed iff a single base/interface original member is common to **all** elements; overloads intersect by original definition; lower via an ancestor cast and virtual/interface call.
  * Extension methods are discovered separately. They may be invoked when in scope and applicable to the receiver, but they do not satisfy the nominal intersection requirement above.

For .NET representation and lowering strategies (switches, interface dispatch), see [implementation notes](dotnet-implementation.md#union-types).

### Nullable types

Appending `?` to a type denotes that it may also be `null`. This works for
both reference and value types.

```raven
val s: string? = null
val i: int? = null
```

Nullable types participate in the type system and overload resolution.

### Null-conditional access

The null-conditional operators `?.` and `?` safely access members, elements, or
invoke nullable values. The expression `expr?.Member` evaluates `expr`; if the
result is `null`, the overall expression evaluates to `null` instead of
throwing. When the receiver is not `null`, the member access proceeds normally.
These operators work for both nullable reference types and nullable value types.

```raven
var str = x?.ToString()

val number: int? = 42
val digits = number?.ToString() // "42"
```

Here `str` is `string?`, and the call to `ToString` only occurs when `x` is not
`null`. When the receiver is a nullable value type, the compiler unwraps the
`System.Nullable<T>` storage, invokes the member on the underlying value, and
wraps the result back into a nullable type.

Null-conditional access also supports direct invocations and element access.
Use `?(...)` to invoke a nullable delegate or callable value, and `?[...]` to
index into a nullable collection or array.

```raven
val f: Func<int, int>? = null
val result = f?(2)

val values: int[]? = null
val first = values?[0]
```

Invoking or indexing a nullable value without a null-conditional operator
produces a diagnostic, since the receiver may be `null`.

The compiler also performs local flow analysis to recognize when a nullable
receiver is proven non-null. In the `true` branch of checks like `if value != null`
or `if value is not null`, or after guard statements like `if value == null { return }`,
member access and invocation on `value` are allowed without `?.` or `?`.

```raven
val f: Func<int, int>? = null

if f != null {
    val result = f(2)
}

if f == null {
    return
}

val result2 = f(2)
```

### Enums

An enum declaration introduces a distinct type whose instances are one of a
fixed set of named constants. Each member is implicitly static and has an
underlying integer value starting at `0` and increasing by one. Explicit numeric
values are not yet supported.

```raven
enum Grades { A, B, C }
```

Enum members can be referenced with the type name or, when the target type is
known, with a **leading dot**:

```raven
var grade: Grades = .B
grade = Grades.C
```

Line-continuation details for leading-dot forms are defined in
[Control flow: Line continuations](control-flow.md#line-continuations).

Importing the members of an enum brings them into scope:

```raven
import Grades.*
val best = A
```

### Discriminated unions

A discriminated union declaration defines a value type composed of a fixed set
of **cases**. Union values are stored inline (as value types) and do not allocate
on the managed heap. Each case acts like an inline constructor with an optional
payload described by a parameter list. Unions use the `union` keyword:

> ❗ Declared `union` types are nominal **tagged unions** (also called
**discriminated unions**). They are separate from
inferred/annotated type unions written with `|`.

```raven
union Token {
    Identifier(text: string)
    Number(value: int)
    Unknown
}
```

Unions may declare type parameters (`union Result<T> { ... }`). Each case shares
the union's type parameters, and can be referenced either via the type name or
with the leading-dot shorthand:

```raven
val token = Token.Identifier("foo")
val other: Token = .Unknown
```

Line-continuation details for leading-dot forms are defined in
[Control flow: Line continuations](control-flow.md#line-continuations).

Each case becomes a nested struct (`Token.Identifier`, `Token.Number`, …) whose
constructor parameters mirror the payload declared on the case. Supplying the
enclosing type name is always allowed; omitting it (for example, `.Unknown`)
requires the context to already expect the containing union so the compiler can
resolve which declaration supplies the case.

For every case `Case`, the compiler synthesizes an implicit conversion
`Case -> Token`. Assigning, returning, or passing a case value therefore
automatically produces the union instance:

```raven
val ok: Result<int, string> = .Ok(99)          // implicit Case -> Result<int, string> conversion
val err = Result<int, string>.Error("boom")
Console.WriteLine(ok)
```

Each case struct also exposes its payload via `get`-only properties and a
`Deconstruct(out ...)` method matching the payload order. These synthesized
members make deconstruction and positional patterns available in Raven and
improve interoperability with other .NET languages.

Pattern matching exhaustively checks every case; see
[Pattern matching](#pattern-matching) for examples of the leading-dot syntax
inside `match` expressions.

## Object-oriented types

For guidance on declaring classes, structs, members, and interfaces, see
[Classes, structs, and interfaces](classes-and-members.md).

## Operators (precedence summary)

Lowest → highest (all left-associative unless noted):

1. Assignment: `=  +=  -=  *=  /=  %=  &=  |=`
2. Null-coalescing: `??`
3. Logical OR: `||`
4. Logical AND: `&&`
5. Bitwise OR: `|`
6. Bitwise AND: `&`
7. Equality: `==  !=`
8. Relational: `<  >  <=  >=`
9. Type tests: `is  as` (binds after relational)
10. Range: `..` (boundaries optionally prefixed with `^`)
11. Additive: `+  -`
12. Multiplicative: `*  /  %`
13. Cast: `(T)expr`
14. Unary (prefix): `+  -  !  typeof`
15. Postfix trailers: call `()`, member `.`, index `[]`

> **Disambiguation notes**
>
> * `(<expr>)` is a **parenthesized expression** unless a comma appears (including trailing), in which case it’s a **tuple**.
> * `<` starts **type arguments** only in a **type context**; elsewhere it’s the less-than operator.
> * The LHS of assignment must be either an **assignable expression** (identifier, member access, element access, etc.) or a
>   **pattern** such as a positional deconstruction.
> * `^` index expressions are parsed as an adjacent prefix form (`^expr`); whitespace between `^` and the operand is not allowed.

## Outstanding questions and suggested follow-ups

The current implementation leaves a few behaviours underspecified. The following
items capture those gaps and outline the preferred direction for addressing them:

- **`if` expressions without `else` in value contexts.** The compiler currently
  allows `val x = if cond { 1 }` even though the false branch does not yield a
  value, which in turn produces incomplete IL. Either require an `else` branch
  when the result is consumed or implicitly supply `()` for the missing branch.
  Both options should be accompanied by diagnostics that guide authors toward
  the intended usage.
- **Element type discovery for `for` loops.** Non-array collections default the
  iteration variable to `object`, losing type information. Extend binding to
  inspect `IEnumerable<T>`/`IAsyncEnumerable<T>` or the enumerator pattern so the
  loop variable reflects the element type instead of falling back to `object`.
 
