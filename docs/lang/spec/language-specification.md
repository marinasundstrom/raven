# Language specification

This page is the entry point to the Raven language specification. It defines
the shared conventions used across the spec set and links to the chapter files
that describe the language in detail.

Implementation details describing how Raven projects map to .NET are documented
in [dotnet-implementation.md](dotnet-implementation.md).

An overview of available types, literal semantics, and conversions can be found
in the [type system](type-system.md).

Attached macro syntax and expansion rules are documented in
[macros.md](macros.md).

Recommended source layout and spacing conventions are documented in the [style
guide](../style-guide.md).

## Specification structure

Use this page for global rules that apply across the language. Use the chapter
pages for the feature-specific normative text:

* [Type system](type-system.md)
* [Classes, structs, and interfaces](classes-and-members.md)
* [Control flow](control-flow.md)
* [Error handling](error-handling.md)
* [Macros](macros.md)
* [.NET implementation notes](dotnet-implementation.md)
* [Grammar (non-normative EBNF)](grammar.ebnf)

## Document conventions

* **Normative requirements** use key words such as “must”, “may”, and “should” to describe observable language behaviour.
* Notes and tips highlight rationale, examples, or implementation remarks. They are informative rather than normative.
* Code snippets use the `.rav` file extension and omit surrounding boilerplate unless it is essential to the rule being described.
* When behaviour is intentionally unspecified or still under design, this
  specification calls it out explicitly.
* Callout boxes use a small, consistent emoji set:
  * `ℹ️` **Info** for factual clarification and context.
  * `⚠️` **Warning** for pitfalls or behavior likely to surprise.
  * `❗` **Important** for distinctions that affect correctness or interpretation.
  * `🧭` **Disambiguation** for parser/binder interpretation rules.
  * `❓` **Open Question** for unresolved design choices that need follow-up.

## Code samples

Representative sample programs live in the repository's [`samples/`](../../../samples/) directory alongside runnable CLI demos.
Each sample intentionally exercises language features such as pattern matching, pipe operators, tuple flow, and .NET interop so
changes to the language can be validated with real code, not just unit tests. The top-level `samples/README.md` explains how to
run them with the Raven CLI.

## Proposals

Future language proposals live [here](../proposals/).

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
| Reserved | `and`, `as`, `await`, `base`, `bool`, `break`, `byte`, `catch`, `char`, `class`, `const`, `continue`, `decimal`, `default`, `double`, `else`, `enum`, `false`, `finally`, `fixed`, `float`, `for`, `func`, `goto`, `if`, `int`, `interface`, `is`, `let`, `long`, `match`, `new`, `nint`, `not`, `null`, `nuint`, `object`, `or`, `permits`, `return`, `sbyte`, `self`, `short`, `sizeof`, `string`, `struct`, `throw`, `true`, `try`, `typeof`, `uint`, `ulong`, `ushort`, `var`, `when`, `while`, `yield` |
| Contextual | `abstract`, `alias`, `explicit`, `final`, `get`, `implicit`, `import`, `in`, `init`, `internal`, `namespace`, `open`, `operator`, `partial`, `out`, `override`, `private`, `protected`, `public`, `ref`, `sealed`, `set`, `static`, `unit`, `use`, `val`, `virtual` |

Reserved keywords are always treated as keywords and therefore unavailable for use as identifiers. Contextual keywords behave like ordinary
identifiers except in the syntactic positions that demand their special meaning—for example, accessibility modifiers
(`public`, `internal`, `protected`, `private`) or accessor modifiers (`get`, `set`). The `partial` keyword is only recognised
when declaring partial types or partial members; see [Partial types and members](classes-and-members.md#partial-types-and-members).

To use a reserved keyword as an identifier, prefix it with `@`. The lexer produces an identifier token whose `Text` still
includes the `@` escape, while the token's `ValueText` omits it. All bound symbols expose the unescaped name, mirroring C#'s
behaviour and ensuring metadata and semantic lookups use the identifier's logical name instead of its escaped form.

```raven
class @int {}

static func @match(@return: int) -> int {
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

#### Diagnostic suppression comments

Raven supports C#-style warning pragmas as directive trivia to suppress diagnostics in source:

* `#pragma warning disable RAV0103`
* `#pragma warning disable RAV9019 RAV9012`
* `#pragma warning restore RAV0103`
* `#pragma warning disable-next-line RAV0103`
* `#pragma warning disable-next-line RAV9019 RAV9012`
* `#pragma warning disable` (suppresses all diagnostics until restore)
* `#pragma warning restore` (restores all diagnostics)
* `// pragma warning disable ...` and `// pragma warning restore ...` are also accepted.

Rules:

* Directives are evaluated in source order.
* A `disable` directive affects diagnostics on that line and subsequent lines.
* A matching `restore` re-enables the specified diagnostic IDs (or all IDs when no IDs are provided).
* `disable-next-line` suppresses the specified diagnostic IDs (or all IDs) for only the following source line.
* These directives are trivia-only; they do not introduce syntax tokens.

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
return `unit`. In .NET, `unit` corresponds to `void` (see [implementation notes](dotnet-implementation.md#unit-type)). The `unit` type participates in generics and tuples like any other type.

## Null and absence

Raven distinguishes nullable values from `unit`:

* `T?` is the canonical way to represent nullable values.
* `unit` (`()`) represents no meaningful result (`void`-like), not nullability.

Carrier types such as `Option<T>` and `Result<T, E>` are described in the
carrier sections of this specification rather than as part of nullability.

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

> ⚠️ **Open Question:** We should decide whether to prefer `val` over `let`.

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
    return Ok       // sugar for `Ok(())`
}
```

The target-typed member form `.Ok` is also valid when the target type is known.

This rule applies uniformly to **all** discriminated unions, not only `Result` or `Option`. The case must declare exactly one constructor parameter whose type is `unit`; cases with additional parameters or non-`unit` payloads still require an explicit argument list.

This mirrors the pattern-matching rule where a bare case such as `Ok` matches
`Ok(())` when the payload is `unit`, with `.Ok` remaining available as the
target-typed shorthand.

### Type inference

When an expression or declaration omits an explicit type, Raven infers one from
the expression. If multiple different types can flow to a location—through
conditional branches or early `return` statements—the inferred result is the
nearest compatible type for the flow.

```raven
val pet = if flag { Dog() } else { Cat() }
// pet has an inferred compatible type
```

Literal expressions infer the underlying primitive type when used to initialize
`val` or `var` bindings. Literal types are subset types of their underlying
primitive, so a literal like `1` can be used wherever an `int` is expected.
When inference gathers multiple results—such as the branches of an `if`
expression—it keeps literal precision only when required by explicit
annotations; otherwise literals widen to their underlying primitive types.
To retain a literal's singleton type for a single value, an explicit annotation
is required.

```raven
var i = 0       // i : int
val j = 0       // j : int
var k: 1 = 1    // k : 1
```

Control-flow expressions participate in the same inference. An `if` expression
whose branches produce different types infers the nearest compatible type for
those results. Literal branches remain precise only when explicitly annotated.
By default, literal branches widen to their underlying primitive types:

#### Branch type inference

```raven
val x: int = 3
val value = if x > 2 { 42 } else { x }
// value : int

val other: long = 0
val widened = if x > 2 { other } else { 42 }
// widened has an inferred compatible numeric type
```

Each branch contributes its inferred type. Literal branches widen as needed to
produce a compatible inferred result type.

Numeric literals choose an underlying primitive type according to their form
and optional suffix. The default rules are designed to be predictable while
still allowing safe narrowing through implicit *constant* conversions.

#### Integer literals

* **Unsuffixed integer literals** default to `int`.
  * If the value does not fit in `int`, the literal is typed as `long`.
* **Base prefixes**:
  * `0b` / `0B` — binary integer literal
  * `0x` / `0X` — hexadecimal integer literal
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
val bits = 0b1010_0101 // binary int literal
val mask = 0xFF  // hexadecimal int literal
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

Functions and methods without an annotated return type default to `unit`; the
declaration return type is not inferred from body expressions.

Lambdas without an annotated return type infer their result by collecting:

1. the types of all explicit `return` statements, and
2. the final expression of the outer body when that body has a value-producing tail expression.

Expression statements in nested statement blocks (for example, inside `if`/`while`/`for`
statement bodies) do not participate in lambda return-type inference. If no value-returning
path exists, the lambda return type defaults to `unit`.

```raven
val example = (x: int) -> {
    if x > 0 { return x }
    "neg"
}
// inferred return type is context-dependent
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

Async function expressions mirror async functions: placing `async` before the parameter
clause (or using `async func`) permits `await` inside the body. When the function expression return type is not annotated
and no delegate supplies one, the compiler wraps the inferred result in
`System.Threading.Tasks.Task<T>` (or `Task` when the body produces `unit`). A delegate
annotation or target type may still specify a concrete `Task` shape, in which case the
function-expression body must evaluate to the awaited result type rather than the task itself.
Annotating an async function expression with a non-`Task` return type is an error.

### Additional type inference rules (normative)

The following clarifications extend the type inference model:

* **Contextual inference**: Raven computes a contextual type based on both expression shape and target type. Inference is bidirectional.
* **Literal arithmetic**: Non-constant operations widen literals to their base type unless constant-folded.
* **Generic inference**: Type argument inference requires a single consistent set of type arguments that satisfies all constraints.
* **Nullability**: Nullable flow follows `T?` rules and safe-navigation propagation.
* **Pattern narrowing**: See [Pattern matching](#pattern-matching) for how `is` and `match` refine variables.
* **Tuples**: Tuple element names do not affect type identity.
* **Ref/out parameters**: `ref` requires exact type match; `out` contributes to inference of the parameter type.
* **Flow stability**: Variable declarations have a fixed declared type. Narrowings are ephemeral and do not change declared type. Captured variables use the join of all flows.
* **Diagnostics**: When conversion fails, diagnostics should identify the conflicting conversions and why. For overloads, diagnostics should explain alternative selections.

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
provides an accessible `IsCompleted: bool` property and a parameterless
`GetResult()` method. If the awaiter’s `GetResult` produces no value, the await
expression’s type is `unit`; otherwise it matches the `GetResult` return type.

Failing any of these requirements produces a compile-time diagnostic identifying
the missing member. The compiler also reports an error when `await` appears
outside an async context.

Evaluation first computes the operand value and calls `GetAwaiter()` to obtain
the awaiter. If `val IsCompleted` is `true`, `GetResult()` is invoked immediately
and the await expression yields its value. Otherwise execution is suspended and
later resumed when the awaiter signals completion; resumption continues after
the `await` with the result of ` GetResult()`.

### Try expressions

`try` captures exceptions as values. `try?` combines capture with carrier
propagation.

| Form | Result type | Success case | Failure case |
| --- | --- | --- | --- |
| `try expr` | `Result<T, Exception>` | `.Ok(value)` or `.Ok(())` | `.Error(exception)` |
| `try? expr` | `T` inside an enclosing carrier-returning context | yields the success payload | propagates the captured error through the enclosing `Result`/`Option` |

#### Concept

`try expr` evaluates `expr` exactly once and converts the outcome to
`Result<T, Exception>`, where `T` is the operand type. If the operand has type
`unit`, the success case is `Ok(())`.

`try? expr` is shorthand for `(try expr)?`. It is valid only when the enclosing
function or lambda returns a compatible `Result<_, _>` or `Option<_>`.

#### Example

```raven
import System.*

func describeNumber(text: string) -> string {
    return try int.Parse(text) match {
        Ok(val value) => "Parsed: $value"
        Error(FormatException ex) => "Invalid format: ${ex.Message}"
        Error(_) => "Unexpected failure"
    }
}

func parseRequiredInt(text: string) -> Result<int, Exception> {
    val value = try? int.Parse(text)
    return .Ok(value)
}

func saveText(path: string, text: string) -> string {
    return try System.IO.File.WriteAllText(path, text) match {
        Ok => "Saved"
        Error(UnauthorizedAccessException ex) => "Access denied: ${ex.Message}"
        Error(IOException ex) => "I/O error: ${ex.Message}"
        Error(_) => "Unexpected failure"
    }
}
```

#### Rules

* The operand may be any expression that is valid in the current context.
* `try expr` does not accept `catch` or `finally` clauses; use statement-form
  `try` for structured exception handling.
* Nested `try` expressions are invalid and produce `RAV1906`.
* A trailing `match` after `try?` is invalid and produces `RAV1908`.
* `await` may appear inside `try expr` when the enclosing context is async.
* In pattern position, `Ok` is shorthand for `Ok(())` when the success payload
  is `unit`; `.Ok` remains available as the target-typed shorthand.

### Result and Option carrier operators

`Result<T, E>` and `Option<T>` share the same carrier terminology throughout the
spec: `?` unwraps or propagates, and `?.` conditionally maps over the success
case.

| Form | Receiver | Result | Empty or error case |
| --- | --- | --- | --- |
| `expr?` | `Result<T, E>` | `T` | propagates `.Error(error)` |
| `expr?` | `Option<T>` | `T` | propagates `.None` |
| `expr?.Member` | `Result<T, E>` | `Result<U, E>` | preserves `.Error(error)` |
| `expr?.Member` | `Option<T>` | `Option<U>` | preserves `.None` |

#### Propagation (`?`)

##### Concept

The postfix `?` operator unwraps a carrier value and propagates the non-success
case to the nearest enclosing carrier-returning function or lambda.

##### Example

```raven
func loadAndParse(path: string) -> Result<int, Exception> {
    val text = ReadAllText(path)?
    val value = ParseInt(text)?
    return .Ok(value)
}

func firstEven(values: int[]) -> Option<int> {
    val value = FindFirstEven(values)?
    return .Some(value)
}
```

##### Rules

* For `Result<T, E>`, `.Ok(value)` yields `value` and `.Error(error)`
  immediately returns `.Error(error)` from the enclosing context.
* For `Option<T>`, `.Some(value)` yields `value` and `.None` immediately returns
  `.None` from the enclosing context.
* `expr?` is valid only when the enclosing function or lambda returns a
  compatible `Result<_, _>` or `Option<_>`.
* The operand is evaluated exactly once; the compiler may introduce temporaries
  to preserve that rule.
* `expr?` performs propagation only when `?` is not followed by a trailer. In
  postfix position, `expr?.Member`, `expr?(...)`, and `expr?[...]` are parsed as
  conditional-access forms instead.

If propagation relies on a user-defined implicit conversion on the error
channel, the compiler reports informational diagnostic `RAV1506`.

#### Conditional member access (`?.`)

##### Concept

Carrier conditional access maps a member access over the success case of a
carrier without unwrapping the carrier itself.

##### Example

```raven
record class User(Name: string, Item: Option<Item>)
record class Item(Name: string)

union LookupError {
    case MissingUser
    case MissingItem
}

func getUser() -> Result<User, LookupError> {
    return .Error(.MissingUser)
}

func userNameLength() -> Result<int, LookupError> {
    val length = getUser()?.Name?.Length?
    return .Ok(length)
}

func selectedItemName() -> Result<string, LookupError> {
    val maybeItem = getUser()?.Item?

    return maybeItem match {
        .Some(val item) => .Ok(item.Name)
        .None => .Error(.MissingItem)
    }
}
```

##### Rules

* For `Result<T, E>`, `expr?.Member` evaluates `Member` only when `expr` is
  `.Ok(payload)` and returns `Result<U, E>`.
* For `Option<T>`, `expr?.Member` evaluates `Member` only when `expr` is
  `.Some(payload)` and returns `Option<U>`.
* The member-access form is the only lifted carrier conditional-access form.
* `expr?[index]` and `expr?(args)` are not lifted for `Result` or `Option`.
* If indexing or invocation is required, unwrap first with `?`, pattern
  matching, or an explicit helper API.

For nullable/reference receivers, Raven also supports null-conditional member
assignment in statement position:

```raven
person?.Name = "Ada"
counter?.Value += 1
```

These forms evaluate the receiver once, execute the assignment only when the
receiver is non-null, and otherwise do nothing.

#### End-to-end example

The following example shows `try`, propagation, and carrier conditional access
working together:

```raven
record class User(Name: string)

union LookupError {
    case Io(message: string)
    case InvalidUser
}

func readNameLength(path: string) -> Result<int, LookupError> {
    val text = try System.IO.File.ReadAllText(path) match {
        .Ok(val content) => .Ok(content)
        .Error(val ex) => .Error(.Io(ex.Message))
    }?

    val length = parseUser(text)?.Name?.Length?
    return .Ok(length)
}

func parseUser(text: string) -> Result<User, LookupError> {
    if text == "" {
        return .Error(.InvalidUser)
    }

    return .Ok(User(text))
}
```

### Standard carrier helper APIs (Raven.Core)

Raven ships `Option<T>`, `Result<T, E>`, and related extension helpers in
`Raven.Core`. These are library APIs (not syntax), but they are part of the
standard language experience and are expected by diagnostics, samples, and
tooling.

`Option<T>` and `Result<T, E>` are defined in `Raven.Core` as `union class`
carriers. This is intentional: the standard carriers follow the conventional
`.NET` union contract without exposing the extra default/uninitialized state
that exists for `union struct` values.

#### `Option<T>` helpers

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

#### `Result<T, E>` helpers

- State checks: `HasOk`, `HasError`
- Channel projection: `IsOk`, `IsError`
- Mapping/composition: `Map`, `Then`, `MapError`, `OrElse`
- Pattern/value helpers: `Match`, `Tap`, `TapError`
- Unwrap helpers: `UnwrapOrElse`, `UnwrapOrDefault`, `UnwrapOrThrow`,
  `UnwrapOr(defaultValue)`, `UnwrapError`
- Enumeration helpers: `ToEnumerable`, `GetEnumerator`

#### Carrier LINQ extensions on `IEnumerable<T>` (`System.Linq`)

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

For unions, an explicit cast from the carrier to a member/case type is also
permitted as an assertion-style extraction:

```raven
val value: Either<int, string> = 42
val left = (int)value
```

This conversion succeeds only when the carrier currently holds the requested
member/case type; otherwise it throws `InvalidCastException`. It is an explicit
union extraction conversion, not a subtype or inheritance conversion.

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

### Encoded string literals

An encoded string literal applies an encoding suffix directly to a string or
multiline string literal and produces bytes instead of a `string`.

```raven
val utf8 = "Hello"u8
val ascii = "Hello"ascii
```

The suffix must appear immediately after the closing delimiter with no
intervening whitespace.

Syntax:

```
encoded_string_literal
    : string_literal encoding_suffix
    ;

encoding_suffix
    : u8
    | ascii
    ;
```

Semantics:

1. Parse the literal normally, including escape decoding.
2. Produce the resulting sequence of Unicode scalar values.
3. Encode that sequence with the specified encoding.
4. Return the encoded bytes as `byte[]`.

Encoded string literals are constant expressions when the underlying string
literal is constant.

Supported encodings:

* `u8` — UTF-8 encoding, emitted without BOM.
* `ascii` — ASCII encoding. If any character is outside the ASCII range
  (`> 0x7F`), the compiler reports an error.

```raven
val data = "Pågen"u8
val ok = "Hello"ascii
val error = "Pågen"ascii // compile-time error
```

Raw/multiline non-interpolated string forms can also be encoded. Interpolated
string literals are not valid with encoding suffixes.

```raven
val ok = """
Hello
World
"""u8

val name = "World"
val error = "Hello ${name}"u8 // compile-time error
```

The resulting `byte[]` contains exactly the encoded bytes of the literal text.
The compiler does not add a BOM or null terminator.

Interpolation is intentionally excluded from encoded literals to keep their
behavior deterministic and compile-time-friendly. When text must be computed at
runtime, construct the string first and encode it explicitly through runtime
APIs such as `System.Text.Encoding.UTF8.GetBytes(...)` or
`System.Text.Encoding.ASCII.GetBytes(...)`.

Additional encodings may be introduced by adding new suffixes.

### Collection expressions

Collection expressions use bracket syntax `[element0, element1, ...]` (with an optional
trailing comma) to build list-like collection types. Adjacent elements may also be
separated by a newline, in which case the syntax tree records `SyntaxKind.None` in the
separator slot. Raven also supports an explicit array form `[|element0, element1, ...|]`
with the same separator rules. Elements are evaluated from left to right.
In addition to ordinary expressions, an element may be written as
`...expression`—called a *spread*. Spreads enumerate the runtime value and insert each
item into the resulting collection in order. The spread source must be convertible to
`System.Collections.IEnumerable` (including arrays and `IEnumerable<T>` implementations);
otherwise diagnostic `RAV2022` is reported. 【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L3620-L3670】【F:src/Raven.CodeAnalysis/DiagnosticDescriptors.xml†L260-L266】

Within a collection expression, a bare range element such as `1..3` or `1..<4` also
expands inline as a sequence of values rather than contributing a single `Range`
instance. This form follows the same iteration semantics as a comprehension sourced
from that range, so `..` is inclusive and `..<` excludes the upper bound.

Collection expressions also support a list-comprehension form:

* `[for item in source => selector]`
* `[for item in source if condition => selector]`
* `[for val (a, b) in source => selector]` and other pattern targets follow the same
  matching/binding rules as `for` statements.

Collection expressions also support dictionary-shaped elements:

* `key: value` inserts one entry,
* `...expr` spreads entries from another dictionary-compatible source,
* `...key: value` inserts one entry in spread position, and
* `for item in source => key: value` / `for item in source if condition => key: value`
  build entries through a dictionary comprehension.
  Comprehension targets may also be patterns, including deconstruction patterns.

When a collection expression contains any dictionary-shaped element, the entire
literal is treated as dictionary-shaped. Positional elements, range elements,
and value-producing collection comprehensions cannot be mixed into that same literal.

The `source` position also accepts range expressions. These follow the same range
iteration semantics as `for ... in start..end` loops.

Comprehensions are lowered by the compiler into collection-building loops, so they
follow the same target-typing and conversion rules as other collection elements.
Pattern-targeted comprehensions also inherit `for`-statement matching semantics:
non-matching elements are skipped, an optional `if` filter runs after the pattern
match succeeds, and outer `let` / `val` / `var` binding keywords supply the binding
mode for otherwise bare pattern captures.

Collection expressions are target-typed:

* **Array targets** — When the expected type is a one-dimensional array `T[]`, the expression allocates a
  new array of that element type. Each item is implicitly converted to `T` before storage,
  and spreads must enumerate values assignable to `T`. 【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L3672-L3738】【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L950-L1016】
  When the expected type is a fixed-length array `T[N]`, Raven also validates the statically
  known element count when it can prove one. Plain elements contribute `1`, and spreading a
  fixed-length array `T[M]` contributes `M`. If the proven total does not match `N`, binding
  reports a size-mismatch diagnostic instead of deferring the error to runtime.
  Multidimensional array types such as `T[,]` are not target-typed by collection/array literal
  syntax; they must be created through runtime APIs or other existing values and then used
  through normal indexing/assignment syntax.
* **Collection targets** — When the expected type is a non-array type with an accessible
  parameterless constructor and an instance `Add` method, the compiler constructs the
  target and calls `Add` for every element. The `Add` parameter determines the element
  conversions, and spread entries must supply compatible values. 【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L3738-L3776】【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L1016-L1096】
  Dictionary-shaped collection expressions use the same builder model, but require an
  accessible instance `Add(key, value)` method instead. Each key expression is converted
  to the first parameter type and each value expression is converted to the second.
  Dictionary spreads require a source compatible with `IEnumerable<KeyValuePair<TKey, TValue>>`
  after key/value conversion.
* **No target type** — Without an expected type, Raven infers a best common element type by
  merging all element contributions (spreads use their enumerated element type). The resulting
  collection kind then defaults from the literal modifiers:
  * bare collection expressions produce `ImmutableList<T>`;
  * `![...]` collection expressions produce `List<T>`;
  * `[| ... |]` array expressions produce CLR arrays.
  Spreads and range elements contribute element types only; they do not change the default
  collection kind. If no compatible common element type can be inferred without falling back to
  `object`, `System.ValueType`, or interfaces, inference fails with a type-mismatch diagnostic
  and an explicit target type is required.
    【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L3776-L3861】
  Dictionary-entry collection expressions instead infer key and value types separately. Bare
  literals default to `ImmutableDictionary<TKey, TValue>`, while `![...]` dictionary-entry
  literals default to `Dictionary<TKey, TValue>`.

Collection expressions also carry optional literal syntax:

* bare literals are immutable-by-default;
* `![...]` requests a mutable default for targetless inference;
* `[| ... |]` requests explicit CLR-array fallback instead of list-family fallback;
* explicit target typing may still override mutability and collection kind.

The intent of this design is to make local collection literals default toward immutable
data-processing code. Mutable collection creation remains available, but it must be made
intentional either through the `!` marker or through an explicit mutable target type.

When no explicit target type is present, bare collection expressions fall back to
`ImmutableList<T>`, `![...]` expressions fall back to `List<T>`, and `[| ... |]`
expressions fall back to CLR arrays.
If code wants another concrete collection type such as `Queue<T>` or `Stack<T>`, it must
provide an explicit target type so Raven can bind the literal through the normal array or
collection-builder rules. The choice between explicit commas and implicit newline separators
does not affect the inferred collection kind.

The current targetless default matrix is therefore:

* `[a, b]` -> `ImmutableList<T>`
* `[
      a
      b
  ]` -> `ImmutableList<T>`
* `![a, b]` -> `List<T>`
* `![
      a
      b
  ]` -> `List<T>`
* `[|a, b|]` -> `T[N]` / `T[]`

An empty collection expression `[]` must be used in a context that supplies a target type;
otherwise its type cannot be inferred. When a target type is available, the compiler
produces an empty instance of that type (an empty array or an initialized collection).
【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L3620-L3651】【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L1170-L1192】

```raven
val numbers: int[] = [1, 2, 3]
val combined = [0, ...numbers, 4]
val squares = [for n in numbers => n * n]
val evenSquares = [for n in numbers if n % 2 == 0 => n * n]
val evenSquaresInRange = [for n in 4..250 if n % 2 == 0 => n * n]
val selectedNames = [for val (2, name) in [(1, "Ada"), (2, "Bob")] => name]

val names: List<string> = ["a", "b"]
val inferred = [1, 2.0]      // inferred as ImmutableList<double>
val inferredList = [1; 2; 3] // inferred as ImmutableList<int>
val mutableList = ![1, 2, 3] // inferred as List<int>
val mutableListAlt = ![1; 2; 3]  // inferred as List<int>
val inferredArray = [|1, 2, 3|] // inferred as int[3]
val expandedArray = [|...inferredArray, 4|] // inferred as int[4]

val byName = ["a": 1, "b": 2] // inferred as ImmutableDictionary<string, int>
val mutableByName = !["a": 1, "b": 2] // inferred as Dictionary<string, int>
val merged = [..."a": 1, ...mutableByName, "c": 3]
val lengths = [for key in [|"a", "bb"|] => key: key.Length]
val doubled = [for val (key, value) in [("a", 1), ("b", 2)] => key: value * 2]
val readonlyLookup: IReadOnlyDictionary<string, int> = ["a": 1, "b": 2]

val baseList: ImmutableList<int> = [2; 3; 4]
val preserved = [7, ...baseList, 5] // inferred as ImmutableList<int>

val a: ImmutableList<int> = [1]
val b: List<int> = [2]
val defaulted = [...a, ...b] // inferred as ImmutableList<int>

val forced: List<int> = [...a, ...b]
val forcedObject: object[] = [1, true]
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

Parameters may also be declared as collectors using a trailing `...` after the
parameter type. A collector parameter must be the trailing parameter.

For convenience syntax (`items: T ...`), the declared parameter type defaults to
`IList<T>` in semantic binding.

If you need to control the exact collection type, use explicit `params` syntax,
for example `params items: int[]` or `params items: IEnumerable<int>`.

At call sites, any extra positional arguments are packed into the collector.
Calls may also use `...expr` in
argument position to expand an existing sequence into the collector.

Raven also supports an explicit collector marker with `params`:
`params items: IEnumerable<int>`. The `...` form and `params` are mutually
exclusive on the same parameter.

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
func makePoint(x: int, y: int, label: string = "origin") -> string {
    return "$label: ($x, $y)";
}

func sum(items: int ...) -> int {
    return items.Length;
}

func sum2(params items: int[]) -> int {
    return items.Length;
}

val swapped = makePoint(y: 2, x: 1);
val mixed = makePoint(3, label: "axis", y: 0);
val invalid = makePoint(x: 1, 2);  // error: positional argument cannot follow `x:`
val a = sum(1, 2, 3);
val xs = [4, 5];
val b = sum(...xs);
```

The compiler binds each named argument to its declared parameter. The call to
`makePoint` named `mixed` demonstrates that positional arguments may precede the
first named argument, while the `invalid` call is rejected because it attempts
to supply a positional argument (`2`) after specifying `x` by name.

#### Object initializers

An **object initializer** is written as a `with` block on a type expression. The initializer selects the type's parameterless constructor, constructs the instance, and then applies the initializer entries in source order.

```raven
val window = Window with {
    Title = "Main"
    Width = 800
    Height = 600
}
```

Init-only accessors are treated as initializer-only members, so they may be assigned in object initializers:

```raven
class Settings {
    val Theme: string { init; }
    val FontSize: int { init; }
}

val settings = Settings with {
    Theme = "Dark"
    FontSize = 14
}
```

Initializer bodies consist of a sequence of **member entries**. A member entry assigns to a writable property, field, or event using `Name <assignment-operator> Expression`.

* `=` is valid for writable fields/properties.
* Compound assignment operators (for example `+=`) are supported and are evaluated as member compound assignment on the initialized instance.
* Events use `+=` / `-=` in initializer context, matching statement assignment rules.

Event subscription is valid in object initializers:

```raven
val button = Button with {
    Clicked += () => WriteLine("clicked")
}
```

Property entries are applied to the newly created instance in source order.

Brace trailers after expressions are reserved for DSL-style blocks and are represented in the syntax tree as trailing block expressions, not object initializers. Until a type opts into DSL trailing blocks, the compiler reports an error for `Type { ... }` and `value { ... }`; use `Type with { ... }` for object initialization and `value with { ... }` for non-destructive updates.

#### Required members and init semantics

Members marked as **required** must be definitely assigned during object construction. A required member may be a field or property. Required members participate in object initializer checking and influence which constructors are considered complete.

```raven
class Person {
    required val Name: string { init; }
    required val Age: int { init; }
}

val p = Person with { Name = "Ada", Age = 36 }   // ok
val q = Person with { Name = "Ada" }            // error: Age must be set
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

Primary-constructor behavior is intentionally split:

1. `class`/`struct`: parameters marked with `val` or `var` are promoted to properties; parameters without a binding keyword are captured in compiler-generated private storage for member access, but are not promoted to public properties. Promoted parameters may include an access modifier (`public`/`internal`/`protected`/`private`) before `val`/`var` to set synthesized property accessibility (default `public`).
2. `record class`/`record struct`: positional parameters define the record's public data shape via synthesized properties and value members. When no binding keyword is present, record parameters are promoted as `val` properties by default. Value-shape synthesis (`Equals`, `GetHashCode`, `Deconstruct`, `ToString`, record copy/with flow, and equality operators) includes only **public** promoted properties; non-public promoted properties are excluded and produce a compiler warning.
3. Constructor calls require invocation syntax (`Foo(...)` or `Foo()`); a standalone type name (`Foo`) is not a value expression, while `Foo with { ... }` is the parameterless object-initializer form.
4. Member declarations cannot reuse the immediate containing type's name.

For semantic-model queries, unqualified identifier access to those captured/promoted members resolves to the originating primary-constructor parameter symbol.

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

With expressions update record values by producing a new record instance while
preserving the original:

```raven
record Point(X: int, Y: int)

val origin = Point with { X = 0, Y = 0 }
val moved = origin with { X = 10 }
```

For records, synthesized copy/clone behavior follows the record value shape and therefore includes only public promoted properties; non-public promoted properties are not copied by synthesized record copy semantics.

When binding a with expression, the compiler selects the first applicable strategy in the following order:

1. **Record clone** — Record types clone first, then apply assignments as initializer-style member assignments.
2. **`Update(...)` convention** — An instance method named `Update` whose parameter names correspond to readable members on the receiver. Each parameter receives either the provided assignment value (if present) or the receiver's current member value.
3. **`With(...)` convention** — Same as `Update`, but with an instance method named `With`.
4. **`WithX(...)` chaining** — For each assignment `X = expr`, invoke a single-parameter method named `WithX`. Methods are invoked in source order, and each invocation receives the assignment's value expression.
5. **Clone/copy fallback** — Use a parameterless `Clone()` method or a copy constructor (one parameter of the receiver type) to create a copy, then apply assignments as initializer-style member assignments.

If none of these conventions apply, the compiler reports `RAV0240` to indicate the type does not support `with` expressions.

### Extensions (Traits)

Extensions provide helper members for an existing receiver type without
modifying the original declaration. The `extension` and `trait` keywords declare
the same construct.

An extension declaration is a namespace-scoped container that targets a
specific type via a `for` clause. Importing the container brings its members
into scope for lookup.

An extension or trait declaration may omit its identifier, in which case the
compiler synthesizes a private, mangled container name. Public extensions or
traits must declare an explicit identifier so the container can be referenced
and imported by name. Applying `fileprivate` keeps the declaration file-local and
mangles the emitted metadata name even when the source uses an explicit
identifier.

Extensions and traits may declare type parameters and generic constraints.
These constraints participate in extension resolution: an extension is
applicable only when its type parameters can be inferred and all declared
constraints are satisfied for the receiver type. Extensions whose constraints
are not met are ignored during member lookup.

Examples:

```raven
extension StringExt for string {
    func ToSlug() -> string => self.Trim().ToLowerInvariant().Replace(" ", "-")
}

import MyApp.StringExt.*
val slug = " Hello World ".ToSlug()
```

```raven
extension for string {
    func IsNullOrWhiteSpace() -> bool => self.Trim().Length == 0
}

val empty = "   ".IsNullOrWhiteSpace()
```

```raven
// Constrained extension: applicable only when the receiver satisfies the constraints.
trait ValueSequenceExt<T> for System.Collections.Generic.IEnumerable<T>
    where T: struct {
    func Sum() -> T { /* ... */ }
}

// If the receiver's element type is not a struct, this extension is ignored during lookup.
```

```raven
// Extension property: participates in member lookup like an instance property.
extension ListIntExt for System.Collections.Generic.List<int> {
    var CountPlusOne: int {
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
extension ListStatics for System.Collections.Generic.List<int> {
    static func Empty() -> System.Collections.Generic.List<int> => System.Collections.Generic.List<int>()
    static DefaultCapacity: int { get => 4 }
}

import System.Collections.Generic.*
import MyApp.ListStatics.*

val empty = List<int>.Empty()
val cap = List<int>.DefaultCapacity
```

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
extension StringExt for string {
    func ToSlug() -> string {
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

extension MyEnumerableExt<T> for System.Collections.Generic.IEnumerable<T> {
    func CountItems<B>(arg: T) -> B {
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

Extension classification is determined per member rather than per container.
This matters for mixed extension containers and imported metadata. A single
extension container may therefore contribute both:

* classic instance extension members, which participate in instance-style
  extension lookup and behave like ordinary extension methods/properties, and
* static extension members, which participate only in static member lookup on
  the receiver type.

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
extension ListExt for List<int> {
    var CountPlusOne: int {
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
static members on the extension container. Binding classifies them separately
from classic extension methods even when they live in the same extension
container. As a result, a mixed container may expose `value.Member(...)`-style
instance extensions and `Type.Member(...)`-style static extensions at the same
time without forcing one model onto the other.

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

static class MathHelpers {
    static func Increment(x: int, amount: int) -> int {
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

Inline lambda targets also support implicit invocation. In `value |> x => ...`,
the compiler infers `x` from the piped value type and invokes the lambda with
the left-hand result:

```raven
val length =
    5
        |> x => x.ToString()
        |> text => text.Length
```

Parenthesized inline lambdas are also valid pipeline targets:

```raven
val name = user |> (u => u.Name)
```

When combining lambda targets with additional pipeline stages, parentheses make
the stage boundaries explicit:

```raven
val normalized =
    userOrError
        |> EnsureActive()
        |> (x => x match {
            Ok(val u) => u.Name
            Error(val e) => "ERR: " + e.ToString()
        })
        |> Normalize()
```

The pipe operator accepts an invocation target (explicit or implicit) or a
property access with a setter on the right-hand side. If the syntax does not
form either shape, diagnostic `RAV2800` is issued.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L2690-L2766】【F:src/Raven.CodeAnalysis/DiagnosticDescriptors.xml†L19-L23】

If the pipeline targets a property, Raven assigns the left expression to that property through its setter before producing the property's type as the result of the pipe expression. Both instance and static properties are supported:

```raven
val container = Container()
val _ = 42 |> container.Value
val _ = 42 |> Container.Count

class Container {
    var Value: int { get; set; }
    static var Count: int { get; set; }
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

`..` produces an inclusive-notation `Range` form, and `..<` produces a half-open
form with an exclusive upper bound. Both can be stored, passed to APIs, or used for
slicing receivers that understand .NET ranges. Both endpoints are optional, and
either endpoint may be written as a **from-end** index by prefixing it with `^`.

```raven
val r = 3..^5
val halfOpen = 3..<10
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

> A standalone type name is not a constructor call in value position.
> Use `Foo()` instead of `Foo`.

The object-initializer `with` form may be used for parameterless construction:

```raven
val window = Window with {
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
of an `if` statement or loop, or as a standalone scoped `{ ... }` statement), it is
bound as a block statement and its expression results are discarded.

### `if` expression

`if` expressions evaluate the condition and execute exactly one branch. The
value of the overall expression is the value produced by the executed branch. If
both branches produce values, the result participates in type inference as
described in [Type inference](#type-inference).

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

`while` also supports a pattern-binding condition:

```raven
while val .Ok(value) = Next() {
    Console.WriteLine(value)
}
```

The right-hand expression is evaluated before each iteration. If the pattern
matches, the body executes and pattern bindings are in scope for that iteration.
If the pattern does not match, the loop exits. The leading `let` / `val` / `var`
binding keyword is required and supplies the binding mode for otherwise bare
captures inside the pattern and for an optional whole-pattern designation:

```raven
while val Person(1, name, _) person = NextPerson() {
    Console.WriteLine(person.Name)
    Console.WriteLine(name)
}
```

### `for` expression

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
for (val x, 0) in points {
    Console.WriteLine(x)
}
```

An optional outer binding keyword may appear before the iteration target:

```raven
for val item in items {
    Console.WriteLine(item)
}

for val _ in items {
    log("processing")
}

for val Person(1, name, _) in persons {
    Console.WriteLine(name)
}

for val Person(1, name, _) person in persons {
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
allowed and optional; `var` is rejected. If the element value is unused, the
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
Like other looping constructs, a `for` expression evaluates to `()`.

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
from-end bounds in a `for` expression reports a diagnostic.

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

### Iterator statements (`yield`, `yield return`, `yield break`)

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

## Pattern matching

Patterns let you inspect values using concise, algebraic syntax. They appear in `is`
predicates and in `match` expressions and participate in Raven’s flow-sensitive
type analysis.

Raven uses two related but distinct surfaces:

* **General pattern matching forms** are used in `is`, `match`,
  `if val pattern = expr`, `while val pattern = expr`, and `for ... in`
  pattern targets. These support the full pattern vocabulary:
  declaration/type patterns, constants and value patterns, comparison and range
  patterns, positional patterns, sequence patterns, dictionary patterns,
  property patterns, nominal deconstruction patterns, member/case patterns,
  boolean pattern combinators, and whole-pattern designations where the
  construct allows them.
* **Deconstruction forms** are used in declaration/assignment positions such as
  `val (a, b) = expr`, `(a, b) = expr`, `val [a, b] = expr`, `[a, b] = expr`,
  `val ["x": value] = expr`, and `["x": value] = expr`. These are not general
  match statements. They are extraction-oriented and use the
  positional/sequence/dictionary deconstruction subset with nested captures,
  discards, typed designations, explicit value comparisons where supported, and
  recursive composition.

Patterns can be used in `match` expressions or statements, or as conditions with
`is` patterns:

```raven
val obj: object? = /* ... */

match obj {
    Foo foo => /* Hit Foo case */
    _ => /* Covers remaining cases for object */
}

if obj is Foo foo {
    // foo is assigned, and not null
}

if lookup is ["a": val first, "b": 2] {
    // first is assigned only when both keys exist and "b" maps to 2
}

if val Person { Name: "Ada", Age: age } = obj {
    // age is assigned only when obj is a Person named "Ada"
}
```

Raven also supports statement-form conditional pattern binding:

```raven
if val (id, name) = person {
    WriteLine(name)
}
```

This form is equivalent to testing the right-hand side with `is` while applying
the outer binding keyword to implicit captures inside the pattern:

```raven
if person is (val id, val name) {
    WriteLine(name)
}
```

Typed implicit bindings work the same way, which makes nullable narrowing
available in statement form:

```raven
val input: int? = null

if val x: int = input {
    WriteLine(x)
}
```

The same syntax also works for hierarchy narrowing, just like `if expr is Type name`:

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

The leading `let` / `val` / `var` is required. A bare `if Pattern = expr` form is
not recognized. When a whole-pattern designation omits its own binding keyword,
it inherits the outer `let` / `val` / `var` binding mode.

Statement-form `while` supports the same pattern-binding header:

```raven
while val .Ok(value) = Next() {
    WriteLine(value)
}
```

The right-hand expression is evaluated at the start of each iteration. If the
pattern matches, the body executes with the pattern bindings in scope. If the
pattern does not match, the loop exits. A bare `while Pattern = expr` form is not
recognized; the leading `let` / `val` / `var` binding keyword is required.

### Dictionary patterns and deconstruction

Dictionary patterns use bracket syntax with keyed entries:

```raven
if lookup is ["a": val first, "b": 2] {
    WriteLine(first)
}
```

Each dictionary-pattern entry evaluates its key expression and then matches the
corresponding value against the nested pattern. A dictionary pattern succeeds
only when:

* the scrutinee is dictionary-compatible,
* every requested key exists, and
* every nested value pattern succeeds.

Dictionary compatibility is defined in terms of the dictionary surfaces Raven
binds directly today: `IDictionary<TKey, TValue>`,
`IReadOnlyDictionary<TKey, TValue>`, and types that implement one of those
interfaces. Key expressions are converted to `TKey`; nested patterns are bound
against `TValue`.

Dictionary deconstruction uses the same keyed syntax in declaration or
assignment position:

```raven
val values: IReadOnlyDictionary<string, int> = ["a": 2, "b": 3]
val ["a": first, "b": second] = values
```

This is extraction-oriented rather than boolean. After validating that the
source is dictionary-compatible, each entry reads the requested key and assigns
the resulting value through the nested designation/pattern.

Only `match` participates in exhaustiveness checking.

### Exhaustiveness

A `match` expression is **exhaustive** when its set of arms covers every
possible runtime value of the input expression (the *scrutinee*).

When a `match` expression (and statement form) is exhaustive, a discard arm (`_ => ...`) is
unnecessary and is reported as unreachable.

Exhaustiveness is determined by analyzing the *runtime value space* implied by
the scrutinee’s static type. The compiler can prove exhaustiveness without a
discard arm when that value space is finite or otherwise fully covered by the
provided patterns.

Exhaustiveness analysis applies in particular to:

* **Discriminated unions** declared with the `union` keyword.
* **Enums**.
* **`bool`** (`true` and `false`).
* **Sealed hierarchies** — a `sealed` class with a `permits` clause (or
  otherwise participating in a closed inheritance set). Closed branches
  contribute concrete leaves; open intermediate branches must be covered by
  matching the intermediate type.

For discriminated unions, exhaustiveness is computed from the union's declared
case set (equivalent to sealed-hierarchy reasoning over a closed subtype set).
Each declared case must be matched by an unguarded arm, or covered by an
unguarded catch-all arm.

In addition, exhaustiveness may be proven through type analysis even when no
explicit finite-case construct is involved. For example:

* Covering all concrete types in a **closed inheritance chain**.
* Covering both the non-null and `null` cases of a nullable type.
* Using **type patterns** (for example, `string`, `MyBaseType`, or other
  concrete runtime types) such that all possible runtime types implied by the
  scrutinee are handled.

For sealed hierarchies with sub-hierarchies:

* If an intermediate subtype is `sealed`, exhaustiveness can be satisfied by
  covering its concrete descendants.
* If an intermediate subtype is not `sealed`, exhaustiveness for that branch
  requires a pattern that targets the intermediate type itself.

If every possible runtime value implied by the scrutinee type is handled by
explicit pattern arms, a discard arm is redundant.

When the scrutinee type is open-ended (for example, `object` or another
extensible base type), exhaustiveness generally cannot be proven without a
discard arm, or type pattern for type `object`.

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
When statement-form `match` is the final statement in a value-returning body,
its selected arm value contributes an implicit tail return.

Likewise, statement-form `if` with an `else` branch contributes an implicit tail
return when it is the final statement in a value-returning body.

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

An optional outer binding keyword may appear before a match-arm pattern:

```raven
match values {
    val [first, second, ...rest] => first + second + rest.Length
    _ => 0
}

match value {
    val Some((x, y)) => x + y
    _ => 0
}

match value {
    val Some((x, y)) pair => pair.Value
    _ => 0
}
```

For match arms, the outer binding keyword uses the same shorthand rule as
deconstruction assignment and `for` pattern targets: it supplies the binding
mode for otherwise bare captures inside the arm pattern. The same ambient
binding mode also applies to an optional trailing whole-pattern designation.
Mixing an outer binding keyword with inline pattern binding keywords in the same
arm is an error.

Statement-form `match` with block arms may use explicit `return`:

```raven
class Evaluator {
    func Eval(scrutinee: bool) -> bool {
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

### Pattern contexts

The same pattern syntax appears in several places, but not every place accepts
every pattern form.

**General matching contexts**

These accept Raven’s full pattern vocabulary:

* `expr is pattern`
* `match expr { pattern => ... }`
* `match expr { ... }` statement form
* `if val pattern = expr`
* `while val pattern = expr`
* `for val pattern in values` and `for pattern in values`

These contexts may use comparison/range/property/member/nominal-deconstruction
patterns, boolean pattern combinators, and optional whole-pattern designations
where the specific construct allows them.

**Deconstruction contexts**

These accept the deconstruction subset:

* `val ( ... ) = expr`
* `( ... ) = expr`
* `val [ ... ] = expr`
* `[ ... ] = expr`
* deconstruction parameter patterns in supported function/lambda positions

Deconstruction contexts are extraction-oriented. They support positional and
sequence decomposition, nested positional/sequence composition, discards,
typed designations, mutability/binding shorthands, and explicit value checks
that are part of those deconstruction forms. They do **not** use the
full general matching surface as assignment targets. In particular, property
patterns, nominal/member/case-pattern heads, comparison-only top-level
patterns, and other pure match-only forms are not assignment/declaration heads.

#### Type and binding patterns

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
  * `match` expression arms (`value match { Box box => ... }`)
  * `match` statement arms (`match value { Box box => ... }`)

  Example:

  ```raven
  class Box<T> {}
  val value: Box<int> = Box<int>()

  if value is Box box {
      // box : Box<int>
  }
  ```

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

  * `Ok(42)` matches the literal value `42`.
  * `Ok(discountedProduct)` matches the runtime value of the in-scope symbol
    `discountedProduct`.
  * `Ok(val n)` binds the payload to a new immutable local `n`.

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
  value (`true`, `"on"`, `42`, or `null`).

* `identifier` — **value pattern**. When a bare identifier appears in pattern
  position and resolves to an in-scope value (local, parameter, field, or property),
  the pattern matches when the scrutinee equals the runtime value of that identifier.

  Value patterns are *not* bindings. To introduce a new binding, an explicit
  binding keyword (`val`, or `var`) is required.

* `.Member` — **target-typed value pattern**. When the scrutinee type is known
  (for example, an enum type or a type with static fields), the leading-dot
  expression resolves against that target type and matches the resulting value.

> 🧭 **Disambiguation:** A bare identifier in pattern position is context-sensitive. If the
> name resolves to a value symbol, it forms a value pattern. Otherwise, it is
> interpreted as a type name and participates in a type or declaration pattern.
> This disambiguation is performed by the binder, not the grammar.

#### Comparison patterns

* `< expr`, `<= expr`, `> expr`, `>= expr`, `== expr`, `!= expr` — **comparison
  pattern**. Matches when the scrutinee compares to the operand using the given
  operator.

  The operand must be a side-effect-free expression (for example, literals,
  consts, or other stable values), ensuring comparison patterns remain predictable
  and optimizable.

  The operand type must match the scrutinee type after nullable/plain-type
  unwrapping. Ordinary implicit numeric widening is not applied inside
  comparison patterns, so matching an `int` scrutinee with `> 0.5` is an error.

  Comparison patterns are commonly used under `not`, `and`, and property patterns,
  e.g. `{ Age: not > 30 }`.

#### Range patterns

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
val value = 42

val result = value match {
    ..4 => "No"
    40..43 => "Yes"
    _ => "Other"
}
```

Range patterns participate in exhaustiveness and subsumption analysis alongside comparison patterns. They are treated as syntactic sugar for a conjunction of comparison operators (for example `40..43` behaves like `>= 40 and <= 43`).

> 🧭 **Disambiguation:** In pattern position, `..` introduces a range pattern rather than a range expression. The expression parser stops at `..` so the bounds are parsed independently.

#### Positional patterns

* `(element1, element2, …)` — **positional pattern**. Matches when the scrutinee
  can be deconstructed positionally with the same arity (either as a tuple or via
  a compatible `Deconstruct` method).

  Positional patterns destructure by position. Each element is itself a pattern and
  may introduce bindings. For example:

  * If the scrutinee type exposes a `Deconstruct` method with matching arity
    (including as an extension method), the positional pattern uses that method
    to obtain positional values.

  * ✅ `(val a, var b)` (explicit mutability)
  * ✅ `(a: int, b: string)` (inline type annotations)
  * ✅ `(int a, string b)` (type-pattern + capture)
  * ✅ `(a: int, _)`
  * ✅ `(val a, == existingValue)` (explicit capture + value pattern)
  * ✅ `(existingA, == existingValue)` (existing-value comparison)

  In freestanding and inline positional patterns, captured variables must use an
  explicit binding keyword (`val`, `var`, or `let`). A bare identifier is
  treated as a value pattern against an existing in-scope value. In assignment
  and declaration deconstruction (`val (a, b) = expr`, `(a, b) = expr`), bare
  identifiers continue to act as deconstruction targets.
  To constrain by type and capture a value in an element, both forms are valid:
  `val name: Type` (Raven-native typed binding style) and `Type name`
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

#### Sequence patterns

* `[pattern1, pattern2, …]` — **sequence pattern**. Matches when the scrutinee
  is a sequence-deconstructable, indexable collection and each sequence element
  pattern matches the corresponding element or segment.

  * Sequence patterns are supported for arrays (`T[]`), `string`, and indexable
    collection types (`Count` + integer indexer, for example `IList<T>` /
    `IReadOnlyList<T>`).
  * The same bracketed shape is also used for collection deconstruction
    assignments and declarations (`[val a, val b] = values`), which support
    arrays, `string`, and indexable collection types.
  * In the syntax tree, bracketed patterns are represented as `SequencePatternSyntax`
    (with `SequencePatternElementSyntax`), distinct from parenthesized positional
    patterns (`PositionalPatternSyntax`).
  * A plain element pattern consumes exactly one element.
  * A fixed-size segment pattern `..N pattern` consumes exactly `N` elements as a
    subsequence.
  * An open rest segment `...pattern` consumes the remaining unmatched
    subsequence and may appear either in the middle of the pattern or at the
    end. A bare `...` is also permitted as a non-capturing rest segment that
    ignores the unmatched subsequence. Likewise, a fixed-size segment may omit
    its designation as `..N` to skip exactly `N` elements without creating a
    binding. At most one open rest segment is permitted.
  * Fixed-size segments may appear multiple times because their widths are fully
    determined by the syntax.
  * In freestanding and inline sequence
    patterns, captures must use `val`/`var`/`let`; bare identifiers are treated
    as value patterns against existing values. Type-constrained captures may be
    written as `val x: T` or `T x`. The same rule applies inside segment forms,
    for example `..2 val start` and `...val rest`. Bare `...` and bare `..N`
    are the non-capturing exceptions.
  * If a sequence pattern contains no open rest segment, the input length must
    match the total fixed width exactly.
  * If a sequence pattern contains an open rest segment, the input length must
    be at least the total fixed width of the non-rest elements.
  * For arrays and indexable collections, single-element captures bind the element
    type and segment captures bind an array slice.
  * When the input is a fixed-length array `T[N]`, captured fixed/rest array
    segments preserve an inferred fixed length when the segment width is
    statically known. For example, `[val a, val b, ...val rest]` against
    `int[4]` binds `rest` as `int[2]`, and `[..2 val head, val tail]` against
    `int[3]` binds `head` as `int[2]`.
  * For `string`, single-element captures bind `char` and segment captures bind
    `string`, even for `..1`.

#### Property patterns

#### Whole-pattern designations

Primary structural patterns may carry an optional trailing designation for the
entire matched value:

* positional patterns: `(pattern1, pattern2) designation`
* sequence patterns: `[pattern1, ...pattern2] designation`
* member patterns: `.Case(...) designation`
* nominal deconstruction patterns: `Type(...) designation`
* property patterns: `Type { ... } designation`

The trailing designation is introduced only if the full pattern succeeds.

```raven
if val (2, > 0.5) point = input {
    WriteLine(point)
}

for val Person(1, name, _) person in persons {
    WriteLine(person.Name)
}

match value {
    val Some((x, y)) pair => pair.Value
    _ => 0
}
```

Rules:

* A trailing designation may be written with an explicit binding keyword, such
  as `val point` or `var point`.
* In constructs that already carry an outer binding keyword (`if val ...`,
  `while val ...`, `for val ...`, `match { val ... => ... }`), the trailing
  designation may omit its own binding keyword and inherits the outer binding mode.
* Without an outer binding keyword, omitting the binding keyword on the trailing
  designation introduces an immutable binding.
* Writing `_` discards the matched value while still enforcing the pattern.

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

  * The designation may use an explicit binding keyword (`val`, `let`, or
    `var`), or inherit the binding mode from an outer construct such as
    `if val` / `while val` / `for val` / an outer match-arm binding keyword.
  * Writing `var p` produces a mutable binding. Omitting a binding keyword
    without an outer binding mode produces an immutable binding.
  * The designation is introduced only if the entire property pattern succeeds.
  * Writing `_` discards the receiver value while still enforcing the pattern.

* `{ member1: pattern1, member2: pattern2, … }` — **inferred property pattern**.
  Like `Type { … }`, but the receiver type is inferred from the scrutinee’s static
  type.

  * If the receiver type cannot be inferred, the pattern is invalid and requires
    an explicit type.
  * `{ }` acts as a non-null test.

* `{ … } designation` — **inferred property pattern with designation**.

  * The designation may use an explicit binding keyword (`val`, `let`, or
    `var`), or inherit the binding mode from an outer construct.
  * Writing `var p` produces a mutable binding.

#### Nominal deconstruction patterns

* `RecordType(pattern1, pattern2, …)` — **nominal deconstruction pattern**.
  Matches when the scrutinee can be treated as `RecordType` and each positional
  subpattern matches the corresponding value produced by `RecordType`’s
  `Deconstruct` shape.

  * Record patterns are valid for deconstructable nominal types, including
    `record` types and other nominal types that expose an accessible
    `Deconstruct` method.
  * Primary-constructor classes and structs with promoted public `val` / `var`
    parameters synthesize a `Deconstruct` method in declaration order, so the
    same pattern form works for them.
  * Each positional element is a pattern, so bindings still require `val`/`var`
    unless an outer construct such as `if val pattern = expr` or
    `while val pattern = expr` supplies the binding mode.
  * An element may optionally include a name before the colon
    (`Name: pattern`). Named elements bind by `Deconstruct` parameter name
    rather than source position and may appear in any order. Unnamed elements
    continue to fill the remaining unmatched parameters in declaration order.
  * A trailing whole-pattern designation may capture the successfully matched
    nominal value: `Person(1, name, _) person`.
  * The number of positional elements must match the selected `Deconstruct`
    parameters; mismatches are errors.
  * If a named element does not match any supported deconstruction
    member/parameter on the target type, the pattern is invalid and reports the
    member-not-found diagnostic.
  * When the scrutinee is a discriminated union, `CaseName(...)` in nominal-deconstruction-pattern
    syntax is interpreted as a discriminated-union case pattern and binds the case
    payload positionally.

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
  * A trailing whole-pattern designation may capture the matched member/case
    value: `.Case(...) caseValue`.
  * Use `_` to explicitly discard a payload.

#### Union case patterns

Body-form unions synthesize named case types, and those cases may be matched
using three equivalent forms. The unqualified case form is the default pattern
syntax; the qualified and target-typed forms are alternatives for disambiguation
or stylistic clarity:

* `Case(...)` or `Case` — unqualified case pattern. This is the default form
  when the case name is unambiguous for the scrutinee's union case set.
* `Union.Case(...)` — explicitly qualified union-member form.
* `.Case(...)` — target-typed/member shorthand when the scrutinee determines the
  union.

For a case carrying a single `unit` payload, a bare `Case` pattern is sugar for
`Case(())`.

Parenthesized unions do not synthesize case names. They are matched using the
ordinary pattern for one of their declared member types:

```raven
union Payment(Cash | Card)

val description = payment match {
    Cash(val amount) => "cash $amount"
    Card(val reference) => "card $reference"
}
```

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
**namespace**, type, static member, or to type expressions such as tuples.

```raven
alias IO = System.IO
alias SB = System.Text.StringBuilder
alias PrintLine = System.Console.WriteLine
alias Pair = (x: int, y: int)
alias Flag = bool
alias Text = string

val sb = SB()
PrintLine("Hi")
val tmp = IO.Path.GetTempPath()
```

Aliasing a method binds a specific overload. Multiple directives using the
same alias name may appear to alias additional overloads, forming an overload
set.

Predefined types may be aliased directly. The supported built-in alias targets
are `bool`, `char`, `int`, `long`, `float`, `double`, `string`, and `unit`
(spelled `unit` or `()`). Raven has no `void`; the `unit` type is used instead
(see [implementation notes](dotnet-implementation.md#unit-type)). If the alias
target is invalid, the compiler emits diagnostic `RAV2020`.

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

namespace A1 {
    import System.*
    import System.IO.*

    // Members here

    namespace B1 {
        // Members here
    }
}

namespace A.B {
    // Members here
}
```

The outermost undeclared namespace is the **global namespace**.

## Enum declarations

An `enum` declaration defines a set of **named constants** backed by an integral
underlying type. Enums model symbolic values and preserve CLR enum semantics.

```raven
enum Color {
    Red
    Green
    Blue
}
```

Enums represent named constants only. They do not support associated payloads
or per-member structure. `match` expressions over enums are not required to be
exhaustive, and the compiler does not enforce completeness.

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

Enums remain appropriate for simple symbolic values, flags, and CLR-compatible
APIs.

### Runtime representation

At runtime, enums are emitted as CLR enum types. The compiler emits a special
instance field named `value__` whose type is the enum’s underlying type. Each
enum member is emitted as a public static literal field whose constant value is
stored using the underlying type.

## Entry points

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
statements execute in source order. Top-level type declarations are hoisted for
binding, so helper types may appear anywhere in the file or its file-scoped
namespace without changing the execution order of file-scope code.

Function declarations (local function statements) within file-scope code are
hoisted and may be referenced from anywhere in that file-scoped region,
regardless of their order. When file-scope code contains *only* function
declarations, the compiler skips synthesizing the implicit `Program.Main`
bridge; entry-point discovery falls back to user-defined candidates such as a
top-level `func Main` alongside other global declarations.

Function and block bodies may also declare local helper `class`, `struct`,
`record`, and `enum` types. These declarations are scoped to the containing
body and exist to encapsulate types that are only used locally. Within a given
body, local type declarations are hoisted for binding like local functions, so
their source order does not affect name lookup inside that body. The compiler
emits them as compiler-mangled nested types under the enclosing containing
type, so they do not publish a stable source-facing outer type name.

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

class Pipeline {
    delegate Stage<T>(ref value: T) -> bool
}
```

Delegates are emitted as sealed, abstract types that inherit from `System.MulticastDelegate`. For every delegate declaration, the compiler synthesizes:

* A constructor `.ctor(object, IntPtr)` that binds a target and method pointer.
* An `Invoke` method whose parameters (including any `ref`/`out`/`in` modifiers) and return type match the declaration.

If the return type clause is omitted, the delegate returns `unit`. Delegate declarations support generic type parameters and constraints using the same `where` clause rules as other type declarations. Accessibility defaults follow the standard type rules: top-level delegates are `internal` unless marked `public`, and nested delegates default to `private` except when declared inside interfaces.

## Functions

```raven
func Foo(a: int, b: int) -> int {
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

Attribute target prefixes are validated by declaration context:

* `[assembly: ...]` is only valid as a compilation-unit attribute list (before
  top-level members).
* `[return: ...]` is only valid on callable return positions (function/method
  return metadata).
* Other explicit targets must match the declaration kind they annotate.

If a target prefix is syntactically recognized but not valid in that position,
the compiler reports an attribute-target diagnostic.

### Parameters

Function, method, and accessor parameters use the `name: Type` syntax. Parameter
names are required and participate in overload resolution alongside their types
and any `ref`/`out` modifiers.

`val`/`var` binding keywords are **not valid** on ordinary function, method,
operator, indexer, or accessor parameters. The only parameter context where
`val`/`var` is valid is primary-constructor parameter promotion.

Lambda parameter lists use the same parameter form and default-value rules.
Parameters in function-like declarations may include attribute lists. This
applies to functions, methods, constructors, delegates, accessors, and lambdas.

Parameters may provide a default value using `= expression` after the type. A
parameter with a default value is optional when invoking the function: callers
can omit that argument and the compiler supplies the stored constant instead.
Only trailing parameters may be optional; omitting an argument fixes the default
for that position and all following parameters must also declare defaults.

```raven
func greet(name: string, punctuation: string = "!") {
    Console.WriteLine("Hello, ${name}${punctuation}")
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

Method overloads may also be influenced by
`System.Runtime.CompilerServices.OverloadResolutionPriorityAttribute`. When two
or more applicable candidates belong to the same overload set, Raven keeps the
highest-priority candidates before applying its normal overload-comparison
rules. This matches the intended .NET/C# interop behavior for both Raven source
methods and imported metadata methods.

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

Type parameter lists follow the same rule: commas may be written explicitly,
but a newline is also a valid separator between adjacent type parameters.

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
class Cache {
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
scoped to its containing body and can capture local variables, parameters, and
`self` from enclosing scopes. Local functions
support the same generic syntax and constraints as file-scoped functions: place
an optional type parameter list after the function name and declare constraints
using the `:` syntax when needed.
When no instance receiver is available (for example inside `static` members or
`static func` local functions), using `self` reports `RAV2801`.

```raven
func outer() {
    func inner<T: struct>(value: T) -> T { value }

    val y = inner(2)
    val point = inner((x: 1, y: 2))
}
```

Bodies may likewise declare local helper `class`, `struct`, `record`, and
`enum` types when the type should remain encapsulated to that body. Local type
declarations participate in body-local name lookup for the entire containing
body, but the declared type is not exposed as a surrounding namespace or outer
type member in Raven source.

### Async functions

The `async` modifier may appear on free functions, methods, and nested
functions. An async declaration opts the body into asynchronous control flow so
`await` expressions can suspend and resume execution. When no return type is
annotated, async declarations default to `System.Threading.Tasks.Task`
(`unit`-returning async body). The compiler does not infer `Task<T>` from
omitted return annotations.

Async functions with an explicit return type must annotate one of the supported
task shapes: `System.Threading.Tasks.Task` or `System.Threading.Tasks.Task<T>`.
Annotating any other type produces a diagnostic, and the compiler continues
analysis as though the return type were `Task`. This rule applies uniformly to
methods, file-scoped functions, and local functions declared inside other
bodies. Property and indexer accessors may also carry `async`; getters must
expose a task-shaped return type to remain valid, while setters may await
asynchronous work before storing values.

Diagnostic analyzers may still suggest adding an explicit return type annotation
based on observed body shape; such suggestions are advisory and do not change
the language binding rule above.

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

### Function expressions and captured variables

Function expressions start with either a parenthesized parameter list
or a single identifier, optionally followed by a return-type arrow. Expression
bodies use `=>`; `func`-introduced block bodies may omit `=>` and use
`func (...) { ... }`. Function expressions may also use modifier forms
`async func`, `static func`, and `static async func`. Function expressions may appear wherever a function value
is expected. When a function expression or nested function statement (`func`) references
a local defined in an outer scope, the compiler lifts that symbol into shared
closure storage so both scopes observe the same value. Each capturing lambda or
nested function materializes a synthesized closure class that stores the body as an
instance method and exposes fields for every captured symbol. Reads and writes
in any scope access those fields directly, so mutating a `var` binding after
creating a lambda immediately affects all delegates that captured it. Capturing
`self` produces a reference to the enclosing instance, and capturing parameters
preserves the argument value from the invoking scope. Nested lambdas reuse the
closure instances produced by their enclosing scopes so that captures shared
across multiple lambda layers continue to reference the same storage locations.
Non-capturing lambdas are emitted using the same closure-carrier convention
(with zero capture fields) so lambda emission remains uniform across contexts.
Synthesized lambda method names follow C#-style metadata naming (`<Method>b__...`).
`static func` declarations do not capture enclosing state, and `self` is not
available in static contexts (`RAV2801`). Static function expressions are also
non-capturing; attempting to capture an outer local or parameter reports
`RAV2204`.

Parenthesized function expressions may place attribute lists immediately before the
parameter list as shorthand. Leading lists are applied contextually:
non-targeted attributes are applied to the first parameter, while
`[return: ...]` lists are applied to the function expression return type.

```raven
val parse = [FromBody](content: string) => content
```

Function-expression parameters may also declare default values using the same trailing
optional-parameter rules as functions and methods.

```raven
val format = (name: string, age: int = 1) => "$name:$age"
```

Function-expression parameter types are optional when the expression is converted to a known
delegate type. The compiler infers the parameter types (and any `ref`/`out`
modifiers) from the delegate's `Invoke` signature and converts the body to the
delegate's return type. If no delegate context is available, diagnostic
`RAV2200` is reported and explicit parameter annotations are required.

Parenthesized function-expression parameter lists also support destructuring
patterns as parameter entries:

* **positional deconstruction** (tuple/`Deconstruct` style), for example `((a, b))`
* **sequence deconstruction** (collection style), for example `([head, ..tail])`

This is primarily target-typed and inference-driven: the underlying parameter
type still comes from the delegate context, and destructuring is then applied
inside the lambda body.

```raven
val pickSecond: ((int, string)) -> string = ((a, b)) => b
val sumTail: (int[]) -> int = ([head, ..tail]) => head + tail[0]
```

Nested deconstruction is recursive in parameter patterns. Positional and
sequence forms may be freely nested as long as each nested segment is
compatible with its inferred input type:

```raven
val project: (((int, string), int[])) -> string =
    (((id, name), [head, ..tail])) => "$id:$name:$head:${tail.Length}"
```

For sequence deconstruction in lambda parameter lists, `..name` and JavaScript-
style `...name` are both accepted as rest syntax.

When a destructuring parameter omits per-element binding keywords, elements are
bound as immutable (`val`) by default. Compatibility is still validated against
the inferred parameter type; non-deconstructable inputs produce the same
deconstruction diagnostics as other pattern-based bindings (for example
`RAV0132`).

Parameters themselves do not carry binding keywords in symbol display/tooling
representations. Hover/signature text renders them as `name: type` (or
`params name: elementType` for collector parameters). In primary-constructor
contexts, `val`/`var` continues to mean promotion to a property rather than a
mutable parameter binding.

Function expressions are target-typed: the same function expression may be assigned to, passed
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

When the selected method is compatible with the target delegate only through
implicit parameter/return conversion (for example, delegate parameter
`KeyValuePair<string, int>` forwarded to method parameter `object`), Raven
synthesizes an internal compiler-generated bridge method and binds the delegate
to that bridge. The bridge performs the required implicit conversions (including
boxing for value types) before invoking the selected method.

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

    func Increment(delta: int) -> int { self.value + delta }

    func Run() -> int {
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
    static func TryAccumulate(ref state: int, out doubled: int) -> bool {
        state = state + 1
        doubled = state * 2
        true
    }

    static func Execute(value: int) -> int {
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
    field Foo: int = 0
}

unsafe func assignThroughPointer() -> int {
    var holder = Holder()
    let pointer: *Holder = &holder
    pointer->Foo = 2
    holder.Foo // 2
}
```

### Pinning managed storage

Raven uses a `use`-scoped pinning form instead of a dedicated `fixed (...) { ... }`
statement. The initializer syntax is:

```raven
unsafe {
    use pointer: *int = fixed &value
}
```

Rules:

* `fixed` is only valid as the initializer of a `use` declaration.
* `fixed` requires an explicit address-of operand written as `&expr`.
* The result type is a native pointer `*T`, where `T` is the addressed storage type.
* The pin lasts for the lifetime of the enclosing `use` scope and is released when that scope exits, including exceptional exits.
* `fixed` is gated by unsafe mode just like other pointer-producing operations.

The explicit `&` keeps address selection separate from pinning: `&expr` identifies the storage, while `fixed` guarantees that storage remains stable for pointer use within the `use` scope.

```raven
unsafe func writeSecond(values: int[]) -> int {
    use pointer = fixed &values[1]
    *pointer = 42
    values[1]
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
    extern static GetTickCount() -> uint;
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

Parameters can also be declared with an explicit by-reference type using
`&Type`. That form is valid when the type itself is intentionally part of
the signature. Separately, `ref`, `out`, and `in` declare by-reference
parameter passing at the call boundary and already imply aliasing, so
their parameter types are written as plain `Type`, not `&Type`. For
example, write `ref value: int`, not `ref value: &int`. When a
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
`ref`, `out`, and `in` parameters immediately alias existing storage
locations. The caller provides that storage with `&expr`; `out`
requires the callee to assign the aliased storage before returning.

```raven
func TryParse(text: string, out result: int) -> bool { /* ... */ }

var total = 0
if !TryParse(arg, out total) {
    Console.WriteLine("Expected number")
}
```

## Local declarations

### Value binding (`val`)

A `val` binding is **immutable** (not reassignable). Types are inferred
unless annotated. A single declaration may declare multiple bindings by
separating declarators with commas.

```raven
val x = "Foo"
val y: int = 2
val a = 1, b = 2
val a: int = 2, b: string = ""
```

Raven treats `let` as an alias for `val`.

### Variable binding (`var`)

A `var` binding is **mutable** (reassignable). A single declaration may
declare multiple bindings by separating declarators with commas.

```raven
var x = "Foo"
x = "Bar"

var y: int = 2
y = 3

var left = 1, right = 2
left = left + right
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
`val`/`var` controls the default mutability for shorthand forms, while each element
uses a designation (possibly nested) to capture or discard the corresponding value.
Elements may include inline type annotations. Positional deconstruction works with
tuples and with any type that exposes a compatible `Deconstruct` method (including
as an extension method).
Elements may also be named as `Name: pattern`. For `Deconstruct`-based
deconstruction, named elements bind by parameter name and may be written in any
order; unnamed elements continue to consume the remaining unmatched parameters
in declaration order. Unknown names are diagnosed.

This is a **deconstruction** surface, not a full general pattern-matching
surface. The left-hand side must start with positional syntax (`(...)`) or
sequence syntax (`[...]`), and nested subpatterns must remain within the
deconstruction family.

Supported as deconstruction heads:

* positional heads: `( ... )`
* sequence heads: `[ ... ]`
* nested positional/sequence subpatterns
* declaration/capture forms, discards, typed designations, and supported
  explicit value checks inside those positional/sequence shapes

Not currently supported as deconstruction heads:

* property patterns: `Type { ... }`
* nominal deconstruction / union-case heads: `Type(...)`, `Some(...)`, `.Case(...)`
* pure comparison heads such as `>= 18`
* range heads such as `1..10`
* boolean-combinator heads such as `p1 and p2`, `not p`

For element type matching and capture, Raven accepts both:

* `val name: Type`
* `Type name` (type-pattern style)

```raven
val (first, second, _) = (1, 2, 3)
var (head, tail: double, _) = numbers()
(first, second, _) = next()
(val lhs, var rhs: double, _) = evaluate()
(int id, string name) = getTuple()
(val id: int, val name: string) = getTuple()
(lhs, == expectedRhs) = evaluate()
val (Items: items, Name: name, Age: age) = person
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

Collection deconstruction uses the same element-pattern rules but with bracket
syntax. In assignments and declarations, collection deconstruction supports
arrays (`T[]`) and indexable collection types (for example `IList<T>` /
`IReadOnlyList<T>`), and elements are bound in sequence order.

In compiler APIs, this bracketed form is surfaced as `SequencePatternSyntax`
rather than `PositionalPatternSyntax`, so tuple/positional and sequence forms
can evolve independently.

For declaration-style deconstruction, Raven supports both:

* explicit per-element binding keywords: `[val a, val b, _] = values`
* shorthand outer binding keyword: `val [a, b, _] = values`

The shorthand is equivalent to applying the same binding keyword to each
identifier element (`val`, `var`, or `let`), matching tuple deconstruction forms
like `val (a, b) = expr`.

These two declaration styles are mutually exclusive in a single deconstruction:
choose either an outer/common binding keyword or per-element binding keywords.
Mixing both is invalid.

```raven
// valid
var [first, second, _] = values
[val first, val second, _] = values

// invalid
val [val first, val second, _] = values
val (val id, val name) = obj
```

```raven
val values: int[] = [1, 2, 3]
[first, second, _] = values
[var head, var tail, _] = values
val [first2, second2, _] = values
var [head2, tail2, _] = values

import System.Collections.Generic.*
val list: List<int> = [1, 2, 3]
[a, b, _] = list
let [c, d, _] = list
```

Use `_` to discard unwanted elements. Nested positional patterns work the same way:

```raven
var ((x, y), val magnitude, _) = samples()
```

Nested sequence/positional patterns can also be combined:

```raven
val [(first, second), [head, ..tail]] = value
```

In freestanding and inline patterns, plain identifiers are value patterns, not
new bindings. Use `val`/`var`/`let` to capture a value. In assignment and
declaration deconstruction, plain identifiers remain binding targets by
default. To match against an existing runtime value in a positional pattern,
you can still use an explicit value pattern:

```raven
match x {
    (val a, == existingValue) => ...
}
```

`== existingValue` is a convenience form. The same outcome can be expressed with
a plain binding plus a `when` guard, but explicit value patterns avoid adding an
extra arm condition when the intent is simple equality against an in-scope value.
It also does not capture that element. If you need the value later, bind it and
compare in a guard/condition (`(a, b) when b == existingValue`, or
`if t is (a, b) && b == existingValue`).

Collection patterns support both fixed-size and rest segments:

```raven
val [..2 start, last] = values
val [first, ...rest] = values
val [first, ...middle, last] = values
val [first, ...] = values
val [first, ..2 middle, last] = "rune"
```

In inline/freestanding sequence patterns, spell captures explicitly:
`..2 val name` or `...val rest`. In deconstruction assignments/declarations,
bare `..2 name` and `...rest` remain valid as binding targets. Bare
`..N` and bare `...` may be used in either form to ignore
the rest of the sequence without creating a binding. Both captured and
non-capturing rest segments may appear in the middle or at the end. When the
input is a fixed-length array, captured `..N` and `...rest` array
segments keep an inferred fixed-length array type. For strings, a
plain element binds `char`, while `..N` and rest segments bind `string`.

Nested deconstruction uses the same recursive compatibility rules in all valid
positions:

* declaration deconstruction (`val (...) = expr`, `val [...] = expr`)
* assignment deconstruction (`(...) = expr`, `[...] = expr`)
* function-expression parameter patterns (`((...), [...]) => ...`)
* `is`/`match` pattern positions

Only the first two bullets above are **deconstruction heads**. The latter two
reuse the same nested positional/sequence forms inside broader general-pattern
contexts that also support additional match-only pattern kinds.

For diagnostics, Raven reports failures at the specific nested subpattern that
is incompatible (arity/type/member mismatch) and only introduces symbols for
successfully bound designators.

> ⚠️ **Runtime shape note:** sequence deconstruction is length-sensitive at
> runtime. If the input sequence does not contain enough elements for the fixed
> prefix/suffix elements in the pattern, evaluation may fail at runtime.
> Plain enumerable-only sources are not treated as sequence-deconstructable.

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

In a synchronous context, the initializer’s type — and any explicit type
annotation — must be convertible to `System.IDisposable`.

In an async context, Raven prefers `System.IAsyncDisposable.DisposeAsync()` when
the resource supports `IAsyncDisposable`; otherwise it falls back to
`System.IDisposable.Dispose()`. The declared type and initializer type must
therefore be convertible to at least one of those disposal shapes. If the
conversion fails, Raven reports the same diagnostic used for other implicit
conversions. 【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L188-L224】

Resources created with `use` behave like ordinary locals: they remain in scope for the enclosing block and participate in definite-assignment rules. When control leaves the block, the resource is **automatically disposed**. Disposal occurs in **reverse declaration order**, ensuring that later resources observe earlier ones still alive.

When you need a narrower lifetime than the enclosing block, Raven also supports
an explicit nested-scope form:

```raven
use stream = OpenRead(path) in {
    // stream is only in scope here
}
```

The `in { ... }` form is equivalent to introducing a nested block whose first
statement is the `use` declaration. This keeps DSL-style trailing blocks
unambiguous:

```raven
use obj = Foo with { Value = 2 } in {
    obj.Run()
}
```

File-scope `use` declarations participate as well: they are disposed after the file’s top-level statements finish executing. 【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L222-L282】【F:src/Raven.CodeAnalysis/CodeGen/Generators/Generator.cs†L54-L87】【F:src/Raven.CodeAnalysis/CodeGen/MethodBodyGenerator.cs†L114-L148】

```raven
use stream = System.IO.File.OpenRead(path)
use reader = System.IO.StreamReader(stream)

val text = reader.ReadToEnd()

// reader.Dispose() and stream.Dispose() run automatically when the scope ends
```

```raven
async func Load(path: string) -> Task<string> {
    use stream = OpenAsyncStream(path)
    return await stream.ReadAllTextAsync()
}
```

In the async example above, Raven uses `DisposeAsync()` when `stream`
implements `IAsyncDisposable`; otherwise it uses `Dispose()`.

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
syntax mirrors a lambda signature: a comma-separated
parameter list enclosed in parentheses followed by `->` and the return type.

```raven
val applyTwice: ((int -> int), int) -> int
val thunk: () -> unit
val comparer: (string, string) -> bool
```

In declaration-oriented lists, a newline may stand in for the expected explicit
separator token. The syntax tree keeps the separated-list shape and stores
`SyntaxKind.None` for those newline-delimited separator slots. If the separator
is omitted on the same line, recovery instead uses a missing separator token of
the expected kind and reports a diagnostic.

Single-parameter functions may omit the surrounding parentheses:

```raven
val increment: int -> int
```

The return portion may itself be any Raven type. Nested arrows associate to the right, so `int -> string -> bool` is
parsed as `int -> (string -> bool)`.

Function annotations are sugar over delegates. When the parameter and return
types match an existing declaration (including the built-in `Func`/`Action`
families), the compiler binds to that delegate. Otherwise it synthesizes an
internal delegate with the appropriate signature so interop with .NET remains
transparent. Parameter modifiers and names are not permitted inside a function
type; specify only the types that flow into and out of the delegate. A `unit`
return represents an action with no meaningful result.

Unnamed function expressions support explicit and shorthand forms:
`func (x: int) => x + 1`, `func (x: int) { x + 1 }`, `async func (x: int) => x + 1`,
`static func (x: int) => x + 1`, and `(x: int) => x + 1`
(or `x => x + 1`). The shorthand form is valid anywhere the equivalent explicit
function expression is valid.

Function expressions may optionally declare a local identifier:
`func Fib(n: int) => Fib(n - 1)`. This identifier is visible only inside the
function-expression body. It does not declare a surrounding local/member name;
the emitted backing method name remains compiler-generated.

When a function expression is target-typed by a delegate requirement (for
example, assignment to `Action<int>` or passing to a delegate-typed parameter),
Raven projects the function value to a compatible delegate. Built-in
`Func`/`Action` delegate shapes are displayed as function signatures in Raven
type displays, while custom delegate types remain visibly named delegates.

### Nullability and `null`

Nullability is **explicit** in Raven. Reference types are non-nullable by
default, and `null` can only flow through nullable annotations (`T?`). The same
rules apply uniformly to reference and value types; the distinction only
affects runtime representation, not the surface type rules.

#### Nullable suppression (`!`)

`!` suppresses nullable checking for a single expression. Use it only when the
programmer has stronger knowledge than the exposed type.

##### Concept

* For nullable references, `expr!` changes the static type from `T?` to `T`
  without inserting a runtime null check.
* For nullable value types, `expr!` unwraps `T?` to `T`.

##### Example

```raven
func ReadName(service: ExternalService) -> int {
    val name = service.TryGetName()!
    return name.Length
}
```

```raven
func Increment(value: int?) -> int {
    val required = value!
    return required + 1
}
```

##### Rules

* `!` affects only the annotated expression.
* `!` does not relax nullability rules for surrounding expressions.
* Using `!` reports warning `RAV0403`.

### Unions

Unions define nominal carrier types with a fixed set of cases. A union value is
always stored as the declared carrier type, and case values convert to that
carrier implicitly when required.

For non-normative rationale and interop notes, see
[Design notes](design-notes.md#unions-and-carrier-types).

| Form | Syntax | Cases | Typical pattern form |
| --- | --- | --- | --- |
| Parenthesized | `union Payment(Cash | Card)` | existing member types | `Cash(...)`, `Card(...)` |
| Body form | `union LookupResult { case Found(id: int) case Missing }` | synthesized case types | `Found(...)`, `Missing` |

#### Parenthesized unions

##### Concept

The parenthesized form declares a carrier over existing nominal or primitive
types. Raven does not synthesize additional case types for this form.

##### Example

```raven
record Cash(amount: decimal)
record Card(last4: string)

union Payment(Cash | Card)

val paidInCash: Payment = Cash(12.50m)
val paidByCard: Payment = Card("4242")
```

##### Rules

* Each listed member type is part of the carrier's closed case set.
* Pattern matching uses ordinary patterns over those member types.
* Construction occurs by constructing a listed member type and then converting it
  to the carrier when needed.

#### Body-form unions

##### Concept

The body form declares a carrier with an ordinary member body. That member body
may contain `case` declarations alongside other members such as methods and
properties. Each `case` declaration synthesizes a named case type.

##### Example

```raven
union LookupResult {
    case Found(id: int)
    case Missing

    func Describe() -> string {
        return self match {
            Found(val id) => "found $id"
            Missing => "missing"
        }
    }
}

val found: LookupResult = Found(42)
val missing: LookupResult = Missing
```

##### Rules

* Each `case` declaration declares one synthesized case type.
* Body-form unions may also declare ordinary members in the same body.
* `case` declarations are valid only inside `union` declarations.
* Case references may use `Union.Case`, `.Case`, or unqualified `Case` when
  resolution is unambiguous.
* A comma or semicolon after a case is optional; when present it terminates that
  case declaration.
* Generic unions are allowed in both forms, for example
  `union Result<T, E> { case Ok(value: T) case Error(error: E) }`.
* `union` declarations may be `partial`. Cases and ordinary members may be
  distributed across partial declarations of the same union.
* The carrier reserves the member names `Value` and `HasValue` for synthesized
  members.
* As with records, an authored `override ToString()` suppresses the synthesized
  union `ToString()`.
* Authored `Equals`, `GetHashCode`, and equality operators on unions are
  currently rejected.

Line-continuation details for leading-dot case forms are defined in
[Control flow: Line continuations](control-flow.md#line-continuations).

#### Case construction and extraction

##### Concept

Case construction creates a case value first. Conversion to the carrier happens
when the surrounding context requires the union type.

##### Example

```raven
val ok: Result<int, string> = Ok(99)
val err = Result<int, string>.Error("boom")

val outcome: Either<int, string> = 42
val left = (int)outcome
```

##### Rules

* `Case(...)` constructs the case value directly.
* `Union.Case(...)` resolves the case through the union surface and constructs
  the same case value.
* `.Case(...)` resolves the case from the target type's union case set.
* Unqualified `Case(...)` is valid only when case lookup is unambiguous. Normal
  lexical lookup wins before union-case lookup.
* Every union carrier exposes a conventional `Value` property whose runtime
  value is the currently stored member or case value.
* `Value` has type `object` when the carrier cannot represent a null active
  value.
* `Value` has type `object?` when the carrier may legitimately report `null`,
  including `union struct` default-state carriers and class carriers whose
  member set includes a nullable member type.
* Every union carrier also exposes `HasValue: bool`, which reports whether the
  carrier currently has an active case/member.
* `TryGetValue(out CaseType)` exposes carrier inspection for each case type.
* An explicit cast from the carrier to a member or case type succeeds only when
  the carrier currently holds that case; otherwise it throws
  `InvalidCastException`.
* Pattern matching is preferred to explicit casts for ordinary extraction.

In pattern position:

* Body-form unions use `Case(...)` or `Case` by default when the case name is
  unambiguous.
* `Union.Case(...)` is available for explicit qualification.
* `.Case(...)` remains available as target-typed shorthand when the scrutinee
  already determines the union.
* Parenthesized unions use ordinary patterns over their declared member types.

### Canonical case-construction forms

Raven supports the following equivalent case-construction forms:

```raven
// Case type construction
Ok(2)
Ok<int>(2)

// Union-member sugar
Result<int, MyError>.Ok(2)

// Target-typed member-binding sugar
val r: Result<int, MyError> = .Ok(2)
```

Binding model:

* `Case(...)` constructs the case type value directly.
* Unqualified `Case(...)` is allowed when case resolution is unambiguous in
  scope; otherwise a qualified form (`Union.Case(...)`) or alias is required.
* `Union.Case(...)` resolves `Case` from the union’s declared case set, then
  constructs the case value.
* `.Case(...)` resolves `Case` from the target type’s union case set.
* For an unqualified identifier in expression position, ordinary lexical lookup
  wins before union-case lookup: locals and parameters first, then visible
  instance/static members and imported symbols, then unqualified union cases.
* If a union value is required, case-to-union conversion applies implicitly by
  constructing the matching carrier value from the case value.

Union invariants:

* Case constructors are independent case-type constructors; they are not
  rebound as union constructors.
* `union struct` reserves its default state as an uninitialized carrier. For
  `default(U)`, `Value` is `null`, `HasValue` is `false`, and no case is active
  until a union constructor populates the carrier.
* In pattern exhaustiveness, that default `union struct` carrier state behaves
  like an additional `null`-like pseudo-case. A `null` arm or ordinary
  catch-all arm covers it.
* `union class` does not have that extra carrier state; a class carrier exists
  only after construction through one of its union cases or constructors.
* For ordinary class carriers with no nullable active member state, `null` is
  not a valid pseudo-case for `Value`.
* `HasValue` is independent from `Value != null`; an active case may still
  project `null` through `Value` when the selected member itself is nullable.
* Union wrapping is represented by carrier construction from a case value.
* Compatibility is decided by case-to-union conversion rules (including
  payload subtype-to-supertype widening where valid).

Type argument behavior:

* Case type arguments may be explicit (`Ok<int>(2)`) or inferred from
  constructor arguments (`Ok(2)`).
* Union type arguments are taken from explicit receiver types
  (`Result<int, MyError>.Ok(2)`) or from target typing
  (`val r: Result<int, MyError> = .Ok(2)`).

For every case `Case`, assigning, returning, or passing a case value
automatically produces the union carrier through case-to-union conversion.
Member-qualified case construction still constructs the case first and then
converts to the carrier when the surrounding context requires the union value:

```raven
val ok: Result<int, string> = Ok(99)          // implicit case-to-union conversion
val err = Result<int, string>.Error("boom")
Console.WriteLine(ok)
```

Each case struct also exposes its payload via `get`-only properties and a
`Deconstruct(out ...)` method matching the payload order. These synthesized
members make deconstruction and positional patterns available in Raven and
improve interoperability with other .NET languages.

Pattern matching exhaustively checks every case; see
[Pattern matching](#pattern-matching) for case-pattern forms (unqualified
`Case`, `Union.Case`, and `.Case`) inside `match` expressions.

### Closed-shape types

Raven has two primary ways to model a finite, closed set of alternatives:

1. **Unions** (`union`)
2. **Sealed hierarchies** (`sealed class` / `sealed record class`)

Both participate in exhaustiveness analysis for `match`, and both represent a
known closed shape at compile time. The key difference is modeling style:

| Use this | When you need |
| --- | --- |
| `union` | Algebraic data modeling with explicit case payloads, carrier-based construction/extraction (`Ok(...)`, `.Ok(...)`, `TryGetValue`), and closed alternatives. |
| `sealed` hierarchy | Object-oriented subtype modeling with shared base behavior, virtual/interface-style design, and class hierarchy semantics. |

#### Choosing between them

Choose **unions** when:

* the alternatives are primarily data cases,
* payloads are part of the case definition,
* construction/pattern matching is the dominant interaction.

Choose **sealed hierarchies** when:

* you are modeling a class family,
* variants share behavior through a base type,
* subtype polymorphism is part of the design.

Both are "closed-shape" constructs; prefer the one that matches your domain
modeling style rather than forcing a single pattern for all cases.

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
14. Unary (prefix): `+  -  !  fixed  typeof`
15. Postfix trailers: call `()`, member `.`, index `[]`, nullable suppression `!`, propagation `?`

> 🧭 **Disambiguation:**
>
> * `(<expr>)` is a **parenthesized expression** unless a comma appears (including trailing), in which case it’s a **tuple**.
> * `<` starts **type arguments** only in a **type context**; elsewhere it’s the less-than operator.
> * The LHS of assignment must be either an **assignable expression** (identifier, member access, element access, etc.) or a
>   **pattern** such as a positional deconstruction.
> * `^` index expressions are parsed as an adjacent prefix form (`^expr`); whitespace between `^` and the operand is not allowed.
> * Prefix unary `+`/`-` are also adjacent forms (`+3`, `-2`); whitespace between the operator and operand is not allowed.
 
