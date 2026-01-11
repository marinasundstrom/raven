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
| Reserved | `and`, `as`, `await`, `base`, `bool`, `break`, `catch`, `char`, `class`, `const`, `continue`, `default`, `double`, `each`, `else`, `enum`, `false`, `finally`, `for`, `func`, `goto`, `if`, `int`, `interface`, `is`, `let`, `match`, `new`, `not`, `null`, `object`, `or`, `return`, `self`, `string`, `struct`, `throw`, `true`, `try`, `typeof`, `var`, `when`, `while`, `yield` |
| Contextual | `abstract`, `alias`, `explicit`, `final`, `get`, `implicit`, `import`, `in`, `init`, `internal`, `namespace`, `open`, `operator`, `partial`, `out`, `override`, `private`, `protected`, `public`, `ref`, `sealed`, `set`, `static`, `unit`, `using`, `val`, `virtual` |

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
    let @and = @return;
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
let $ffiResult = call()
let value_1 = value0
let 数据 = call()
let сумма = total1 + total2
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
let answer = 42  // the ultimate answer
let greeting = "hello"  // 😀 emoji and other symbols are fine

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

`let` (or alternatively `val`) introduces an immutable binding, `var` introduces a mutable one, and `const`
produces an immutable binding whose value is baked in at compile time. A binding may
declare its type explicitly or rely on the compiler to infer it from the initializer
expression.

```raven
let answer = 42         // inferred int
val answer = 42         // alternative to 'let': inferred int

var name = "Alice"    // inferred string, mutable

const greeting = "Hi"  // inferred string constant

let count: long = 0     // explicit type
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
Shadowing is permitted for both `let` and `var` bindings, but it produces the
warning diagnostic `RAV0168` to help catch unintentional redeclarations. Parameters of
the enclosing function count as previous declarations for this purpose, so a local that
reuses a parameter name both shadows it and triggers the same warning.

```raven
let answer = 41
let answer = answer + 1 // RAV0168 (warning)
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
For example, the enum shorthand `.B` in `var grade: Grades = .B` uses the declared type
`Grades` to resolve the member. Numeric literals and `null` similarly adapt to their
target types. Type inference for `let`, `var`, and `const` bindings uses this mechanism to
determine the variable's type from its initializer.

### Type inference

When an expression or declaration omits an explicit type, Raven infers one from
the expression. If multiple different types can flow to a location—through
conditional branches or early `return` statements—the inferred result becomes a
**union** of those types. The compiler does not collapse distinct types to their
nearest common base; returning `Dog` and `Cat` infers `Dog | Cat`, not `Animal`.

```raven
let pet = if flag { Dog() } else { Cat() }
// pet has type: Dog | Cat
```

Literal expressions infer the underlying primitive type when used to initialize
`let` or `var` bindings. Literal types are subset types of their underlying
primitive, so a literal like `1` can be used wherever an `int` is expected.
When inference gathers multiple results—such as the branches of an `if`
expression—it normalizes the union before exposing it. Literal members collapse
to their underlying type when a non-literal of the same type also flows to the
branch, so the union reflects only the distinct possibilities that can escape.
To retain a literal's singleton type for a single value, an explicit annotation
is required.

```raven
var i = 0       // i : int
let j = 0       // j : int
var k: 1 = 1    // k : 1
```

Control-flow expressions participate in the same inference. An `if` expression
whose branches produce different types infers a union of those results. The
union is normalized so redundant members collapse away while distinct literal
branches remain precise:

#### Collapsing branch unions

```raven
let x: int = 3
let value = if x > 2 { 42 } else { x }
// value : int          (literal 42 collapses into the `int` branch)

let digits = if x > 2 { 1 } else { 2 }
// digits : 1 | 2       (distinct literal branches remain literal)

let other: long = 0
let ambiguous = if x > 2 { other } else { 42 }
// ambiguous : int | long
// let target: int = ambiguous  // error: not every branch converts to int
```

Each branch contributes its inferred type to the union. Because the `else`
branch in the first example is the literal `42`, and the other branch produces
an `int`, the literal collapses into the `int` branch. When all branches are
distinct literals, the union records them individually so downstream pattern
matching can reason about the precise set of constants. When inference observes
multiple primitive types, the union retains each one. Assigning that union to a
single numeric type triggers a diagnostic because not every branch converts to
the target.

Numeric literals choose an underlying primitive type. Integer literals default
to `int` but upgrade to `long` when the value exceeds the `int` range.
Floating-point literals default to `double`; appending `f` or `F` produces a
`float` literal; appending `m` or `M` produced as `decimal`.

```raven
var l = 4_000_000_000  // l : long
var f = 3.14f          // f : float
var d = 3.14           // d : double
var m = 9.99m          // m : decimal
```

Overload resolution applies the same rule: a literal argument converts to its
underlying type when selecting among method overloads. For example,
`Console.WriteLine(1)` binds to `Console.WriteLine(int)` if such an overload
exists, and `Console.WriteLine("test")` chooses `Console.WriteLine(string)`.

Functions and lambdas without an annotated return type infer their result by
collecting the types of all explicit `return` statements and the final expression
of the body. If no value-returning path exists, the type defaults to `unit`.

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
let a = 42
let makeAdder = () => a + 3

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
let maybe = if flag { 0 } else { 1.0 } // int | float

let n: int = maybe    // error: float not assignable to int
let o: object = maybe // ok: both int and float convert to object

let pet = if flag { Dog() } else { Cat() } // Dog | Cat
let a: Animal = pet   // ok: Dog and Cat derive from Animal
let s: string = pet   // error: neither member converts to string
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
or an exception into a discriminated union. The operand must be any expression
that is valid in the current context. The compiler assigns the `try` expression
the union type formed by its operand type and `System.Exception`, enabling
pattern matching on successful results versus failures. Nested `try`
expressions are disallowed and produce `RAV1906`.

Execution enters a `try`/`catch` block that stores the operand’s value in a
temporary. If evaluation completes without throwing, the temporary value is
converted to the union’s result case and becomes the expression’s final value.
If evaluation throws an exception, the runtime catches the `System.Exception`
instance, converts it into the union’s exception case, and yields that value
instead. The union conversions follow the same rules as other union expressions
when targeting explicitly typed variables. 【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L1007-L1022】【F:src/Raven.CodeAnalysis/CodeGen/Generators/ExpressionGenerator.cs†L3118-L3156】

`await` may appear inside a `try` expression when used in an async method or
lambda. The awaited result still flows to the success case, and exceptions
propagate to the union's exception case without altering the `try` expression's
shape. 【F:test/Raven.CodeAnalysis.Tests/Semantics/ExceptionHandlingTests.cs†L89-L123】

### Cast expressions

Explicit casts request a conversion to a specific type and use C# syntax.

```raven
let d = (double)1
let i = (int)3.14  // numeric narrowing
let s = obj as string
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
let textType = typeof(string)
let listType = typeof(System.Collections.Generic.List<int>)
```

`typeof` is useful when reflecting over metadata or when passing type objects to
APIs such as `Activator.CreateInstance`.

### Default expressions

`default` produces the zero-initialized value for a type. Use the explicit form
`default(T)` to request the default value for a known type `T`. The target-typed
literal `default` instead relies on the surrounding context—such as a variable
annotation, a return type, or an argument position—to supply the type. When no
target type is available, the compiler reports `RAV2011` because the literal
cannot be inferred.

```raven
let zero = default(int)
let emptyText: string = default
```

### String literals

```raven
let hello = "Hello, "
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

### String interpolation

Embed expressions directly into strings using `${...}` without requiring a prefix.
For simple identifiers, a shorthand `$identifier` form avoids the braces.

```raven
let name = "Alice"
let age = 30
let greeting = "Hello $name!"
let msg = "Name: ${name}, Age: ${age}"
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
let name = "ليلى"
let city = "دبي"
let welcome = "\u200Fمرحبا ${name}! أهلا بك في ${city}"
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
let numbers: int[] = [1, 2, 3]
let combined = [0, ..numbers, 4]

let names: List<string> = ["a", "b"]
let inferred = [1, 2.0]  // inferred as object[]
```

#### Element access

```raven
let list = [1, 42, 3]
let a = list[1]
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
arguments are supported anywhere an argument list appears, including function
invocations, object creation, constructor initializers, and attribute usage.

```raven
func makePoint(x: int, y: int, label: string = "origin") -> string
{
    return $"{label}: ({x}, {y})";
}

let swapped = makePoint(y: 2, x: 1);
let mixed = makePoint(3, label: "axis", y: 0);
let invalid = makePoint(x: 1, 2);  // error: positional argument cannot follow `x:`
```

The compiler binds each named argument to its declared parameter. The call to
`makePoint` named `mixed` demonstrates that positional arguments may precede the
first named argument, while the `invalid` call is rejected because it attempts
to supply a positional argument (`2`) after specifying `x` by name.

### Extensions

Extensions provide helper members for an existing receiver type without
modifying the original declaration. The `extension` keyword declares a
namespace-scoped container that targets a specific type via a `for` clause.
Importing the container (for example, `import MyApp.StringExt.*`) brings its
members into scope for lookup.

Each member inside the body is implicitly an extension member for the receiver
type. Members may be function declarations or computed properties. The compiler
synthesizes a `self` parameter whose type matches the receiver and passes it as
the first argument whenever the member is invoked. The `self` parameter behaves
like a `let` binding: it cannot be reassigned but may be used to access members
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
let result = 5 |> Square() |> AddOne()

let result = AddOne(Square(5))
```

When the pipeline targets an invocation, the syntax mirrors a regular call:

```raven
let result = 5 |> MathHelpers.Increment(2)

public static class MathHelpers {
    public static Increment(x: int, amount: int) -> int {
        return x + amount
    }
}
```

The pipe operator accepts either an invocation or a property access with a setter on the right-hand side. If the syntax does not form either shape, diagnostic `RAV2800` is issued.【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L2690-L2766】【F:src/Raven.CodeAnalysis/DiagnosticDescriptors.xml†L19-L23】

If the pipeline targets a property, Raven assigns the left expression to that property through its setter before producing the property's type as the result of the pipe expression. Both instance and static properties are supported:

```raven
let container = Container()
let _ = 42 |> container.Value
let _ = 42 |> Container.Count

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
`let offset = ^2` is valid without annotations. When indexing arrays, from-end
indices are computed using the array's length and are evaluated exactly once
alongside the receiver.

### Range expressions

`..` produces a `Range` value that can be stored, passed to APIs, or used for
slicing receivers that understand .NET ranges. Both endpoints are optional, and
either endpoint may be written as a **from-end** index by prefixing it with `^`.

```raven
val r = 3..^5
let head = ..3
let tail = 3..
let all  = ..
```

Forming a range evaluates each supplied boundary exactly once, left-to-right.
Each boundary expression must be implicitly convertible to `int`; the `^` prefix
indicates the operand counts from the end of the receiver instead of from the
start. The resulting `Range` uses `Index.FromStart` for ordinary boundaries and
`Index.FromEnd` for prefixed ones, and omitting a boundary produces
`Range.StartAt`, `Range.EndAt`, or `Range.All` accordingly. A range expression
retains its `Range` type even when no target type is provided, enabling
declarations like `val r = 3..^5` without additional annotations.

### Bitwise operators

The binary `&` and `|` operators perform bitwise combination on `int`, `long`, and matching enum operands. When both operands are `bool`, they evaluate **without** short-circuiting and return `bool`, allowing direct use in non-conditional contexts or within compound assignments. Operands must share the same enum type when applied to enums; the result has that enum type.

Compound assignments `&=` and `|=` are available and apply the corresponding binary operator after evaluating the left-hand side once. These operators share left-to-right associativity with other Raven binary operators, and their precedence sits between the logical (`||`, `&&`) and equality operators.

Enum member accesses support **leading-dot** syntax when a target type is already known, including inside bitwise combinations and argument lists:

```raven
let flags: BindingFlags = .NonPublic | .Static

func WithBinding(flags: BindingFlags) { /* ... */ }

WithBinding(.Public | .Instance)
```

### Object creation

Objects are created by **calling the type name** directly, just like any
other method.

```raven
let sb = StringBuilder()
sb.AppendLine("Foo")
````

Generic types work the same way:

```raven
let list = List<int>()
list.Add(2)
```

Raven also supports the `new` keyword for **backwards compatibility** and
for cases where you want to be explicit about creating an object:

```raven
let sb = new StringBuilder()
let list = new List<int>()
```

This way it’s clear that *constructor-as-call* is the default, and `new` is optional/explicit.  

### Tuple expressions and access

Tuples can be **named** or **positional**. Both projections are available.

```raven
let tuple = (a: 42, b: 2)
Console.WriteLine(tuple.a)      // named
Console.WriteLine(tuple.Item1)  // positional
```

### Block expression

A block is an expression; its value is the value of its last expression
(or `()` if none). Each block introduces a new scope for local declarations.

```raven
{
    let x = 10
    x + 1
}
```

### `if` expression

`if` expressions evaluate the condition and execute exactly one branch. The
value of the overall expression is the value produced by the executed branch. If
both branches produce values, the result participates in type inference as
described in [Type inference](#type-inference).

```raven
let res =
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
    let item = list[i]
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
    let ok = tryOnce()
    if not ok {
        goto start
    }
}
```

### `throw` statement

`throw expression` aborts the current control path by raising an exception. The
expression must be implicitly convertible to `System.Exception`; otherwise the
compiler reports `RAV1020`. As with other imperative statements, `throw` cannot
appear in expression contexts such as the arms of an `if` expression.
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

### Pattern matching

Patterns let you inspect values using concise, algebraic syntax. They appear in `is`
predicates and in `match` expressions and participate in Raven’s flow-sensitive
type analysis.

#### Pattern forms

Patterns compose from the following primitives.

##### Type and binding patterns

* `Type` — **type pattern**. Succeeds when the scrutinee can be treated as `Type`.
  If the pattern introduces no designation, it behaves like a type test.

* `Type name` — **typed binding**. Succeeds when the scrutinee can be treated as
  `Type`, then binds the converted value to `name` as an immutable local in the
  success scope.

* `let name` / `val name` / `var name` — **variable pattern**. Always matches and
  introduces a binding. `let`/`val` produce an immutable local; `var` produces a
  mutable one.

  Parenthesized designations such as `let (first, second): (int, string)` bind
  each element positionally.

* **Implicit `val` in bindings.** When a binding is expected but `let`/`val`/`var`
  is omitted, the binding is treated as `val` (immutable). For example, `.Ok(n)`
  is equivalent to `.Ok(val n)`.

##### Discards

* `_` / `Type _` — **discard**. Matches without introducing a binding. The typed
  form asserts the value can be treated as `Type` while still discarding it.
  Because `_` is reserved for discards, writing `_` never creates a binding.

  Discards participate in exhaustiveness: an unguarded `_` arm is a catch-all and
  satisfies any remaining cases (even if earlier arms introduced bindings).

##### Constant patterns

* `literal` — **constant pattern**. Matches when the scrutinee equals the literal
  value (`true`, `"on"`, `42`, or `null`). Literal patterns piggyback on Raven’s
  literal types, so they also narrow unions precisely.

##### Relational patterns

* `< expr`, `<= expr`, `> expr`, `>= expr`, `== expr`, `!= expr` — **relational
  pattern**. Matches when the scrutinee compares to the operand using the given
  operator.

  The operand is an expression, but the binder should restrict it to “constant-ish”
  operands (for example, literals, consts, or other side-effect-free values), so
  relational patterns remain predictable and optimizable.

  Relational patterns are often used under `not`, `and`, and property patterns, e.g.
  `{ Age: not > 30 }`.

##### Tuple patterns

* `(element1, element2, …)` — **tuple pattern**. Matches when the scrutinee is a
  tuple with the same arity and each element matches the corresponding subpattern.

  Tuple patterns destructure positionally. Each element is itself a pattern and
  may introduce bindings. For example:

  * `(a, b)`
  * `(val a, val b)`
  * `(x: int, y: string)`
  * `(first: int, second)`

  If an element uses a name without an explicit `let`/`val`/`var`, the binding is
  treated as `val`.

  An element may optionally include a name before the colon (`name: pattern`) to
  bind the element value while still applying a nested pattern.

##### Property patterns

* `Type { member1: pattern1, member2: pattern2, … }` — **property pattern**. Matches
  when the scrutinee is not `null` and can be treated as `Type`, then evaluates each
  listed member subpattern against the corresponding instance member on the value.

  * Each `member: pattern` entry targets an **instance field** or a **readable instance
    property** named `member` on `Type`. (Properties must have an accessible getter and
    must not be indexers.)
  * The nested pattern is type-checked against the member’s type.
  * Access checks apply: a property pattern may only read members that are accessible at
    the use site.
  * Property patterns fail if the scrutinee is `null`.
  * Member subpatterns are evaluated left-to-right. Bindings introduced by earlier
    subpatterns are in scope for later subpatterns in the same property pattern.
  * The empty property pattern `Type { }` matches any non-`null` value that can be treated
    as `Type`.

* `{ member1: pattern1, member2: pattern2, … }` — **inferred property pattern**. Like
  `Type { ... }`, but the receiver type is inferred from the scrutinee’s static type.

  * If the scrutinee’s static type is a concrete named type (and not just `object`), that
    type is used as the receiver type for member lookup.
  * If the receiver type cannot be inferred (for example, the scrutinee is `object`,
    an unconstrained type parameter, or a union where multiple candidates could apply),
    the pattern is invalid and the compiler reports a diagnostic requiring an explicit
    type.
  * The empty inferred property pattern `{ }` matches any non-`null` scrutinee (it is a
    non-null test).

##### Discriminated union case patterns

* `.Case` / `Type.Case` — **case pattern**. Matches a discriminated union case by
  name.

  * The leading `.` resolves against the current scrutinee in a `match` arm or `is`
    expression.
  * A qualifying type name forces lookup against that union regardless of the scrutinee’s
    static type.
  * Case patterns may supply nested subpatterns in parentheses to bind or validate payload
    fields, mirroring the parameter list declared on the case:
    `.Identifier(text)` or `Result<int>.Error(let message)`.
  * Parentheses are optional for parameterless cases: `.Unknown` and `.Unknown()` are
    equivalent.
  * The payload arity must match the declared parameter list.
  * Each nested subpattern is typed to the corresponding case parameter.
  * Case payloads are read from the generated case properties before evaluating nested
    patterns, so `.Ok(value)` binds `value` with the case’s declared type.
  * Bare payload identifiers implicitly bind immutable locals, so `.Ok(payload)` is
    equivalent to `.Ok(val payload)`; use `_` to explicitly discard a payload.

##### Pattern combinators

* `pattern1 and pattern2` — **conjunction**. Succeeds only when both operands match.
  Bindings from either operand are available after the conjunction.

* `pattern1 or pattern2` — **alternative**. Matches when either operand matches.
  Parentheses may be used to group alternatives.

* `not pattern` — **complement**. Succeeds when the operand fails. `not` does not
  introduce bindings even if its operand would.

Precedence: `not` binds tighter than `and`, which binds tighter than `or`. `or` associates
left-to-right. Parentheses override precedence.

```raven
if payload is string {
    Console.WriteLine("plain text payload")
}

if payload is string body {
    Console.WriteLine("bytes ${body.Length}")
}

if payload is string _ {
    Console.WriteLine("stringly typed legacy input")
}

if value is "ok" or "pending" {
    Console.WriteLine("proceed")
}

if mode is not ("on" or "off") {
    Console.WriteLine("unexpected mode")
}

if token is .Identifier(text) {
    Console.WriteLine($"identifier {text}")
}

if token is .Identifier(let text) and text != "" {
    Console.WriteLine($"non-empty identifier {text}")
}
```

Property patterns:

```raven
if x is Foo { Value: true } {
    WriteLine("Foo with Value == true")
}

if x is Foo { Value: val v } {
    WriteLine("Foo.Value bound to v = $v")
}

if x is Foo { Data: (a, b) } {
    WriteLine("Tuple destructured: a=$a, b=$b")
}

if x is Foo { Age: not > 30 } {
    WriteLine("Age <= 30")
}

// Inferred receiver type (x is Foo here), so Foo is implied:
if fooTrue is { Value: true } {
    WriteLine("Also consistent Foo")
}

// Non-null test:
if x is { } {
    WriteLine("x is not null")
}
```

#### `is` expression

`expr is pattern` evaluates to `bool`. On success, the compiler narrows the
expression to the pattern’s accept set inside the `true` branch and discards the
matched portion from the `false` branch. Any bindings introduced by the pattern
are locals scoped to the `true` branch.

```raven
let token: object = read()

if token is int n {
    Console.WriteLine(n + 1) // token is narrowed to int here
} else if token is "quit" {
    Console.WriteLine("bye")
}
```

When the pattern is a property pattern (`Type { ... }`), the `true` branch also
treats the scrutinee as `Type` for member access, and any bindings introduced by
nested subpatterns are in scope within the `true` branch.

When the pattern is an inferred property pattern (`{ ... }`), the `true` branch
treats the scrutinee as the inferred receiver type for member access. The empty
form `{ }` narrows the scrutinee to “non-null”.

#### Flow-sensitive narrowing

Both `is` and `match` feed Raven’s flow analysis. Within the successful branch
of `is`, or inside a matched arm, the scrutinee is narrowed to the pattern’s
accept set and all pattern bindings are in scope. The complement carries through
the opposite branch.

Guard expressions (`when ...`) do not participate in narrowing themselves, but they
may reference bindings established by the pattern they accompany.

Property patterns contribute narrowing as well: inside a successful property
pattern, the scrutinee is treated as the stated `Type` (or the inferred receiver
type), and each nested subpattern may introduce additional bindings and narrowing
for the corresponding member values.

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

let pi = PI
```

Extension methods defined on imported types are also brought into scope. This
enables consuming .NET helpers such as `System.Linq.Enumerable.Where` or
`System.MemoryExtensions.AsSpan` directly from Raven source:

```raven
import System.Collections.Generic.*
import System.Linq.*

let odds = List<int>()
odds.Add(1)
odds.Add(3)
let filtered = odds.Where(value => value % 2 == 1)
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

let sb = SB()
PrintLine("Hi")
let tmp = IO.Path.GetTempPath()
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

* The method returns `unit`, `int`, `Task`, or `Task<int>`.
* It has no type parameters.
* It declares either no parameters or a single parameter of type `string[]`
  (representing the command-line arguments).

If exactly one method satisfies these conditions, it becomes the entry point for
the compilation. When no method qualifies, the compiler reports
`RAV1014` *Entry point 'Main' not found*. Declaring more than one valid
`Main` (including mixing top-level statements with a matching method) causes the
compiler to emit `RAV1017` *Program has more than one entry point defined*.

When the selected entry point returns `Task` or `Task<int>`, the compiler emits
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
and returns the awaited integer as the process exit code. The bridge also leaves
console writes intact so the awaited value can be observed by both the caller
and the host operating system. 【F:test/Raven.CodeAnalysis.Tests/CodeGen/AsyncILGenerationTests.cs†L405-L476】

Library and script output kinds ignore the entry point search; they never report
missing or ambiguous entry-point diagnostics.

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

### Parameters

Function, method, and accessor parameters use the `name: Type` syntax. Parameter
names are required and participate in overload resolution alongside their types
and any `ref`/`out` modifiers.

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

let number = identity(42)         // inferred T = int
let text = identity<string>("hi")
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

    let y = inner(2)
    let point = inner((x: 1, y: 2))
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
`let` or `var`, the initializer (or an explicit type annotation) supplies that
context so the delegate type—and corresponding overload—can be determined.
When no delegate context is available, diagnostic `RAV2201` is reported and the
method must either be invoked directly or annotated with a delegate type.

```raven
let writeLine: System.Action<string> = Console.WriteLine
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
let writeLine = Console.WriteLine             // error: overloaded method group
let writeLine: System.Action<string> = Console.WriteLine // ok
```

Passing `Console.WriteLine` as an argument to a parameter of type
`System.Action<string>` likewise selects the `string` overload without requiring
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
        let increment = self.Increment
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
        let callback = Accumulator.TryAccumulate
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

```raven
let value = 42
let pointer: *int = &value
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

let value = 0
let alias = &value      // alias : &int
let raw: *int = &value  // raw : *int
```

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

### Value binding (`let`)

A `let` binding is **immutable** (not reassignable). Types are inferred
unless annotated.

```raven
let x = "Foo"
let y: int = 2
let a: int = 2, b: string = ""
```

### Variable binding (`var`)

A `var` binding is **mutable** (reassignable).

```raven
var x = "Foo"
x = "Bar"

var y: int = 2
y = 3
```

### Constant binding (`const`)

A `const` binding is immutable like `let` but additionally requires a compile-time
constant initializer. The compiler embeds the resulting value directly into the
generated IL so the symbol can be referenced from other assemblies without
executing the initializer.

```raven
const pi: double = 3.141592653589793
const banner = "Ready"      // inferred string constant
```

Const bindings support the primitive constant forms recognized by .NET: numeric and
character literals (including the appropriate suffixes), `true`/`false`, strings, and
`null` for reference types. Type inference works the same way as `let`; the initializer
must still be a compile-time constant.

Const applies to both local bindings and fields. Member declarations treat `const`
fields as implicitly `static`; the value is emitted as metadata so other assemblies can
import it without running an initializer.

Tuple patterns let you bind or assign multiple values at once. The outer
`let`/`var` controls the tuple's mutability, while each element uses a
designation (possibly nested) to capture or discard the corresponding value.
Elements may include inline type annotations.

```raven
let (first, second, _) = (1, 2, 3)
var (head, tail: double, _) = numbers()
(first, second, _) = next()
(let lhs, var rhs: double, _) = evaluate()
```

Existing locals can participate in tuple assignments alongside new
bindings. Mixed `let`/`var` designations and inline type annotations are
supported in both declarations and assignments:

```raven
var first = 0
var second = 0

(first, second, _) = (1, 2, 3)
let (third, fourth: double, _) = toTuple()
var (let fifth, var sixth: double, _) = project()
```

Use `_` to discard unwanted elements. Nested tuples work the same way:

```raven
var ((x, y), let magnitude, _) = samples()
```

The discard identifier also appears in ordinary assignment statements. Writing
`_ = Compute()` produces a discard assignment statement whose left-hand side is a
dedicated discard expression. The assignment still evaluates the right-hand
expression, but the result is ignored. Discard assignments follow the same rules
as tuple assignment: they never declare a binding and may carry a type
annotation when overload resolution needs guidance. `AssignmentStatementSyntax`
exposes an `IsDiscard` helper when analyzers need to detect this pattern.

### Resource declarations (`using`)

Prefixing a local declaration with `using` introduces a scoped disposable resource. The
declaration follows the normal `let`/`var` syntax and must include an initializer. Both the
initializer's type and the declared type must be convertible to `System.IDisposable`; if
either conversion fails, Raven reports the same assignment diagnostic used for other
implicit conversions. 【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L188-L224】

Resources created with `using` remain in scope like ordinary locals but are automatically
disposed when control leaves the enclosing block. Disposal runs in reverse declaration
order so that later resources observe earlier ones still alive. File-scope `using`
declarations participate as well: they are disposed after the file's top-level statements
finish executing. 【F:src/Raven.CodeAnalysis/Binder/BlockBinder.cs†L222-L282】【F:src/Raven.CodeAnalysis/CodeGen/Generators/Generator.cs†L54-L87】【F:src/Raven.CodeAnalysis/CodeGen/MethodBodyGenerator.cs†L114-L148】

```raven
using let stream = System.IO.File.OpenRead(path)
using var reader = System.IO.StreamReader(stream)

let text = reader.ReadToEnd()
// reader.Dispose() and stream.Dispose() run automatically when the scope ends
```

## Types

### Type annotations

Use type annotations where inference is insufficient (e.g., for
parameters, some bindings, or return types):

```raven
let a = 2
let b: int = 2

func add(a: int, b: int) -> int { a + b }
```

### Tuple types

Tuple types use parentheses with comma-separated element types and map to
`System.ValueTuple`:

```raven
let pair: (int, string) = (42, "answer")
```

Elements may optionally be named with a `name: Type` pair. Names exist only for
developer clarity and do not participate in type identity or assignment:

```raven
let tuple2: (id: int, name: string) = (no: 42, identifier: "Bar")
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
let applyTwice: (int -> int, int) -> int
let thunk: () -> unit
let comparer: (string, string) -> bool
```

Single-parameter functions may omit the surrounding parentheses:

```raven
let increment: int -> int
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

```raven
let a: int | string
let b = if flag { 1 } else { "one" }  // b : int | string
```

#### Literal types in unions

A **literal type** represents a single constant value of a primitive, string, bool, or enum type. Literal types may appear anywhere a type is expected and compose naturally with unions.

```raven
let n: 1 | 2 | 3
let s: "on" | "off" | "auto"
let e: Grades.A | Grades.B
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
let maybe = if flag { 1 } else { null }  // int | null
```

If a union contains `null` and exactly one non-nullable type `T`, it implicitly converts to `T?` (both in inference and explicit annotations):

```raven
let x: int? = maybe         // ok
let y: string? | int        // error: explicit nullable types must not be unioned
```

To model absence explicitly, Raven recommends the **Option union** defined in
`src/Raven.Core/Option.rav` (`System.Option<T>`). It behaves like a
`T | null` union for both reference and value types and includes an implicit
conversion to the nullable form (`T?` / `Nullable<T>`) when interacting with
existing .NET APIs that expect nullable types.

#### Assignability and conversions

Let `U = T1 | … | Tn` be a source union.

* **Value → union**: An expression of type `S` may be assigned to `U` if `S` is assignable to **at least one** member `Ti`.
* **Union → value**: `U` may be assigned to `S` only if **every** member `Ti` is assignable to `S`.
* **Literal checking**: Assigning to a **finite literal union** requires the value to be a compile-time constant equal to one of the listed literals.
* **Variant interfaces**: When `Ti` or `S` is a generic interface or delegate, assignability follows the CLR's variance annotations. Covariant parameters permit `T<Derived>` to flow to `T<Base>`, and contravariant parameters accept `T<Base>` where `T<Derived>` is expected. Raven interface declarations expose the same behaviour with `out` and `in` modifiers on their type parameters, so source and metadata symbols participate in the same set of conversions.

```raven
let a: "true" | 1 = 1      // ok
let b: "true" | 1 = 2      // error: 2 not permitted by '"true" | 1'
let c: "yes" | "no" = "yes"  // ok
```

When a union value is assigned or returned to an explicit target type, the compiler checks each constituent individually:

```raven
let u = if flag { 0 } else { 1.0 }  // int | float

let i: int = u       // error: float not assignable to int
let o: object = u    // ok: both convert to object
```

#### Type inference with control flow

Control-flow expressions that produce different types infer a union of those results. The union is normalized; literal branches collapse into their base if a non-literal of that base also flows.

```raven
let x = if flag { 42 } else { 3 }        // x : 42 | 3
let y = if flag { 42 } else { parseInt() } // y : int   (42 collapses into int)
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
let msg = u.ToString()   // treat receiver as the ancestor that declares ToString
// lower: ((Ancestor)u).ToString()
```

If any element **hides** the base member with `new` (different original), or an overload isn’t present on some element, the access is rejected with a diagnostic suggesting a cast or pattern match.

Examples:

```raven
open class Animal { virtual Speak() -> string }
class Dog : Animal { override Speak() -> "woof" }
class Cat : Animal { override Speak() -> "meow" }

let a: Dog | Cat
let sound = a.Speak()    // ok via Animal.Speak
```

```raven
open class Base { virtual P: int { get; set; } }
class D1 : Base { override P : int { get; set; } }
class D2 : Base { override P : int { get; } }

let u: D1 | D2 = D1()
let x = u.P      // ok (getter common)
u.P = 3          // error (setter not common)
```

#### Operations on literal types

Operations generally **widen** literals to their base types unless constant-folding applies:

```raven
let k: 2 | 42 = 2
let z = k + 1       // z : int
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
let s: string? = null
let i: int? = null
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

let number: int? = 42
let digits = number?.ToString() // "42"
```

Here `str` is `string?`, and the call to `ToString` only occurs when `x` is not
`null`. When the receiver is a nullable value type, the compiler unwraps the
`System.Nullable<T>` storage, invokes the member on the underlying value, and
wraps the result back into a nullable type.

Null-conditional access also supports direct invocations and element access.
Use `?(...)` to invoke a nullable delegate or callable value, and `?[...]` to
index into a nullable collection or array.

```raven
let f: Func<int, int>? = null
let result = f?(2)

let values: int[]? = null
let first = values?[0]
```

Invoking or indexing a nullable value without a null-conditional operator
produces a diagnostic, since the receiver may be `null`.

The compiler also performs local flow analysis to recognize when a nullable
receiver is proven non-null. In the `true` branch of checks like `if value != null`
or `if value is not null`, or after guard statements like `if value == null { return }`,
member access and invocation on `value` are allowed without `?.` or `?`.

```raven
let f: Func<int, int>? = null

if f != null {
    let result = f(2)
}

if f == null {
    return
}

let result2 = f(2)
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

Importing the members of an enum brings them into scope:

```raven
import Grades.*
let best = A
```

### Discriminated unions

A discriminated union declaration defines a value type composed of a fixed set
of **cases**. Each case acts like an inline constructor with an optional payload
described by a parameter list. Unions use the `union` keyword:

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
let token = Token.Identifier("foo")
let other: Token = .Unknown
```

Each case becomes a nested struct (`Token.Identifier`, `Token.Number`, …) whose
constructor parameters mirror the payload declared on the case. Supplying the
enclosing type name is always allowed; omitting it (for example, `.Unknown`)
requires the context to already expect the containing union so the compiler can
resolve which declaration supplies the case.

For every case `Case`, the compiler synthesizes an implicit conversion
`Case -> Token`. Assigning, returning, or passing a case value therefore
automatically produces the union instance:

```raven
let ok: Result<int> = .Ok(99)          // implicit Case -> Result<int> conversion
let err = Result<int>.Error("boom")
Console.WriteLine(ok)
```

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
>   **pattern** such as a tuple deconstruction.

## Outstanding questions and suggested follow-ups

The current implementation leaves a few behaviours underspecified. The following
items capture those gaps and outline the preferred direction for addressing them:

- **`if` expressions without `else` in value contexts.** The compiler currently
  allows `let x = if cond { 1 }` even though the false branch does not yield a
  value, which in turn produces incomplete IL. Either require an `else` branch
  when the result is consumed or implicitly supply `()` for the missing branch.
  Both options should be accompanied by diagnostics that guide authors toward
  the intended usage.
- **Element type discovery for `for` loops.** Non-array collections default the
  iteration variable to `object`, losing type information. Extend binding to
  inspect `IEnumerable<T>`/`IAsyncEnumerable<T>` or the enumerator pattern so the
  loop variable reflects the element type instead of falling back to `object`.
 
