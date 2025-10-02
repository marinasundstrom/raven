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

You find them [here](../../../src/Raven.Compiler/samples/).

## Proposals

You find proposals for future language features [here](../proposals/).

## File extension

The file extension for source code files is: `.rav`.

> Perhaps in the future we should use `.rvn`.

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
| Reserved | `and`, `as`, `base`, `bool`, `catch`, `char`, `class`, `double`, `each`, `else`, `enum`, `false`, `finally`, `for`, `func`, `if`, `int`, `interface`, `is`, `let`, `match`, `new`, `not`, `null`, `object`, `or`, `return`, `self`, `string`, `struct`, `true`, `try`, `var`, `when`, `while` |
| Contextual | `abstract`, `alias`, `get`, `import`, `in`, `init`, `internal`, `namespace`, `open`, `partial`, `out`, `override`, `private`, `protected`, `public`, `ref`, `sealed`, `set`, `static`, `unit`, `using`, `virtual` |

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

`let` introduces an immutable binding while `var` introduces a mutable one. A binding
may declare its type explicitly or rely on the compiler to infer it from the
initializer expression.

```raven
let answer = 42        // inferred int
var name = "Alice"   // inferred string, mutable
let count: long = 0    // explicit type
```

If the type annotation is omitted, an initializer is required so the compiler can
determine the variable's type. With an explicit type, the initializer may be
omitted.

Control-flow constructs such as `if`, `while`, and `for` are expressions whose
statement forms are described in [Control flow](control-flow.md).

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
target types. Type inference for `let` and `var` bindings uses this mechanism to
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
`float` literal.

```raven
var l = 4_000_000_000  // l : long
var f = 3.14f          // f : float
var d = 3.14           // d : double
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
the inferred return type (treating `unit`/`void` as an action). Captured
variables participate in the enclosing flow analysis before the delegate type is
constructed, so the lambda observes the same declared type as any other use of
the variable.

```raven
let a = 42
let makeAdder = () => a + 3

makeAdder() // returns 45, makeAdder : System.Func<int>
```

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

```raven
let name = "Alice"
let age = 30
let msg = "Name: ${name}, Age: ${age}"
Console.WriteLine(msg)
```

Escapes inside the literal portions of an interpolated string follow the same
rules as ordinary string literals, ensuring Unicode escapes work uniformly in
both forms.

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

When the target has optional parameters, omitted trailing arguments are filled
in using the defaults declared on the parameter list. The supplied arguments are
matched positionally before defaults are considered.

#### Extension methods

Raven follows the CLR extension model so source and metadata helpers behave the
same way. An extension method is a `static` method whose first parameter is
treated as the **receiver**. Raven considers a declaration an extension when it
appears on a `static` method inside a module or static class that can be
imported like any other type and the method carries the .NET
`ExtensionAttribute`. The attribute may be spelled either `[Extension]` or
`[ExtensionAttribute]`; both forms produce the required metadata. Raven emits the
same attribute when compiling the method so C# and other CLR languages recognize
it as an extension.【F:src/Raven.CodeAnalysis/Symbols/Source/SourceMethodSymbol.cs†L197-L233】

The receiver parameter determines which expressions may invoke the extension.
Additional parameters follow the ordinary parameter rules: they may be generic,
optional, `params`, or accept lambdas. Generic receiver parameters are
substituted during method type inference, so helpers like `Where<T>(this
IEnumerable<T>, Func<T, bool>)` become available to Raven code as soon as the
appropriate namespace is imported.

```raven
import System.Runtime.CompilerServices.*

public static class NumberExtensions {
    [Extension]
    public static Double(x: int) -> int {
        return x + x
    }
}

public static class EnumerableExtensions {
    [Extension]
    public static Where<T>(source: IEnumerable<T>, predicate: Func<T, bool>)
        -> IEnumerable<T> {
        // implementation omitted
    }
}
```

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
and default the iteration variable to `object`. Like other looping constructs, a
`for` expression evaluates to `()`.

### Pattern matching

Patterns let you inspect values using concise, algebraic syntax. They appear in
`is` predicates and in `match` expressions and participate in Raven's
flow-sensitive type analysis.

#### Pattern forms

Patterns compose from the following primitives:

- `Type` — type pattern; succeeds when the scrutinee can convert to `Type`.
  No binding is introduced.
- `Type name` — typed binding; succeeds when the scrutinee converts to
  `Type`, then binds the converted value to `name` as an immutable local.
- `_` / `Type _` — discard; matches anything without introducing a binding.
  The typed form asserts the value can convert to `Type` while still discarding
  it. Because `_` is reserved for discards, writing `_` as the designation never
  creates a binding. Discards participate in pattern exhaustiveness: an
  unguarded `_` arm is considered a catch-all and satisfies any remaining cases
  even when earlier arms introduced bindings.
- `literal` — literal pattern; matches when the scrutinee equals the literal.
  Literal patterns piggyback on Raven's literal types, so `"on"` or `42`
  narrow unions precisely.
- `(element1, element2, …)` — tuple pattern; matches when the scrutinee is a
  tuple with the same arity and each element matches the corresponding
  subpattern. Tuple patterns destructure positionally, so nested patterns may
  test or bind each element independently. Each element may introduce a name
  before the colon (for example `(first: int, second: string)`) to bind the
  matched value while applying the nested subpattern.
- `pattern1 or pattern2` — alternative; matches when either operand matches.
  Parentheses may be used to group alternatives.
- `not pattern` — complement; succeeds when the operand fails. `not` does not
  introduce bindings even if its operand would.

`or` associates to the left, `not` binds tighter than `or`, and parentheses
override the default precedence as needed.

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
```

#### `is` expression

`expr is pattern` evaluates to `bool`. On success, the compiler narrows the
expression to the pattern's accept set inside the `true` branch and discards the
matched portion from the `false` branch. Any bindings introduced by the pattern
are immutable locals scoped to the `true` branch.

```raven
let token: object = read()

if token is int n {
    Console.WriteLine(n + 1) // token is narrowed to int here
} else if token is "quit" {
    Console.WriteLine("bye")
}
```

#### `match` expression

`match` uses braces with newline-separated arms. The keyword follows the scrutinee expression, so any expression can flow directly into a `match`:

```raven
scrutinee match {
    pattern [when guard] => result
    pattern => result
}
```

Arms are evaluated top to bottom. The first matching arm produces the
expression's value. Guards run after the pattern succeeds; they do not
influence exhaustiveness checking.

The scrutinee expression is evaluated exactly once before any arm runs.
Each arm introduces its own scope: pattern bindings are in scope for the
optional `when` guard and the arm's result expression, but they do not leak
into later arms. Guards use the `when` keyword and execute only after the
pattern succeeds; if the guard evaluates to `false`, evaluation falls through
to the next arm.

```raven
let state: "on" | "off" | "auto"

let description =
    state match {
        "on" or "auto" => "enabled"
        "off"         => "disabled"
    }
```

When multiple arms yield different types, the result type is the normalized
union of the arm results.

```raven
func classify(value: object) -> string {
    value match {
        string text when text.Length > 0 => text
        0                                 => "zero"
        _                                 => "unknown ${value}"
    }
}
```

Local helpers use the same `func` syntax—there is no separate F#-style `let` function form.

`match` requires exhaustiveness. For finite literal unions, every literal must
appear (or `_` used). When the scrutinee type includes an open set (for example
`int`, `string`, or a class hierarchy), add a catch-all arm (`_` or a broader
type pattern) to cover the remainder. Because `_` is a discard, it never
introduces a binding and always matches, so placing it last is a common way to
describe fallback behavior. Missing coverage produces `RAV2100`; redundant arms
that can never be chosen produce unreachable diagnostics (`RAV2101`), and
catch-alls that are unnecessary because earlier arms already cover every case
produce `RAV2103`.

#### Flow-sensitive narrowing

Both `is` and `match` feed Raven's flow analysis. Within the successful branch
of `is`, or inside a matched arm, the scrutinee is narrowed to the pattern's
accept set and all pattern bindings are in scope. The complement carries through
the opposite branch. Guard expressions do not participate in this narrowing, but
they may use the bindings established by the pattern they accompany.

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

### File-scope code rules

Files may start with executable statements that aren't enclosed in a function or
type. This file-scope code forms the application's entry point and is translated
into `Program.Main`. Only console applications may include file-scope code, and
it may appear in at most one file per compilation. When present, these
statements must come before any other declarations in the file or its
file-scoped namespace.

Function declarations (local function statements) within file-scope code are
hoisted and may be referenced from anywhere in that file-scoped region,
regardless of their order.

### Entry point resolution

Console applications begin executing at the synthesized `Program.Main` method
that backs file-scope code. When a project does not contain file-scope
statements, the compiler instead looks for a user-defined entry point. Any
static method named `Main` qualifies when it meets the following requirements:

* The method returns `unit`, `void`, or `int`.
* It has no type parameters.
* It declares either no parameters or a single parameter of type `string[]`
  (representing the command-line arguments).

If exactly one method satisfies these conditions, it becomes the entry point for
the compilation. When no method qualifies, the compiler reports
`RAV1014` *Program.Main entry point not found*. Declaring more than one valid
`Main` (including mixing top-level statements with a matching method) causes the
compiler to emit `RAV1017` *Program has more than one entry point defined*.

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

### Generic functions and methods

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

Method declarations use the same syntax:

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
implement. Violating a constraint produces a diagnostic identifying the failing
type argument and the unmet requirement.

### Nested functions

Functions may be declared inside other functions. Such a function is
scoped to its containing body and can capture local variables.

```raven
func outer() {
    func inner(x: int) -> int { x + 1 }
    let y = inner(2)
}
```

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

### `ref`/`out` arguments

Parameters can be declared by reference using `&Type`. Use `out` before
the parameter name to indicate that the value must be assigned by the
callee. At call sites, pass the argument with the address operator `&`.
(Exact rules are contextual; the binder enforces that the target is
assignable.)

```raven
func TryParse(text: string, out result: &int) -> bool { /* ... */ }

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

`null` may appear as a union member:

```raven
let maybe = if flag { 1 } else { null }  // int | null
```

If a union contains `null` and exactly one non-nullable type `T`, it implicitly converts to `T?` (both in inference and explicit annotations):

```raven
let x: int? = maybe         // ok
let y: string? | int        // error: explicit nullable types must not be unioned
```

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
class Base { virtual P: int { get; set; } }
class D1 : Base { override P : int { get; set; } }
class D2 : Base { override P : int { get; } }

let u: D1 | D2
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

The null-conditional operator `?.` safely accesses members or elements of a
nullable value. The expression `expr?.Member` evaluates `expr`; if the result is
`null`, the overall expression evaluates to `null` instead of throwing. When the
receiver is not `null`, the member access proceeds normally. This operator works
for both nullable reference types and nullable value types.

```raven
var str = x?.ToString()

let number: int? = 42
let digits = number?.ToString() // "42"
```

Here `str` is `string?`, and the call to `ToString` only occurs when `x` is not
`null`. When the receiver is a nullable value type, the compiler unwraps the
`System.Nullable<T>` storage, invokes the member on the underlying value, and
wraps the result back into a nullable type.

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

## Object-oriented types

For guidance on declaring classes, structs, members, and interfaces, see
[Classes, structs, and interfaces](classes-and-members.md).

## Operators (precedence summary)

Lowest → highest (all left-associative unless noted):

1. Assignment: `=  +=  -=  *=  /=  %=`
2. Null-coalescing: `??`
3. Logical OR: `||`
4. Logical AND: `&&`
5. Equality: `==  !=`
6. Relational: `<  >  <=  >=`
7. Type tests: `is  as` (binds after relational)
8. Additive: `+  -`
9. Multiplicative: `*  /  %`
10. Cast: `(T)expr`
11. Unary (prefix): `+  -  !  typeof`
12. Postfix trailers: call `()`, member `.`, index `[]`

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
 
