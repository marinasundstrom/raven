# Literals and fundamental expressions

This article covers casts, compile-time type and name queries, default values,
and Raven's string literal forms.

## Cast expressions

Explicit casts request a conversion to a specific type and use C# syntax.

```raven
let d = (double)1
let i = (int)3.14  // numeric narrowing
let s = obj as string
```

`(T)expr` performs a runtime check and throws an `InvalidCastException` when the value cannot convert to `T`. Use this form for downcasts, numeric narrowing, or unboxing scenarios.
`expr as T` attempts the conversion and returns `null` (or a nullable value type) instead of throwing on failure.

For unions, an explicit cast from the carrier to a member/case type is also
permitted as an assertion-style extraction:

```raven
let value: Either<int, string> = 42
let left = (int)value
```

This conversion succeeds only when the carrier currently holds the requested
member/case type; otherwise it throws `InvalidCastException`. It is an explicit
union extraction conversion, not a subtype or inheritance conversion.

## `typeof` expressions

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

## `sizeof` expressions

The `sizeof` operator produces the size, in bytes, of a compile-time type. The
operand must be a type syntax and is not evaluated. The expression always has
type `int`.

```raven
let intSize = sizeof(int)       // 4
let charSize = sizeof(char)     // 2
```

## `nameof` expressions

The `nameof` operator produces the **unqualified source name** of a symbol as a `string` at compile time. The operand is **not evaluated**; instead, the compiler validates the operand syntactically and semantically and substitutes the referenced symbol’s name.

The operand may refer to:

* a local variable or parameter
* a field or property
* a method, function, or event
* a type or type member
* a qualified or member-accessed symbol

The result of a `nameof` expression is always of type `string` and is a compile-time constant.

```raven
let x = 42
let name = nameof(x)          // "x"

let text = nameof(System.Console.WriteLine)
```

When applied to a member access, only the **final identifier** is returned:

```raven
nameof(System.Collections.Generic.List<int>)   // "List"
nameof(Console.WriteLine)                      // "WriteLine"
```

Using `nameof` with an invalid operand (such as a literal, invocation, or arbitrary expression) produces a binding diagnostic. Because `nameof` is evaluated at compile time, it cannot observe runtime state and has no side effects.

`nameof` is commonly used for diagnostics, argument validation, logging, and reflection-friendly APIs where symbol names must remain stable under refactoring.

Macros are compile-time symbols rather than runtime types or values.
`nameof(quote)` therefore produces `"quote"` when that imported alias resolves,
while `nameof(Raven.Macros.Quote)` produces `"Quote"`. A macro cannot be used as
the operand of `typeof`; doing so reports that the macro is not a type.

## Default expressions

`default` produces the zero-initialized value for a type. Use the explicit form
`default(T)` to request the default value for a known type `T`. The target-typed
literal `default` instead relies on the surrounding context—such as a variable
annotation, a return type, or an argument position—to supply the type. When no
target type is available, the compiler reports `RAV2011` because the literal
cannot be inferred.

For reference types and reference-constrained type parameters, `default`
produces `null`. That value can flow to nullable targets, but a non-nullable
target requires an explicit nullable suppression (`!`) and reports `RAV0403`.

```raven
let zero = default(int)
let emptyText: string? = default
let requiredText: string = default!
```

## String literals

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

### Multiline string literals

A multiline string literal is written with triple double quotes and can span
any number of lines:

```raven
let text = """
    Line one
    Line two
"""
```

Multiline literals are raw: their contents are taken as written and escape
sequences are not decoded. A sequence such as `\n` therefore remains a
backslash followed by `n`.

Multiline literals can interpolate values using the same `$identifier` and
`${expression}` forms as ordinary strings.

Indentation trimming applies equally to plain and interpolated multiline
strings, so indentation used to align the source does not become accidental
leading whitespace in the value.

The delimiters may appear on their own lines or immediately next to content.
The first matching closing delimiter ends the literal. Reaching the end of the
file first produces an unterminated-string diagnostic.

Trivia and comments may appear adjacent to the delimiters but are not part of the literal value.

## String interpolation

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

## Encoded string literals

An encoded string literal applies an encoding suffix directly to a string or
multiline string literal and produces bytes instead of a `string`.

```raven
let utf8 = "Hello"u8
let ascii = "Hello"ascii
```

The suffix must appear immediately after the closing delimiter with no
intervening whitespace.

Raven first evaluates the literal text, including ordinary escape decoding, and
then encodes its Unicode scalar values. The result has type `byte[]`.

Encoded string literals are constant expressions when the underlying string
literal is constant.

Supported encodings:

* `u8` — UTF-8 encoding, emitted without BOM.
* `ascii` — ASCII encoding. If any character is outside the ASCII range
  (`> 0x7F`), the compiler reports an error.

```raven
let data = "Pågen"u8
let ok = "Hello"ascii
let error = "Pågen"ascii // compile-time error
```

Raw/multiline non-interpolated string forms can also be encoded. Interpolated
string literals are not valid with encoding suffixes.

```raven
let ok = """
Hello
World
"""u8

let name = "World"
let error = "Hello ${name}"u8 // compile-time error
```

The resulting `byte[]` contains exactly the encoded bytes of the literal text.
The compiler does not add a BOM or null terminator.

Interpolation is intentionally excluded from encoded literals to keep their
behavior deterministic and compile-time-friendly. When text must be computed at
runtime, construct the string first and encode it explicitly through runtime
APIs such as `System.Text.Encoding.UTF8.GetBytes(...)` or
`System.Text.Encoding.ASCII.GetBytes(...)`.
