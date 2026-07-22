# Fundamental expressions

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

## `typeof` expressions

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

## `sizeof` expressions

The `sizeof` operator produces the size, in bytes, of a compile-time type. The
operand must be a type syntax and is not evaluated. The expression always has
type `int`.

```raven
val intSize = sizeof(int)       // 4
val charSize = sizeof(char)     // 2
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
val zero = default(int)
val emptyText: string? = default
val requiredText: string = default!
```

## String literals

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

### Multiline string literals

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

## String interpolation

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

## Encoded string literals

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
