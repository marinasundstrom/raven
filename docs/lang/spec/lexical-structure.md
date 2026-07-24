# Lexical structure

Lexical rules define how source text is divided into names, keywords, literals,
comments, and punctuation.

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

The lexer defines the complete keyword set in
[`Tokens.xml`](https://github.com/marinasundstrom/raven/blob/main/src/Raven.CodeAnalysis/Syntax/Tokens.xml), which
classifies each keyword as either reserved or contextual.

| Kind | Keywords |
| --- | --- |
| Reserved | `and`, `as`, `await`, `base`, `bool`, `break`, `byte`, `catch`, `char`, `class`, `const`, `continue`, `decimal`, `default`, `double`, `else`, `enum`, `false`, `finally`, `fixed`, `float`, `for`, `func`, `goto`, `if`, `int`, `interface`, `is`, `let`, `long`, `loop`, `match`, `new`, `nint`, `not`, `null`, `nuint`, `object`, `or`, `permits`, `return`, `sbyte`, `self`, `short`, `sizeof`, `string`, `struct`, `throw`, `true`, `try`, `typeof`, `uint`, `ulong`, `ushort`, `var`, `when`, `while`, `yield` |
| Contextual | `abstract`, `alias`, `explicit`, `final`, `get`, `implicit`, `import`, `in`, `init`, `internal`, `namespace`, `open`, `operator`, `partial`, `out`, `override`, `private`, `protected`, `public`, `ref`, `sealed`, `set`, `static`, `unit`, `use`, `val`, `virtual` |

Reserved keywords are always treated as keywords and therefore unavailable for use as identifiers. Contextual keywords behave like ordinary
identifiers except in the syntactic positions that demand their special meaning—for example, accessibility modifiers
(`public`, `internal`, `protected`, `private`) or accessor modifiers (`get`, `set`). The `partial` keyword is only recognised
when declaring partial types or partial members; see [Partial types and
members](inheritance-and-partial-types.md#partial-types-and-members).

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

Function expressions may also spell a parameter as `_`. The discard parameter
still consumes the corresponding
delegate parameter slot for arity and type inference, but it does not introduce
a name that can be referenced in the function body and it is excluded from
unused-parameter diagnostics.

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
It is specified in
[`Model.xml`](https://github.com/marinasundstrom/raven/blob/main/src/Raven.CodeAnalysis/Syntax/Model.xml) and
drives code generation of the immutable syntax tree API.

The model describes the set of node kinds and their children.
The **parser** applies the grammar and contextual rules to construct these nodes.

In short: the model defines the shape; the parser defines the rules,
as outlined in this specification.
