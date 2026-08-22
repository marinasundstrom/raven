# Desired Compiler API result shapes after bootstrap

> Design direction. This is the intended Raven-facing Compiler API after the
> compiler can consume Raven.Core types without creating a bootstrap cycle.
> Existing nullable and record-based APIs are transitional and are not a
> compatibility constraint while Raven remains experimental.

Raven's Compiler API should express Raven's own modeling principles. It should
remain familiar to users of Roslyn where those concepts fit, but it should not
copy nullable C# signatures when Raven has a more precise carrier for the
meaning of an outcome.

The same principle applies to the eventual Raven-authored compiler
implementation. Self-hosting is an opportunity to exercise the language as it
is intended to be used: compiler data structures, inputs, intermediate state,
and public APIs should use Raven constructs whenever those constructs express
the model accurately. Bootstrap constraints may require temporary .NET or C#
shapes, but those shapes should not become the design target for the
Raven-authored compiler.

## Shape outcomes by meaning

Choose the smallest result shape that represents the distinctions a caller
must handle:

| Meaning | Desired Raven shape |
| --- | --- |
| A value may legitimately be absent | `Option<T>` |
| An expected operation either succeeds or fails | `Result<T, TError>` |
| Several closed outcomes carry different data | A purpose-built union |
| An advanced outcome has structured subcategories | Payload or nested unions |
| Successful recovery can coexist with diagnostics | A purpose-built recovery result or union |
| A violated invariant, cancellation, or host failure | Exception/cancellation mechanism |

Do not force every API into `Result`. For example, a parser may return recovered
syntax and diagnostics at the same time; a binary success/error split would
discard useful information. Model that as its own result domain. Conversely,
do not create a custom union when `Option<T>` already says everything callers
need to know.

Nullable references remain appropriate when the API faithfully projects a .NET
metadata, reflection, or framework contract whose ABI is nullable. At that
boundary, a Raven-facing convenience can explicitly project the platform shape
into `Option` or another union. Null should not become the default representation
of absence merely because the compiler implementation is currently written in
C#.

## Use unions at their natural scale

When a value may hold one of several known types and the alternatives do not
need a new domain identity, use Raven's ad hoc (standard) union type directly.
This applies to parameters, return values, fields, locals, and intermediate
compiler state; it is not limited to error handling:

```raven
func BindTarget(target: ExpressionSyntax | PatternSyntax) -> BoundNode
```

An ad hoc union such as `ExpressionSyntax | PatternSyntax` states the complete
set of accepted value types without introducing a wrapper whose only purpose
is storage. Prefer it over a shared base type, `object`, parallel nullable
fields, or an untyped container when the actual domain is closed.

Syntax modeling is an important example. A child slot that permits exactly two
syntax node types can declare those types as a union instead of forcing both
through a new intermediate base class:

```raven
val Target: ExpressionSyntax | PatternSyntax
```

This lets the syntax model describe the grammar's actual alternatives and
avoids inheritance introduced only to make heterogeneous storage possible. It
does not prohibit a meaningful syntax hierarchy: common identity, traversal,
or behavior may still belong on `SyntaxNode` or another genuine abstraction.
The union removes the need for an artificial shared ancestor when the only
shared fact is that one slot accepts either type.

When that same set of alternatives has a stable meaning of its own, appears
repeatedly across an API, needs documentation or members, or should remain
nominally distinct from another union with the same member types, give it a
name with an ordinary parenthesized union declaration:

```raven
union BindTarget(ExpressionSyntax | PatternSyntax)
```

Use a body-form union instead when the alternatives are named cases that need
their own case-specific payloads. Naming is a modeling choice, not a default
requirement imposed merely because a value can have multiple possible types.

This distinction should be applied throughout bootstrap design reviews. Ask
first whether the state is a closed set of existing types, then whether that
set itself has a durable domain identity. The answers determine whether the
natural Raven shape is an ad hoc union, a named parenthesized union, or a
body-form union.

## Interoperability

`Option`, `Result`, and Raven-declared unions compile to ordinary .NET types.
They therefore remain callable from C# and other .NET languages. The C# usage
may be more explicit than Raven's exhaustive `match`, but interoperability does
not require the primary Raven API to erase its semantic distinctions.

Keep the underlying cases and payloads discoverable through normal metadata,
documentation, and symbol APIs. Avoid a parallel nullable API solely for C#;
add an adapter only when a real host integration benefits from one.

## Bootstrap migration

The current dependency graph makes pervasive Raven.Core result types difficult:
the compiler must build before the Raven core library that it compiles. Treat
that as a staging constraint:

1. Keep the current compiler layer buildable with bootstrap-safe .NET shapes.
2. Identify nullable and ad hoc result contracts by their intended meaning.
3. Introduce Raven-facing projections once they can be built without a cycle.
4. Move the owning APIs to `Option`, `Result`, or purpose-built unions after the
   bootstrap boundary supports them.
5. Remove redundant transitional contracts rather than maintaining two semantic
   models for compatibility.

The Compiler API is experimental, so migration should favor the clean final
shape over preserving accidental signatures.

## Application to macro APIs

Macro APIs dogfood this direction. A syntax-category requirement currently
returns `TSyntax?` because it lives in the bootstrap compiler assembly; its
eventual Raven-facing shape is `Option<TSyntax>`. Diagnostic-bearing fragment
parsers retain recovered syntax, the consumed source span, and diagnostics and
therefore need a richer recovery result rather than a plain `Result`.

Expansion outcomes may ultimately form a purpose-built union—for example,
expression expansion, declaration replacement, introduction, or failure—with
payloads that contain contributions, diagnostics, dependencies, and source
provenance appropriate to that case.

See also [The meaning of Raven features](../../lang/feature-meaning.md) and the
[macro authoring guide](../../macro-authoring.md).
