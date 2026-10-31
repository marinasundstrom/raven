# Raven-native Compiler API result shapes and pre-bootstrap adoption

> Long-term API design guidance, not a prerequisite for the source port.
> [ADR-0004](../architecture/decisions/0004-port-first-bootstrap.md) selects faithful
> Raven translation alongside C# maintenance, followed by parity and gradual
> idiomatic evolution. C# API migration is optional; preserve existing contracts
> initially where that enables test reuse and clear comparison.

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

## Porting and API evolution are separate decisions

Follow the [bootstrap procedure](../architecture/bootstrap-procedure.md) for
parallel C#/Raven maintenance, test reuse, parity and self-hosting, the later C#
freeze, and eventual neoCLR hosting. Do not combine the initial source translation
with a broad redesign of result shapes. The compiler should become idiomatic
Raven through separately validated changes after correspondence is established.

The C# implementation can construct and return Raven-authored .NET types, so an
API family may migrate before or during porting when justified. This is optional,
not a mandatory bootstrap-v2 stage. Such a migration must preserve a buildable,
explicit compiler/Core dependency graph with exact artifact provenance.
`Raven.Macros` remains a higher-layer consumer, never a compiler dependency.

## Adopt native contracts in bounded changes

Migrate one coherent API family at a time:

1. State the meaning of every nullable, boolean-plus-out-value, exception, or
   weakly typed result in that family.
2. Select `Optional`, `Result`, an ad hoc union, or a purpose-built union according
   to that meaning.
3. Add Raven and C# consumer tests for metadata shape, construction, matching,
   diagnostics, and ordinary runtime behavior.
4. Update macros, language services, analyzers, and samples through the public
   compiler API rather than adding host-specific adapters.
5. Remove the transitional contract once callers have migrated. Avoid two
   long-lived APIs that encode the same outcome differently.

Optional macro inputs are an early, concrete candidate. An absent syntax input
should eventually be modeled as `Optional<ExpressionSyntax<T>>`, not as a
required parameter followed by a nullable parameter with a default value. This
keeps the contract meaningful and respects Raven's ordinary parameter-ordering
rules. The same reasoning applies beyond macros wherever absence is expected
and nonexceptional.

This phase does not justify replacing every internal C# nullable reference.
Start at stable public and cross-component boundaries where Raven-native types
improve the model. Internal representation can migrate when it helps the owning
component without obscuring compiler correctness or bootstrap provenance.

## Shape outcomes by meaning

Choose the smallest result shape that represents the distinctions a caller
must handle:

| Meaning | Desired Raven shape |
| --- | --- |
| A value may legitimately be absent | `Optional<T>` |
| An expected operation either succeeds or fails | `Result<T, TError>` |
| Several closed outcomes carry different data | A purpose-built union |
| An advanced outcome has structured subcategories | Payload or nested unions |
| Successful recovery can coexist with diagnostics | A purpose-built recovery result or union |
| A violated invariant, cancellation, or host failure | Exception/cancellation mechanism |

Do not force every API into `Result`. For example, a parser may return recovered
syntax and diagnostics at the same time; a binary success/error split would
discard useful information. Model that as its own result domain. Conversely,
do not create a custom union when `Optional<T>` already says everything callers
need to know.

Nullable references remain appropriate when the API faithfully projects a .NET
metadata, reflection, or framework contract whose ABI is nullable. At that
boundary, a Raven-facing convenience can explicitly project the platform shape
into `Optional` or another union. Null should not become the default representation
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

Use the case-declaration form of a nominal union instead when the alternatives
are named cases that need their own case-specific payloads. Naming is a
modeling choice, not a default requirement imposed merely because a value can
have multiple possible types.

This distinction should be applied throughout bootstrap design reviews. Ask
first whether the state is a closed set of existing types, then whether that
set itself has a durable domain identity. The answers determine whether the
natural Raven shape is an ad hoc union or a nominal union, using either the
parenthesized or case-declaration form.

## Interoperability

`Optional`, `Result`, and Raven-declared unions compile to ordinary .NET types.
They therefore remain callable from C# and other .NET languages. The C# usage
may be more explicit than Raven's exhaustive `match`, but interoperability does
not require the primary Raven API to erase its semantic distinctions.

Keep the underlying cases and payloads discoverable through normal metadata,
documentation, and symbol APIs. Avoid a parallel nullable API solely for C#;
add an adapter only when a real host integration benefits from one.

## Validation of an API migration

For a deliberately selected API family:

1. Record the existing meaning and compatibility boundary before changing types.
2. Pin the compiler and Core inputs and verify their identities; avoid a compiler
   depending on Core that only that same compiler can build.
3. Exercise the new shapes from Raven and C# consumers, macros and language tools.
4. Check metadata, runtime behavior, incremental queries, clean builds and package
   consumption. Do not rely on stale outputs or a global installation.
5. Migrate callers and tests with preserved behavioral assertions, then remove
   redundant transitional contracts.

During the source port, keep shared fixes aligned in both compiler implementations.
After the C# freeze, normal API evolution belongs in Raven. Preserve the exact
trusted artifacts needed for reproduction and rollback; no particular historical
v1/v2/v3 stage is a prerequisite for selecting an API improvement.

## Application to macro APIs

Macro APIs dogfood this direction. A syntax-category requirement currently
returns `TSyntax?` because it lives in the bootstrap compiler assembly; its
eventual Raven-facing shape is `Optional<TSyntax>`. Diagnostic-bearing fragment
parsers retain recovered syntax, the consumed source span, and diagnostics and
therefore need a richer recovery result rather than a plain `Result`.

Expansion outcomes may ultimately form a purpose-built union—for example,
expression expansion, declaration replacement, introduction, or failure—with
payloads that contain contributions, diagnostics, dependencies, and source
provenance appropriate to that case.

See also [The meaning of Raven features](../../lang/feature-meaning.md) and the
[macro authoring guide](../../macro-authoring.md).
