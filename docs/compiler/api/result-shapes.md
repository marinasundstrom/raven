# Raven-native Compiler API result shapes and pre-bootstrap adoption

> Design direction. The immutable bootstrap-v1 foundation will be selected only
> after the pre-bootstrap stabilization gates pass. The next compiler line may
> then begin adopting Raven.Core contracts while the compiler implementation
> remains in C#. This is preparation for a later source port, not the source
> port itself. Existing nullable and record-based APIs are transitional and are
> not a compatibility constraint while Raven remains experimental.

This direction is established by [ADR-0002: Qualify the bootstrap foundation
after stabilization](../architecture/decisions/0002-qualify-bootstrap-foundation-after-stabilization.md).

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

## Foundation release and stage separation

The release that passes the pre-bootstrap qualification gates is the last
release before Raven-native types begin to participate deliberately in compiler
API contracts. Keep its tag, SDK archives, NuGet family, VSIX, and source commit
immutable. Together they form the trusted bootstrap-v1 toolchain that can build
and diagnose the first bootstrap-v2 experiments.

The important boundary is between bootstrap versions, not between public
release version numbers:

1. **Bootstrap v1** is the qualified and frozen full C# compiler plus the first
   trusted `Raven.Core` it compiles.
2. **Bootstrap v2** remains the full C# compiler, but exposes selected public
   contracts using the Raven-authored `Option`, `Result`, and union runtime
   representations produced by bootstrap v1.
3. **Bootstrap v3** is the full compiler implemented in Raven. It may be reached
   one stable component boundary at a time while the C# compiler remains the
   differential oracle.

Bootstrap v2 consumes the exact, hash-verified bootstrap-v1 `Raven.Core`
assemblies checked into the repository rather than rebuilding Core with the
compiler that is currently being built. The build graph must make that
provenance visible and must prevent an accidental source-build cycle.
Before the first API migration, prove the assembly identity, load, packaging,
and side-by-side rules for the bootstrap-v1 and bootstrap-v2 `Raven.Core`
artifacts. Do not rely on whichever copy happens to win output-directory
resolution.

The existing dependency direction remains intentional. `Raven.CodeAnalysis`
may adopt foundational runtime contracts from a bootstrap-safe `Raven.Core`.
`Raven.Macros` remains a version-matched higher layer that consumes and
exercises compiler APIs; the compiler API must not acquire a reverse dependency
on the standard macro implementation library. Compiler-hosted macro contracts
continue to live in `Raven.CodeAnalysis`, while Raven-authored providers live in
`Raven.Macros`.

## Adopt native contracts before porting source

The C# implementation can construct and return Raven-authored .NET types. That
allows contract migration to start before any compiler subsystem is rewritten
in Raven. The C# call site may be more explicit, but it implements the same
public semantic model consumed naturally from Raven.

Migrate one coherent API family at a time:

1. State the meaning of every nullable, boolean-plus-out-value, exception, or
   weakly typed result in that family.
2. Select `Option`, `Result`, an ad hoc union, or a purpose-built union according
   to that meaning.
3. Add Raven and C# consumer tests for metadata shape, construction, matching,
   diagnostics, and ordinary runtime behavior.
4. Update macros, language services, analyzers, and samples through the public
   compiler API rather than adding host-specific adapters.
5. Remove the transitional contract once callers have migrated. Avoid two
   long-lived APIs that encode the same outcome differently.

Optional macro inputs are an early, concrete candidate. An absent syntax input
should eventually be modeled as `Option<ExpressionSyntax<T>>`, not as a
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

`Option`, `Result`, and Raven-declared unions compile to ordinary .NET types.
They therefore remain callable from C# and other .NET languages. The C# usage
may be more explicit than Raven's exhaustive `match`, but interoperability does
not require the primary Raven API to erase its semantic distinctions.

Keep the underlying cases and payloads discoverable through normal metadata,
documentation, and symbol APIs. Avoid a parallel nullable API solely for C#;
add an adapter only when a real host integration benefits from one.

## Pre-bootstrap migration plan

The current source-build graph still makes pervasive Raven.Core result types
difficult: the compiler normally builds before the Raven core library that it
compiles. Treat that as a staging constraint, solved with an explicit published
foundation rather than an implicit cycle:

1. Qualify and preserve one reproducible bootstrap-v1 compiler and checked-in
   Core artifact family.
2. Prove a deterministic bootstrap-v2 build that verifies and consumes the
   checked-in Core rather than resolving it from a repository output, package
   cache, or global installation by accident.
3. Identify nullable and ad hoc result contracts by their intended meaning.
4. Pilot `Option`, `Result`, and union contracts in small, high-value compiler
   API families while the implementation remains in C#.
5. Exercise those contracts from Raven-authored macros and tools, and from C#
   interoperability tests.
6. Expand adoption only after clean builds, incremental builds, packaging,
   editor hosts, and public consumers all resolve the same type identities.
7. Begin porting compiler components later, behind stable boundaries and
   differential tests against the C# implementation.
8. Remove redundant transitional contracts rather than maintaining two
   semantic models for compatibility.

The Compiler API is experimental, so migration should favor the clean final
shape over preserving accidental signatures.

Before any Raven-authored compiler component becomes authoritative, require:

- a fresh machine can restore the exact bootstrap-v1 toolchain and reproduce
  the checked-in Core artifacts when an exceptional rebuild is required;
- an ordinary bootstrap-v2 build consumes the checked-in Core without invoking
  the bootstrap-v1 compiler;
- the bootstrap version used to build each foundational assembly is recorded and
  inspectable;
- no build succeeds only because a repository-local NuGet feed, global SDK, or
  stale output supplied an untracked assembly;
- Raven and C# consumers agree on the public metadata and behavior of migrated
  contracts;
- the current C# compiler remains the behavioral oracle for syntax, semantics,
  diagnostics, operations, metadata, and runtime results;
- rollback means selecting the immutable qualified bootstrap-v1 toolchain, not
  reconstructing an approximation from a later source tree.

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
