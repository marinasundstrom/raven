# Nullability, absence, and null flow

Raven encourages programs to model intentional absence with `Option<T>` and to
handle alternatives with patterns. Nullable types remain part of the language
because Raven is a .NET language: existing libraries use null, applications are
adopted gradually, and not every boundary can be redesigned at once.

The resulting policy is:

> Use `Option<T>` when absence is part of the domain. Use `T?` when null is part
> of an interoperability or migration boundary. In either case, handle the
> possible states explicitly before using the value.

This is guidance, not a second type system. Nullable Raven code still has a
precise, strict meaning.

## One unified nullable type model

`T?` means that a value of `T` may also be null. Raven applies that rule
uniformly to reference and value types:

```raven
let name: string? = ReadName()
let count: int? = ReadCount()
```

Both declarations are nullable in Raven's symbol model. Their CLR
representations differ, but that ABI detail does not create separate source or
semantic rules. Public semantic APIs preserve the declared nullable type while
reporting what is known about a particular use through its null-flow state.

A flow fact does not mutate a declaration. If `name` is declared as `string?`,
it remains declared as `string?` even at a position where analysis proves that
its current value is not null.

## Nullable values must be handled

By default, Raven reports an error when code dereferences a value that may be
null:

```raven
func PrintLength(value: string?) -> unit {
    // RAV0402: Possible null reference access
    WriteLine(value.Length)
}
```

Code must first establish a non-null value. Raven's preferred forms make that
result explicit with a pattern binding:

```raven
func PrintLength(value: string?) -> unit {
    if let text: string = value {
        WriteLine(text.Length)
    }
}
```

The same idea works in a `match`, where every state can be visible in one
place:

```raven
let description = match value {
    string text => "Length: ${text.Length}"
    null => "No value"
}
```

Patterns are preferred because they state both the condition and the value that
is safe to use. They also scale to `Option`, `Result`, unions, and richer data
shapes without introducing a separate null-only control-flow vocabulary.

For a nullable closed hierarchy, exhaustiveness includes every permitted leaf
type plus `null`:

```raven
let description = match value {
    SubClassA a => Describe(a)
    SubClassB b => Describe(b)
    null => "No value"
}
```

An open hierarchy cannot enumerate every future subtype. It also needs either
a base-type arm, which binds the remaining non-null values, or a discard arm:

```raven
let description = match value {
    SubClassA a => Describe(a)
    SubClassB b => Describe(b)
    null => "No value"
    BaseClass other => DescribeUnknown(other)
}
```

Using `_` instead of `BaseClass other` is appropriate when the remaining value
is intentionally ignored. Keeping `null` explicit before that fallback makes
the null-reference state visible rather than absorbing it into `_`.

## Direct null checks are compatibility forms

Raven also supports direct checks:

```raven
if value is not null {
    WriteLine(value.Length)
}
```

`is null` and `is not null` are valid Raven. They are useful when translating
.NET-shaped code, interoperating with nullable APIs, or adopting Raven without
rewriting every local flow. A successful `is not null` branch currently narrows
the original value inside that branch. Its declared type remains `T?`, and the
non-null fact does not escape the branch unless control flow proves it on every
continuing path.

These forms are described as compatibility forms because Raven documentation
should teach pattern bindings and exhaustive matching first. “Compatibility”
does not mean deprecated, unsafe, or unsupported.

Equality operators are a separate concern. `value == null` and
`value != null` may invoke user-defined equality and therefore may not be
strict null tests. `RAV9015` can replace those comparisons with `is null` or
`is not null`. That safety-oriented fix does not make direct null checks the
preferred way to model domain absence.

## Prefer `Option` for domain absence

If a missing value is an expected state of the application, represent it in
the API:

```raven
func FindCustomer(id: CustomerId) -> Option<Customer> {
    // ...
    None
}

let message = match FindCustomer(id) {
    Some(let customer) => "Found ${customer.Name}"
    None => "Customer not found"
}
```

`Option<T>` gives absence a named, closed shape. It supports exhaustive
matching and distinguishes an absent result from a nullable payload when that
distinction matters. `Result<T, E>` serves the corresponding role when an
expected operation can fail with useful error information.

Do not mechanically replace every nullable type in imported or boundary APIs.
An application can accept a `string?` from .NET, handle it once, and then pass
an `Option<string>` or a proven `string` into the rest of its domain code.

The built-in `RAV9012` analyzer reports nullable flow that appears to model
domain absence. Its code fix can rewrite simple, local null-guarded flows to an
`Option` pattern when the transformation is contained and unambiguous. It does
not automatically redesign public APIs or rewrite arbitrary control flow; those
changes require a domain decision from the developer.

## What null flow analysis does

Null flow analysis tracks facts established by assignments, branches, loops,
patterns, calls, and .NET nullable-flow attributes. Its purpose in Raven is to
prove safe access and find likely null-reference bugs in interop-oriented or
gradually migrated code.

It is not Raven's primary model for absence, and it should not grow into a
reason to organize domain APIs around mutable nullable state. When analysis
cannot prove that a nullable value is safe, prefer another explicit pattern or
binding over increasingly implicit inference.

Raven keeps these layers distinct:

1. Declared nullability determines whether a type is `T` or `T?`.
2. Boundary and conversion checks enforce those declared contracts.
3. Patterns establish explicit safe branches and bindings.
4. Null flow analysis carries additional facts through control flow and reports
   possible dereferences.

Disabling a flow diagnostic does not remove nullable annotations, change symbol
identity, alter emitted nullable metadata, or disable syntax-directed pattern
refinement.

## Configuration

Null-flow diagnostics are enabled by default. An MSBuild project can suppress
flow-derived possible-null-reference diagnostics with:

```xml
<PropertyGroup>
  <EnableNullFlowAnalysis>false</EnableNullFlowAnalysis>
</PropertyGroup>
```

This setting does not turn nullability off. Declared annotations, conversions,
metadata, boundary checks, pattern refinement, and flow-sensitive semantic
information remain available.

Raven's policy analyzers are configured independently through
`.editorconfig`. For example:

```ini
[*.rvn]
# Encourage Option or Result for nullable domain flow.
dotnet_diagnostic.RAV9012.severity = warning

# Disable the equality-to-strict-null-check suggestion if it is not useful.
dotnet_diagnostic.RAV9015.severity = none
```

Projects can therefore choose how strongly to enforce Raven's preferred style
without changing what `T?`, a pattern, or a flow fact means.

## Current policy boundary

Raven currently has one nullable semantic model, not separate strict and
compatibility languages. A future project profile might bundle stricter or more
migration-oriented diagnostic settings, but it must not silently change symbol
types, metadata contracts, or the meaning of patterns. Any such profile remains
an explicit design question rather than current behavior.

Continue with [Pattern matching](spec/pattern-matching.md),
[Raven.Core and `Option`](../compiler/raven-core-library.md), and
[Analyzer configuration](../compiler/analyzers/configuration.md).
