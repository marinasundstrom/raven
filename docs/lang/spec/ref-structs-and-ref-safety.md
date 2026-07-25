# Ref structs and ref safety

Ref structs are specialized value types whose instances cannot move to the
managed heap. They support memory-efficient abstractions such as spans and
managed-reference fields while preserving stack lifetimes.

## Ref struct declarations

A struct may use the `ref` modifier to declare a ref-like value type:

```raven
ref struct Buffer<T> {
    field Value: T
}
```

The modifier is valid only on `struct` declarations and must appear consistently
on every declaration of a partial struct. A source-declared ref struct is
classified as ref-like by the semantic model, so the same storage, capture,
generic-argument, async, and iterator restrictions that apply to consumed .NET
ref-like types also apply to it.

The emitted type carries
`System.Runtime.CompilerServices.IsByRefLikeAttribute`, including when the ref
struct itself is generic, so other .NET compilers and reflection classify it
the same way.

`readonly ref struct` additionally prevents mutable instance storage. Instance
fields must use `readonly`, and property storage must use `val` rather than
`var`. The emitted type carries both `IsByRefLikeAttribute` and
`IsReadOnlyAttribute`; `INamedTypeSymbol.IsReadOnly` exposes the same fact for
source and metadata types. Partial declarations must agree on both modifiers.

## Managed-reference fields

Ref structs may declare managed-reference fields using `&T`:

```raven
ref struct IntReference {
    field Value: &int
}
```

Ref fields are instance-only and cannot be declared in ordinary structs or
classes. Their referent cannot itself be ref-like or a type parameter that
allows ref structs. `IFieldSymbol.RefKind` reports `Ref` for source and consumed
metadata fields, and their CLR signatures use the standard `BYREF` element
type. Dereferencing a managed ref field does not require unsafe mode; raw
pointer dereferences still do.

## Scoped parameters and locals

The `scoped` modifier restricts a value or managed reference so it cannot escape
its permitted lifetime. It precedes a by-reference modifier:

```raven
func Consume(scoped value: System.Span<int>) {}
func Mutate(scoped ref value: int) {}

scoped val buffer: System.Span<int> = stackalloc int[4]
scoped val reference = &value
```

`scoped value: Span<int>` and a scoped ref-like local are classified as
`ScopedValue`. `scoped ref value: int` and a scoped managed-reference local are
classified as `ScopedRef`. The compiler symbol API exposes this through
`IParameterSymbol.ScopedKind` and `ILocalSymbol.ScopedKind`.

By-value `scoped` parameters and locals must be ref-like. `scoped ref`,
`scoped in`, and `scoped out` parameters may refer to ordinary value types.
As in C#, `out` parameters and `ref` parameters whose type is ref-like are
implicitly scoped even when the keyword is omitted.

Consumed .NET parameters annotated with
`System.Runtime.CompilerServices.ScopedRefAttribute` expose the same
classification. Constructed generic symbols preserve it.

## Escape and storage rules

A scoped value cannot be:

* returned directly or through a local alias;
* exposed through a `ref` or `out` assignment;
* stored in a field through `self` or another by-reference receiver;
* captured by a lambda or local function;
* kept live across `await` or `yield`.

The restriction follows scoped values through ref-like fields and through
ref-like invocation results. Receiver values and arguments supplied to
unscoped parameters contribute to a result's escape scope; arguments supplied
to scoped parameters do not.

A ref struct value also cannot escape when one of its ref fields refers to a
local variable or one of its ref-like fields contains `stackalloc`-backed
storage. References and spans supplied by ordinary parameters may be stored and
returned when their storage is owned by the caller.

An override or explicit interface implementation may add a scoped restriction,
but it cannot remove one required by the overridden or implemented contract.
This applies to ordinary methods and indexer accessors. Definition and
implementation parts of a partial method must use identical scoped modifiers
for corresponding parameters.

## Generic ref-like arguments

Generic declarations opt into ref-like type arguments with the
`allows ref struct` anti-constraint:

```raven
func Accept<T>() where T: allows ref struct {}
```

The semantic model exposes this as
`TypeParameterConstraintKind.AllowByRefLike`, and emission sets the standard
CLI `AllowByRefLike` (`0x20`) generic-parameter flag. Without the
anti-constraint, a ref-like type such as `Span<T>` is rejected as a type
argument.

Within the generic declaration, the constrained type parameter is treated as
potentially ref-like: it cannot be captured, stored in heap fields or arrays,
or persisted across `await` and `yield`. The anti-constraint must appear last,
may be specified only once, and cannot be combined with `class`.
