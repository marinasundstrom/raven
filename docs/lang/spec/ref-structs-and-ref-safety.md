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
on every declaration of a partial struct. Source-declared ref structs follow
the same storage, capture, generic-argument, async, and iterator restrictions as
ref-like types imported from .NET.

`readonly ref struct` additionally prevents mutable instance storage. Instance
fields must use `readonly`, and property storage must use `val` rather than
`var`. Partial declarations must agree on both modifiers.

## Managed-reference fields

Ref structs may declare managed-reference fields using `&T`:

```raven
ref struct IntReference {
    field Value: &int
}
```

Ref fields are instance-only and cannot be declared in ordinary structs or
classes. Their referent cannot itself be ref-like or a type parameter that
allows ref structs. Dereferencing a managed ref field does not require unsafe
mode; raw pointer dereferences still do.

## Scoped parameters and locals

The `scoped` modifier restricts a value or managed reference so it cannot escape
its permitted lifetime. It precedes a by-reference modifier:

```raven
func Consume(scoped value: System.Span<int>) {}
func Mutate(scoped ref value: int) {}

scoped let buffer: System.Span<int> = stackalloc int[4]
scoped let reference = &value
```

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

Without the anti-constraint, a ref-like type such as `Span<T>` is rejected as a
type argument.

The [.NET implementation notes](dotnet-implementation.md#ref-like-metadata)
describe the attributes and generic flags used for cross-language interop.

Within the generic declaration, the constrained type parameter is treated as
potentially ref-like: it cannot be captured, stored in heap fields or arrays,
or persisted across `await` and `yield`. The anti-constraint must appear last,
may be specified only once, and cannot be combined with `class`.
