# neoCLR-only language features

**neoCLR only.** These features require an explicitly selected neoCLR target and
its runtime contracts. .NET remains Raven's primary and default target; ordinary
samples, templates and getting-started instructions continue to target .NET.

This group collects exclusive language capabilities. Shared features remain in
the main reference, with runtime representation explained where it matters.

## Native Self contracts

`Self` in a neoCLR interface describes the concrete type that declares the
conformance, without adding a source generic parameter to the interface:

```raven
module Example

public interface Cloneable {
    func Clone() -> Self
}

public struct Count : Cloneable {
    func Clone() -> Self => self
}
```

Here `Cloneable.Clone` retains a native Self signature, while `Count.Clone`
returns `Count`. Uppercase `Self` is the native type contract; lowercase `self`
is the ordinary instance receiver and is also used on .NET. On .NET, a
user-declared type named `Self` remains an ordinary type, with no native contract.

For classes, Self is anchored to the class declaring conformance. If `Base`
declares `Cloneable`, an inherited `Clone` on `Derived` still returns `Base`.
An override preserves that result. A derived class can explicitly redeclare the
conformance and implement `Cloneable.Clone` with a `Derived` result. Inheriting a
Base-returning member alone does not promise a Derived result to generic callers.

**Experimental implementation limits:** semantic analysis enforces these rules,
but the native metadata writer currently rejects inherited-only Self conformance
when validating the derived class. Explicit derived reimplementation has native
signature coverage. Instance dispatch through a generic parameter with only a
nominal interface bound is also currently rejected by the metadata library;
positive generic signature checks do not establish that dispatch capability.
These are implementation gaps, not language restrictions. See the
[validation ledger](https://github.com/marinasundstrom/raven/blob/main/docs/compiler/architecture/target-reconciliation-2026-10.md).

## Intersection types

An intersection type `A & B` describes a value satisfying both constituent types.
Intersection types in value and signature positions are **neoCLR only**; see
[intersection types](type-system.md#intersection-types) for semantics and limits.

.NET continues to accept `where T: A & B` as ordinary conjunctive generic
constraints. That spelling does not introduce a runtime intersection type on .NET.

## Structural function category constraints

The generic constraint `where T: func` requires a native structural function type
and is **neoCLR only**. See [function types](functions.md) for callable semantics.
Ordinary function declarations, lambdas and function values remain shared language
features, with different runtime representations.

## Related target distinctions

neoCLR code uses [`module` declarations](namespaces-and-imports.md#neoclr-module-declarations)
where .NET code uses `namespace`. The shared parser accepts both spellings, and
qualified lookup and imports are shared; the keyword does not select the target.

Structural unions are shared language features with different runtime
implementations. They are not part of the exclusive feature group.
