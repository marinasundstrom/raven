# Semantic intersection types

Status: compiler API foundation. Source value binding, compound-type conversion
classification, and runtime representation are not enabled by this slice.

`Compilation.CreateIntersectionTypeSymbol(params ITypeSymbol[])` constructs an
immutable semantic intersection. Its return type is `ITypeSymbol`: normalization
may leave one ordinary constituent, in which case the factory returns that type.
Empty inputs and null arguments are API misuse and throw argument exceptions;
source errors continue to use compiler diagnostics.

Compound results implement `IIntersectionTypeSymbol` and expose
`ConstituentTypes`, with `TypeKind.Intersection`. They have no nominal metadata
name or declaration container. They are not synthesized interfaces, wrappers,
or runtime carrier types. Generated symbol visitors expose `VisitIntersectionType`.

## Normalization and identity

The factory flattens nested intersections, removes equal constituents, and removes
established nominal supertypes using class ancestry and implemented/inherited
interfaces. Numeric widening and user-defined conversions do not remove bounds.
This is deliberately not a complete subtype algebra: nullable bounds,
type-parameter constraint entailment, variance-based simplification, and empty
intersection detection are not implemented here. Unrelated types can still form
a descriptive intersection; constructing the symbol does not prove it inhabited.

Constituent presentation retains first-occurrence order after normalization.
`SymbolEqualityComparer` compares compound constituents without regard to order,
and hashes them in an order-independent way. Each constituent comparison uses
the selected comparer's nullability and containment policy. Comparers do not
perform additional subtype normalization or collapse a compound into a nominal
type when a comparison policy makes its bounds redundant.

Display uses `A & B`, groups nullable and array intersections as `(A & B)?` and
`(A & B)[]`, and groups function/union constituents. Display order is not a type
identity or dispatch rule. Constructed type and method substitution traverses all
constituents and reruns normalization after replacements; an unchanged substitution
preserves the existing symbol instance.

## Members and source boundary

`GetMembers` combines the constituents' declared members and deduplicates the same
symbol. Distinct declarations remain distinct even when their signatures match.
The symbol also exposes combined interface sets. This does not implement
expression-level overload resolution or inherited-member lookup for intersection
receivers. Those belong to the following binder slice. Single-symbol convenience
lookups do not choose an arbitrary candidate when several remain.

The existing constraint subset continues to expose ordinary flattened nominal
bounds through `ITypeParameterSymbol.ConstraintTypes`. Source intersection syntax
outside supported constraints still reports RAV0363, including expressions that
would normalize to a single type. `GetTypeInfo` on compound syntax does not yet
bind this new symbol; the factory is the public entry point for this stage.

No emitter mapping, storage erasure, ABI annotation, Runtime Contract option,
or native neoCLR support is introduced. APIs that create array or constructed
generic symbols can describe combinations that cannot be emitted. Consumers must
not infer runtime support from the existence of a semantic symbol.

## Validation

The generator/build script and targeted test-project build succeeded. All 285
focused intersection, symbol equality/display, nullable substitution, and generic
method/type tests passed on .NET 11, including normalization before and after
diagnostics binding. Whitespace formatting and `git diff --check` passed. This is
compiler API validation, not evidence of first-class compound runtime support.
The previously recorded full-baseline union-import failures remain outside this
slice; no full-green baseline or neoCLR execution is claimed.

See the [language proposal](../lang/proposals/drafts/union-and-intersection-types.md)
for the intended full semantics and [Runtime Contracts](runtime-contracts.md) for
the implemented constraint behavior and validation evidence.
