# Semantic intersection types

Status: compiler API foundation, source constraint queries, and initial reference
membership conversions. Source value binding and runtime representation remain
disabled.

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

## Reference conversions

`Compilation.ClassifyConversion` recognizes implicit reference membership for
intersections of non-nullable named reference types (classes, interfaces, and
delegates). Entry requires the source to satisfy every destination constituent.
Projection may use any source constituent that proves the destination's nominal
reference relationship, including inherited interfaces and existing variance
rules. A stronger conjunction can therefore convert to a weaker conjunction.
Equivalent intersections remain identity conversions regardless of source order.

Membership and projection report `IsImplicit` and `IsReference`, not boxing,
numeric, or user-defined conversion flags. They cannot combine two conversions
that produce different objects. Failure to prove membership returns no conversion;
checked narrowing is not implemented here, even when ordinary interface casts
would exist. This is a semantic classification, not an implemented emit path.

Existing nullable reference wrappers lift supported conversions: `(A & B)?` can
project to `A?`, but a nullable source cannot establish non-null intersection
membership. Adding a nullable destination wrapper is allowed. Nullable constituents
such as `A? & B?`, type-parameter entailment, array participation, and value-type
intersection conversions remain deferred, apart from ordinary identity. This
slice does not introduce union conversion algebra or an empty/bottom type.

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
would normalize to a single type. `GetTypeInfo` and `GetSymbolInfo` on a top-level
intersection constraint, including nested conjunctions and parentheses, expose
the normalized semantic type. A redundant conjunction may return an ordinary
nominal type instead of `IIntersectionTypeSymbol`. Constituent names still resolve
individually, and the declaration's `ConstraintTypes` remain ordinary CLI bounds.
Resolution of a missing constituent does not return a partial intersection.

This source binding is limited to conjunctions that constraint analysis flattens.
Intersections inside generic arguments, nullable types, arrays, or other wrappers
do not become supported merely because the enclosing syntax is a constraint.
Declaration validation remains authoritative for legality and accessibility of
bounds; a type query is not a successful-compilation or emit guarantee.

Language services can obtain the compound constraint's type and display through
the normal semantic APIs, without reconstructing types in the LSP. This slice
does not add intersection receiver completion or change TextMate syntax coverage.

No emitter mapping, storage erasure, ABI annotation, Runtime Contract option,
or native neoCLR support is introduced. APIs that create array or constructed
generic symbols can describe combinations that cannot be emitted. Consumers must
not infer runtime support from the existence of a semantic symbol.

## Validation

Reference-conversion integration passed all 399 overload-resolution tests before
and after the change, plus 118 focused intersection and conversion tests afterward
on .NET 11. This validates semantic classification and the retained source gates,
not runtime intersection storage, dispatch, or reference-identity execution.

Whole-constraint query integration passed 247 focused intersection, constraint,
generic method/type, and accessibility tests on .NET 11, including cold/warm
queries and diagnostics after failed queries. Targeted builds and whitespace
formatting succeeded. See the Runtime Contract notes for the test boundaries.

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
