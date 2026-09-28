# Semantic intersection types

Status: compiler API foundation, source constraint queries, initial reference
membership conversions, binder member-candidate lookup, and non-method receiver
ambiguity diagnostics, and internal reference-local lowering. Source value binding
and public runtime representation remain disabled.

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
would exist. Classification alone does not guarantee emission; only the internal
local subset described below has a lowering path.

Existing nullable reference wrappers lift supported conversions: `(A & B)?` can
project to `A?`, but a nullable source cannot establish non-null intersection
membership. Adding a nullable destination wrapper is allowed. Nullable constituents
such as `A? & B?`, type-parameter entailment, array participation, and value-type
intersection conversions remain deferred, apart from ordinary identity. This
slice does not introduce union conversion algebra or an empty/bottom type.

## Members and source boundary

`GetMembers` combines the constituents' declared members and deduplicates the same
symbol. Distinct declarations remain distinct even when their signatures match.
The symbol also exposes combined interface sets. Single-symbol convenience
lookups do not choose an arbitrary candidate when several remain.

The binder's `SymbolQuery` has a separate intersection instance-member path. It
collects inherited members from each constituent and deduplicates by symbol
identity, so a shared declaration in an interface diamond appears once. A derived
interface declaration hides the matching inherited signature along its own path,
but unrelated declarations survive even when their signatures match. Class views
retain ordinary inheritance/hiding and do not expose explicit interface
implementations. Object members are fallback candidates when no constituent
supplies the matching signature. Interface traversal memoization is local to one
query, not a public or cross-snapshot cache.

These method candidates feed normal overload resolution: distinct applicable
overloads can be selected, while indistinguishable unrelated declarations remain
ambiguous regardless of constituent order. For an already-typed semantic intersection
receiver, ordinary member-expression binding filters non-method candidates by
accessibility before selecting a member. Multiple accessible candidates report
RAV0365 and produce an ambiguous bound expression retaining every candidate,
even when property types match. Shared inherited declarations bind once; an
inaccessible declaration does not hide an accessible sibling. Property reads and
assignments use this check. This is binder groundwork, not support for source
intersection annotations or runtime dispatch.

Static lookup on the compound type returns no candidates because
the intersection has no static dispatch owner. Ordinary nominal and type-parameter
constraint lookup are unchanged. This does not yet bind source intersection
receivers or provide their completion, indexing, extension lookup, or emission.

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

No global emitter mapping, ABI annotation, Runtime Contract option,
or native neoCLR support is introduced. Internal local erasure is described below.
APIs that create array or constructed
generic symbols can describe combinations that cannot be emitted. Consumers must
not infer runtime support from the existence of a semantic symbol.

## Standard .NET local representation probe

`IntersectionReferenceErasureTests` executes a candidate reference representation
using ordinary Raven source: store one object reference, then cast that same
reference to the selected member's declaring class or interface. These are
hand-written equivalents of a possible lowering, not tests of enabled `A & B`
local syntax or an implemented intersection lowering pass.

The probes cover interface/interface identity and shared mutation,
class/interface identity and virtual dispatch, and distinct explicit interface
implementations with the same signature. A separate membership probe requires
both bounds on the same value and rejects objects satisfying only one bound and
null. No carrier, synthesized interface, proxy, or native structural-type runtime
facility is needed for these cases.

The candidate implementation should keep the semantic intersection on the source
local while lowering its storage to `object`. Evaluate the initializer once.
Every assignment must prove all bounds before storing the reference. Project
to the selected declaration's owner before member access, or to the requested
nominal type before passing or returning a constituent view. Member ambiguity
must be resolved before lowering; erasure must never choose an implementation.
Runtime-checked entry must test every bound against that single reference, with
an explicit null policy. The probe does not implement checked intersection casts.

This is a local-only representation, not a global `GetClrType` mapping. A global
mapping would also affect fields, signatures, arrays, and generic arguments
without an agreed ABI. Before opening the source gate, the compiler needs a
complete storage-aware lowering and tests for inferred escapes, captures,
async/iterator hoisting, byref aliases, and remaining member operations.
Until those paths have a supported representation or deliberate diagnostic,
RAV0363 remains in force. Value-type intersections and compound generic identity
are outside this reference-only experiment.

## Internal local lowering

The lowerer now maps already-bound locals whose intersection constituents are
non-nullable named reference types to fresh `object` storage locals. The source
symbols are not mutated, and the mapping belongs to that lowering instance.
Declarations, local accesses, and local reassignment share the mapped storage.
Registration precedes initializer rewriting so an earlier rewrite cannot retain
an intersection-typed CLI local accidentally.

Implicit membership conversions erase to an ordinary object reference. Implicit
semantic projections become explicit CLI reference conversions after erasure.
Instance method calls and property/field reads project an erased receiver to the
selected declaration's containing type. Lowering does not perform member lookup,
resolve ambiguity, prove membership, or implement runtime-checked intersection
entry. Those obligations remain with binding and conversion classification.

`IntersectionLocalLoweringTests` supplies semantic intersection locals to the
normal lowerer and caches the resulting body for ordinary emission. Unlike the
hand-written representation probes, these tests execute the implemented lowering.
They cover both constituent orders, reassignment, property reads, method calls,
class virtual dispatch, nominal projections, shared mutation, reference identity,
and single evaluation of an initializer. They also check that the semantic local
retains its original type after lowering.

This internal path is not a source feature switch or an escape checker. Captures,
async/iterator hoisting, byref access, compound signatures/generic arguments,
nullable/value-type intersections, property writes, indexers, and events have not
been integrated or validated here. The existing RAV0363 source gate still rejects
all intersection local annotations, so incomplete paths cannot be reached by
ordinary Raven source. Language-service support and TextMate syntax are unchanged.

## Validation

Internal local lowering passed seven emitted-program tests and the expanded
114-test intersection/control-flow/use/propagation regression set on .NET 11.
The pre-change 104-test intersection/control-flow baseline passed. Targeted
compiler builds for net10.0/net11.0, the test build, whitespace formatting, and
diff checks succeeded. Runtime evidence is limited to .NET 11 and the supplied
bound-input subset; source annotations and escape paths were not enabled.

The local reference-representation probe passed all four emitted-program tests
on .NET 11, plus the combined nine-test constraint-emission/reference-owner runtime
set. The five existing runtime tests passed before the addition. The targeted
test-project build, whitespace formatting, and diff checks succeeded. This is
runtime evidence for ordinary reference storage and casts, not for an implemented
intersection lowering, native neoCLR behavior, or other runtime targets.

Receiver-binding fixtures inject a semantic intersection local and bind parsed
member expressions and assignment statements. They cover same/different property
types, both constituent orders, all ambiguity candidates, shared inherited
properties, and accessibility. They do not enable or test intersection storage
or runtime execution. The 233-test baseline, all 10 new receiver tests, and the
expanded 271-test regression set passed on .NET 11. The generator/build script,
targeted test build, whitespace formatting, and diff checks succeeded.

Member-candidate integration passed 162 focused intersection, lookup, interface,
constraint, and overload tests on .NET 11 after targeted builds. The tests include
source and imported inheritance, shared-declaration deduplication, ambiguity in
both constituent orders, and cold/warm queries. Source receiver binding and
runtime dispatch are not exercised or enabled by this slice.

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
