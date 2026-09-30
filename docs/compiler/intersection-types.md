# Semantic intersection types

## Experiment on hold

As of 2026-09-30, development is paused on `codex/intersection-constraints`.
The implementation checkpoint is `f32fd576e`; this branch is not a candidate
for wholesale integration into main. Independently useful compiler fixes are
being extracted and validated against ordinary CLI contracts on main.

Source intersection value/storage annotations remain rejected with RAV0363.
Internal lowering tests do not constitute source-language support. Before
resuming source-value work, complete return and generic-inference boundary
checks, audit unsupported operations (including events), and evaluate language
service support. Native neoCLR structural types and a public compound ABI
remain future work. Existing validation is modern .NET evidence only, not
execution evidence for neoCLR, .NET Framework, or NanoFramework.

## Shared-main rebase — 2026-09-30

Rebased onto the merged main foundation b40fe495d. The original proposal is
already present on main; the remaining 15 feature commits were replayed.
Constraint binding retains main's declaration-owned type-parameter substitutions
while accepting the intersection branch's individual constraint references.
General storage regressions and the feature's recursive-constraint test remain.

The generator/build script passed for the rebased branch, and all 188 focused
intersection, storage-constraint, constrained-hierarchy and symbol-display tests
passed on .NET 11, including the internal lowering/runtime fixtures. Whitespace
formatting and diff checks passed. Source-storage gates and the experiment's
paused scope are unchanged. .NET versus neoCLR representation/semantics remain
feature design work; no native neoCLR execution is claimed.

## Implementation checkpoint

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
Instance method calls, property/field reads and writes, and indexer reads/writes
project an erased receiver to the selected declaration's containing type.
Indexer assignment reuses the projected indexer access, preserving ordinary
receiver, index-argument, and right-hand-side evaluation. Lowering does not perform member lookup,
resolve ambiguity, prove membership, or implement runtime-checked intersection
entry. Those obligations remain with binding and conversion classification.

`IntersectionLocalLoweringTests` supplies semantic intersection locals to the
normal lowerer and caches the resulting body for ordinary emission. Unlike the
hand-written representation probes, these tests execute the implemented lowering.
They cover both constituent orders, reassignment, property reads, method calls,
class virtual dispatch, nominal projections, shared mutation, reference identity,
and single evaluation of an initializer. They also check that the semantic local
retains its original type after lowering. Write coverage verifies shared state
through another constituent, both constituent orders, and single evaluation of
index arguments and assigned values. These emitted assemblies are also checked
with ILVerify when the tool is available: JIT execution alone can accept missing
receiver projections that are not verifiable CLI code.

This internal path is not a source feature switch or an escape checker. Captures,
async/iterator hoisting, byref access, compound signatures/generic arguments,
nullable/value-type intersections, and events have not
been integrated or validated here. The existing RAV0363 source gate still rejects
all intersection local annotations, so incomplete paths cannot be reached by
ordinary Raven source. Language-service support and TextMate syntax are unchanged.

Before enabling source locals, a storage/escape gate must diagnose unsupported
uses at binding time, including inferred uses without an intersection annotation:

- Capturing the compound local in a lambda or local function.
- Hoisting it into an async or iterator state machine.
- Taking its address or passing it through `ref`/`out` storage aliases.
- Letting the compound type escape into an inferred return, field, array element,
  delegate signature, or generic type/method argument.
- Using still-unimplemented receiver operations or nullable/value-type shapes.

Projection to a supported nominal reference type is not itself an illegal escape:
that view can use the normal nominal ABI. The storage and suspension checks below
implement part of this boundary; RAV0363 remains the source restriction.

## Semantic storage and capture checks

The binder's shared storage validation now checks the semantic type rather than
depending only on rejected intersection syntax. A direct intersection, or one
inside an array, nullable wrapper, tuple, address/pointer/reference wrapper,
delegate signature, generic argument, or constructed containing type, reports
RAV0363 and becomes an error type. This also protects callers that supply inferred
types to storage validation. The type factory itself remains descriptive and does
not reject these combinations.

The traversal follows stored type structure, not nominal members, base types, or
type-parameter constraints. Thus a nominal `T` with ordinary conjunctive CLI
constraints is not mistaken for an intersection storage type. Delegate traversal
guards against revisiting named types. This is a representation restriction, not
a declaration that intersections are ref-like or have stack-only semantics.

Existing capture and suspension diagnostic paths also report RAV0363 for compound
storage: captured locals/parameters, locals identified as crossing an await,
async parameters, and iterator locals/parameters. They use the same nested-type
check. Nominal constituent views remain eligible for the existing normal rules.
Focused tests supply semantic symbols and bound bodies because source annotation
binding is still deliberately disabled.

Address-of expression binding and `ref`/`out`/`in` argument binding now check the
operand's semantic storage type before constructing a bound address. They report
RAV0363 for direct or nested intersections, including already-bound local and
parameter symbols that did not pass through a source annotation. Ordinary nominal
storage retains existing mutability and addressability checks. By-value arguments
are not rejected by this address-specific check. A nominal projection stored in
its own local is different from an alias to erased compound storage; this does
not introduce byref conversions between those storage types.

These checks do not yet constitute permission to enable source locals. The shared
storage check still rejects all compound locals, including the internally lowered
subset. Return/generic inference still needs complete enforcement before a
narrowly scoped local allowance. The audit found that inferred return types are
published through multiple function/lambda/async paths, while explicit method
type-argument validation is separate from overload-driven inference. Guarding
only explicit type syntax or a single constraint validator is therefore
insufficient. Constructed-method references and inferred constructor arguments
also need coverage. No new public compiler API,
language-service bypass, TextMate change, or runtime metadata contract is added.

## Validation

Address/byref binding passed 17 new cases within a 221-test intersection/byref/
scoped regression set on .NET 11, plus 10 separately run byref runtime tests.
The pre-change 204-test baseline passed. Before the fix, all 16 compound-address
cases returned bound addresses instead of errors; the by-value control passed.
Targeted compiler/test builds, whitespace formatting, and diff checks succeeded.
The tests supply semantic symbols without enabling source annotations.

Semantic storage checks passed 20 new diagnostic cases and the expanded 220-test
intersection/static-type/ref-like/scoped/byref regression set on .NET 11. The
115-test pre-change intersection/static-type baseline passed. New coverage
includes ordinary and synthesized delegate signatures, nested containing types,
capture locations, real bound await analysis, iterator storage, nominal controls,
and constrained type parameters. Targeted compiler/test builds, whitespace
formatting, and diff checks succeeded. This does not enable source locals or
claim complete inferred-escape coverage.

Member-write lowering passed all 13 internal-local runtime tests with ILVerify
available and used, plus the 164-test intersection/indexer/property-assignment
regression set on .NET 11. The 158-test pre-change baseline passed. Before adding
the projections, the six new cases executed but failed IL verification; afterward
they passed both checks. Targeted compiler/test builds, whitespace formatting,
and diff checks succeeded. Source binding and escape checks remain gated.

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
