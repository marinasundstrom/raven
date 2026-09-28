# Proposal: Union and intersection types

Status: Draft design. The initial syntax and nominal generic-constraint subset
are implemented; first-class compound semantic types and their runtime
representations remain proposed. Examples outside that subset describe proposed
syntax and semantics, not verified runnable programs.

The initial constraint subset accepts interfaces and at most one distinct class
bound, including grouped conjunctions. Other compound constraint bounds are
diagnosed. See the current [constraint specification](../../spec/type-system.md#constraints)
and [compiler contract notes](../../../compiler/runtime-contracts.md) for supported
behavior and validation limits. The semantic algebra below is the broader goal,
not a claim that normalization or native compound types have shipped.

## Motivation and current behavior

Make `A | B` and `A & B` semantic type expressions, independent of their runtime
representation. A union accepts values belonging to either constituent; an
intersection accepts a single value belonging to both constituents.

```raven
let stream: OutputStream & InputStream = duplexStream
let resource: MyBase & Disposable = resourceFactory()
let result: int | string = 42
```

The interfaces in these examples describe nominal implementation requirements.
Having similarly named members alone does not establish membership.

Raven already parses union types. The current [union specification](../../spec/unions.md)
defines ad-hoc `T1 | T2` syntax through `System.Union<T1, T2>` carriers, with
supported arities through five. This draft proposes a different semantic basis;
it does not claim that changing that basis preserves all existing behavior.
Named `union` declarations retain their declared identity and case semantics.

Related background includes the [type syntax draft](type-syntax.md),
[type compatibility proposal](../type-compatibility.md), and existing
[generic constraints](../../spec/type-system.md#constraints).

## Syntax and scope

The goal is compositional syntax in all type-expression positions, including
annotations, generic arguments, function types, casts, patterns, and constraints.
Each position still applies its normal semantic restrictions. A union in a base
type clause does not grant multiple alternative base classes.

Proposed precedence, from strongest to weakest, is existing primary/postfix type
syntax, intersection, then union. Parentheses override precedence:

```raven
A | B & C       // A | (B & C)
(A | B) & C
(A & B)?
List<A & B>
```

The parser must distinguish parenthesized types from tuples and function types.
Function-type operand boundaries must be specified with the existing function
type grammar before implementation. Expression-level bitwise operators retain
their existing meaning.

`class`, `struct`, and constructor constraints remain constraint syntax, not
ordinary constituent types. Existing comma-separated constraints stay valid.

## Proposed semantic rules

For subtype relationships, written here as `S <: T`:

- `S <: A & B` when `S <: A` and `S <: B`.
- `A & B <: A` and `A & B <: B`.
- `A <: A | B` and `B <: A | B`.
- `A | B <: T` when `A <: T` and `B <: T`.

These describe membership and subtyping, not arbitrary implicit conversions.
Numeric and user-defined conversions need separate applicability and ranking
rules; two conversions producing different objects cannot prove intersection
membership for one object.

The proposed semantic identity is associative, commutative, and idempotent:
`A & B` equals `B & A`, and `A | A` simplifies to `A`. Subsumption removes
redundancy: if `Derived <: Base`, then `Derived & Base` is `Derived`, while
`Derived | Base` is `Base`. Source ordering can remain available for display
without defining type identity or overload priority.

An intersection is not a tuple or an adapter joining two objects. Converting a
reference value into an intersection and projecting it to either constituent
must preserve the underlying object's identity.

Unrelated class types have an empty intersection under single inheritance.
A sealed class and an interface it does not implement also have an empty
intersection. An unsealed class not currently implementing an interface can
have a subclass that does. Interface intersections remain potentially inhabited
unless stronger evidence proves otherwise.

The compiler should represent an empty type internally for flow analysis.
Whether authors may explicitly name empty intersections, and whether Raven
exposes a bottom-type spelling, remain design decisions.

Null belongs to a compound type according to constituent membership. In
particular, `A? & B?` can include null, while `(A & B)?` explicitly adds it.
Nullable value representation and existing `T | null` behavior require dedicated
compatibility rules; they must not be changed incidentally by normalization.

### Member lookup and narrowing

An intersection exposes members from both constituents. The same inherited
member reached through multiple paths is one candidate. Distinct declarations
with the same name participate in normal overload resolution; unresolved
conflicts require an explicit constituent view. Constituent order never chooses
an implementation. Static abstract members remain subject to the target's
generic dispatch rules, not ordinary instance-member projection.

Initially, union member access should require narrowing, except for members
obtained through an established common supertype. Structural merging of
same-named members with different signatures is outside the initial feature.

Successful type tests refine the tested value by intersection. A union pattern
matches any constituent; an intersection pattern requires every constituent on
the same value. Negative branches require subtraction in flow analysis, which
need not always have a source-level type spelling. Exhaustiveness must account
for overlapping constituents, rather than counting them as disjoint cases.

### Union overlap and existing carriers

The recommended model for semantic unions is membership without a selected
alternative. If an object implements both `A` and `B`, it satisfies both type
tests even when held through `A | B`. Ordinary match arm ordering resolves
overlap. A representation may carry implementation tags, but these must not
change membership semantics.

This differs from a tagged sum that remembers which constructor was selected.
Named unions remain the way to express distinct domain cases, including cases
whose payload types overlap. Before adopting these rules for existing ad-hoc
union syntax, audit construction, matching, equality, default states, reflection,
and cross-assembly import of `System.Union` carriers. Migration or versioning is
an explicit prerequisite, not an implementation detail.

## Constraints versus first-class types

```raven
func copy<T>(stream: T) where T: OutputStream & InputStream
func use<T>(resource: T) where T: MyBase & Disposable
```

These constraints can flatten into ordinary CLI generic constraints: multiple
interfaces, or one base class plus interfaces. No intersection carrier is needed.
Existing comma-separated constraints already have conjunctive semantics.
Normalization can remove redundant class bounds, but remaining constraints must
obey CLI legality rules. General compound expressions are not automatically legal
CLI constraints.

By contrast, `T: A | B` is a disjunctive constraint. Ordinary CLI constraints
cannot encode that choice. `(A | B) & C` also cannot generally flatten into a
conjunction of nominal constraints. Silently dropping such a requirement from
public metadata is not an acceptable implementation.

An intersection constraint on `T` is also different from supplying `A & B` as a
type argument. The former constrains an actual concrete type argument; the
latter asks the runtime to represent the compound type itself.

The CLI constraint model is documented in
[ECMA-335](https://ecma-international.org/publications-and-standards/standards/ecma-335/),
Partition II, sections on generic parameters and `GenericParamConstraint`.

## .NET representation options

| Position | Candidate implementation | Limitation |
| --- | --- | --- |
| Conjunctive generic constraints | Existing CLI constraint entries | Constituents must normalize to legal constraints |
| Reference-value locals and narrowing | Existing reference storage plus casts for constituent access | Compiler tracks the full semantic type |
| Parameters, returns, fields | Erased reference type plus descriptive metadata and boundary checks | Other languages and reflection see the erased signature |
| Compound generic arguments | Carrier or explicitly specified erasure | Changes runtime generic identity and interoperability |
| Disjunctive constraints | Native target support or diagnostic | No direct ordinary CLI constraint encoding |

An erased signature might use a class constituent or `object`. An ABI proposal
must select a deterministic rule and define recursive type annotations for
arrays, delegates, and nested generic arguments. It must also define imports,
overload collisions after erasure, overrides, interface implementation, and
validation at external call boundaries. Attributes alone cannot enforce that an
external caller supplied a value satisfying every constituent.

Public mutable fields are particularly problematic: external stores bypass
method-entry validation. Writable aliases, `ref`/`out`, array covariance, and
mutable generic collections likewise require explicit soundness rules. Initial
support should diagnose unsupported escaping/storage positions rather than
pretend a local representation solves the public ABI.

An optional `Intersection<A, B>` carrier could validate and retain one underlying
reference and expose both views through compiler conventions. It must not accept
two independent objects. The carrier does not automatically implement arbitrary
interfaces `A` and `B`, and cannot inherit an arbitrary class constituent. Thus it
cannot itself satisfy existing external constraints requiring those constituents.
Unwrapping helps value calls but cannot generally repair generic instantiations.

A synthesized interface inheriting both interfaces is not equivalent either:
existing implementations of both are not nominal implementations of that new
interface. Proxy wrappers introduce additional identity and dispatch semantics.

Value types require a separate decision about boxing, mutable state, copies,
and constrained calls. A first reference-value implementation must not silently
claim equivalent support for value types or byref-like types.

## neoCLR representation

A native compound-type facility could remove the need to port the ad-hoc
`Union<T1, ...>` carrier family. This is a proposed capability, not a claim about
current neoCLR support, and does not eliminate named union case representations.

The runtime/compiler contract must define:

- Metadata encoding, canonical identity, and cross-module equivalence.
- Layout and lifetime for reference and value constituents.
- Assignability, casts, type tests, and generic constraint enforcement.
- Generic instantiation identity, substitution, and variance.
- Dispatch for intersection members and type introspection behavior.

Raven's semantic types should not be aliases for neoCLR metadata constructs.
The selected backend maps semantic types to its supported representation and
reports unsupported uses. Configuration follows the existing
[Runtime Contract direction](../../../compiler/runtime-contracts.md); this draft
does not introduce an option or select a concrete ABI.

General compiler mechanisms should be developed and validated on main-based
branches. neoCLR-specific encodings, integration, and policy stay on the
experimental branch and receive independent runtime validation.

## Implementation sequence

1. Decide union overlap and migration policy; specify normalization, nullability,
   member ambiguity, conversions, and target support diagnostics.
2. Add intersection syntax and semantic compound-type symbols. Keep semantic
   identity independent of storage; support substitution and public symbol/API
   display without exposing backend or cache internals.
3. Implement intersections in legal CLI generic constraints, with diagnostics,
   imported/emitted metadata, member lookup, and observable execution coverage.
4. Implement reference-value local intersections and narrowing. Diagnose uses
   requiring an ABI that has not yet been specified.
5. Specify and validate public .NET ABI conventions and compound generic
   arguments before enabling those positions. Treat carriers as representation
   choices, not semantic definitions.
6. Integrate a separately specified neoCLR native contract and test its actual
   metadata, casts, dispatch, generic behavior, and execution.

Every implementation slice must update the language spec, grammar, compiler/API
documentation, and changelog as applicable. Evaluate hover, completion, semantic
tokens, diagnostics, signature display, and TextMate grammar. Language services
must obtain semantic compound types through the normal compiler APIs.

Tests should cover type equivalence, substitution, ambiguous members, overlapping
union patterns, nullability, rejected target positions, cross-assembly imports,
and observable identity. Do not substitute emitted-opcode assertions for behavior.
Modern .NET execution is not evidence of .NET Framework, NanoFramework, or neoCLR
execution. Validation of implemented slices belongs in the compiler contract
notes; the broader semantic model and target ABIs in this draft remain unvalidated.
