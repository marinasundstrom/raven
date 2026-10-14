# Semantic intersection experiment

Status: unpublished compiler API foundation on `codex/neoclr-union-intersection`,
2026-10-14. Selectively reuses `02d669acd` and `c4d57d282` from the parked
intersection branch. Source syntax, storage and native emission remain pending.

## Compiler APIs

`Compilation.CreateIntersectionTypeSymbol(params ITypeSymbol[] constituentTypes)`
returns an immutable `ITypeSymbol`. It flattens intersections, removes duplicates
and established nominal supertypes, and returns the ordinary constituent when
only one remains. Empty inputs throw `ArgumentException`; null inputs or elements
throw `ArgumentNullException`. Construction does not prove that a type is inhabited.

Compound results implement `IIntersectionTypeSymbol.ConstituentTypes` and have
`TypeKind.Intersection`, no nominal metadata name and no declaration container.
Symbol visitors receive `VisitIntersectionType`. Equality and hashing ignore
constituent order; display preserves first-occurrence order and groups array and
nullable forms. Constructed type/method substitution normalizes replaced bounds.

`GetMembers` combines declared constituent members, deduplicating the same symbol.
Distinct declarations remain distinct even with identical signatures. Inherited
member lookup, receiver overload resolution and ambiguity diagnostics are separate
work. Single-symbol convenience queries do not choose an arbitrary candidate.

`Compilation.ClassifyConversion` recognizes implicit reference entry when a nominal
reference satisfies every bound, and projection when a bound proves the destination.
Conversions between intersections may drop proven bounds but cannot invent them.
Boxing, numeric widening and user-defined conversions do not establish membership.
Existing interface variance and outer nullable reference lifting remain applicable.
Array constituents, type-parameter entailment, nullable constituent algebra and
empty-intersection detection are deferred. Identity remains symbol identity even
for descriptive combinations outside this conversion subset.

## Runtime and source boundary

.NET has nominal interfaces and generic constraints, but no first-class CLI
intersection signature. These semantic symbols deliberately separate compiler
identity from storage: they select neither an object-erasure ABI nor a carrier.
The benefit is preserving all bounds for future target validation; the cost is
that creating a symbol does not imply any emitter can represent it. See the
[existing design comparison](../lang/proposals/drafts/union-and-intersection-types.md).

There is no new Runtime Contract setting, syntax binding, operation shape or
editor grammar in this slice. Ordinary ad-hoc unions retain their carrier behavior.
The native portable signature boundary rejects these symbols, including nested
array forms, until an explicit target capability and metadata mapping exist.
The older intersection parser, constraint binder, receiver lowering and .NET
storage-erasure experiment are not imported.

neoCLR's branch-only interpreter transport is described in the
[bridge handoff](neoclr-cli-bridge.md#structural-reference-compositions-runtime-experiment-2026-10-14).
Compiler normalization additionally removes nominal supertypes; runtime syntax
normalization currently flattens/deduplicates operands without that reduction.
Descriptor identity must be reconciled before import/export is enabled. No
Raven-generated compound program or native JIT/AOT execution is claimed.

## Validation on the native compiler branch

The fresh-workspace and post-change `scripts/codex-build.sh` runs passed on the
installed .NET 11 SDK (compiler builds target net10.0 and net11.0). The focused
net11.0 test run passed all 89 cases: 24 intersection cases and 65 existing symbol
equality, nullable substitution and conversion controls. The intersection tests
include cold/post-diagnostics queries, exclusion of boxing/user conversions,
variance and nullability, and portable signature rejection. Generated visitors
were refreshed; touched C# files were whitespace-formatted.

The pre-change overload-resolution feature suite passed 414/415 cases. Its failure,
`OverloadResolutionPriority_PrefersHigherPriorityMetadataMethod`, reports RAV0500
because the metadata fixture's `Library` type is inaccessible. The post-change run reproduced exactly the same 414 passes and one failure;
there were no additional failures. This is not an intersection test. No full-suite, native metadata import, editor execution or
Raven-generated neoCLR compound-program validation is claimed by these results.


## Superseded branch cleanup

After validation, the old local branches were removed with their exact tips
preserved as annotated tags: `archive/2026-10-14/intersection-constraints`
(`6c112574c`) and `archive/2026-10-14/neoclr-structural-types` (`b92a0d4a7`).
Uncarried parser, member lookup and lowering experiments remain recoverable from
these tags. Remote branches were not changed. The active feature branch and the
main/native compiler lines remain separate.
