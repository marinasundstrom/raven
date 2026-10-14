# Semantic intersection experiment

Status: unpublished compiler API foundation on `codex/neoclr-union-intersection`,
2026-10-14. Selectively reuses `02d669acd` and `c4d57d282` from the parked
intersection branch. The follow-up source slice adds constraint syntax and queries; value storage and native emission remain pending.

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

There is no new Runtime Contract setting, value-storage binding or expression
operation shape. TextMate already recognizes the `&` operator. Ordinary ad-hoc unions retain their carrier behavior.
The native portable signature boundary rejects these symbols, including nested
array forms, until an explicit target capability and metadata mapping exist.
The parser and constraint binder are selectively carried in the follow-up slice;
receiver lowering and .NET storage erasure are not imported.

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

## Source constraint slice — 2026-10-14

Selectively carries `08b53d8c4`, `41b684e3f` and `c6a303f0d`. The generated
`IntersectionTypeSyntax` node exposes a separated `Types` list; `&` binds more
tightly than `|`, with existing prefix-byref and function-return grouping preserved.
Missing right operands report a parser diagnostic and preserve later declarations.

Conjunctions in inline and `where` generic constraints flatten into ordinary
nominal bounds. `GetTypeInfo` and `GetSymbolInfo` on the conjunction expose a
normalized semantic intersection (or its sole remaining constituent), including
cold queries in generic method scope. `ITypeParameterSymbol.ConstraintTypes`
continues to expose individual bounds. Constituent errors remain diagnosed after
non-reporting semantic queries. Member access through the constrained type
parameter uses existing generic member lookup; no intersection-receiver binding
or signature merging is introduced.

RAV0364 rejects unsupported constituent kinds, distinct class bounds and class
bounds combined with `struct`. This provisional subset is not the final type
algebra. Value-storage positions report RAV0363 even for singleton-normalizing
expressions; generic/array/nullable wrappers do not bypass the gate. For ordinary
.NET emission, constraints use ordinary CLI bounds, without a new carrier or ABI.
This does not enable native compound storage or broaden the native target's
existing generic-constraint capabilities. No new Runtime Contract option is added.

The grammar and type-system reference describe this branch-only subset. TextMate
already tokenizes `&`; semantic queries provide compiler facts for future editor
integration. No real-editor validation is claimed. General missing-constraint
query diagnostic handling is a candidate for independent main-line validation,
not a reason to merge this feature branch wholesale.

### Source-slice validation

`scripts/codex-build.sh` passed and refreshed syntax nodes, factories, visitors and
diagnostics. On net11.0, all 1,199 selected syntax/intersection, generic-method,
generic-storage, byref-parameter and constraint cases passed. This includes an
incremental missing-bound edit/recovery, semantic queries before diagnostics,
unsupported storage wrappers, and two .NET execution cases returning 42 with
ordinary reflected constraints and a separate compilation consumer.

The required broad pre-change baseline stopped after 1,346 passes and two existing
`DocumentationCommentIdBuilderTests.AssemblyLevelMembersUseNamespaceQualifiedDocumentationIds`
failures (empty ID for assembly-level `Score`). The post-change rerun of that
93-case batch reproduces exactly 91 passes and the same two failures. No green
full baseline is claimed.
This slice does not claim native compound execution, native generic-constraint
coverage beyond existing capabilities, or real-editor validation. Whitespace
formatting and diff checks passed.
