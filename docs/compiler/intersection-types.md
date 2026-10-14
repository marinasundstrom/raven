# Semantic intersection experiment

Status: unpublished compiler API foundation on `codex/neoclr-union-intersection`,
2026-10-14. Selectively reuses `02d669acd` and `c4d57d282` from the parked
intersection branch. The follow-up source slice adds constraint syntax and queries; general value storage remains pending; explicit native call-only emission is now available as described below.

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
The experimental importer retains the native descriptor independently of normalized compiler symbols; native export remains gated. No
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

## Experimental native semantic import (2026-10-14)

`NeoClrMetadataReference.ReadExperimentalIntersectionAssembly(ReadOnlySpan<byte> image,
NeoClrPrimitiveBootstrap? bootstrap = null)` copies a native declaration image into
an immutable semantic reference. This explicit API admits the metadata reader's
bounded nongeneric reference-intersection free-function parameters/results. The
optional CLI primitive bootstrap must be supplied as the matching compilation
reference; otherwise use the existing native semantic core configuration. Existing
`ReadAssembly` APIs, project loading and CLI switches retain their default closed
composition profile. There is no new Runtime Contract option.

Invalid images, exceeded native reader limits and normalized structural unions in
exposed parameter/result signatures (including unions nested in intersections)
throw `InvalidDataException` before reference publication. Bodies/locals remain
opaque to compiler import. This API is not an executable-body verifier. A union
that the metadata reader has already collapsed to one nominal constituent has no
remaining structural-union descriptor. Existing dependency and target validation
continue to produce compiler diagnostics.

Canonical `ReferenceCompositionTypeInfo` views map through
`Compilation.CreateIntersectionTypeSymbol`. Cold `GetSymbolInfo`/`GetTypeInfo`,
method parameter/result queries and inferred local types therefore preserve
intersection semantics. Compiler normalization may remove nominal supertypes;
`reference.Definition` keeps the unreduced native signature. No metadata identity
is reconstructed from display text or a normalized compiler symbol.

By default, emission of a compilation referencing any admitted compound callable is rejected
with NEOMETA001, including when normalization reduces that callable's compiler
type to a nominal type and even when the reference is unused. Both native and
PE/#Neo entry points preserve the output stream on rejection. The explicit native
call-only option below opens a bounded path; PE/#Neo remains closed.
Explicit intersection value-type syntax still reports RAV0363. Receiver/member
merging, structural union symbols, source storage emission and JIT/AOT remain open.
There is no CLI carrier or object-erasure encoding for these imports.

Validation uses `NeoClrMetadataProbe --intersection-symbols <core.dll>` against
neoCLR metadata checkpoint `97bbcf76b`: schema-2/schema-3 NEOX, cold/warm semantic
queries, order-independent symbol identity, nominal-supertype reduction with
retained native descriptors, insufficient-bound rejection, nested union rejection,
default-reader rejection and both emission gates pass. The existing
`--nullable-symbols <core.dll>` control passes before and after. These are semantic
and rejection checks, not evidence of Raven-generated compound execution.

A probe setup also exposed an unrelated cold `GetAssemblyOrModuleSymbol` null
loader failure before compilation setup. It is deferred as a general compiler
candidate for Raven main with independent validation; this slice uses the intended
source semantic query paths and does not change that API.

### Native callable-emission prerequisite

neoCLR `930ddf60` adds explicit host callable import/serialization preserving raw
composition descriptors, including nominally redundant bounds. Three host-emitted
native callers return 42 in the opt-in interpreter. Raven's semantic-import probe
still passes against this revision; the compiler's NEOMETA001 emission guard is
unchanged. The next source-emission slice must carry those raw contracts through
lowering and call encoding rather than reconstructing them from normalized symbols.

## Native call-only emission (2026-10-14)

The final optional constructor parameter
`NeoClrEmitOptions(..., bool allowExperimentalIntersectionCalls = false)` and
read-only `AllowExperimentalIntersectionCalls` property select an unpublished
native call capability. Rebuild host tools against this development API revision.
Use it with `NeoClrCompilationEmitter.Emit` (native JSON), an explicit
`ReadExperimentalIntersectionAssembly` reference and matching dependency bindings.
The ordinary defaults, project/CLI switches and .NET Runtime Contracts do not opt
in. `EmitMetadataAssembly` still reports NEOMETA001 for compound references.

Supported source use forwards values directly between nongeneric imported native
free functions, for example `func Main() -> int => Consume(Echo(Produce()))`.
The portable lowerer admits intersection call signatures under the target-specific
capability without admitting them as storage types. Discarded results are popped.
The native adapter resolves the method token within its exact immutable semantic
snapshot, checks the bound artifact digest and imports the original descriptor.
Both native artifact-only and explicit legacy snapshot bindings are supported.
The descriptor is never reconstructed from the simplified compiler return/parameter
type, so redundant `A & Both` bounds survive native serialization.

The metadata host verifier requires exact normalized compound stack types. Different
raw contracts that both simplify to Both can bind semantically but emission rejects
them with NEOMETA003 before changing the output. This is a temporary verifier gap,
not a new source conversion rule. Nominal/compound injection or projection, general
cross-compound conversions, locals/spills, authored compound parameters/results,
fields, generics, nullable/wrapper compounds and member dispatch remain outside
this emission slice. Cross-dependency bounds requiring a resolver are not enabled
by this adapter path. Semantic admission does not prove executable support.

There is no CLI erasure/carrier ABI: output call operands retain native intersection
arrays and scoped nominal identities. Interpreter execution requires neoCLR's
`experimental-compositions` feature. Structural union import and JIT/AOT remain
closed. This narrow native-only path is a deliberate divergence from .NET/CLI,
which has no first-class intersection signatures; its benefit is preserving all
bounds and one reference, at the cost of an explicit host option and limited
storage/conversion support.

`NeoClrMetadataProbe --intersection-calls <core.dll> <output-directory>` checks
ordinary and redundant intersections, raw call descriptors, nested calls, discarded
results, both dependency binding forms, default/PE rejection, authored-local
rejection and distinct raw-contract rejection after identical nominal reduction.
Four generated programs return 42 in the opt-in interpreter. Existing
`--intersection-symbols` and `--nullable-symbols` controls pass; 24 focused
intersection symbol/conversion tests pass against the rebuilt net11.0 compiler.
No general storage, broad platform-matrix or JIT/AOT success is claimed.
