# Metadata import and emission boundaries

Author-directed design, 2026-10-02. This is the intended contract; the current
native implementation still has the coupling listed below. It does not claim
that the importer/emitter separation is complete.

## Ownership

Metadata source → importer → Raven symbols → semantic analysis/lowering → emitter
→ backend-owned output references/declarations/body generation → assembly.

The importer resolves input assemblies and populates the compiler symbol model.
The emitter consumes those symbols and the bound/lowered program. It must not
call the importer, examine importer-specific symbol subclasses, reopen input
metadata to rediscover a signature, or reuse reader definitions/resolvers/handles.
Symbol-owned lazy materialization remains an implementation choice of the semantic
model; emission must access it through the same source-neutral symbol contracts.

The compiler host owns reference selection and target Runtime Contract configuration.
Import failures and incomplete semantic contracts must diagnose before output is
published. Metadata visibility, overload selection, conversions and generic constraint
semantics belong in the semantic layers; backend representation and unsupported
instruction/metadata capability checks belong in emission contracts.

## Two independent authoring boundaries

Raven's compiler emission interfaces describe compiler declarations, symbol references,
logical types and lowered instructions. Neither Reflection/Reflection.Emit nor the
NeoCLR metadata library's builders, definitions, tokens or generator types belong in
these shared contracts. Backend implementations own their concrete output handles.
Existing portable callable/type plans and ILinearMethodBuilder are starting points;
the older IILBuilder still exposes Type, MethodInfo and Reflection.Emit operands and
is not yet the intended source-neutral boundary.

Separately, the Cecil-like metadata library needs its own IILGenerator-style authoring
contract. Method builders declare members and provide a body generator; generators
provide typed Emit overloads, convenience operations, locals and labels. Definitions
own stored bodies, and writers encode them. This library API is useful without Raven.
Raven's NeoCLR adapter translates compiler operations into it; the two interfaces do
not inherit from one another. A .NET backend may use Reflection.Emit now and Cecil
later without changing the compiler contract. This direction does not authorize a
new Reflection API clone or require identical generator interfaces across libraries.

## Information that must survive import

The symbol model must carry enough meaning to reconstruct supported output references:

- Exact assembly identity and resolved declaration identity, including namespace,
  nested owner, metadata name/arity and assembly-level function ownership. Resolve
  forwarding to a canonical declaration without losing required assembly scope.
- Recursive type shapes: primitive semantics, nominal kind, arrays/ranks, byrefs,
  constructed arguments and generic parameters identified by owner and ordinal.
- Complete callable/field signatures and required representation information:
  return/parameter/ref modes, static/instance/constructor distinctions, calling
  convention, visibility, virtual/interface dispatch, readonly and relevant modifiers.
- Generic constraints, base types, interfaces and canonical property/event accessor
  associations as those profiles become supported. Unsupported required information
  must diagnose, rather than be silently dropped.

Use semantic values and compiler-owned immutable identities, not opaque source objects
hidden behind an interface or object-valued payload. OriginalDefinition and constructed
symbols retain declaration identity; per-emission caches map these identities to fresh
output handles. Assembly identity alone must not weaken current dependency snapshot
validation: host-selected immutable artifact identity is a separate explicit input
contract. If native linking requires an ordinal, it needs a documented compiler-owned
linkage value and artifact scope, or a writer/linker change; it must not be recovered by
emitter calls into the importer. Ordinary CLI encoding should not inherit native-only
requirements. Runtime Contract mappings remain explicit and target-selected.

Import-time dependency resolution and emission-time output-reference mapping are
different operations. The latter creates references from already resolved symbols;
it does not search input definitions again. Merely wrapping the current resolver in
an interface would not satisfy this boundary.

## Current violations and bounded migration

Int32Emitter currently reads NativeMethodSymbol.Definition and NativeFieldSymbol.Definition,
checks reader assembly object identity, passes NativeAssemblyResolver into metadata
ImportReference, and searches dependency type/method definitions for CLI-backed symbols.
These are temporary implementation dependencies, not the desired architecture.
The Cecil-like MethodBuilder currently exposes body operations directly.

1. Inventory the symbol information used by one native namespace-function reference.
   Add missing compiler-owned identity/signature/linkage facts and a source-neutral
   reference description, using existing symbols where they already suffice.
2. Add metadata-library reference authoring from semantic descriptions, independent of
   reader definitions. Preserve exact scope and snapshot checks. Re-emit an imported
   namespace-function call without emitter access to reader definitions/resolvers.
3. Extend to nominal/constructed types, fields, constructors and interface dispatch;
   move each case only with positive execution and negative identity/signature tests.
4. Independently introduce the metadata library's body-generator contract and migrate
   its clients. Keep any compatibility shims explicitly temporary; do not make Raven's
   shared interfaces depend on it. Stored-body editing remains a separate API concern.
5. Adapt .NET emission through the same compiler contracts in bounded slices, preserving
   its default behavior. A later Cecil backend can replace the Reflection.Emit adapter.

Prove the boundary with equivalent imported and source-defined symbol contracts,
reference-order independence, generic owner identity, and wrong/missing dependency
rejection. A test should reconstruct emission references from compiler-owned data
without a reader handle. Runtime consumers must still compile, load and execute;
.NET controls protect existing behavior. Do not claim reader disposal is supported
until semantic materialization and its lifetime contract actually guarantee it.

## Tradeoffs and limits

Compared with the current .NET Reflection/Reflection.Emit path, this adds explicit
signature/identity mapping and backend caches, but removes the requirement that input
and output share reflection objects. It supports native metadata extensions without
putting native encodings in ordinary semantic logic. It does not promise arbitrary
lossless assembly rewriting through compiler symbols; that belongs to the metadata
library's definition reader/writer. No performance improvement is claimed. Keep
canonical symbols and cache output handles per emission; measure if mapping costs
become material. Existing CLI/Cecil design research in neoCLR's metadata design
remains applicable; this is a project ownership decision, not a new format change.


### First symbol-only emission slice (2026-10-02)

Native namespace functions with primitive, method-parameter and single-vector signatures
now reconstruct output references using IMethodSymbol and a compiler-owned
ResolvedAssemblyArtifact value. The latter contains exact assembly identity and the
selected input image's SHA-256, not a reader handle. The importer copies method flags
into symbol state. The backend maps the signature from symbols, compares the host
binding's captured digest, and calls the metadata library's CreateFunctionReference.
It does not use NativeMethodSymbol.Definition, input tokens or the metadata resolver on
this path. Resolver creation is lazy so this profile does not instantiate it.

This is intentionally bounded. Nominal signatures, type-owned methods and fields still
use the old reader-backed path; host dependency setup still accepts definitions and
performs existing validation. Native symbol lazy materialization is unchanged. No claim
is made that readers can yet be disposed before emission or that all compiler boundaries
are independent. The metadata library's separate IILGenerator is still planned.

The format remains unchanged: native linking selects namespace/name/signature. The
artifact digest guards compiler/output consistency; it is not encoded runtime integrity.
Explicit Runtime Contract selection, CLI primitive core and translated System remain
required. .NET emission is unchanged. The seven native consumers compile and execute
with exit 42, including generic vector calls and negative reference checks; the library's
107 C# test groups pass and its generic vector reference executes in both containers.
Next remove nominal type-reference reconstruction's dependency on input definitions.


### Symbol-only root-class references (2026-10-02)

ImportExternalType now authors native public top-level root-class references from
INamedTypeSymbol and ResolvedAssemblyArtifact values. It preserves namespace, metadata
name, arity, exact dependency and artifact identity; generic construction maps symbol
arguments recursively. The selected profile is invariant/unconstrained, has no declared
interfaces, and has no non-Object base. It performs no type-row search or definition
import on this path. The metadata library's new CreateTypeReference shares canonical
output identity and snapshot checks with existing reader imports.

Interfaces, inheritance, nested/value types, member references and nominal function
signature imports remain reader-backed. This avoids removing conversion/dispatch facts
before their semantic-to-output contracts exist. Host input setup and lazy symbol
materialization still retain readers. Runtime Contract configuration, CLI primitive
core and translated System bootstrap are unchanged; the format is unchanged.

All seven native consumers compile and execute (42). Metadata C# checks cover interning,
argument copying and negative digest/core/identity/arity contracts; the authored generic
class reference works with imported members on CLR and both native containers (42).
The library IILGenerator remains a separate pending API. Next migrate nominal callable
references without moving any metadata-library interfaces into shared compiler contracts.


### Nominal namespace-call reconstruction (2026-10-02)

The symbol-only namespace-function path now handles external root-class signatures,
including closed/open generic constructions and vectors. Signature admission reuses
the root-class identity predicate, so mapping cannot silently fall back to type-row
lookup for an admitted signature. Type arguments map recursively from symbols and
each dependency retains its exact artifact check. The metadata writer now recognizes
authored nominal call references as validated contracts during graph validation.

This covers cross-dependency Box<T> forwarding. Type-owned methods, fields, richer
type profiles, host setup and symbol materialization remain reader-backed. The
separate library instruction generator is still pending. Runtime Contract and
bootstrap configuration and native metadata encoding are unchanged.


### Root-class member contracts (2026-10-02)

Public concrete nonvirtual root-class methods and constructors now use the symbol-only
path. The backend reconstructs the declaring reference, static/constructor distinction
and scoped signature from symbols, then authors the member reference. Generic owner
parameters retain owner ordinals and use existing constructed-reference substitution.
Instance generic methods, virtual/interface dispatch, value/nested profiles and fields
remain on prior paths. Dependency snapshot checks, explicit Runtime Contract mapping,
CLI primitive core and translated System bootstrap are unchanged; host input setup and
semantic materialization still retain readers. The separate library generator is pending.

Validation: all seven native consumers compile and execute (42). All 107 metadata C#
groups pass, including authored member identity/conflict/owner/scope checks. The generic
owner consumer now authors constructor/Get/Set/Same from semantic values and executes
on CLR and both native containers (42). Next migrate fields and dispatch facts.


### Explicit native field layout (2026-10-02)

The optional compiler-owned IInstanceFieldLayoutSymbol carries the zero-based instance
slot, scoped by its containing type and ResolvedAssemblyArtifact. The native importer
copies it in one field enumeration (including private fields); readonly status is also
copied into symbol state. The emitter authors public nongeneric root-class field
references from these values and semantic storage types, without NativeFieldSymbol
casts or reader definitions on this path. Root primitive/nominal/vector storage is
supported. Richer layouts, constructed owners and interface/value/nested profiles retain
the previous route or limitations. Ordinary .NET uses names rather than this layout fact.

The slot is an asserted target linkage value; snapshot checks retain the exact artifact
binding but cannot validate a dishonest slot without reopening metadata. C# checks
cover conflicting slots/contracts and readonly-store rejection. Seven native consumers
execute (42), including alias writes and public fields following private storage. Runtime
Contract, CLI primitive core and translated System requirements are unchanged. Native
format and opcodes are unchanged. The independent library generator remains pending.


### Interface relationships and dispatch (2026-10-02)

Nongeneric native interfaces and root classes implementing them now author identities
and direct interface edges from symbols. The backend caches identities before following
relationships, and admits a bounded interface graph; deeper/unhandled profiles retain
the prior route. Authored interface members use abstract/virtual symbol flags to create
dispatch contracts without input method definitions. Signature and field-storage mapping
can therefore carry interfaces without losing assignability information. The metadata
library checks cycles and classification conflicts and derives transitive conversions.

All seven native consumers execute (42), including inherited method/property dispatch
and interface-valued storage/aliasing. 107 metadata C# groups pass, with authored graph
and dispatch-body checks. Generic interfaces, class inheritance, class virtual dispatch,
remaining unsupported members, host setup and lazy semantic materialization are still
outside this separation. Runtime Contract configuration and primitive/translated System
bootstrap remain unchanged; the separate library generator API is pending.


### Independent library body generator (2026-10-02)

The NeoCLR adapter now passes the metadata library's IILGenerator through its body,
call and field emission helpers. Shared Raven ILinearMethodBuilder and other compiler
contracts are unchanged. The adapter obtains a generator from each declared method;
metadata builders remain declaration/reference handles rather than the adapter's body
writer. The library's generator writes the same definition-owned body and initially
delegates to the existing builder engine. Legacy library builder operations remain
compatible; no loaded-body editing or instruction insertion is implied.

All seven native consumers compile and execute (42). The metadata library's 108 C#
groups pass, with typed/raw emit, scope rejection, canonical generator access and CLR
execution. Generic-owner generation through the interface runs on CLR and both native
containers. Runtime Contract and CLI primitive/translated System bootstrap are unchanged.

Audit: translated CLI bindings, unsupported type/dispatch profiles, host reference
configuration and lazy semantic materialization still depend on readers. Their
remaining boundaries must be addressed before claiming full import/emission separation
or reader disposal before emission. Internal library engine separation is also pending.


### Generator engine ownership (2026-10-02)

The metadata library now implements body mutation, local/label creation and immediate
operand validation in MethodILGenerator. Its builder instruction methods forward to the
generator for compatibility. Raven's adapter continues to use IILGenerator, with no
shared compiler changes. The definition remains the single body store. Writer-side
graph/flow validation and operation representation are unchanged, as are Runtime
Contract mappings, bootstrap requirements and supported opcodes. Mixed legacy/generator
use preserves handle ownership. All 108 metadata C# groups and seven native consumers
pass; generic-owner generation executes on CLR and both native containers (42).


### Static declaration containers (2026-10-02)

Static native classes now qualify as declaration owners on the symbol-only reference
path. Their static and generic-static methods use authored references rather than
NativeMethodSymbol.Definition. Owner admission is deliberately separate from signature
value admission: this does not make static classes instantiable or valid storage types.
Existing signature, accessibility, generic-constraint and artifact checks remain.

All seven native consumers execute (42), including static overloads and generic calls.
The metadata generic-static test authors references from values for CLR and both native
containers (42); 108 metadata C# groups pass. Remaining reader paths include final/virtual
class contracts, unsupported profiles, translated CLI compatibility bindings, host setup
and lazy symbol materialization. Runtime Contract/bootstrap behavior is unchanged.


### Native callable fallback removed (2026-10-02)

The native callable reader-definition fallback has been removed. After symbol-only
namespace/member authoring, an unsupported native callable now diagnoses instead of
reading NativeMethodSymbol.Definition. That unused property is removed from native
method symbols. Translated CLI compatibility binding remains a separate path. Input
signature materialization and field/type/host reference paths still retain reader data;
this is not a claim of whole-compilation reader disposal.

The audit corrected an earlier assumption: native concrete interface implementations
are ordinary methods in the current writer format. Only abstract interface contracts
carry the native virtual flag. The CLI projection's final/virtual implementation flags
must not be invented in native symbols. No final-virtual API or native format change
was added. Direct calls on concrete implementations and their getters were added to
the probe alongside existing interface dispatch. All seven consumers execute (42).
Existing 108 metadata-group evidence is reused because the library is unchanged.
Runtime Contract and primitive/translated System bootstrap requirements are unchanged.


### Native type/field fallbacks removed (2026-10-02)

Native type and field reference construction now fails closed on unsupported symbol
contracts, like callable construction. Int32Emitter no longer creates a native metadata
resolver or casts fields to NativeFieldSymbol to access reader definitions. The unused
field definition property is removed. Supported native references are authored from
compiler symbols, exact artifact values, explicit layout ordinals and interface edges.
Definition lookup remaining in this emitter is confined to translated CLI compatibility.

All seven native consumers compile and execute (42), covering fields, external signatures,
interfaces and generic constructions. Existing diagnostic checks still pass. The library
and runtime are unchanged, so previous 108-group metadata evidence is reused. Host
dependency setup and lazy semantic materialization still retain readers; full disposal
before emission is not established. Next separate host-native input binding from the
translated CLI binding contract. Runtime Contract and bootstrap requirements are unchanged.


### Native host bindings without reader definitions (2026-10-02)

`NeoClrMetadataDependency(NeoClrMetadataReference reference, AssemblyIdentity coreLibrary)`
binds the exact native reference registered in the compilation. Both arguments are
required (`ArgumentNullException` otherwise). It captures the reference's immutable
artifact identity and SHA-256; constructing the binding does not read, write, hash or
copy an assembly image. The emitter authors output references from semantic symbols.

Use `new NeoClrMetadataDependency(nativeReference, coreIdentity)` for native inputs.
`Reference` exposes the registered compiler reference, `CoreLibrary` the explicit host
contract, and `NativeImplementation` is null. `Definition` throws
`InvalidOperationException` for this overload: it is a legacy snapshot accessor.
The existing `(MetadataReference, AssemblyDefinition, AssemblyIdentity,
NativeLibraryDefinition?)` overload is retained for CLI/translated inputs and existing
native snapshot callers; native snapshots must still be the semantic reference's exact
snapshot and cannot specify a translated implementation.

Emission rejects duplicate identities/symbols, unregistered reference instances, core
mismatches and mismatched legacy native snapshots with NEOMETA002, without output.
All seven native probe consumers now use the new overload and execute (42); C# negative
checks cover these configuration errors and the unavailable Definition accessor.
Runtime Contract selection, explicit CLI primitive core and translated System bootstrap
remain unchanged. The compiler reference still owns lazy semantic reader state; this
change removes the separately supplied emission snapshot, not that reader lifetime.
No shared .NET compiler contract, instruction encoding or metadata-library API changes.
