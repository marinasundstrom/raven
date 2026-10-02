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
