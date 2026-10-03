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


### Closed generic native field storage (2026-10-02)

The generic consumer assessment found that imported Box<int> calls and signatures
worked, but public fields containing Box<int> or Box<int>[] failed NEOMETA001 during
emission. The native field authoring path now admits closed generic reference values
and vectors, recursively checking arguments. It still requires a public instance field
on a nongeneric root-class owner with an explicit semantic storage ordinal. Open
parameters, generic owners, static/inherited fields and unsupported value profiles
remain excluded. No importer definitions or resolvers are used by the emitter.

NativeGenericSymbolChecks now builds BoxStorage in a second native assembly referencing
the generic library, then reads, replaces and mutates its scalar/vector fields. Both
reference orders compile; all seven consumers execute (42). The metadata API's C# test
executes the equivalent field references on .NET and both native containers, retaining
CLI generic signature/MemberRef encoding and native slot encoding. Runtime Contract,
primitive core and System bootstrap remain unchanged; no shared .NET compiler behavior
or format/runtime changes. Direct fields on generic declaring owners and generic
interface imports remain follow-up work for larger class-library consumers.


Generic-owner fields (2026-10-02): native public instance fields now support constructed
unconstrained root-class owners. Raven reads open field type/layout facts from compiler
symbols and binds consumer type arguments through ImportedConstructedFieldReference.
The metadata library preserves the open CLI MemberRef signature with a constructed
TypeSpec parent; stack validation uses the substituted type, while native emission keeps
the existing ordinal. IILGenerator owns instruction authoring. No importer definition
is reused by emission. Open caller parameters retain their scope; invalid arity, foreign
arguments, unconstructed field operands and out-of-scope arguments reject. Seven Raven
consumers execute (42), including generic forwarding and nominal mutation; 108/108 C#
metadata groups pass, with .NET and both native containers executing the field case.
Runtime Contract/core/System bootstrap and instruction encoding remain unchanged.


### Generic native interface imports (2026-10-02)

The native reader now retains constructed same-assembly interface relationships and
their type arguments, including open owner parameters. Raven maps these into existing
constructed symbols, preserving parameter owner identity, inherited interfaces and
invariant argument checking. Its emitter authors generic interface identities and
conversion edges from symbols, then binds interface calls to constructed references.
The loader is not consulted by emission. Metadata consumers may also import generic
interface methods and generic-owner fields from loaded native definitions.

NativeGenericConsumer now dispatches through MutableValue<int> -> Value<int> and
Value<Item>, including generic forwarding and class-to-inherited-interface conversion.
Both reference orders and incompatible argument diagnostics pass; all seven consumers
execute (42). NativeGenericOwnerChecks validates immutable reader relationships,
cycles/scope rejection, authored and reader-import dispatch, and field substitution.
All 108 C# groups pass; equivalent field/dispatch code executes on .NET and both native
containers. CLI TypeSpec/MemberRef and native dispatch/slot encodings are unchanged.

The supported interfaces are public, top-level, unconstrained and invariant. Native
relationship declarations currently resolve within their defining assembly. Emitting a
new class that implements an external interface or a new interface inheriting an
external interface remains a separate capability; this slice consumes already declared
relationships. Variance, constrained/value/nested profiles and instance generic methods
remain outside this native reader profile. Explicit primitive core, translated System
bootstrap and Runtime Contract configuration remain unchanged. This does not claim full
class-library import or remove the lazy semantic reader lifetime.


### Shared pure metadata view direction (2026-10-02, proposed)

The author requests a dependency-aware metadata view above neoCLR reader/writer
definitions, reusable by Raven and future NeoCLR metadata-only Introspection and
System.Runtime.Reflection.Emit. Reuse exact-identity IAssemblyResolver and loaded
snapshots; add a bounded resolution context and constructed type/member views rather
than a Reflection facade or a CLI projection. Definitions preserve original declarations;
views retain provenance and substitute arguments. An emitter explicitly imports contracts
into its own output graph. Metadata inspection must not load executable runtime types.

Raven's importer may consume this library view, but must copy semantic facts into its
symbols. Emission continues to depend on symbols, never the view/context/resolver.
No shared compiler API, Runtime Contract, native encoding, primitive-core bootstrap or
execution behavior changes in this design checkpoint. The first planned slice is an
immutable exact-identity snapshot catalog with diamond/cycle/conflict tests, followed
by constructed member views and one native importer integration. Author clarification: this is a prototype for the .NET-hosted Raven compiler. A future
NeoCLR-hosted compiler may reuse its lessons, but neither a port nor the same API or
implementation is required. Do not delay the prototype for speculative portability;
retain metadata/execution separation and keep future reuse exploratory.


### Metadata facade integration (2026-10-02)

The C# metadata library now supplies MetadataLoadContext and Introspection-shaped
assembly/module/nominal views. Raven removes NativeAssemblyResolver and uses one fixed
context per immutable compilation, retained through an internal weak-key lifetime
adapter. The context owns exact dependency resolution and canonical view identity;
Raven maps assembly identity and module-local type token into compiler-owned symbols.
Validation still applies Raven's target and duplicate-reference diagnostic policy.

All seven native consumers compile and execute (42); 109 metadata C# groups pass,
including exact versions, snapshot conflicts, diamonds, legal assembly cycles and
context isolation. No runtime loading or Reflection facade is introduced. The emitter
remains symbol-only. Runtime Contract, explicit CLI primitive core and translated
System bootstrap are unchanged. Signature substitution and member mapping remain in
the importer pending constructed/member facade views. This prototype follows the
runtime System.Introspection shape without committing a future identical port or API.


Constructed/field facade checkpoint (2026-10-02): the C# Introspection model now has
canonical primitive, vector, owner-parameter and constructed-type views plus declared
FieldInfo views. Definitions remain open; constructed owners substitute field signatures
simultaneously, preserving caller parameter scope and declaration identity. Recursive
nominal fields resolve without eagerly expanding members. Foreign/Void/bare-generic
arguments, wrong arity and unsupported method-parameter scopes reject explicitly.

Raven now consumes facade field types and closed signature projections, caching symbol
mapping by canonical view identity. This preserves array identity across fields, methods
and constructors; the existing integration assertion caught and verified that boundary.
Open method-signature adaptation remains in Raven until method/parameter views exist.
No emitter dependency on the context, Runtime Contract change, bootstrap change or
runtime/metadata encoding change is introduced. The runtime model informs names and
semantics but its guest implementation is unchanged. No performance claim is made.


Method/parameter facade checkpoint (2026-10-02): MethodInfo, ParameterInfo and
MethodGenericParameterTypeInfo now project namespace functions and declared methods,
including methods viewed on constructed owners. Owner and method argument scopes are
separate and substitution is simultaneous. Method/parameter identities remain canonical
within the context; no invocation or runtime loading is introduced.

Raven now builds native return/parameter symbols from these views and maps scoped
parameter identities back to the declaring compiler symbols. Its recursive signature
walkers and type/method generic-signature caches have been removed; the view-to-symbol
cache preserves signature identity. Language binding and special constructor return
semantics stay in Raven. This changes no Runtime Contract, primitive core/System
bootstrap, emission contract or metadata/runtime encoding. Properties and interface
relationship views remain next; generic method construction is not yet a facade API.

109 C# groups pass, including mixed owner/method scopes, generic function vectors,
constructed-owner returns, canonical method identity and invalid/foreign scopes.
All seven Raven native consumers compile and execute (42).


Property/interface facade checkpoint (2026-10-03): nominal and constructed views now
expose declared properties and directly declared interface relationships. Property
result/index types and interface arguments use the facade's simultaneous owner-scope
substitution; accessors share canonical method views. Indexed metadata excludes the
setter value parameter, including setter-only properties. Missing dependencies fail
explicitly, and CLI interface materialization remains unsupported rather than empty.

Raven consumes projected property types and direct interface views. Accessibility,
accessor association, parameter symbols and inherited-interface traversal remain compiler
responsibilities for this slice. Compared with .NET Reflection's property inspection,
this API has no get/set invocation or visibility filtering: it reports declaration data
and index types only. GetDeclaredInterfaces intentionally promises direct edges, not
Reflection's transitive GetInterfaces behavior. This keeps substitution reusable without
silently choosing compiler member-lookup or inheritance policies.

No Runtime Contract configuration, CLI primitive-core/translated-System bootstrap,
emission contract, guest API or serialized/runtime format changes. Native external
interface declarations and CLI relationship decoding remain separate gaps. Generic
method construction, constructor-specific views and bounded transitive metadata traversal
remain follow-up work. No performance claim is made.

Validation: the targeted NeoClrMetadataProbe build and all seven native semantic/emission/runtime
consumers pass (exit 42), including generic inherited interfaces and indexed properties.
The host metadata library passes 109/109 C# groups.


Interface closure checkpoint (2026-10-03): GetInterfaces on nominal/constructed metadata
views now returns distinct direct and inherited interfaces, with composed owner argument
substitution. Iterative depth-first traversal follows metadata order; identity includes
constructed arguments. Cyclic declaration paths reject, even when arguments differ.
Traversal is bounded to 4,096 distinct views and 65,536 visited edges, with cached
read-only results per owner. This replaces Raven native AllInterfaces recursion;
Raven retains language symbol substitution and binding policy.

The .NET 10 baseline is Type.GetInterfaces (Microsoft Learn, retrieved 2026-10-03):
https://learn.microsoft.com/en-us/dotnet/api/system.type.getinterfaces?view=net-10.0
It includes inherited interfaces and substitutes constructed arguments. We use those
semantics for the supported native root-class/interface profile; our explicit DFS order
and traversal bounds are metadata-library policy, not claims of exact CLR ordering.
Compared with leaving recursion in each consumer, this centralizes metadata traversal
and bounds at the cost of retaining per-owner closure results. General base classes,
CLI relationship decoding and constrained parameter queries remain unsupported.
No Runtime Contract, emitter, bootstrap, guest API or encoding changes.

Validation: 109 C# metadata groups and all seven native import/emission/runtime consumers
pass (42), including generic inherited interface dispatch.


## Dual-target driver acceptance baseline (2026-10-03)

NeoClrMetadataProbe --dual-driver <rvnc.dll> <neoclr> <fresh-output> executes ordinary
compiler commands for both targets. --dual-driver-inventory records failures without
claiming acceptance. It checks Hello World/helper invocation and a separately compiled
generic interface/class library with constructor, field, property and alias mutation.
Library source is removed before consumer compilation. .NET execution uses dotnet exec
with the explicit net10 compiler runtimeconfig; no reference-only artifact is executed.

Before driver native-import migration: both .NET cases and native Hello pass; the native
library consumer rejects with NEOMETA001 (undeclared instance field). This is a baseline,
not a completed dual-target gate. Evidence records command outputs and source/artifact hashes.
No Runtime Contract or compiler behavior changes in the test-only slice.


## Direct native compiler command (2026-10-03)

`rvnc neoclr --core-reference NeoCLR.CoreProbe.dll --reference Library.dll -o App.dll App.rvn`
now imports API-produced native assemblies using NeoClrMetadataReference and artifact-only
emission bindings. It selects CompilationOptions.NeoCLR and its explicit primitive/runtime
contracts. Native references never fall back to CLI projection. The core reference is
required when --reference is supplied; this is an experimental command compatibility change.
Without native references/core selection, the existing host primitive bootstrap remains
available. --system-symbols/--system-method is labelled legacy and remains an explicit
partial callable projection, not a native library import fallback.

Paired driver acceptance now uses `--dual-driver <rvnc.dll> <neoclr> <NeoCLR.CoreProbe.dll>
<fresh-output>`. Both .NET and NeoCLR Hello/helper and separate generic library consumers
execute (42); the consumer has no library source. Duplicate identities, missing transitive
dependencies, ordinary CLI references, malformed input and unsupported source reject without
publishing output. Host .NET runtime execution uses an explicitly hashed runtimeconfig.
The wider source-library/collections gate remains open; runtime encoding is unchanged.


Declaration-fact checkpoint (2026-10-03): metadata views now expose declared accessibility,
nominal abstract/sealed/static flags, instance/type-initializer classification, and
constructor enumeration on open/constructed owners. Constructor views share the callable
cache and substitute owner arguments. Raven maps supported metadata visibility to its own
accessibility and no longer decodes those type/method/field attribute bits itself.

This follows the existing CLI attribute contract rather than defining new access rules.
GetConstructors includes non-public instance constructors and type initializers explicitly;
GetMethods continues to exclude constructors. No invocation or implicit visibility filtering.
Existing canonical property accessor associations are retained. Supported native parameter
signatures remain by-value; byref/out, wider constrained/nested/value profiles still reject
at the reader boundary instead of losing their modes. This slice does not broaden encoding,
Runtime Contracts, the bootstrap, or runtime behavior.

Validation: 109/109 C# metadata groups, all seven native semantic/emission/runtime
consumers (42), paired driver cases on both targets and native driver rejection checks pass.


External-interface gate baseline (2026-10-03): the paired harness now also supports
--dual-driver-external / --dual-driver-external-inventory with the same driver/runtime/core/
output arguments. Contracts, Box<T> implementation and consumer compile in separate
invocations; source is removed before downstream compilation. The .NET split executes 42.
Native Hello passes, but native implementation emission rejects in SourceTypePlan before
encoding because the interface identity is external. This is a failing future acceptance
case, not a claim that the external-interface slice is complete.

Required changes span SourceTypePlan/SourceInterfacePlan capability admission, symbol-only
external relationship authoring in the NeoCLR adapter, metadata relationship validation/
round-trip, and runtime dependency/dispatch verification. Current writer/reader relationship
attachment requires owned definitions. Do not simply remove admission checks or duplicate
contracts into the implementation assembly. No compiler/runtime behavior changes in this
baseline test slice.


## Native/legacy consumer inventory (2026-10-03)

| Path | Current callers | Action |
| --- | --- | --- |
| Native reference with value-only NeoClrMetadataDependency | NeoClrCommand --reference, ExternalNativeChecks, NativeSymbolChecks and native driver cases | Keep: native declarations enter Raven through introspection; emission uses symbol facts and captured artifact identities. |
| CLI primitive reference plus explicit NativeImplementation seed | NeoClrCommand --runtime-seed and NamespaceFunctionChecks | Keep the explicitly permitted bootstrap. It is a configured service binding, not fallback for native library references. |
| CLI definition snapshot plus optional translated implementation | ExternalSignatureChecks, GenericLibraryChecks, ImportedInterfaceChecks, ImportedValueChecks, HelloWorldChecks | Retain as explicitly legacy comparison coverage until replacement consumers are identified. Do not delete based on the native gate alone. |
| NeoClrSystemSymbols partial Int32 projection | NeoClrCommand --system-symbols/--system-method and existing System-symbol probes | Legacy opt-in; keep separate from ordinary native reference loading. |
| Ordinary .NET metadata references | DotNetSemanticDataLoader and existing .NET compiler workflow | Retain existing Reflection implementation; the native library does not replace it in this milestone. |

No legacy branch is removed in this slice. Runtime Contract configuration and bootstrap
ownership remain unchanged. This inventory is a caller audit, not a promise to retain
legacy modes permanently or justification for adding more projections.

Native field and property construction now consumes the already-projected FieldInfo and
PropertyInfo collections from the owner's introspection view. Property accessors resolve
by their facade-provided module-local tokens through the existing canonical method-symbol
table. Raven still combines accessor accessibility according to its supported language
policy. Definitions remain for other declaration categories and union transport; this is
not a claim that all native symbol construction is definition-free.

The symbol importer does not supply builders or accessor objects to emission. Emission
continues to use Raven symbols and explicit host artifact bindings. No public facade,
metadata encoding or .NET loader/codegen changes are made.

Validation: build the native probe with NeoClrMetadataProject configured and run
`--native-symbols-runtime <NeoCLR.CoreProbe.dll> <neoclr> <System.neox> <fresh-output>`.
Both before and after runs pass all seven consumers (exit 42), plus native semantic and
rejection checks. The C# canonical property test additionally verifies setter membership
and public property accessibility, alongside getter/setter associations, write-only
indexers, private setters, static properties, external signatures and generic substitution.
The probe requires its documented introspection-capable CoreProbe; ArrayListCore lacks
that typeof contract and rejects during setup. That configuration rejection is not a
regression in property import.

### Native type materialization cleanup (2026-10-03)

Native type materialization now enumerates ModuleInfo.GetTypes and passes canonical
NominalTypeInfo views to ordinary type, union, case and companion symbols. Physical
nesting and Raven union membership remain distinct; ownership checks use canonical
views rather than reader definitions. Removed unused raw-signature mapping helpers.
This is importer cleanup only: Runtime Contract configuration, explicit primitive
bootstrap/runtime seed, emitter operands, metadata encoding and ordinary .NET behavior
are unchanged. No additional CLI projection or fallback is introduced.

Validation: the source-union driver gate compiles and consumes the native library and
runs unchanged application-order-collections with exact expected output (exit 0).
All seven native consumers also pass (exit 42), including semantic and rejection checks.

### Native array interface receiver emission (2026-10-03)

The unchanged collection-capabilities sample exposed a missing receiver conversion:
`let values: System.Array<int> = [40, 0]; values.Count` selects the inherited
Collection<int> accessor while the receiver remains a vector in the bound tree.
Portable body planning now emits its existing reference-conversion operation for an
array receiver when the member owner is an interface, the backend admits that operation,
and Raven classifies the conversion as implicit reference conversion. This uses semantic
identities only; the native adapter still authors references from symbols/artifact bindings.

Unlike ordinary CLR arrays and their CLR collection interfaces, native vectors use the
explicit source Array<T> backing and its configured interface graph. That nominal backing
and runtime ownership manifest remain unchanged. The adjustment supplies the receiver
representation required by the existing verifier; it adds no format, opcode, bridge,
reflection handle or structural array semantics. The established .NET body emitter is
unaffected. This portable-planner fix is a deferred general candidate, not a binder fix:
independent main-line validation must establish a caller requiring this planner path.

The native driver gate now compiles a minimal Count/mutation regression and unchanged
Option, Option propagation and collection-capabilities samples against the separately
built source library. It verifies and executes each, checking exact output/status. The
unchanged broad order-collections application still passes. Wider sample assessment
finds separate remaining gaps: no-result callback import, captured function emission,
and absent Int64.CompareTo/Date bootstrap/library declarations. Do not rewrite samples
or treat these as successful execution.

Validation also preserves all seven native runtime consumers. Focused C#
SharedArrayBodyTests and SharedInterfaceDispatchTests pass on the ordinary .NET target.

### Receiver-bound callback prerequisite (2026-10-03)

Native portable emission now admits method groups bound to an owned, nongeneric,
nonvirtual reference-instance method. It evaluates and emits the receiver before the
existing FunctionBind semantic operation. Target-specific builders remain inside the
native adapter; the importer is not consulted. The metadata library consumes the
receiver and emits the existing native instance-target bit, or CLR ldftn/newobj with the
object receiver. Static method-group behavior remains supported.

This supports closure environments as ordinary reference objects without defining their
compiler lowering yet. The native instance-callback consumer binds Matcher.Matches,
changes Matcher.Target after binding, and passes the predicate into a separately compiled
ArrayList.Find. Return 42 confirms current receiver state and mutation through the original
object. The original list-filter sample remains blocked on captured BoundFunctionExpression;
it has not been rewritten or substituted. Next work is closure-frame planning/lowering,
with shared mutable captures and lexical lifetime tests rather than copying all captures.

Runtime Contract/bootstrap choices, ordinary .NET body emission and metadata version are
unchanged. Value receivers, generic owners/targets, virtual dispatch bindings and imported
binding targets remain unsupported by this bounded writer path. No structural Function
feature branch is integrated. Validation: native broad driver gate, 128 metadata groups,
CLR/native receiver-identity execution, and 31 focused .NET callback/contract tests pass.
This planner change is a deferred general candidate pending an independent main-line
caller; it is not a general binder behavior correction.

## Native immutable reference captures (2026-10-03)

The portable body plan exposes logical capture loads only under the explicit native
capability profile. The NeoCLR adapter creates private frame fields, a constructor
and an instance Invoke method through metadata builders/GetILGenerator. A fresh frame
is allocated when the lambda is evaluated. It stores the same objects referenced by
immutable local bindings; object mutations remain visible to all aliases. No importer
objects or runtime reflection handles participate in emission. Existing instance
Function bindings carry the frame; metadata and runtime encodings are unchanged.

The current profile rejects mutable bindings, value captures, captured parameters and
receiver captures before output publication. It does not implement general shared
variable storage or fix the separately recorded .NET per-iteration scalar closure issue.
Ordinary .NET body emission is unchanged. Runtime Contract/bootstrap selection remains
explicit and unchanged; this does not integrate structural Function feature branches.

Validation: unchanged library-list-filters executes with exact output against the
separately built native source library. A focused consumer tests escaped callbacks,
independent factory state and shared reference mutation; mutable capture rejects without
publishing output. The expanded broad native gate and seven native consumers pass,
with focused ordinary .NET function tests. The portable operation is a deferred general
candidate until an independent backend consumer warrants validation on main.

## Primitive local captures and promoted operands (2026-10-04)

The native capture profile now also admits immutable Int32, Int64, Boolean and Byte
locals, stored by value in the existing fresh frame for each lambda evaluation.
Reference captures still retain object identity. A consumer of the separately built
FunctionEqualityComparer/HashMap library exercises a captured integer divisor;
escaped callbacks and callbacks created in a native array loop retain distinct values.
Mixed-width capture arithmetic exposed a missing portable emission conversion:
the adapter-independent plan now uses the bound built-in operator's promoted operand
types to widen Byte/Int32 to Int64. It does not redo overload resolution or modify
binding. CLR's ordinary emitter remains unchanged; .NET execution controls and native
execution cover this promotion, without assertions about exact instructions.

Mutable locals, arbitrary structs, parameters and receiver captures remain rejected;
no claim is made about repairing the separately tracked ordinary .NET loop-capture
issue. Existing Runtime Contract, explicit ownership manifest and metadata/runtime
encodings are unchanged. The full comparer sample still needs StringComparer runtime
services and primitive CompareTo contracts. The portable promotion fix remains a
deferred general candidate until an independent backend caller justifies main-based
validation; this change does not require merging the native target into main.
