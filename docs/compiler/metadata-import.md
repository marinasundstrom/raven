# Explicit-only metadata import

## .NET compilation preset

`CompilationOptions.DotNet` returns fresh immutable defaults for a .NET target
whose semantic inputs are explicitly supplied references. Framework/package
resolution remains with the caller or project tooling:

```csharp
var options = CompilationOptions.DotNet
    .WithOutputKind(OutputKind.DynamicallyLinkedLibrary);
var compilation = Compilation.Create("Example", syntaxTrees, references, options);
```

The preset uses `new MetadataImportOptions()`: discover the core defining
`System.Object` from the supplied references, without adding host fallback paths.
The .NET target uses that discovered core identity for emission as well. No
framework version, installed SDK reference pack, or package is selected by this
preset. Existing `With...` copies retain this policy. Host execution services are
still separate and .NET-specific; this does not remove all host reflection from
the emitter.

`new MetadataImportOptions("Core.Name")` retains the older named-core import
mode, with independent emission selection. `new CompilationOptions()` retains
host-assisted import. A null `MetadataImportOptions` still means host-assisted;
a non-null options object with a null `CoreAssemblyName` means explicit-only
core discovery. This is a transitional representation using the current import
options, pending the agreed TargetPlatform/Contract API.

For discovered-core mode, an explicit `TargetCoreAssemblyName`, when supplied,
must match the discovered core's simple name. Conflicting explicit `EmitOptions`
core identities report RAVT003 before output is written. Missing-core setup is
reported as RAVT004 by compilation diagnostic collection and emission; see the
failure behavior below. An empty reference list never gains implicit host
references through this preset.

## Host runtime implementation paths

`Targets.DotNetRuntimeAssemblyPathResolver` owns the existing reference-to-runtime
path heuristics used by host assembly registration, called through the .NET target.
It handles NuGet ref/lib layouts, SDK packs/shared layouts, and recognized framework
reference packages. This is host implementation lookup, not semantic reference
resolution: its results are not added to an explicit-only target reference set.

Preserved policies include preferring a matching NuGet lib path, then descending
path order among available package lib candidates; SDK-pack fallback uses the matching
shared-framework version. Recognized framework reference packages first look for
an installed exact version, then prefer stable numeric versions within the requested
major, before falling back to package lib lookup. These are existing host lookup
heuristics, not a new framework compatibility guarantee or cross-compilation policy.

The resolver accepts an internal shared-framework root override so path selection
can be tested against temporary layouts without depending on the machine's installed
frameworks. Production derives that root from the compiler host's runtime as before.
`DotNetHostRuntime` owns host assembly loading and caches separately from this
path policy. Reflection-to-semantic-symbol projection still has dependencies in
Compilation.

## Host assembly service ownership

Each `DotNetCompilationTarget` owns one `DotNetHostRuntime`. The service holds
per-compilation metadata-to-runtime assembly associations, local path registrations,
and local runtime assembly caches. Existing process-wide path/runtime caches and
trusted-platform discovery move with it, retaining their current lifetime,
initialization and lookup order. This extraction does not introduce deterministic
unloading or change default AssemblyLoadContext behavior.

The service performs runtime assembly registration/loading, host type lookup and
host emit-core discovery. The target injects this service directly into its loader;
metadata import no longer calls back through Compilation for path or runtime
assembly registration. Compilation's remaining runtime-type lookup adapters retain
setup and argument checks. The service does not retain Compilation or semantic symbols;
symbols supplied for host lookup are only used during that call. Compatible
metadata sessions may still be shared independently, while each new compilation
gets its own host service and symbol caches.

Host fallback paths remain available only under existing host-assisted import
policy. Explicit-reference target definitions are not replaced by runtime
implementations, even when those implementations are already in shared host caches.
The compiler still exposes existing reflection core handles; replacing the target
remains incomplete. ReflectionTypeLoader is now owned by the per-compilation .NET
target as described below.

## Metadata session and core ownership

`DotNetCompilationTarget` holds the selected metadata core, executable host core,
and host emit core alongside its metadata session. Shared setup requests a semantic
loader from the target after checking reference fingerprints; it no longer handles
a .NET session directly. Existing `Compilation` core-assembly properties forward
to the target without triggering setup. Target operations use their owning
compilation for contract validation and emission.

Host handles are seeded before setup can reenter reflection services. Metadata
initialization then selects the metadata core and registers its host mapping before
importing symbols. This preserves setup ordering, initialization diagnostics and
collection-based session lifetime. These remaining reflection-facing adapters
still need attention before a second target can be selected.

## Reflection-to-symbol projection ownership

Each `DotNetCompilationTarget` owns one lazy `ReflectionTypeLoader`, including its
reflection type, metadata identity and method/type-parameter projection caches.
The target passes that projector explicitly to `DotNetSemanticDataLoader`, which
uses it for imported PE modules. Compilation's reflection entry points forward to
the same projector; they no longer create or own an independent instance.

Projector allocation does not initialize the compilation or load references.
This permits reflection queries before setup and allows metadata import to reuse
the projector during same-thread setup reentrancy. Lazy initialization publishes
one projector even when multiple callers request it concurrently. This does not
introduce a broader concurrency guarantee for every projection operation.

The projector remains compilation-bound and is never stored in shared metadata
sessions or host caches. Compatible snapshots may share a metadata session but
retain distinct projectors and projected symbols. This is an ownership extraction;
projection algorithms, nullability, generic substitution and the existing public
`Compilation.GetType(Type)` surface remain unchanged. The .NET-specific projector
is not added to the neutral semantic-data-loader interface.

## Target initialization failures

Known option contradictions are checked first and reported as RAVT003, before
reference loading. See [configuration validation](runtime-contracts.md#configuration-validation-before-loading).
A consistent configuration can still fail to establish its core as described below.

Compilation-wide, tree-scoped, and document-scoped diagnostic collection report
RAVT004 when the .NET loader cannot establish its metadata core. This includes
explicit-only discovery with no supplied core defining System.Object, and known
I/O, access, invalid-image or type-load failures while opening the metadata core
session. The diagnostic retains the configured core identity and underlying
failure detail when available. It does not substitute host definitions.

Emission returns an unsuccessful EmitResult before declaration binding or writing
to either PE or PDB output, even if the caller supplied precomputed diagnostics.
RAVT004 remains a fatal error regardless of diagnostic suppression/severity
settings: there is no initialized target against which compilation can continue.
Diagnostic collection stops at this failure; it does not claim a complete set of
semantic diagnostics. Syntax-only diagnostics require no target initialization.

Failed setup is not marked complete or reused by another snapshot. A new
compilation with corrected references can initialize normally; repeated or
concurrent diagnostic requests on the invalid compilation remain failures.
Failures are not cached as successful sessions. Cancellation and unrelated
compiler exceptions are not translated into RAVT004.

This boundary covers core-session establishment, not every possible later import
failure. Direct semantic queries still require a valid target and can throw on
initialization failure. No partial semantic environment or error-symbol core is
introduced. Further loader failures and earlier option validation remain follow-up
work.

## Implementation boundary

The .NET implementation materializes resolver inputs in an immutable
`Metadata.DotNetMetadataReferenceSet`. It owns path normalization, identity
deduplication, and ordered candidate selection. Entries contain identity strings
and paths, not mutable reflection identities or a snapshot of assembly bytes.
`DotNetMetadataContextFactory` owns the stream-backed resolver, context
construction, and portable assembly identity reads.
`DotNetMetadataSession` owns the resulting context and path/identity loading,
including the existing path-to-identity fallback. `DotNetSemanticDataLoader`
selects the reference paths and metadata core and creates fresh sessions;
`DotNetCompilationTarget` supplies explicit reference and import-option inputs to
session setup and owns the resulting session and any reuse candidate. `Compilation`
gates incremental session reuse using import options and portable-reference
fingerprints. The target adopts only the previous session, never the previous
compilation, target, projector or host service. Reference import passes
through `ISemanticDataLoader`; `DotNetSemanticDataLoader` owns loaded-assembly and
assembly-symbol caches, dependency traversal, and construction of PE assembly/module
symbols for each compilation. Its dependencies are the metadata session,
reflection projector and host service. The projector still binds it indirectly to
the owning compilation; this does not make the loader reusable across snapshots.
Compilation-to-compilation references remain in
the shared compilation layer. The .NET loader explicitly registers selected paths
through the .NET host service before creating a new session. Metadata
construction no longer accepts a host-registration callback. The resolver keeps
the first sorted simple-name candidate; host registration still visits all
selected entries in order and retains the last path for that name. These remain
separate compatibility policies.

These are incremental extractions described in the
[target boundary plan](architecture/target-boundaries-and-bootstrap-plan.md).
The factory is .NET-specific and returns `MetadataLoadContext`; it is not yet a
platform-neutral provider. Public APIs, reference precedence, fallback policy,
configuration errors, and context lifetime are unchanged. Compatible snapshots
share a session without retaining the previous compilation. A session keeps no
compilation, symbol cache, or registration callback. Individual compilations do
not dispose its context because other snapshots can still use it; the existing
collection-based lifetime is retained. Deterministic shared-session disposal is
not introduced by this extraction.

The loader interface returns semantic symbols rather than reflection assemblies.
It currently uses the existing `MetadataReference`/`IAssemblySymbol` reference
surface; these shapes can be redesigned for neoCLR as needed. Selection is still
fixed to .NET through the internal `DotNetCompilationTarget`, which composes the
loader with its runtime contract and existing code generator. Core-library
discovery is loader-owned, but compilation still holds reflection core handles for its existing runtime/emission services.
Reflection type projection and host runtime registration also retain .NET
dependencies. This is an import boundary,
not a claim that another target can already replace all semantic data loading.
The goal is to select loader, platform/runtime contract, and codegen together for
.NET and later neoCLR; independently mixed targets and cross-compilation are out
of current scope.

### Imported assembly discovery

Shared assembly discovery in `CompilationSymbolLookup` uses the internal
`IImportedAssemblySymbol` contract for simple-name/arity lookup and extension
conversion container discovery. The .NET implementation remains `PEAssemblySymbol`;
its reflection metadata index is private to that implementation. The contract
returns Raven symbols and exposes no reflection objects or cache-control APIs.

Simple-name lookup still prefers matching source declarations, then the first
matching imported assembly/type. Nested types participate by their own name and
arity. Extension conversion discovery returns candidate containers; binding
continues to decide applicability. Public `IAssemblySymbol` and semantic-model
APIs are unchanged in this slice, but they can be redesigned under
[ADR-0003](architecture/decisions/0003-target-owned-metadata-and-emission.md).
Symbol creation and emitter reflection access remain boundaries to extract.

Namespace extension discovery uses `INamespaceExtensionLookup`, a capability
implemented by PE namespaces and composed by merged namespaces. It does not
classify a namespace as purely imported: merged namespaces can include source
declarations and multiple providers. Shared lookup and merged discovery no longer
dispatch on concrete PE namespace types. General namespace/type traversal remains
available for namespaces without the capability; source extension traversal and
receiver applicability are still separate compiler responsibilities.

These contracts are intermediate steps toward target-owned metadata and symbols,
not a fixed plugin ABI. The runtime/platform contract governs semantic rules,
the type environment, representations, and supported features. One or more symbol
sources supply that environment; one or more compatible code generators implement
it. Sources need not use metadata files or CLI assemblies. Changing contract can
require rebuilding imported symbols and rebinding; unsupported features or
incompatible source/backend combinations require diagnostics. These selection
and validation APIs remain future work.

### Existing resolver compatibility rules

The .NET resolver keeps the first supplied path for each full assembly identity,
then sorts the remaining paths case-insensitively. Resolution prefers an exact
identity match and otherwise uses the first simple-name match in that sorted
set. This fallback is not strict version admission. Invalid, missing, or
non-managed candidates are skipped while constructing the resolver; this does
not guarantee that directly importing those references will succeed.

Session path loading first reads the requested file. If it fails and a fallback
identity was supplied, the session attempts identity resolution. Without that
identity, missing-file and invalid-image failures propagate. The resolver itself
does not discover host assemblies outside its supplied paths; host-assisted
discovery is performed by the .NET loader through the target host service before
constructing the session.

`DotNetMetadataResolutionTests` characterizes these compatibility rules using
isolated CLI fixtures. Future strict target admission needs a separate contract
and diagnostics rather than an incidental change to these fallback rules.

## Configuration

`CompilationOptions.WithMetadataImportOptions(new MetadataImportOptions("System.Runtime"))`
selects a metadata core assembly and imports dependencies only from the compilation's
supplied PE references. Include the core and all required transitive dependencies.
For example, when supplying a .NET reference pack, `System.Runtime` contains the core
metadata definitions. Omitting Console then makes Console unavailable even if it exists
in the compiler host. A missing/unusable core cannot initialize MetadataLoadContext and
is a configuration error; this option does not synthesize a substitute core library.

The default is `null`, preserving host-assisted .NET reference discovery. Set the option
to null to return to that mode. Option-copy methods preserve the policy, and changing it
prevents incremental metadata/semantic state transfer from the prior compilation.

This is a compiler API building block for restricted targets, not a complete target
framework, CLI switch, or sandbox for executing compiler extensions. It does not change
host execution, framework projections, runtime reflection used by the emitter, or
`EmitOptions.TargetCoreLibraryIdentity`. Metadata assembly lookup retains Raven's
existing identity/simple-name matching within the explicit input set; exact identity
admission and full dependency validation belong to a target profile.

.NET's MetadataLoadContext uses a supplied resolver rather than imposing host lookup:
[Microsoft documentation](https://learn.microsoft.com/en-us/dotnet/standard/assembly/inspect-contents-using-metadataloadcontext).
Raven's default seeds that resolver with host assemblies for compatibility. Explicit-only
mode omits this seeding and names the metadata core separately from the host core. This
uses the existing .NET mechanism; it costs callers an explicit complete reference set.
Host-side emitter services remain unchanged so this slice does not promise independent
core-library emission or execution on another runtime.

Compiler API consumers must rebuild: the optional constructor argument preserves
existing source calls, but changes the constructor's binary signature in this preview.

## Target-only types during emission

When `EmitOptions.TargetCoreLibraryIdentity` is specified, persisted emission can use
named metadata types and closed constructions of metadata types directly. Target-only
reference assemblies need not be loaded as executable assemblies into the compiler
host. Generic member references retain the definition's generic parameters even when
the declaring type is constructed; metadata proxies retain `ref`/`out`/`in` addressing.
Retargeted constructors use the same temporary-token approach as methods: the final
PE references the target constructor and retains its definition signature. Temporary
constructor types are removed before writing the artifact. This also avoids asking
Reflection.Emit to encode modified generic types from MetadataLoadContext directly.
Default emission and the separate metadata-import policy remain unchanged.

This does not claim complete cross-target emission: mixed source/metadata generic
constructions, generic methods, and the entire framework surface need further coverage.
A successful emit is not runtime validation. Consumers should check dependency closure
and execute against their actual target. No language syntax or editor API changed.

## Project-backed editor imports

`RavenMetadataCoreAssemblyName` opts an MSBuild Raven project into the same explicit-only
metadata policy. Supply the named core and its dependencies as project references:

```xml
<PropertyGroup>
  <RavenMetadataCoreAssemblyName>Target.Core</RavenMetadataCoreAssemblyName>
  <ImplicitImports>disable</ImplicitImports>
  <RavenFrameworkProjections>None</RavenFrameworkProjections>
</PropertyGroup>
<ItemGroup>
  <Reference Include="Target.Core"><HintPath>refs/Target.Core.dll</HintPath></Reference>
</ItemGroup>
```

The project evaluator disables automatic host framework references when this property
is set, even if RavenUseHostFrameworkReferences is true. Explicit reference/package
items still belong to the project's supplied input set; callers must supply the correct
artifacts. The language server also stops automatically adding host Raven.Core,
Raven.Macros and their support references. Missing target references remain missing;
there is no silent fallback to the host library. Leaving the property unset preserves
existing project behavior.

This carries the existing compiler policy across project loading and the language
server. It is not an SDK target, a retargeted emit setting, or a new build pipeline.
In particular, naming a neoCLR core does not make ordinary `dotnet build` execute on
neoCLR. A target-specific emitter/importer is still required. The configured project
provides target-aware completion using the same semantic APIs as other Raven projects.

Explicit metadata-core projects also omit the automatically generated .NET
TargetFrameworkAttribute source. Their host/tooling TFM does not establish a guest
framework identity or guarantee that System.Runtime.Versioning exists. Target authors
may supply their own assembly attributes when supported by their reference surface.

## Project-owned emission core

`CompilationOptions.WithTargetCoreAssemblyName("Target.Core")` selects the emission
core from the assembly already resolved by explicit metadata import. Configure both
from an evaluated project (or an imported target-pack `.props` file):

```xml
<PropertyGroup>
  <RavenMetadataCoreAssemblyName>Target.Core</RavenMetadataCoreAssemblyName>
  <RavenTargetCoreAssemblyName>Target.Core</RavenTargetCoreAssemblyName>
</PropertyGroup>
```

With this opt-in, ordinary `Compilation.Emit` and `rvnc project.rvnproj` use that
core identity without an external runner constructing EmitOptions. RAVT003 rejects
inconsistent import/emission selection and conflicting explicit EmitOptions before
writing the assembly. Missing or unusable core references report RAVT004 when core-session
establishment fails during diagnostic collection or emission. Named-core import-only callers retain the old
behavior when the emission setting is absent. Discovered-core mode, including
`CompilationOptions.DotNet`, selects emission automatically. Legacy default .NET
emission and explicit
`--target-core-library` use without a project selection remain available.

Option copies preserve the selection; incremental semantic reuse accounts for changes.
The existing public CompilationOptions constructor signature is retained. Editor and
compiler configuration diagnostics come from the same compilation rather than an LSP
special case. A new compiler is required to interpret the new project property.

Explicit metadata projects also take precedence over a project-system service's host
framework override. Automatic compiler-support references are omitted; project/package
references remain explicit inputs. The command-line driver does not add host frameworks,
Raven.Core, Raven.Macros or Raven.CodeAnalysis back after project evaluation. It keeps
the project's embedded core-shim and runtime-async defaults instead of deriving them
from the compiler host. Explicit command-line reference and runtime-async selections
remain explicit inputs; this is not a sandbox for compiler plugins.

This closes a CLI/editor inconsistency while retaining .NET's separation between
reference metadata and executable code. It does not implement a complete target-pack
schema, binary loading in another runtime, or removal of every downstream adapter.
The neoCLR experiment independently verifies emitted dependency closure and execution;
Raven retains normal metadata/CIL emission rather than a neoCLR-specific backend.

## Direct neoCLR metadata importing: next integration work (2026-10-02)

Author direction now prioritizes native semantic importing over expanding CLI reference
translation. This follows neoCLR's existing metadata-library architecture: optional
builders over definitions, definitions encoded as metadata and packaged in PE, with
readers reversing those boundaries. The independent library remains the common model
for authoring, reading and eventual Introspection. Raven owns semantic symbols and
binding; it must not parse native JSON/CBOR records or introduce a parallel metadata model.

The detailed sequence is recorded in neoCLR's docs/design/extended-cli-metadata.md,
“Direct native semantic import: implementation alignment (2026-10-02)”. This is an
application of the existing direction and ADR-0003, not a replacement architecture.

The audit found an existing ISemanticDataLoader/IImportedAssemblySymbol boundary, but
Compilation still owns DotNetCompilationTarget; target setup opens a .NET metadata
session, and PE symbols require ReflectionTypeLoader/reflection objects. A native
loader therefore also needs target composition and core-selection work. Do not route
a native reference through CreateReferenceAssembly or MetadataLoadContext to claim
completion. Preserve current .NET loader behavior while making native selection explicit.

First complete native declaration materialization in the metadata library's existing
definition model, including exact identity/scopes and namespace functions. Then add a
bounded native-reference semantic test through Compilation/SemanticModel: lookup,
GetSymbolInfo/GetTypeInfo, accessibility, overloads and unsupported/missing references.
An explicit CLI core can temporarily supply primitive symbols for that first test,
provided the native dependency itself is never projected or reflection-loaded. Record
that limitation; native System/core import is a later acceptance step.

Retain original native definition identity into codegen and prove a Raven consumer calls
the library and executes on neoCLR (42). Expand nominal members, generic substitution,
interfaces and structural Function signatures through the same definitions, then resolve
core/iteration/propagation contracts from native System. Symbols belong to each compilation;
reuse only compatible immutable reader data. Never unify distinct source and imported
assemblies by name to bypass the currently observed bootstrap identity failure.

No native importer, new Runtime Contract setting or semantic behavior is implemented by
this audit. Existing source collection and translated-library execution evidence remains
valid; broad source bootstrap work follows the native import foundation.

Dependency resolution is not reflection emulation. The metadata library already exposes
IAssemblyResolver (exact identity to AssemblyDefinition) and rechecks references against
the returned identity. Native materialization should use that same contract. A compiler
import session supplies explicit sources, caches immutable definitions and creates its
own symbols; discovery/probing remains caller policy. Publish declaration identities
before resolving dependent signatures to support legal assembly cycles without duplicate
symbols or authoritative queries observing incomplete members. Diagnose missing,
conflicting, mismatched and unsupported dependencies distinctly. Exact registered inputs
are sufficient initially; host Assembly.Load, Type and MemberInfo are not prerequisites.
Native diamond/cycle and snapshot-isolation tests remain part of the planned integration.

### Reader foundation available (2026-10-02)

The independent metadata library now exposes AssemblyDefinition.ReadNativeAssembly
for native PE/#Neo containing primitive nongeneric namespace functions. It returns the
existing definitions with exact assembly references, native namespace ownership and
entry identity. MethodDefinition.TryGetSignature reads logical signatures without a
CLI conversion; IAssemblyResolver accepts native snapshots and checks exact scopes.
The native manifest has no MVID, so snapshot/definition ownership must participate in
compiler identity rather than relying on Guid.Empty. Bodies are opaque; loaded editing
and broader nominal/generic signatures are not admitted by this first profile.

96 C# metadata groups validate this foundation, including ordinary CLI compatibility.
Raven's semantic loader is not yet connected. Next support a native function dependency
in the compiler and preserve its identity into native emission, while retaining existing
.NET load/emit behavior. No new Runtime Contract configuration is introduced here.

The author has deferred evaluating Cecil as a replacement for .NET reflection until
the neoCLR target is implemented. Current work remains support for both targets'
assembly loading and emission, not recreating reflection or replacing the .NET backend.

### First native-reference semantic provider (2026-10-02)

NeoClrMetadataReference.ReadAssembly(ReadOnlySpan<byte>) in the independent adapter
project now accepts the metadata library's bounded native-function PE profile. Definition
exposes its owned immutable AssemblyDefinition; input is neither converted to CLI nor
loaded through reflection. The compiler's internal semantic-reference provider boundary
composes with the existing .NET loader. Only the neoCLR target admits this reference.
An explicitly registered CLI primitive core remains required by the current target
setup, so this is not a standalone native-core importer.

Native assembly/module/namespace/method/parameter symbols belong to each compilation.
Lookup and GetSymbolInfo/GetTypeInfo consume them directly, including overload and
accessibility checks, without inventing a declaring type for namespace functions.
Imported parameters have implicit ordinal display names because this reader profile
does not retain declared parameter names; only positional calls are supported.
Native no-result signatures map to the selected language Unit type.

References use snapshot identity for input equality; exact artifact identity governs
dependency matching and native assembly-symbol equality. The import configuration
requires one explicitly supplied native reference per required full identity. Missing
or mismatched dependencies, duplicate identities and wrong target selection report
RAVT003 before loading. No package/filesystem probing or version roll-forward is added.
Dependency symbols are resolved through the compilation after declaration publication;
the first consumer tests direct dependencies, not full cyclic-graph qualification.

Compilation.Emit's default CLI emitter rejects these references before output. The
native emitter currently reports its missing native callable adapter (NEOMETA001),
also without output. Next connect original native definition identity to metadata call
imports and execute a cross-assembly consumer. Generic/nominal/structural import and
native System/core loading remain subsequent work.

The --native-symbols C# probe validates both reference orders, native namespace overloads,
semantic type and repeated lookup identity, compilation isolation, accessibility, invalid
arguments, missing/version-mismatched dependencies, distinct assembly versions, duplicate
inputs and output-free emission rejection. All 67 focused .NET target and symbol-equality tests pass as separate
regression evidence. Existing public MetadataReference file/image factories still mean
CLI references; this native entry point is explicit and its experimental API may evolve.

### Direct native function emission (2026-10-02)

NativeMethodSymbol retains its metadata definition, so native call emission imports
that exact declaration through AssemblyBuilder.ImportReference rather than searching
CLI types or projecting the library. Supply a NeoClrMetadataDependency whose Reference
is the registered NeoClrMetadataReference and whose Definition is that reference's
Definition, with the matching explicit core identity. A replacement snapshot or a
translated implementation binding is rejected with NEOMETA002 before output is written.
This is the current host configuration contract, not automatic dependency discovery.

The --native-symbols-runtime C# probe now covers API-authored and Raven-authored native
libraries, direct native symbol loading, consumer emission and actual runtime calls
returning 42. No native dependency is loaded through .NET reflection. The primitive
core still uses the explicit CLI bootstrap. Nominal/generic native declarations, full
native System loading and dependency probing remain pending; ordinary .NET behavior
and its reflection provider are unchanged.

### Direct native static types (2026-10-02)

NeoClrMetadataReference also accepts fieldless nongeneric top-level static classes
with primitive static methods. The metadata reader supplies canonical TypeDefinition
and MethodDefinition ownership. Raven's native provider now exposes named-type lookup,
static/abstract/closed classification, public/internal types and public/internal/private
members. Method calls retain their exact native definition and use the existing native
import/emission path. Ordinary .NET loading remains unchanged.

The C# native-symbol runtime probe compiles NativeTypeLibrary from Raven source, reads
its native metadata directly, binds Boolean and Int32 overloads, emits NativeTypeConsumer
and executes it in neoCLR. Both reference orders and access/signature failures are
checked. Explicit CLI primitive core and exact native emission bindings remain required.
Instance types, nominal signatures, fields/properties, nested types and generics are
still rejected by this reader profile; no unsupported members are silently omitted.

An ordinary .NET metadata regression also exposed a shared identifier-expression
accessibility gap for public static methods on internal types. Its binder fix and test
are isolated from the native provider changes for independent integration.

Validation: all three native consumers return 42; 96 metadata contract groups and
39 .NET accessibility tests pass. The independent binder fix is e3afed13c on the
integration branch, not merged to main by this slice.

### Direct native instance construction (2026-10-02)

The native reader/provider now also admits fieldless nongeneric top-level instance
classes with primitive method signatures and constructors. Type flags, receiver
presence and constructor attributes/kind are preserved in the shared definitions and
compiler symbols. No constructors are invented. Public constructor/instance imports
reuse the existing metadata references, NewObject and Call emission paths. A Raven
consumer constructs Calculator, stores its reference in locals and invokes Add(20, 22)
through an alias; the native runtime returns 42. Private constructors/methods produce
RAV0500. Existing namespace-function and static-overload controls remain in the probe.

The CLI primitive core bootstrap and exact explicit dependency bindings remain required.
Fields/properties, value/interface/nested/generic declarations and nominal signatures
are still outside this direct-reader profile. An additional `alias != calculator` /
`Calculator() == calculator` consumer reached NEOMETA001 (BoundBinaryExpression): the
portable lowerer currently admits primitive comparisons, not these reference comparisons.
That exploratory case is recorded as a separate lowering gap, not executable evidence.
No new shared binder change or runtime-format change is part of this slice.

### Native primitive field symbols and stateful consumers (2026-10-02)

NativeNamedTypeSymbol now owns field symbols from the shared FieldDefinition graph,
including primitive type, access, readonly flag and exact containing type/assembly.
The reader retains native field origin tokens and answers TryGetPrimitiveType directly;
it does not translate the dependency into CLI metadata. Core primitive resolution stays
lazy so symbol publication does not recursively load the bootstrap.

NativeTypeLibrary's Calculator stores its constructor argument in a private Int32 field.
The consumer constructs Calculator(20), stores an alias and invokes Add(22), which reads
the stored value and returns 42 in neoCLR. Public field binding and RAV0500 for private
field access are tested too. Direct public field reads across assemblies bind, but
emission reports NEOMETA001 with empty output: external field operands still need an
adapter. This limit is tested and is not worked around with a CLI projection.

The explicit CLI primitive core and exact native dependency binding are unchanged.
Nominal/vector/structural field signatures, properties and wider type categories remain
outside this direct-reader slice. Shared .NET loading/emission paths are unchanged.

### Direct native field operands (2026-10-02)

The earlier external-field emission gap is closed for public primitive instance fields
on public nongeneric top-level reference classes. The emitter resolves NativeFieldSymbol's
exact definition through the explicit dependency snapshot, then uses an immutable
ImportedFieldReference for Ldfld/Stfld. Existing owned/constructed field paths remain.
Readonly stores are rejected; source accessibility checks continue to reject private
members. Native operands use the validated declaring field ordinal, including preceding
private fields, rather than looking up fields by name at runtime.

The class consumer writes Visible through a local alias, reads it through the original
reference and passes it to Add, returning 42. A separate NativeFieldConsumer constructs
Calculator(42) and directly reads Visible (42). The explicit CLI primitive core and
matching native dependency bindings remain required. Nominal field signatures, generic/
value owners, translated layouts and reference-comparison lowering remain separate gaps.
The library also writes ordinary CLI MemberRefs for its imported-field API; C# tests
execute that encoding on the CLR. Raven's existing .NET backend remains unchanged.

### Native local nominal signatures (2026-10-02)

The opt-in native provider now resolves SignatureType.ReferencedType through the loaded
module's definition-keyed symbol map. Parameter and result symbols are lazy and cached,
so signatures may reference classes declared later without exposing a partially built
module or recursively loading the primitive core. Factory results, namespace/static/
instance method arguments/results and constructor arguments preserve canonical type
identity. The emitter imports these definitions through the independent metadata API;
no translated CLI dependency is introduced. Ordinary .NET providers are unchanged.

NeoClrMetadataProbe validates both reference orders, semantic identity, bad arguments,
native emission and four runtime consumers returning 42. The source native type library
uses a factory, class identity calls and a constructor receiving another class. The
explicit CLI primitive core and exact dependency bindings remain the bootstrap contract.
Only local nongeneric root class signatures are newly admitted; nominal fields, external
signature dependencies, value/interface/generic types and full native System import
remain pending. This changes neither language syntax nor Runtime Contract configuration.

### Native local nominal fields (2026-10-02)

NativeFieldSymbol uses the metadata library's FieldDefinition.TryGetSignature and lazily
resolves local nominal references through the module's canonical type map. This preserves
forward/cyclic field declarations without querying an incompletely published module.
The existing emitter imports the exact field snapshot, now with nominal storage types;
no shared binder, .NET provider or Runtime Contract configuration changes are required.

The C# probe's native library includes a Calculator-valued field. Its consumer replaces
the value, mutates the replacement, checks the original is unchanged and returns 42 in
neoCLR. Wrong field assignments diagnose. Both reference orders and canonical type
identity are tested. All four native consumers pass; CLI primitive core/translated
System bootstrap inputs remain explicit. External/generic/value/interface/array field
signatures and full native System loading remain pending.

### Explicit external native signature resolution (2026-10-02)

Native references now carry assembly-scoped TypeReferences in method/constructor and
field signatures. Validate checks the complete explicitly supplied native reference set
for duplicate identities, missing/wrong versions and missing exported types, reporting
RAVT003. Lazy signature resolution uses IAssemblyResolver over immutable snapshots and
then the referenced compilation-owned module's canonical symbol map. It never loads
runtime reflection types or probes files. Emission uses an analogous resolver over exact
validated metadata bindings; Runtime Contract configuration is unchanged.

The new C# ExternalNativeChecks probe compiles PayloadLibrary, HolderLibrary and a
consumer with references in both orders. Method/constructor/field types must be the same
Payload symbol. Invalid dependencies diagnose even when source does not use the member.
The five-consumer runtime harness supplies both libraries and verifies 42 from the
three-assembly consumer. Ordinary .NET provider/import behavior is unchanged. Full native
System loading and generic/value/interface/array signatures remain outside this profile.

### Direct native array signatures (2026-10-02)

NativeModuleSymbol shares signature mapping for methods and fields, with a concurrent
module-local cache for primitive, nominal and one-dimensional array symbols. Resolution
remains lazy until module publication; external array elements resolve through explicit
native dependencies. Fields and methods with the same signature in a module share array
symbols, and their element is the canonical symbol from the declaring assembly.

The existing emitter imports array method/field contracts through the independent metadata
library's recursive signature mapping. No new syntax, binder policy, .NET provider or
Runtime Contract setting is introduced. The C# probe checks array symbol identity in both
reference orders and executes cross-library array aliasing/element replacement in neoCLR
(42); all five runtime consumers pass. Jagged/multidimensional/generic/value/interface
array profiles and full native System importing remain pending.

### Imported setter accessibility (2026-10-02)

The shared binder requires an accessible ordinary setter for property assignment,
compound assignment, pipeline writes and increments. IsMutable alone cannot grant access to a private
setter on an imported property. Existing constructor auto-property initialization and
field-only storage rules remain separate. This is a general .NET/compiler correction,
reproduced with a C# metadata fixture independently of the native provider; it changes
diagnostics for formerly accepted invalid writes, not metadata encoding or Runtime
Contract configuration.

Validation: the C# fixture reproduced four missing diagnostics before the fix;
76 focused property/accessibility tests pass afterward, including pipeline writes.
This correction is isolated for independent shared-line integration.

### Direct native non-indexed properties (2026-10-02)

NativePropertySymbol consumes PropertyDefinition.TryGetSignature and shares the lazy
module signature map with fields/methods. Properties reuse the exact accessor symbols
in the declaring type's member list, setting AssociatedSymbol and PropertyGet/PropertySet
MethodKind before publication. Property visibility follows the most visible accessor;
each accessor retains its own visibility. No reflection/CLI projection is used.

The emitter uses the target's signature capabilities for source properties and imports
canonical getter/setter method operands. Shared lowering admits static properties on
external reference owners only when that capability is enabled; default .NET capabilities
are unchanged. The separately committed setter-accessibility fix (23161cffb) applies
to both providers and has independent .NET tests. No Runtime Contract change is needed.

C# semantic checks cover both reference orders, accessor identity, nominal/vector types,
static/readonly/private setters and invalid assignments. All five runtime consumers
execute (42), including source native libraries with class/array properties. The CLI
primitive core and translated System remain bootstrap inputs; indexed properties,
generic/value/interface owners and full native System importing remain pending.

### Imported indexer accessibility (2026-10-02)

Indexer candidate selection now checks property and relevant accessor accessibility.
Assignment fallback to a readable indexer still requires an accessible setter or the
existing writable-byref contract. This shared binder correction is independently
reproduced with C#/.NET private indexer setters for simple and compound assignment;
public overloads remain usable. No Runtime Contract or metadata format change is needed.

### Shared emission name normalization (2026-10-02)

SourceTypePlan splits the normalized fully qualified metadata name into namespace
and local name instead of subtracting the raw MetadataName length. Synthesized
owners may expose an already qualified MetadataName, which previously caused a
negative substring length before emission. The existing .NET self-override/indexer
regression now passes. This general correction is isolated from native loading;
75 focused indexer/accessibility tests pass with both corrections applied.

### Direct native indexed properties (2026-10-02)

NativePropertySymbol uses the full logical property-signature overload and exposes
IsIndexer plus a lazily cached parameter list from its canonical accessor symbols.
Setter-only metadata excludes the setter value from that list, though source indexer
resolution still requires a getter. Existing overload binding and accessor-call
emission handle supported primitive/nominal/vector contracts. Source indexer value
admission now uses explicit NeoCLR capabilities; no Runtime Contract change is needed.

The native payload/holder/consumer test verifies Int32/String overloads, canonical
external value types, indexed replacement/read, private setters and wrong index types
in both reference orders. All five native consumers return 42; 102 metadata groups
and 75 focused .NET indexer/accessibility tests pass. General binder/name-normalization
corrections are isolated in 9ee5aad97 and 2df6f3f6d for independent integration.
Native core loading and broader owner categories remain pending.
