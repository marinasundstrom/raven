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
core identities report RAVT003 before output is written. Missing-core setup still
throws `FileNotFoundException`; translating loader setup failures into diagnostics
is follow-up work. An empty reference list never gains implicit host references
through this preset.

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
`Compilation` gates incremental session reuse. Reference import passes
through `ISemanticDataLoader`; `DotNetSemanticDataLoader` owns loaded-assembly and
assembly-symbol caches, dependency traversal, and construction of PE assembly/module
symbols for each compilation. Compilation-to-compilation references remain in
the shared compilation layer. The .NET loader explicitly registers selected paths
through compilation host services before creating a new session. Metadata
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
discovery is performed by `Compilation` before constructing the session.

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
writing the assembly. Missing or unusable core references still follow the existing
metadata-loader configuration failure path. Named-core import-only callers retain the old
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
