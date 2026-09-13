# Explicit-only metadata import

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
metadata-loader configuration failure path. Import-only callers retain the old
behavior when the emission setting is absent; default .NET emission and explicit
`--target-core-library` use without a project selection remain available.

Option copies preserve the selection; incremental semantic reuse accounts for changes.
The new optional constructor parameter preserves source calls; compiler API consumers must rebuild. Editor and
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
Target integrations must independently verify emitted dependency closure and execution.
Raven retains normal metadata/CIL emission.
