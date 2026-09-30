# Experimental native neoCLR emission adapter

Development API on `codex/metadata-consumer`, in the optional .NET 10 project
`src/Raven.CodeAnalysis.NeoClr`. The metadata implementation remains the separate
`NeoCLR.Metadata.Experimental` project; it does not depend on Raven. The adapter
requires an explicit `NeoClrMetadataProject` build property and is not part of the
default solution, compiler driver, or `Compilation.Emit` target composition.

## Public API

All types below are in `Raven.CodeAnalysis.NeoClr`:

```csharp
public static class NeoClrCompilationEmitter
{
    public static NeoClrEmitResult Emit(
        Compilation compilation, Stream output, NeoClrEmitOptions options);
    public static NeoClrEmitResult EmitMetadataAssembly(
        Compilation compilation, Stream output, NeoClrEmitOptions options);
}
public sealed class NeoClrEmitResult
{
    public bool Success { get; }
    public ImmutableArray<Diagnostic> Diagnostics { get; }
}
public sealed class NeoClrEmitOptions
{
    public NeoClrEmitOptions(AssemblyIdentity identity, AssemblyIdentity coreLibrary,
        IEnumerable<NeoClrMetadataDependency> dependencies);
    public AssemblyIdentity Identity { get; }
    public AssemblyIdentity CoreLibrary { get; }
    public ImmutableArray<NeoClrMetadataDependency> Dependencies { get; }
}
public sealed class NeoClrMetadataDependency
{
    public NeoClrMetadataDependency(MetadataReference reference,
        AssemblyDefinition definition, AssemblyIdentity coreLibrary);
    public MetadataReference Reference { get; }
    public AssemblyDefinition Definition { get; }
    public AssemblyIdentity CoreLibrary { get; }
}
```

`AssemblyIdentity` and `AssemblyDefinition` are from
`NeoCLR.Metadata.Experimental.Model`. Constructors reject null arguments; options
copy the dependency sequence and reject null entries. All configuration properties
are read-only. No constructor loads a runtime assembly or invokes the compiler.

`Identity` supplies the native output name/version/culture; it must be unsigned,
unflagged, and have the compilation's assembly name. `CoreLibrary` is an explicit
host assertion of the primitive core contract, and each dependency must assert the
same contract. No host identity is silently inferred by this library.

Each dependency maps the **exact MetadataReference instance** registered with the
compilation to a read-only metadata snapshot. Calls match the resolved assembly
symbol using `SymbolEqualityComparer`, not just a simple assembly name. Snapshot
names must match, and duplicate assembly symbols/metadata identities are rejected.
The host is responsible for keeping the snapshot and reference file consistent
(including version, signature and contents) and supplying matching native dependency
artifacts. Raven's public assembly symbol currently has no complete identity/MVID
contract: these checks do not authenticate snapshots or replace a native loader.

At most 256 dependencies are accepted. One or more source trees with console or library output,
no macro trees, and `TargetPlatform.DotNet` as the primitive binding bootstrap is
supported. This does not enable the separate neoCLR CLI bridge Runtime Contract.

## Result, diagnostics and streams

Semantic errors retain their original Raven diagnostics and prevent native emission.
Warnings are preserved in the result. A successful result writes all validated native
format-5 bytes at the stream's current position. The stream is never closed, rewound
or truncated. Source/configuration/metadata validation completes before the first
write, so validation failure preserves both bytes and position, including on an
existing output stream. Host I/O failures propagate and can leave partial output;
there is no filesystem transaction or rollback guarantee.

| ID | Meaning | Location |
| --- | --- | --- |
| NEOMETA001 | Unsupported source operation, signature, declaration or dependency call | Relevant source syntax |
| NEOMETA002 | Unsupported/mismatched output or dependency configuration | None |
| NEOMETA003 | Metadata writer rejects the bounded graph or a resource limit | None |

Only one backend diagnostic is reported per attempt. Null/unwritable stream arguments
throw normal argument exceptions. Unexpected compiler failures and stream I/O errors
are not hidden as source diagnostics. No cancellation API is offered in this slice.

## Supported source and executable example

Declarations from every source tree are collected before any body is emitted. Each
body uses its own semantic model, so cross-file calls do not depend on source-tree
order. Declaration/token order follows the compilation tree order; byte-for-byte
equality across reordered files is not promised.

The existing primitive subset remains: top-level block-bodied functions with required
Int32 value parameters/results, returns, Int32 constants, parameter loads, local/static
calls, and intrinsic unchecked/unlifted addition, subtraction and multiplication.
Public nongeneric static classes in the global namespace may contain public static
block-bodied Int32 methods. Raven's default public method accessibility is accepted;
explicit public is optional. Library output omits the entry point. Nonpublic library
functions/methods/types are rejected rather than widened to the writer's public-only
metadata contract. Top-level console functions remain supported; library exports in
this slice use static classes. Namespace declarations, inheritance, primary
constructors, nested types and additional class contracts remain unsupported.

Fields, instance calls, generics, structural types, arbitrary statements, attributes,
async, captures, checked/lifted operators and named/default/expanded arguments remain
unsupported. The metadata writer also bounds methods, parameters, bodies and artifacts.

The C# integration runner compiles a Raven library to native metadata, derives its
reference-only projection, and registers the
same reference object used to create its compilation:

```csharp
var options = new NeoClrEmitOptions(outputIdentity, coreIdentity,
    [new NeoClrMetadataDependency(reference, snapshot, coreIdentity)]);
using var output = new MemoryStream();
var result = NeoClrCompilationEmitter.Emit(compilation, output, options);
if (!result.Success)
    throw new InvalidOperationException(string.Join("\n", result.Diagnostics));
var nativeBytes = output.ToArray();
```

Run the [probe](../../../tools/NeoClrMetadataProbe/README.md) for the complete example.
It validates source spans, unchanged failed output, binding diagnostic identities,
configuration rejection, writer limits, repeatability and host stream failures, then
asks neoCLR to verify and run the emitted application with result 42. A two-file
version runs in both source-tree orders, and a rejected expression in a later file
retains its actual tree/path/span without writing output. Its validation
report records compiler, adapter, metadata library, runtime and artifact hashes.

## Architecture and next boundary

This uses the existing Raven `EmitResult` success/diagnostic pattern, but a separate
result type avoids opening the core compiler's internal emitter composition solely
for an experiment. Like the .NET emitter, the adapter consumes compiler semantic
facts; unlike Reflection.Emit, it writes native metadata without executable reflection
handles. Costs are a bounded source subset, explicit host reference contracts and the
.NET input-provider bootstrap. Neither a native semantic loader nor production target
registration is implemented. The next slice should drive those seams with an actual
metadata input case, retaining shared compiler fixes on the shared line.

## Native input bridge

The probe now builds its `NeoClrMetadataDependency` from a native metadata snapshot:
call `NativeAssemblyDefinition.ReadAssembly(nativeBytes)`, then
`CreateReferenceAssembly(explicitCoreIdentity)`, register those PE bytes with the
existing semantic provider, and read that same projection with `AssemblyDefinition`
for emission imports. The host still owns matching the reference/snapshot pair.
The projection is marked reference-only and contains throwing placeholders, so use
the original native artifact at runtime. Native bodies and transitive implementation
dependencies are not projected. See the bridge document for limits and the intended
replacement by a native semantic provider; the adapter public API is unchanged.

The transitive consumer demonstrates separate compile-time and runtime dependency
sets: primitive reference projections omit implementation references, while
`NativeAssemblyDefinition.References` retains them for explicit host/runtime resolution.
The host must supply the native transitive closure to neoCLR; the emitter does not
locate files, resolve that closure, or execute dependencies automatically.

## PE/#Neo output

`EmitMetadataAssembly` accepts the same compilation, stream and options as `Emit`.
It validates/emits to a private native buffer, then calls the separate metadata API's
`RuntimeAssemblyContainer.Write` before writing caller output. It returns the same
diagnostic result: compiler/unsupported-input failures are preserved, container
validation failures are NEOMETA003, and failed validation leaves the caller's bytes
and position unchanged. Null/unwritable arguments throw; caller I/O exceptions
propagate and may leave partial output. The stream is never closed.

`Emit` continues returning format-5 JSON. `EmitMetadataAssembly` instead returns an
unsigned PE32 with a reference-only CLI projection and required #Neo execution section
256/schema 1 containing those native bytes. The native payload remains JSON; no
faster-loading claim or binary-native decoder is implied. The entire envelope is
limited to 1 MiB. Projection MVIDs are fresh, so PE byte determinism is not promised.

The host can register a library container directly with `MetadataReference.CreateFromFile`
and pair that exact file with `RuntimeAssemblyContainer.ReadCliProjection` for the
`NeoClrMetadataDependency` snapshot. No temporary reference file is needed. neoCLR
loads the native payload of the same library file through `--module`. CLI declarations
are a compile-time projection; their throwing bodies are never the runtime implementation.
The host must preserve file/snapshot consistency; the binding digest does not prove
semantic equivalence of arbitrary dual metadata views or authenticity.

The two-library probe checks actual PE loading and execution to 42, both module orders,
missing/wrong dependencies, native/PE payload equivalence, failure output preservation
and caller stream ownership/I/O behavior. This opt-in API does not register a native
compiler symbol provider or change Runtime Contract/default .NET composition.
