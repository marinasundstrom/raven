# Experimental native neoCLR emission adapter

Development API on `codex/metadata-consumer`, in the optional .NET 10 project
`src/Raven.CodeAnalysis.NeoClr`. The metadata implementation remains the separate
`NeoCLR.Metadata.Experimental` project; it does not depend on Raven. The adapter
requires an explicit `NeoClrMetadataProject` build property and is not part of the
default solution. Hosts select it explicitly through `EmitOptions.WithBackend`; the
opt-in compiler driver exposes the `neoclr` command. Ordinary .NET remains the default.

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
        IEnumerable<NeoClrMetadataDependency> dependencies, MetadataReference? consoleReference = null);
    public AssemblyIdentity Identity { get; }
    public AssemblyIdentity CoreLibrary { get; }
    public ImmutableArray<NeoClrMetadataDependency> Dependencies { get; }
    public MetadataReference? ConsoleReference { get; }
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

The bounded subset supports Int32/Int64/Boolean/String value parameters and results,
plus Unit/no-result returns. Global-namespace assembly functions preserve public or
internal access; unmodified source functions use their bound accessibility. Public/
internal nongeneric static classes may contain public/internal/private static methods,
including partial declarations and namespaces. Blocks and arrow bodies share lowering.
Initialized locals, assignments, primitive arithmetic/bitwise operations, supported
conversions, calls, conditional values and lowered loops use shared emission contracts.
Value blocks permit internal control flow but reject returns/outgoing jumps and disposal.
Library output omits the entry point; internal functions can be explicit console entries.

Fields, instance calls, generics, structural types, attributes, async, captures,
checked/lifted operators and optional/expanded arguments remain outside the bounded
producer. Namespace-scoped native functions require a future namespace contract.
Private ownerless functions are not admitted. The metadata writer also bounds methods,
parameters, bodies and artifacts. Public class facades can expose library behavior;
direct source import of projected global functions remains outside this integration.

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
`RuntimeAssemblyContainer.WriteBinary` before writing caller output. It returns the same
diagnostic result: compiler/unsupported-input failures are preserved, container
validation failures are NEOMETA003, and failed validation leaves the caller's bytes
and position unchanged. Null/unwritable arguments throw; caller I/O exceptions
propagate and may leave partial output. The stream is never closed.

`Emit` continues returning format-5 JSON. `EmitMetadataAssembly` instead returns an
unsigned PE32 with a reference-only CLI projection and required #Neo execution section
256/schema 2 containing a bounded CBOR encoding of the native module. Runtime
loading now avoids JSON parsing; schema 1 remains readable by the matching runtime.
Compiler-host emission still uses JSON as an intermediate. The entire envelope is
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

## Initial console contract

`NeoClrEmitOptions.ConsoleReference` optionally authorizes a bounded mapping from the
exact registered compiler assembly reference to native System.Console. Null disables
console emission. An unregistered reference produces NEOMETA002. Matching uses the
assembly symbol, fully qualified type name and resolved method signature, not source
spelling. Only static `System.Console.WriteLine(string)` expression statements with
one unnamed, non-null string literal are supported. Imported nullable string
annotations are unwrapped through the public nullability API; the argument still must
be a non-null literal. Other overloads, Write, unregistered/different assembly symbols
and unsupported argument shapes produce NEOMETA001 before any output write.

The native metadata builder's `WriteConsoleLine` emits `ldstr`, the bundled System
WriteLine call and `pop` for its current Void-valued result. This is a temporary
platform contract, not general CLR Console import or string signature support. The
CLI reference projection needs no string signature because the literal exists only
in the native body. Ordinary .NET target behavior remains unchanged.

`tools/NeoClrMetadataProbe --hello-only` checks an Int32-returning Main printing
Hello World directly, then Main returning `Greet()` where Greet prints the line. Both
verify/load/run as PE/#Neo, print exactly one line and exit zero. Entry points and
source helper signatures remain Int32 in this bounded slice.

### Binary payload migration

As of this feature-branch slice, EmitMetadataAssembly emits required execution schema
2. A schema-1-only neoCLR runtime fails closed; use the matching metadata/runtime
checkout. Emit continues returning JSON, and the independent library's Write continues
producing schema-1 PEs when compatibility output is needed. No Runtime Contract
configuration, ordinary .NET behavior, Console mapping or signature scope changes.
The compiler's native semantic provider and production target registration remain open.


The bounded native producer now admits public/internal/private static methods in
public/internal nongeneric static types. Method access survives CLI reference
projection; existing binding diagnostics reject inaccessible calls before emission.
Native verification independently enforces access. This does not extend assembly
function visibility or introduce protected/instance method support.

Block and expression bodies are accepted for supported functions and static methods.
Both consume compiler-lowered statements, preserving result conversions and Unit
calls. Unsupported expressions retain source-located diagnostics before writing.

Primitive value-producing if/else uses matching Int32/Int64/Boolean/String joins.
Value blocks may initialize locals, assign local values and call supported methods
before a trailing primitive expression. Internal if/loop control flow is supported. Disposal, returns and jumps outside
value blocks are rejected before writing; statement-form loops remain supported.

## Namespaced functions and real Math source — 2026-10-01

Raven now admits block/file namespace functions through a distinct shared target
capability, preserving the full semantic namespace and simple name. Both bounded
backend profiles opt in; ordinary .NET remains the default. Native functions retain
no type owner. No Runtime Contract setting changes. The native adapter requires the
independent metadata/runtime namespace slice `e8611966`; its CLI reference projection
uses reversible encoded global names. Direct source import of these projected globals
is still deferred, as is the general native metadata importer.

`NeoClrMetadataProbe --class-library-runtime <runtime/raven/src> <fresh-output> <neoclr>`
selects the original integer Min/Max/Sign declarations and their System.Math namespace.
It excludes unrelated declarations/imports without rewriting function signatures or
bodies. The selected library now emits successfully. A separate Main source exercises
11 boundary cases (Int32 endpoints, equality and all Sign branches), in both file
orders, through ordinary CLI execution and binary native verification/execution to 42.
The probe also asserts exact native System.Math namespace and null owners; it writes
source hashes, selected source and runtime hash. This uses host core primitives and
is not a full System build. Whole Math/UnicodeScalar/GC files still stop in binding
on absent native library dependencies. The order-collections consumer is the next
acceptance expansion; its constructors/properties, generics and delegates exceed the
current static primitive producer.
