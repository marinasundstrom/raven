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

At most 256 dependencies are accepted. One or more source trees with console or library
output and no macro trees are supported. The existing `TargetPlatform.DotNet` host
bootstrap remains available. `CompilationOptions.NeoCLR` can now use its explicit
`NeoCLR.CoreProbe` CLI declaration snapshot with the direct native backend. The adapter
validates the profile and requires Object, Int32, Int64, Boolean, String and Unit to
belong to one imported core matching `CoreLibrary` (name, version, culture and token).
Core mismatches produce NEOMETA002 before output is written. For this profile, pass
the snapshot reference as `ConsoleReference` to authorize supported WriteLine calls.

This is an explicit temporary CLI declaration binding contract, not native symbol
loading or an implementation-bootstrap seed. The runtime uses native primitive and
no-result representations; it does not execute the declaration snapshot's CLI bodies.
Iteration/propagation/typeof/Self contracts still bind through the selected profile;
unsupported bodies and declarations remain rejected by the backend capabilities.

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


### Definition-first metadata dependency checkpoint (2026-10-01)

The separate metadata library now supports authored assembly/type/field definitions,
shared method declarations, and direct assembly-level function construction with
EntryPoint assignment. Its builder APIs wrap those declarations; Raven's adapter
continues to use that compatibility facade. Body definitions and loaded editing are
still pending. This changes neither Runtime Contract configuration nor Raven binding
semantics, target admission or ordinary .NET emission.

Rebuilt against neoCLR `codex/extended-cli-metadata` (type/field slice `fb381118`,
method declaration slice `09166c9a`, followed by direct function construction), the
external-signature runtime probe verifies and returns 42. The generic-library probe
also passed against the initial definition slice. Compiler source was `55a29312f` on
`codex/metadata-consumer`; runtime SHA256 was
`193B7F995EE4FC92A086439FB1FDBDAFC3A0C58139F0CAF30CB4A2D8640A432D`.
The unchanged collections sample's Option<Order> import/union/native identity gaps
remain open. These validations establish facade compatibility, not completion of the
compiler or general metadata editing API.

The subsequent static type-method slice adds direct declarations through append-only
TypeDefinition.Methods, with existing builder methods entering that same collection.
Raven rebuild and external-signature native execution still pass (42); compiler source,
Runtime Contract and encodings are unchanged. Direct instance declarations and bodies
remain pending.

Direct root-class constructor and instance-method authoring also passes the rebuilt
external-signature probe (42). The metadata contract test executes manual object creation,
readonly initialization and an instance call on CLR/neoCLR. This expands authoring only;
Runtime Contract, binding and target import admission remain unchanged.

Direct nongeneric interface types and abstract contract methods now work in the metadata
API and execute through existing interface implementation/CallVirtual helpers (42).
The rebuilt Raven external-signature probe also passes. Relationship definitions and
canonical bodies remain pending; target admission and Runtime Contract are unchanged.

Interface relationships and method-body instruction/local/label storage now belong to
metadata definitions. Existing builders retain compatible handles and use that storage;
Raven required no source changes. The rebuilt external-signature native probe still
returns 42. Arbitrary instruction editing, remaining property/generic migration and loaded
body decoding remain pending; Runtime Contract and compiler admission are unchanged.

Property definitions and accessor associations now share identity with builder facades.
CLR reflection/native execution and the rebuilt Raven probe pass (42). The author clarified
the intended division: Cecil-like definitions with Reflection.Emit-style generation
builders, without a replacement/compatibility promise. Existing builder entry points,
Runtime Contract and target admission are unchanged.

Direct generic type construction and definition-owned parameter names/constraint storage
now pass CLR/native execution (42) and the rebuilt Raven external-signature probe. Existing
constraint helpers and target admission are unchanged. The author clarified the intended
builders → definitions → metadata → PE pipeline, with reverse reader boundaries; separate
encoding/packaging APIs and editable reader materialization are not yet implemented.

## Imported value signatures (development, 2026-10-02)

The neoCLR adapter opts into external value signatures independently from external
reference signatures. Public top-level struct signatures, including bounded generic
constructions, map through registered `NeoClrMetadataDependency` snapshots and the
separate metadata library. Imported static methods on nongeneric owners can return
and accept those values; locals and forwarding retain their category. Missing or
incompatible dependencies reject without output. Ordinary .NET defaults, binding
and Runtime Contract configuration are unchanged.

The temporary CLI projection uses standard VALUETYPE/GENERICINST signatures. Native
format-5 manifests optionally retain imported category through `value_type_references`;
the metadata reader/writer own this annotation and the updated runtime validates it
against loaded definitions. Images using it require that runtime. Existing images
remain supported. This annotation should be replaced by native indexed signature
metadata, not become a language restriction. Imported instance members, generic-owner
member resolution, arbitrary cross-dependency TypeRefs and union body lowering remain
open. The probe `--imported-value-runtime` verifies/runs a Raven consumer of a
metadata-authored library with result 42 and checks missing dependency rejection.
The unchanged collections sample now passes Option<Order> declaration admission and
stops at invocation lowering; it has not executed.

## Imported constructed dispatch (development, 2026-10-02)

Raven now opts into external instance-call admission through a separate backend
capability. Public nonvirtual/final class members and nongeneric abstract interface
members can use unconstrained invariant reference owners with type arguments.
The compiler resolves the open method against an explicit dependency snapshot and
checks receiver category, dispatch kind and full parameter/result signature before
constructing a consumer-scoped reference. Missing dependencies still reject without
writing output. Runtime Contract and ordinary .NET defaults are unchanged.

Compared with CLR, the metadata API uses the same VAR substitution, constructed
TypeSpec/MemberRef shape and callvirt null-checking/dispatch semantics. The existing
native generic interface dispatcher executes the resulting binary assemblies; no
new instruction or alternate runtime lookup convention is needed. The supported
producer subset now includes nongeneric root classes implementing closed generic
interfaces. Definitions own the relationship, builders append it, and native readers
preserve it in the CLI projection. This expands the host library's supported subset;
older experimental readers cannot project these relationships.

`--imported-interface-runtime` in Raven proves constructed interface and final class
calls against a metadata-produced library (42). The C# metadata tests independently
execute CLR/native dispatch (42), reject incorrect owner/arity/opcode contracts and
exercise native null-receiver failure. The unchanged collections sample advances
past MutableMap.TryAdd and ArrayList.Add to BoundPropagateExpression; no complete
collections execution is claimed. Owners are the shared callable/body admission
layer, neoCLR adapter, independent metadata library and existing native dispatcher.

The CLI reference projection remains a temporary symbol-loading bridge. Native
symbol import should eventually preserve these contracts directly. Cross-dependency
TypeRefs, imported constructors/value-instance methods, instance generic methods,
extensible class virtual slots and generic interface inheritance remain outside this
slice. They are implementation limits, not neoCLR language rules.

## Concrete case lowering checkpoint (2026-10-02)

The unchanged collections sample's apparent propagation rejection was caused by a
shared lowering exception in a later `Option<Order>(None())` expression. A concrete
case target must construct the case directly, matching the existing .NET fallback
emitter. Shared lowering now honors that target; binding and Runtime Propagation,
Self and Unit configuration remain unchanged. The fix is independently committed
as `dcc77ef5f` on the main-based `codex/compiler-fixes-from-neoclr` branch. All 25
focused imported-case/propagation tests pass on both branches using .NET 11.

The CLI bridge control still emits 7168 bytes. Native emission now reaches the
synthesized uninitialized `out` local and rejects `only initialized value locals`.
Managed local addresses, byref signatures, imported value receivers and protocol
failure paths must be implemented as explicit shared/backend contracts before this
propagation protocol can execute. Do not replace out parameters with unrelated value
calls or silently initialize them to bypass the capability check. Existing CLR/CIL
byref and address semantics remain the baseline. No new metadata encoding or runtime
support is claimed by this checkpoint, and the complete application has not executed.


### Managed-reference emission checkpoint (2026-10-02)

The portable signature and body plans now preserve writable ref/out parameters,
local addresses, indirect reads/writes and explicit output indices. Both adapters opt
into `AllowsManagedReferences`; absent that capability, the bounded shared path retains
its rejection/fallback behavior. .NET emits ordinary byref signatures and CIL; neoCLR
uses the independent metadata API's BYREF/out contracts and native instructions.
Inline `out var` symbols acquire an uninitialized local slot on first address use;
synthesized declarations may likewise remain uninitialized until an out call assigns
them. No synthetic default values are inserted. Source binding still owns language
assignment diagnostics; the metadata producer independently validates native body flow.

The C# capability/parity tests assert successful shared lowering and observable CLR
mutation in Debug/Release, including Out reflection. NativeProfileRefOut emits and
executes assignment, output forwarding and ref mutation (42) through the normal neoCLR
profile alongside the existing primitive/interface/array/Unit controls. The focused
emission/propagation/byref suite passes 64 tests (24 baseline tests passed before edits).

Runtime Propagation/Self/Unit and target-core configuration are unchanged. The temporary
CLI declaration snapshot still supplies symbols; native importing is pending. Readonly
references, escaping byref values, field/array addresses and imported value receivers
are not admitted by this slice. The unchanged collections sample now passes synthesized
out-local admission and rejects `TryGetOutput(out output: Order) -> bool` instance-call
admission; CLI control still emits 7168 bytes. Full propagation/native System mapping
remains open. No new metadata encoding, runtime opcode or binder change is introduced.

This is a shared emission capability feature, not an independent binding regression
fix. It remains on the metadata-consumer integration line; consider a separate shared-line
port after the bounded API is reviewed, without conflating that with native backend
readiness. The previous independently proven concrete-union lowering fix remains on its
main-based fix branch.


### Imported value-receiver checkpoint (2026-10-02)

`AllowsExternalValueInstanceCalls` opts the native adapter into direct calls on public
nongeneric methods of imported ordinary or constructed value types. Concrete virtual
implementations must be final. Shared lowering takes the address of an owned local or
forwards an existing ref/out receiver; both adapters represent the call explicitly.
The metadata import match requires the managed receiver contract as well as the output
indices and open signature. This follows CLI value-instance calling semantics and
preserves mutation of caller storage. There is no boxing or unconstrained virtual call.

The ordinary .NET shared profile does not enable this new capability; its existing
emission fallback remains available. Runtime Contract Propagation/Self/Unit settings and
binding are unchanged. By-value parameter addresses, temporary/field/array receivers,
source value declarations, value constructors and constrained interface dispatch remain
outside the bounded native profile. These are emitter limitations, not language rules.
The CLI declaration snapshot still supplies symbols; a native metadata importer remains
pending. This shared capability remains a candidate for separate shared-line integration,
not an independently isolated binder fix.

`NeoClrMetadataProbe --value-receiver-runtime` compiles a Raven consumer of a separately
produced native library. Receiver mutation, a generic value-owner setter and generic out
calls verify and run on neoCLR with result 42. Missing dependency registration rejects
without modifying output. All 28 focused external-signature, capability and shared-emission C# tests pass on
.NET 11, including explicit receiver admission and ordinary .NET execution. The unchanged collections sample now passes TryGetOutput admission and
stops at `BoundThrowStatement`; its CLI control still emits 7168 bytes. Propagation
lowering contains an invalid-carrier throw guard, so dropping that guard would change
behavior. Native terminal failure semantics and their shared lowering contract need a
separate slice; full collections execution is not yet supported.

Deferred independent investigation: the initial .NET control using
`if !value.TryGet(out var result) { return 0 }` followed by `return result` reported
RAV0103 after semantic-plan inspection and subsequent ordinary emission. The final
receiver control uses an explicitly declared output local; the native executable still
covers inline output locals. This observation has not been isolated against main and
must not be labeled a regression caused or fixed by this capability. Reduce the query/
emission sequence independently before deciding whether to port a compiler fix.
