# neoCLR CLI bridge and native metadata replacement

Status: temporary bridge, 2026-09-30. The intended destination is a native neoCLR
semantic-data/metadata layer that exposes neoCLR's types, features and semantics
to Raven's shared semantic model, paired with a compatible neoCLR code generator.
CLI encodings below are migration mechanisms, not the specification of that target.
New platform features must not be restricted merely because CLI metadata cannot
express them. Until a supported path exists, diagnose unsupported use explicitly.

## Current pipeline

The runtime's bridge generates `NeoCLR.CoreProbe.dll` as a CLI reference surface.
Raven's .NET metadata session/reflection loader projects it into compilation-owned
symbols. `CompilationOptions.NeoCLR` selects runtime mappings and policies but
still uses the existing CLI emitter. The emitted application DLL then goes through
the external `Probe --import` bridge, producing `App.neoil` and a source/identity
map. The neoCLR verifier and runtime consume that output with a matching System
library. Successful C# reflection or CLI emission alone does not validate this chain.

The reference exporter and application importer are both bridge components.
Replacing only the reference input does not remove the CLI output bridge.
`ISemanticDataLoader` is the current input seam; its `MetadataReference` and
`IAssemblySymbol` surface and the reflection-backed symbol implementation still
need evaluation for native metadata. Their current CLI shape is not a requirement
on neoCLR. Preserve snapshot-owned symbols and incremental correctness throughout.

## Branch and artifact scope

General Raven target plumbing and the existing nominal delegate ABI are on shared
main. Native structural Function work is reserved for Raven
`codex/neoclr-structural-types` and is not enabled on main. Native structural
Function support is on neoCLR's `codex/structural-types` branch. The old native
Self branch inherited that experiment; integration extracts Self onto nominal main
instead of merging its structural ancestry. These are different
repositories and different integration states. Native Function types remain deliberately deferred: the author requires neoCLR's
metadata layer and the remaining compiler support before including the feature.
An exploratory run against that feature bundle neither merges nor enables it and
is not an acceptance gate for the shared target.

## Behavior inventory

This inventory covers the present compiler profile and known bridge mechanisms;
it is not an exhaustive importer opcode/API catalog. “Native requirement” is the
replacement direction, not a claim of an implemented native loader or backend.

| Area | Current CLI bridge behavior | Native requirement / classification |
| --- | --- | --- |
| Core and references | Explicit `NeoCLR.CoreProbe` metadata/emission core. The profile validates the exact core and unit mapping. No host reference injection. | Load the selected neoCLR platform's own semantic data and identities. The probe DLL/name is temporary transport; explicit reference ownership remains required. |
| Unit and no-result calls | Named value-type `System.Void` represents inhabited unit in value/generic positions; CLI VOID denotes no stack result. The importer distinguishes these contexts. | Preserve neoCLR's unit value and separate call-result convention directly. This semantic distinction survives removal of the encoding. |
| Function types | Raven transports function signatures through `Func`/`Action`-shaped CLI types. neoCLR unit functions use an inhabited result shape even for the existing nominal Func ABI; ordinary .NET targets use Action. This encoding does not establish structural identity or assignability. The importer maps supported delegate shapes to native structural functions. | Load structural function signatures, identity and assignability directly; do not make CLR delegate families or their arities the native semantic model. |
| Tuples | The profile selects value-type `System.Tuple` names. Import normalizes those probe types to Raven's existing tuple special-type identifiers, historically named System_ValueTuple. | Expose tuple structure and members from native metadata. CLI family names and the special-type alias are compatibility machinery, not native reference-type semantics. |
| Namespace functions / terminal Fault | CLI containers and a TopLevel marker stand in for namespace functions. Shared lookup consumes INamespaceMemberContainer; the PE provider interprets the attribute. Fault classification checks assembly, namespace, static nongeneric signature, one by-value string parameter and void/unit result. It currently also applies through legacy imported-assembly recognition. | Represent callable ownership and terminal behavior in the native contract. Avoid permanent dependence on CLR container spelling, probe identity or attribute encoding. |
| Iteration and arrays | Contract maps Iterable/Iterator and member names; a generic array-shape type describes APIs over CLI array transport. Array covariance defaults off. | Load actual neoCLR array/protocol relationships and capabilities. Retain target semantics; replace CLI projection assumptions. |
| Propagation | Explicit three-parameter `System.Propagatable` protocol is projected through nominal CLI references and Raven binding. | Preserve output/residual relationships and protocol semantics using native types. The protocol is platform behavior; CLI representation is replaceable. |
| typeof | Runtime context and TypeInfo names are configured; the current compiler checks a RuntimeTypeHandle-taking provider and emits the CLI path. | Define native type identity/token and introspection operations without requiring CLR reflection handles. Keep language typeof semantics and target introspection APIs distinct. |
| Characters | Profile enables grapheme representation and disables Unicode-scalar mode. Existing binder/emitter options implement the mapping. | Model the runtime's character/text contract directly; neither CLR Char nor an emitter option alone defines neoCLR semantics. |
| Async | Heap state machines, cancellation propagation and disabled exception capture are current profile defaults. Task naming also has a legacy heap-state/core-name trigger. | Separate platform async capabilities from lowering/backend representation. Document which behavior is semantic and which is transport before replacing it. |
| Nullable value syntax | Source nullable values default off in the profile. This is not proof that all nullable metadata is absent or that runtime nullability equals .NET annotations. | Establish native nullability semantics and then expose them through types, conversions, flow and diagnostics; do not infer them from CLI annotation capacity. |
| API admission / signatures | Import catalogs map selected APIs and receiver conventions. Signature projection/substitution has explicit shape/depth/admission checks; optional pointer/open-method support depends on the path. | Inventory rejected forms and distinguish importer gaps from actual runtime restrictions. Never silently erase unsupported semantics to satisfy a CLI catalog. |
| Identity and source maps | Imported application identities encode assembly/name/signature components; CLI tokens are local references. Generated adapters and sidecar maps assist translation. | Use structured stable native identities and native source locations. Do not turn the temporary text encoding or CLI token into the new metadata ABI. |
| Self | Explicit NeoCLR target plus `RuntimeSelfTypeContract` enables a fieldless marker, conformance-owned substitution and checked Self signatures. The importer admits bounded Number/Clonable dispatch; .NET rejects the configuration. The preset leaves the marker opt-in. | Replace the CLI marker and bounded importer recognition with native identities/signatures while preserving conformance ownership and unsupported-use diagnostics. See the dedicated Self tests and neoCLR consumer evidence; the historical Function smoke below is not Self evidence. |
| Intersections and records | Intersection work remains on its feature branch. The preset does not configure record-equatability/hash mappings. | Design native semantics and per-target support explicitly; bridge restrictions are not native semantic restrictions. |

Compiler owners: `Targets/NeoClrCliProfile`, `NeoClrCliCompatibility`,
`NeoClrCliRuntimeContract`, shared `CliRuntimeContract`, the legacy compatibility
path in `DotNetRuntimeContract`, `Compilation.CreateFunctionTypeSymbol`,
`Symbols/PE/PENamedTypeSymbol` and `BoundNodeFacts` under `src/Raven.CodeAnalysis`.
Runtime-side sources live in the neoCLR repository under
`docs/experiments/raven-target`, notably `RuntimeSignatures.cs`,
`DelegateBindings.cs` (nominal main), `FunctionBindings.cs` (structural branch), `VoidStorageValidation.cs`, the reference declarations and
API binding catalogs. Runtime documents `void-semantics.md`, `function-types.md`,
`raven-signature-projection.md` and `raven-import-identities.md` explain their
respective contracts; older documents describe dated checkpoints, not a current
complete support matrix.

The explicit neoCLR contract now owns profile validation, nominal inhabited
callback results, tuple-family selection and marker-gated Self availability.
The .NET contract retains legacy probe-core ABI triggers without enabling Self.
Shared CLI symbol validation remains in `CliRuntimeContract`; this split does not
make its marker, handle or type-name conventions native semantic requirements.
Native metadata/codegen must replace those transport assumptions as described in
the table. Loader, emitted ABI and runtime importer inputs are unchanged. The contract split
passes 147 compiler tests on .NET 11, including Self, profile validation, typeof,
callback/tuple compatibility and incremental reuse; compiler builds cover .NET
10/11. No new native runtime execution is claimed for this extraction.

Resolved CLI contract checks now run in Compilation's shared emission dispatch,
including the emitting macro-plugin compilation, before invoking the backend.
The .NET emitter owns output-core identity validation and artifact options. A
future native emitter must receive a compilation whose selected contract has
already been validated, then enforce its own artifact/ABI requirements. This
ownership change preserves current bridge encodings and importer inputs. Validation
passed 96 compiler tests on .NET 11, including resolved-contract rejection with
supplied diagnostics and macro-plugin emission; builds cover .NET 10/11. This is
compiler/CLI evidence, not native runtime execution.

Namespace-member discovery now accepts a provider-owned container fact without
requiring a PE symbol or resolving attributes in shared lookup. Source namespace
projection and the broad legacy TopLevel-name import rules are preserved. Fault
classification deliberately remains stricter: the ordinary namespace-container
capability is not proof of the exact runtime marker or terminal behavior. Native
metadata must replace that bridge-specific recognition with callable ownership
and terminal semantics; no native capability is enabled by this extraction.
Validation passed 56 namespace/flow tests on .NET 11, including a custom-marker
Fault negative case and non-PE discovery, with compiler builds on .NET 10/11.
No native runtime execution or importer/encoding change is claimed.

## Compatibility and replacement work

Explicit selection coexists with legacy core-name triggers. The runtime props, installed bundles and existing callers have not yet migrated
to the explicit preset. Keep that fact visible when changing a trigger. Strict DotNet
versus NeoCLR capability enforcement is still incomplete.

For each bridge behavior added, changed or removed, record:

1. The native semantic intent and the corresponding CLI representation.
2. Information preserved, projected, lost or rejected, including its compiler and
   runtime owners. Distinguish a workaround from a platform rule.
3. A focused positive/negative check and, where applicable, actual importer/runtime
   evidence with matching artifact revisions or hashes.
4. The native metadata/symbol/codegen capability that replaces it, plus the caller
   migration and compatibility trigger that can then be retired.

The native replacement must represent features without CLI erasure and keep the
shared semantic model authoritative. New symbol forms or semantic-model behavior
may be necessary; merely renaming a metadata provider cannot establish new types,
conversions, feature rules or lowering semantics. Choose loader, platform contract
and codegen coherently. Arbitrary cross-target combinations remain out of scope.

## Exploratory evidence, not feature acceptance

On 30 September, Raven `9a58e1356` was used with the installed
`development-20260929-functions` feature bundle. Reference, importer, System library
and runtime hashes matched its manifest. A temporary function/unit/tuple/typeof
consumer compiled, imported, verified and ran with output 42, 7, True. The runtime
was built from `19c6725f` (bundle revision `4d1e7506`), on the Function-types feature
history rather than neoCLR main. Logs remain local at
`/tmp/raven-neoclr-profile-slice39/evidence.json` on the validation host.

The author clarified that Function types are intentionally excluded until the
metadata layer and full compiler support exist. Consequently this experiment is
not a committed smoke gate, a supported-feature claim, or a reason to migrate
runtime props. The props change and proposed Function-dependent smoke fixture were
withdrawn before committing. Existing installed bundles and apps were untouched.
No native metadata loader, native emitter or neoCLR-main feature qualification is
established by this result.

## Retain compiler groundwork without enabling deferred semantics

Keep general compiler-side target plumbing and unrelated CLI compatibility on
shared Raven main, preserving ordinary .NET behavior. Per the author's subsequent
clarification, native Function/structural-type-specific work stays on feature branches
in both Raven and neoCLR. Raven function syntax and
.NET delegate support are not the same feature as neoCLR native structural Function
semantics; neither should be blanket-disabled merely because the latter is deferred.
Structural Function semantics remain feature-branch work while native metadata
catches up. The inhabited unit-result encoding remains shared because neoCLR main
also requires it for nominal callbacks.

Promotion requires explicit target capabilities and meaningful tests for native
type identity, conversion/assignability, introspection and emission. Gate unsupported
operations when their required capability can be identified; do not treat a probe
type name or a successful narrow example as proof of complete Function support.
Avoid another permanent compiler branch split or rolling back unrelated shared fixes.

## Next metadata design slice

Specify the native semantic-data contract before promoting deferred features:

- Structured module/type/member identities and reference/version relationships.
- Native type forms, including function signatures, generic parameters/constraints,
  unit/no-result distinctions and the runtime's other explicitly supported forms.
- Semantic flags and relationships required by binding, conversion, flow and
  diagnostics; classify unresolved Self/intersection/nullability semantics separately.
- Symbol ownership, lazy loading, malformed/missing-data diagnostics and invalidation
  when target metadata changes, without requiring CLR reflection objects.
- The matching output/backend contract. An input loader alone cannot eliminate the
  CLI application importer or qualify a new feature's emitted behavior.

Use that schema to identify shared semantic-model extensions and capability gates.
Only promote a deferred feature once its native metadata, symbol/semantic behavior
and supported emission/execution path are defined and tested. Designing native
metadata support does not itself enable Function types, Self or intersections.

## Extension discovery provider boundary (2026-09-30)

The CLI bridge still obtains extension receiver and member-presence facts from
PE metadata's extension attributes and markers. Shared type queries now consume
`IExtensionTypeInfo`; constructed symbols substitute receiver types. This changes
ownership, not the bridge encoding or supported extension semantics. Type-level
facts alone do not establish member applicability; member-level decoding still
contains PE-specific paths. Native metadata can replace this discovery encoding
with semantic extension facts, but still needs corresponding member binding and
backend support. The extension regression suite validates the current CLI path;
non-PE fixture coverage validates capability dispatch, not native execution.

### Member receiver decoding ownership (2026-09-30)

`IExtensionReceiverResolver` now isolates the bridge's member receiver decoding.
Its PE implementation resolves marker receivers and preserves existing generic
mapping: marker type parameters can be re-anchored to method parameters by
ordinal; constructed owners use containing-type substitution. These are CLI
encoding rules, not requirements imposed on native neoCLR metadata. Core lookup
consumes the resolved receiver without applying those remappings itself.

The encoding and its limitations are unchanged. Native replacement requires a
provider that supplies receivers in the requested member context and matching
metadata/backend support. Extension semantic regressions validate the CLI path;
non-PE fixtures verify preservation of provider receiver identity and constructed
member context. They do not constitute native neoCLR execution evidence.

### Shallow method identity ownership (2026-09-30)

The CLI bridge still uses PE module version IDs and metadata tokens to distinguish
method declarations during shallow lookup. That encoding now lives behind
`IMethodLookupIdentity`; core lookup treats the provider key as opaque and adds
generic method arguments. If reflection cannot supply identity, the PE provider
retains the old containing-type/name/parameter-count fallback, which can conflate
same-count overloads. No new native semantic restriction is introduced.

A native metadata provider should supply its own stable declaration keys without
loading signatures or requiring CLI tokens. This is candidate deduplication, not
complete native identity/equality support. Non-PE fixtures verify lazy dispatch
and generic argument distinction; CLI overload, lookup and entry-point tests
validate existing behavior. Native neoCLR execution is not claimed.

### Lazy parameter facts (2026-09-30)

The CLI bridge exposes lazy parameter facts through `IMethodParameterInfo`.
Reflection remains responsible for count/type reads, by-ref element normalization,
optional/default flags and ParamArray recognition. The PE adapter preserves its
existing count-of-one fallback when count inspection fails; this is a bridge
limitation, not a native metadata requirement. Other providers can explicitly
report unavailable facts, and shared queries do not force full signature loading
on failure.

Native replacement needs equivalent semantic count/type/usage facts in each
method's context, without reflection or CLI attribute requirements. Conversion
scoring still has PE-specific paths and is not covered by this abstraction.
Semantic-model caching and optional/params regressions validate current CLI
behavior; non-PE fixtures cover lazy dispatch and failure propagation. No native
neoCLR execution is claimed.

### Available-state conversion classification (2026-09-30)

The bridge's quick argument/receiver conversion classification now resides in
`PEMethodSymbol.ParameterConversions`, behind `IParameterConversionClassifier`.
It preserves CLI name equality (including supported rank-one array shapes), the
existing numeric widening table and the System.Object fallback. Unrecognized
argument shapes or unreadable parameter names decline the shortcut; classified
nonmatches retain the existing rejection behavior. This bounded table is not
complete conversion analysis and is not a native neoCLR conversion specification.

Core lookup owns candidate ranking and general symbol-conversion fallback. A
native provider can decline this optional shortcut or supply classifications from
its own semantic representation; full native metadata and backend support are
still required. Non-PE tests verify ranking, context forwarding and rejection vs
unavailable results without signature loading. PE fixtures and semantic-model
caching tests validate the CLI behavior; native execution is not claimed.

### Overload-priority reflection fallback (2026-09-30)

The CLI bridge still falls back to reflection for overload priority after source
and semantic-attribute lookup. PE now owns that decoding behind
`IMethodOverloadPriority`: it examines the base definition when available, reads
the exact System.Runtime.CompilerServices.OverloadResolutionPriorityAttribute and
returns no fallback fact on unreadable metadata. Nonvirtual/new-slot declarations
are read directly, fixing an unconditional GetBaseDefinition call that failed in
MetadataLoadContext before their attributes could be read. Slot-reusing override
fallback remains unavailable when base-definition reflection is unsupported.
Existing lookup precedence is
unchanged; this does not introduce a new native inheritance rule.

Native metadata can supply priority as a semantic fact through the capability,
without CLI attributes or MethodInfo. Shared applicability/grouping/ranking remain
compiler-owned. Non-PE overload selection and existing source/metadata priority
regressions validate the boundary; no native neoCLR execution is claimed.

### Parameter type-default facts (2026-09-30)

The CLI bridge synthesizes a type default when OptionalAttribute provides no
explicit constant, retaining the existing decoded value and a type-default flag.
PE now exposes that flag through `IParameterDefaultValueInfo`; shared optional
argument binding and display no longer test for PEParameterSymbol. Source syntax,
Option.None markers, explicit constants and null handling retain their behavior.

A native provider can describe a synthesized type default without CLI attributes;
full native default-value representation and emission remain future work. The
flag only applies to parameters already reporting an explicit default. Non-PE
binding/display tests and existing optional-argument/display suites validate the
boundary; native neoCLR execution is not claimed.

### Array provider boundary (2026-09-30)

CLI array shape is now provided by `PENamedTypeSymbol.ArrayTypes` through
`IArrayTypeProvider`, not hard-coded in the shared array symbol. PE retains
rank-one vector restrictions, configured Iterable/ArrayShape projection, exact
assembly/arity checks, member owner preservation and ordinary .NET collection
interface fallback. Invalid explicit shapes still do not invent host interfaces.
Array storage/signatures and bridge emission are unchanged.

A native base-type provider can supply interfaces and members for its own ranks
and element types. Shared arrays no longer inherit PE behavior or assume .NET
interfaces for a non-PE base. Native storage, metadata and codegen are still needed.
Non-PE provider tests cover shape dispatch, duplicate interfaces and member owner
identity; existing array, variance, iteration-contract and imported-array tests
validate the CLI bridge. Native neoCLR execution is not claimed.

### Constructed parameter default preservation (2026-09-30)

The shared constructed-method and containing-type parameter wrappers now preserve
the provider's type-default flag. CLI OptionalAttribute decoding remains PE-owned;
wrapping the resulting parameter no longer erases that fact before binding or
display. No bridge encoding changes. Native providers using the same capability
benefit from identical forwarding without a dependency on CLI attributes.

Six regressions cover generic method/type substitution and nested wrappers, with
positive type-default and negative literal controls. Before the correction, all
three type-default cases bound as errors; afterward they retain the constructed
parameter type and display `default`. This is symbol/binding validation on .NET,
not native neoCLR execution evidence.

### Shared operation API prerequisite (2026-09-30)

The first independent-metadata consumer exposed missing binary operator facts and an
incorrect invocation Instance projection. Raven now exposes bound operator facts and
actual receivers through its public operations API. No CLI encoding or Runtime Contract
configuration changes. The correction belongs to shared compiler APIs, not to a permanent
neoCLR-specific branch. The current CLI bridge still owns its existing mapping; this
API improvement only makes a later native emitter possible without syntax guessing.
Focused .NET operations tests cover this surface; native consumer evidence is recorded
separately with its exact supported subset.

### Native metadata consumer probe (2026-09-30)

The opt-in NeoClrMetadataProbe consumes the separate metadata API to write native
format-5 application bytes (originally from public operations; now from compiler-lowered bodies). Top-level source functions
remain type-independent in native output. The dependency has two API-produced forms:
a PE read by the existing .NET semantic provider, and native bytes loaded by neoCLR.
This is a temporary bootstrap representation, not a requirement that the native target
continue to import PE or depend on .NET reflection.

No existing CLI bridge encoding changed. The application bypasses the CLI importer;
frontend metadata loading remains .NET-owned. Exact restrictions and the reproduction
command live in tools/NeoClrMetadataProbe/README.md. The native loader/emitter target
adapters will replace that bootstrap. Owners: Raven for symbols/operations/adapters,
the separate library for format/model/emission, neoCLR for native loading/verification.

### Read-only call imports — 2026-09-30

The independent host library now provides `AssemblyBuilder.ImportReference` and
`MethodBuilder.Call(ImportedMethodReference)`. This follows the existing Cecil-style
import direction: a compiler consumes immutable definitions and imports a scoped
reference into its output. Compared with CLR reflection/Reflection.Emit, this needs
no loaded runtime assembly or executable method handle. Compared with full Cecil
imports it deliberately supports only unsigned, top-level-owner static Int32/void
signatures; no generic substitution, access checking or arbitrary IL translation.
The cost is an explicit host assertion of the dependency core contract and a bounded
format-5 naming agreement. Private reference-only nodes reuse both writers' existing
call encodings without retaining producer bodies. A future general signature model
should replace these bounded nodes as supported compiler cases require it.

Raven's feature-branch probe now receives only the read-only dependency snapshot;
it does not receive its builder graph. The primitive .NET Runtime Contract remains
the binding bootstrap, and ordinary .NET code generation is unchanged. Native
semantic-data loading and production emitter installation remain the next integration
stages; structural support stays later. C# contract tests pass (22 groups); both the
native global-function gate and Raven probe verify/run to 42. Missing dependency,
wrong revision, unsupported operation and binding diagnostics remain checked.

### Compiler-owned native adapter checkpoint — 2026-09-30

The working operations consumer is now the optional `Raven.CodeAnalysis.NeoClr`
project, with `NeoClrCompilationEmitter.Emit`, immutable output/core/dependency options
and a success/diagnostic result. The probe is a C# caller of this reusable adapter.
The metadata API remains a separate project with no Raven dependency. The new project
is opt-in through NeoClrMetadataProject; ordinary .NET behavior/default builds and
`Compilation.Emit` composition are unchanged.

Calls use explicit compiler-reference-to-snapshot bindings and resolved assembly-symbol
identity instead of a simple-name selection. Host snapshot consistency and the primitive
core assertion remain explicit responsibilities until a native provider owns them.
NEOMETA001 carries source locations; NEOMETA002 rejects incompatible configuration;
NEOMETA003 reports writer limits/invalid graphs. Original binding diagnostics survive.
All validation precedes output writes; host I/O failures propagate and can partially
write. Supported source/format-5 encoding remains the existing static Int32 subset.

C# consumer checks cover expression spans, unchanged rejected output, original error
identities, invalid/duplicate/unregistered/core bindings, writer limits, multi-tree
rejection, repeated output and stream ownership/failure. The emitted application still
verifies/runs in neoCLR with result 42. Hash evidence includes the new adapter binary.
Native symbol loading and production target registration remain pending. Structural
support remains later; this does not change the runtime bridge's platform capabilities.

### Multi-file native adapter checkpoint — 2026-09-30

The optional compiler-owned emitter now accepts multiple source trees. It collects
all supported top-level declarations before emitting bodies and retains each body's
own semantic model. Like the ordinary .NET compiler, valid cross-file calls bind
independently of file order; native metadata token/declaration order still follows
input order. No shared binding or .NET emitter change was required. Macro trees and
unsupported constructs remain excluded under the existing explicit bootstrap contract.

The new two-file regression first failed with the adapter's NEOMETA002 single-tree
restriction. After the refactor, Helper.rvn/Main.rvn and the reversed input order
both verify/run to 42 in neoCLR. A division expression in the later helper file
produces NEOMETA001 with that file's source location and leaves output unchanged.
The original one-file and adapter contract checks still pass. Validation evidence
includes hashes for all three applications. Native symbol loading and production
registration remain separate next steps; the metadata library stays independent.

### Native dependency input through a reference projection — 2026-09-30

The independent metadata project now reads its bounded native format-5 declaration
contract with `NativeAssemblyDefinition.ReadAssembly`. It checks manifest/name/origin
consistency, duplicate and unsupported declaration fields, owner/signature contracts
and resource bounds. Bodies remain opaque: successful metadata reading does not imply
successful native verification or execution. General native schemas, arbitrary types,
structural metadata and executable rewriting remain unsupported.

The snapshot can create a reference-only PE using an explicit core identity. The
projection preserves supported callable/type declarations and marks the assembly with
ReferenceAssemblyAttribute; placeholder bodies throw. It omits the native entry point
and implementation dependency references. These signatures need only primitive types.
This is the temporary native-input bridge, following the .NET separation of compilation
contracts and executable implementations, not a new executable CLI representation of
native code. The cost is an extra PE and the existing .NET symbol provider. Native
ISemanticDataLoader/symbol construction should ultimately consume native declarations
directly; the projection must then be retired, not made a permanent platform rule.
Primary comparison: [Microsoft reference assemblies](https://learn.microsoft.com/en-us/dotnet/standard/assembly/reference-assemblies).

The Raven probe now emits only the original native dependency from its producer,
reads that native artifact, and creates MetadataProbeLibrary.reference.dll from the
reader. It binds that reference with the existing .NET primitive Runtime Contract,
imports the read-only callable contract, and emits native applications. The original
native dependency (never the reference PE) is supplied to neoCLR. The one-file and
both two-file orders verify/run to 42; diagnostic/stream checks still pass. The report
records the reference projection hash alongside the native dependency/application hashes.

Ownership: the metadata project owns reading/projection; Raven owns compiler binding
and emission; the host supplies matching explicit core identities and native runtime
dependencies. Ordinary .NET defaults and existing CLI targets are unchanged. The
metadata project stays separate, and all work remains on the existing feature branches.
C# contracts pass (23 groups), including malformed inputs, projection ownership,
reference marking and rejection of execution loading by .NET. No production native
semantic loader or target registration is claimed.

### Raven library-to-application native case — 2026-09-30

Both sides of the integration case now originate in Raven source. The optional adapter
accepts library output without an entry point and public nongeneric static classes in
the global namespace containing public static Int32 methods. Ordinary Raven default
public method accessibility is accepted. Nonpublic members/types/library globals and
additional type contracts are rejected; the public-only metadata writer must not
silently widen a library's visibility. Console top-level functions remain native
functions outside types. Broader visibility/namespace/type support is still pending.

The producer declares MathLibrary.Twice overloads and a Multiply helper. Raven emits
the library as native format 5; the independent metadata API reads it and projects
reference-only declarations. A separate Raven application binds the one-argument
overload, emits a native external call, and neoCLR executes the original library's
local helper call. The one-file and both multi-file input orders return 42. No producer
builder graph or hand-authored native dependency body is used in the case.

The new case first failed with NEOMETA002 because the adapter accepted only console
output. Focused C# checks now confirm entry-less library output, overload/local-call
execution, source-located visibility/type rejection with unchanged output, and native
missing-dependency/wrong-revision errors. Existing diagnostic/stream and multi-file
checks pass. The .NET primitive Runtime Contract and reference-only input bridge remain
explicit; no default .NET behavior, general binder, metadata library API or runtime
format change was needed. The next replacement remains a native semantic provider and
production target composition, with further supported constructs driven by real cases.

### Transitive native runtime acceptance — 2026-09-30

The end-to-end chain now consists entirely of Raven-compiled native assemblies:
application -> MetadataProbeLibrary -> ArithmeticDependency. The outer library imports
the inner library's native declarations through the temporary reference projection.
The application references only the outer projection; Arithmetic is absent from its
symbol lookup and the outer reference PE's AssemblyRef rows, since all public
signatures use primitives. Like .NET reference assemblies, implementation dependencies
remain outside that compile-time signature surface. They are still required at runtime.

`NativeAssemblyDefinition.References` now exposes the exact direct native identities
in manifest order as an owned read-only list. It does not resolve dependencies or build
a transitive closure. The host explicitly supplies both native dependencies to neoCLR.
The existing runtime reference validator (`src/references.rs` in neoCLR), loader,
verifier and VM accept the emitted chain: all three application variants return 42,
and reversed runtime module order also returns 42. Missing direct/transitive modules
and wrong direct/transitive revisions fail verification with the expected diagnostics.
No runtime implementation change was necessary for this supported format-5 graph.

This is actual loading and execution of the emitted native format, not a claim based
on PE readability or reader roundtrips. Reference-only PEs are compiler input only.
The author reiterated runtime loading as a required acceptance gate. Direct PE/#Neo
loading and structural NEOX semantics are still unimplemented, and the .NET primitive
binding bootstrap remains temporary. Neither limitation is hidden by this test.

Validation: 24 C# metadata contract groups pass, including direct identity/list
ownership and rejection checks; Raven adapter/library/multi-file contracts pass; the
runtime checks above pass. Reports include both native dependencies and both reference
projection hashes. Compiler integration remains on codex/metadata-consumer and the
independent metadata/runtime checkout on codex/extended-cli-metadata.

## Initial direct runtime container — 2026-09-30

The optional `NeoClrCompilationEmitter.EmitMetadataAssembly` now uses the independent
metadata API to produce PE/#Neo files. The existing .NET primitive Runtime Contract
and semantic provider remain the bootstrap: bind against reference-only CLI declarations
in those files and pair them with `RuntimeAssemblyContainer.ReadCliProjection` snapshots.
The metadata project owns encoding/projection; Raven owns semantic mapping and backend
diagnostics; neoCLR owns native admission, dependency linking, verification and execution.
Default .NET and existing production target composition are unchanged.

The same PE library files are now runtime inputs. Required section 256/schema 1 carries
authoritative native format-5 metadata and bodies; CLI stubs do not execute. This
supersedes the earlier JSON-only runtime checkpoint above. Native bodies and dependency
identities retain their semantics, including top-level functions and transitive references.
The report records application/library/runtime/API hashes; single/multi-file cases
and reversed module order execute to 42 and dependency rejection checks pass. C#
adapter checks cover equivalent native payloads and unchanged failed output.

This bridge still stores JSON inside the PE and makes no performance improvement claim.
The author highlighted parsing cost; binary native encoding and separate loading, linking,
verification and execution measurements are the next evaluation. Required structural
schemas, native compiler symbol loading and production emitter registration remain
pending. There is no guest Introspection assembly loader API yet. Work stays on
`codex/metadata-consumer` with neoCLR's `codex/extended-cli-metadata`; ordinary .NET
behavior and shared main are unaffected. See the [adapter API](api/neoclr-emission.md#peneo-output)
and the probe's tracked validation report for the tested artifacts.

### Hello World acceptance — 2026-09-30

The author selects Hello World, then an entry-point call to another function, as
the first acceptance targets. Both now pass through PE/#Neo runtime loading: the
first prints in Main, the second calls Greet; both print exactly one line and exit 0.
The host explicitly supplies `NeoClrEmitOptions.ConsoleReference`, an exact registered
reference authorizing only System.Console.WriteLine with a non-null string literal.
This adds no global Runtime Contract/default .NET mapping. The metadata API emits
native ldstr/call/pop; pop discards the bundled System library's current Void value.
Native string signatures, arbitrary Console overloads and no-result source entry
points remain outside this slice. The production replacement belongs in the native
platform-call/type contracts; Raven owns semantic matching, the independent API owns
encoding, and neoCLR owns System output. C# checks reject missing/wrong/unregistered
bindings and unsupported overloads without touching output.

### Binary payload boundary — 2026-09-30

The opt-in PE emitter now uses the independent `RuntimeAssemblyContainer.WriteBinary`
API. Required section 256/schema 2 carries bounded CBOR of the same native format-5
model. neoCLR's CLI/module path preserves binary input and deserializes directly into
runtime metadata, without JSON text conversion. The compiler-host intermediate and
reference-only CLI projection remain unchanged. Explicit core/Console bindings and
ordinary .NET target defaults are unchanged. Schema-1 containers remain readable;
schema-1-only runtimes reject new output. Match the experimental runtime branch.

Hello World, entry-point function calls, both source-file orders and the transitive
library case pass with binary containers. Required schema/bounds/UTF-8/duplicate-key
rejections are covered by the metadata/runtime tests. Native symbol-provider work,
indexed tables, wider signatures and production registration remain open. The runtime
repository records load/link/verify/execute timings separately; no execution-speed
claim follows from avoiding JSON parsing.

### Class-library bootstrap direction

The author identifies compiling neoCLR's runtime class library and loading its symbols
into Raven as the next important consumer. Existing JSON may first be translated into
neoCLR assemblies, preserving native metadata and bodies, before direct emission
covers the complete library. The current bounded writer/reader is not yet a general
class-library translator. A real library slice should drive missing signature/member
coverage, with unsupported information rejected rather than omitted.

The existing .NET semantic provider can initially consume an explicit CLI reference
projection because the declaration models are still similar. Native metadata remains
authoritative as semantics diverge; keep projection mappings behind the compiler's
loader contract so a native ISemanticDataLoader can replace them. The independent
metadata project stays separate from Raven. This is a bootstrap plan, not a claim
that System.Runtime already compiles through this experimental emitter.

### Unit-returning native helpers and library methods — 2026-09-30

The opt-in `NeoClrCompilationEmitter` now accepts Unit/no-result functions and public
static methods with required Int32 parameters. Statement calls, explicit bare returns
and implicit fall-through returns emit through the independent Cecil-style metadata
API. An Int32 entry point can call a Unit helper without a synthetic source return value.
Imported method matching includes the result contract as well as parameter count.

Configuration remains the explicit .NET primitive binding bootstrap (`TargetPlatform.DotNet`),
unsigned output/core identities and registered `NeoClrMetadataDependency` bindings;
Console literal output additionally requires `ConsoleReference`. No Runtime Contract
setting, ordinary .NET emission or production target registration changes. Native intent
is a no-result function; the temporary CLI reference projection represents it as `void`,
which Raven binds as Unit. The native PE/#Neo schema-2 section owns execution, and the
projection contains reference-only bodies. The compiler adapter owns mapping and
validation; the separate metadata library owns encoding. A native symbol provider and
broader native backend will replace the projection/bootstrap.

C# consumer checks compile and execute four Hello variants (Int32 and Unit helpers,
explicit and implicit returns), then emit a Raven library and reload its projection into
Raven. A second compilation calls its Unit overload and returns its Int32 overload;
neoCLR verifies both assemblies and prints exactly one Hello World line with exit zero.
Rejected discarded Int32 calls, named arguments, Unit entry points and unsupported
Console mappings leave the output stream unchanged. This is bounded linear-body support:
entry points still return Int32, and generic/instance methods, general result types,
control flow and complete runtime class-library compilation remain pending.

### Namespaced native library types — 2026-09-30

The opt-in adapter now traverses block-scoped, nested and file-scoped namespace
members. Public nongeneric static classes retain their bound namespace and metadata
name in the independent metadata API. As with .NET type identity, two types with the
same short name in different namespaces remain distinct; imports affect Raven lookup,
not native ownership. No format change or new namespace table is needed for this slice.

The C# consumer emits `Example.First.Math` and `Example.Second.Math`, with a call
between them, in both source-file orders. It reads the resulting CLI projection,
checks both namespaces, binds an independent Raven application using imported and
qualified names, and verifies/runs each application against its binary library in
neoCLR to 42. Namespace-scoped free functions and nested types are still rejected with
source diagnostics and unchanged output: the current native function model lacks a
namespace contract, so flattening those functions would lose ownership.

Configuration remains `TargetPlatform.DotNet` for the primitive semantic bootstrap,
with explicit core/output identities and dependency bindings. Runtime Contract options
and default .NET emission are unchanged. The compiler adapter owns symbol-to-metadata
mapping, the independent library owns PE/#Neo encoding, and neoCLR loads the native
schema-2 section. CLI declarations remain a temporary reference-only representation;
a native symbol provider will replace that projection. Generic/instance members,
namespace-owned native functions and complete runtime class-library compilation remain
pending. This extends the earlier global-namespace-only compiler checkpoint.

### Opt-in native compiler command — 2026-09-30

Build `src/Raven.Compiler` with `-p:NeoClrMetadataProject=/absolute/path/to/neoclr/tools/metadata/NeoCLR.Metadata.Experimental/NeoCLR.Metadata.Experimental.csproj`
to enable `rvnc neoclr`. The metadata library remains an independent project and is
not a dependency of ordinary compiler builds. Without that property the command
reports how to enable it; default .NET emission is unchanged.

```sh
dotnet rvnc.dll neoclr --library -o Library.dll Library.rvn
dotnet rvnc.dll neoclr --reference Library.dll -o App.dll Main.rvn Helper.rvn
neoclr verify App.dll --module Library.dll
neoclr run App.dll --module Library.dll
```

The command compiles supplied source files through the bounded native adapter and
writes PE/#Neo schema-2 assemblies directly. Global source functions become native
assembly-owned functions; classes retain namespaces. Repeated `--reference` options
load native API-produced PE assemblies, validate their native declarations, and expose
their CLI reference-only projections to Raven. No CLI-to-neoil application translation
is involved. Binding/encoding failures create no destination, existing outputs are
refused, and unknown options fail. Successful output creation uses CreateNew; a later
I/O failure may leave a partial file. Output identity is its filename without extension,
version 1.0.0.0, unsigned. Without `-o`, the first source's extension becomes `.dll`.

This is a source-file command, not project/MSBuild/publish integration. It generates no
PDB, runtimeconfig or apphost and does not execute under .NET. Explicit dependency
paths are required; there is no automatic dependency resolution. C# process checks
compile a namespaced library and a two-file application, exercise native assembly-owned
Unit function calls and Console literal output, and verify/run in neoCLR to 42.
Failures cover unsupported source, ordinary CLI references, invalid options and existing
output preservation. The full earlier metadata probe remains green.

**Symbol-loading boundary:** Host .NET primitive/Console/System.Runtime references
remain a temporary semantic bootstrap (`TargetPlatform.DotNet`). The command configures
that core identity and Console contract explicitly, without changing Runtime Contract
options. It does not yet accept the translated standalone System assembly as a core
reference: that file has broad format-5 declarations and no CLI projection, beyond the
writer-shaped declaration reader's subset. The author's required next loader acceptance
is to obtain Raven symbols from that translated System/System.Runtime metadata itself,
then compile against and execute with the matching runtime assembly. Implement a native
symbol provider or faithful broader projection; do not replace those symbols with host
.NET equivalents or silently omit unsupported declarations. Emission and symbol loading
are parallel integration requirements; full format adaptation is not a prerequisite for
making the supported native compiler path usable.

Validation also builds the compiler without `NeoClrMetadataProject`, checks that its
.deps.json has no native adapter/metadata dependency, confirms the explicit disabled
command diagnostic and compiles a normal .NET Int32 application successfully. Testing
used the net10.0 host; other host/target matrices were not rerun.

### Translated System callable import — 2026-09-30

Raven can now bind an explicitly selected static Int32 callable read from the translated
standalone System assembly, then emit its original native call identity. The independent
metadata library inventories the binary native module; `CreateStaticInt32ReferenceAssembly`
builds a deliberately partial reference-only view. The existing DotNetSemanticDataLoader
and reflection-backed PE symbols import that view. This reuses semantic import rather
than introducing a second binder or substituting handwritten host declarations.

```sh
dotnet rvnc.dll neoclr --system-symbols System.neox \
  --system-method System.Math.Min/2 -o App.dll App.rvn
neoclr verify App.dll --system System.neox
neoclr run App.dll --system System.neox --show-result
```

The tested source is `func Main() -> int { return System.Math.Min(42, 99) }`.
The symbol test checks that the bound method belongs to the generated native view's
assembly, not the host .NET implementation. Native execution returns 42 using the same
translated collection System assembly (417 types, 4,090 functions). Both compiler API
and actual rvnc process paths are covered. Generic-arity name collisions remain in the
inventory; public visibility comes from native visibility and available origin metadata.
Private methods, internal owners, missing selections and unsupported signatures fail.
Native Result-returning `System.Math.Abs/1` is rejected rather than changed to .NET Abs.

Each `--system-method` explicitly selects qualified native name plus Int32 parameter
count; it can be repeated. No unselected methods are claimed to be imported. The static
view preserves callable ownership/name/signature but projects owners as static classes;
instance shape, fields, properties, generic contracts, parameter names and attributes
remain unprojected. Primitive binding still uses the host core (`TargetPlatform.DotNet`);
this is **not complete System/Core import**. In this mode host Console/System.Runtime
facade references and literal Console mapping are disabled, because host forwarders can
win name lookup. No Runtime Contract option changes. General namespace/type collisions
still require native core composition, not lookup-order assumptions.

`NeoClrEmitOptions.SystemSymbols` accepts an explicit `NeoClrSystemSymbols` binding
(reference, projection assembly name, native inventory, selected functions). The exact
reference must be registered; invalid selections produce NEOMETA002 without output.
Native method selection additionally matches the bound assembly, owner, name, Int32
parameters and result. Emission uses `MethodBuilder.Call(NativeFunctionDefinition)`;
this bootstrap only supports the implicit `System` module. The caller must supply the
matching System artifact to runtime verify/run: no System revision/image digest is yet
encoded. General dependency identity, richer signatures and full core import remain
required follow-up work. Projection files created by the command are temporary and
removed after compilation. The metadata library remains separate from Raven.

Reproduction through the C# consumer:
`NeoClrMetadataProbe --system-symbols <runtime> <rvnc.dll> <System.neox> <fresh-output>`.
The output includes source, reference projection, native assemblies and validation hashes.

**Architecture direction:** ISemanticDataLoader and ICompilationEmitter are existing
composition boundaries, but DotNetSemanticDataLoader and PE symbols still depend on
MetadataLoadContext, Assembly/Type/MethodBase. This bridge adapts native metadata into
that importer. Extract common declaration/signature inputs for shared symbol construction
as coverage grows; the projection is not a permanent native representation. The native
operation emitter already avoids Reflection.Emit. The general .NET code generator still
uses ILGenerator and builders throughout, so removing that dependency requires a common
body/instruction abstraction and backend writers, not just replacing one emitter class.

### Unit entry points — 2026-09-30

Native Raven output now accepts parameterless Unit entry points as well as Int32
entry points, whether assembly-owned functions or public static class methods.
For example, `func Main() { System.Console.WriteLine("Hello World") }` emits through
the independent metadata API, loads/verifies in neoCLR, prints one line and exits zero.
A Unit entry point may call a Unit helper, return explicitly, or have an empty body.
This supersedes the earlier Int32-only entry restriction; generic/instance/argumented
entry points and broader source constructs remain outside the bounded native adapter.

Native format 5 already represents no-result functions with `returns: Void` and
`no_result: true`. The writer and declaration reader now admit that existing contract
for an entry point; no new transport schema or runtime opcode is needed. Ordinary CLI
output uses void and a managed entry token, matching the familiar CLR no-result entry
contract. PE/#Neo keeps its CLI view reference-only and the native entry authoritative.
No inhabited Void value or artificial integer return is inserted into the source body.

Configuration remains explicit native emission with the current primitive binding
bootstrap and optional Console/System-symbol contracts. Runtime Contract settings and
default .NET compiler emission are unchanged. Tests cover metadata roundtrips, actual
.NET invocation of ordinary CLI output, native API and compiler-command execution,
empty bodies, global/static entry ownership, and rejection of extra return-stack values,
parameterized entries and foreign entry methods. The .NET metadata model owns the
entry contract, Raven maps Unit to no result, and neoCLR's existing loader/VM executes it.
Full System symbol import and richer metadata/backend coverage remain the main follow-up.

### Opcode-based metadata emission — 2026-09-30

Raven's bounded native operation emitter now writes its linear bodies through the
independent metadata library's `MethodBuilder.Emit` overloads. Supported logical
opcodes are Ldc_I4, Ldarg, Add, Sub, Mul, Call and Ret. Integer operands and typed
builder/imported/native call operands use separate overloads; unsupported opcode/operand
pairs fail before mutation. Native System calls retain their selected native identity.
Existing LoadConstant/LoadArgument/arithmetic/Call/Return helpers delegate to the same
path, with unchanged stack, ownership, backend and resource validation.

This provides low-level construction for the currently implemented subset, not arbitrary
CLI bytes or all neoIL opcodes. Console literal emission remains its explicit native
convenience operation; there is no general string operand yet. Branches, locals,
exception regions, public instruction objects and ILProcessor-like body insertion remain
future work. The underlying body representation is still internal. Enum numeric values
are not serialized opcode values and do not define an on-disk ABI.

The .NET comparison is typed Emit overload ergonomics without taking a dependency on
Reflection.Emit. The independent metadata writer still chooses native/CLI encoding and
validates the complete body. This creates a compiler-facing emission surface that can
grow toward backend reuse; it does not yet replace Raven's general .NET code generator.
Target/runtime configuration and existing temporary reference projections are unchanged.
C# checks compare helper/Emit artifacts, execute ordinary CLI output to 42, reject bad
operands without body mutation, and cover imported/native calls. Raven's existing native
compiler/runtime cases and selected translated-System calls validate its actual use.

### Shared emission pipeline — 2026-09-30

Native emission now enters the ordinary `Compilation.Emit` pipeline. Select it with
`new EmitOptions().WithBackend(new NeoClrEmissionBackend(nativeOptions))`; the
`rvnc neoclr` command and existing `NeoClrCompilationEmitter` convenience APIs use
that same path. The compiler owns setup, declarations, semantic diagnostics, macro
preparation and resolved Runtime Contract validation. The backend returns only its
own diagnostics and creates fresh metadata builders on every call. No global backend
registration or dependency from the core compiler to the independent metadata library
is introduced. Clearing the backend restores the selected target's default emitter.

`ICompilationEmissionBackend` is the artifact boundary, not a replacement for the
semantic target. Native configuration still requires the .NET primitive bootstrap,
explicit reference projections and matching native dependency artifacts. Binary PE/#Neo
is the default native artifact; JSON interchange remains available explicitly. Native
PDB output and `EmitOptions.TargetCoreLibraryIdentity` rewriting reject with NEOMETA002
before either stream is changed; configure native core identity in `NeoClrEmitOptions`.
The native source/signature subset and reference-only CLI bodies are unchanged.
Macro preparation belongs to the common pipeline; native macro support is not established.

Compared with the previous .NET-only emission entry point, this lets independently
packaged backends reuse compiler validation without depending on Reflection.Emit.
It does not make the existing .NET `TypeBuilder`, method handles or IL operands portable:
those remain the next extraction boundary. Share declaration/body lowering where
semantics agree, with backend-owned type/method handles and explicit capabilities
where representations differ (notably assembly-owned native functions versus CLI
carrier types). Do not turn unsupported native shapes into silent CLI fallbacks.
The cost is an explicit backend API whose implementations must validate artifact options
and keep per-emission mutable state private.

Focused C# tests cover backend selection, preserved option copies, compiler/contract
failure before backend entry and default .NET PE emission. The native executable probe
covers shared-pipeline/wrapper equivalence, unchanged output on unsupported artifact
options, Hello World, helper calls, dependencies and translated System.Math.Min.
The general backend API is a shared-line integration candidate; it remains on the
consumer branch with the native adapter pending reconciliation with Raven main.

### Shared linear-body lowering and backend method builders — 2026-09-30

The native emitter's existing operation lowering now lives in the core compiler as
`LinearMethodBody`. It produces an immutable plan containing compiler symbols and
logical constant/argument/arithmetic/call/return instructions. It carries source syntax
for diagnostics but no Reflection.Emit or neoCLR metadata handles. `ILinearMethodBuilder`
is an internal compiler implementation contract; the optional native assembly receives
friend access, without a dependency from the core compiler to the metadata library.

Two adapters consume that same plan. The .NET adapter uses the existing method builder,
IL-builder factory and runtime-symbol resolver. The native adapter uses the independent
metadata library's typed Emit overloads and explicit local/dependency/System mappings.
Console literal calls retain distinct backend policy: .NET calls the bound CLI method;
neoCLR requires the explicit registered Console reference and emits its native mapping.
Imported CLI Unit results are discarded only when the actual CLI signature returns a
value; native no-result encoding is unchanged.

Normal .NET emission uses the shared body path for nongeneric static Int32 source
methods in release mode without requested PDB output. Complete lowering happens before
opening a builder, so unsupported bodies use the existing general generator without
partial IL. Debug/PDB, generic, synthesized and other methods retain their prior path.
This is incremental codegen reuse, not a new independent .NET compiler. The current
native Int32/Unit source subset is unchanged and still reports unsupported constructs.

Compared with Reflection.Emit's direct Type/MethodInfo/OpCode use, the common plan lets
both producers reuse language-level body decisions while keeping handle resolution and
encoding in adapters. Its cost is a small per-body plan allocation; no performance gain
is claimed. Type declarations, fields, method signatures, generic constraints, locals,
branches, exception regions and debug positions still need equivalent abstractions.
The established .NET declaration generator remains responsible for type/method creation,
attributes, type completion and PE writing. Broader metadata builder unification remains
open; this slice abstracts executable method bodies, not the complete TypeBuilder API.

Validation includes C# .NET release/debug execution, unchecked Int32 arithmetic, calls,
source-located rejection with general-generator fallback, and release PDB sequence points.
The executable probe emits one compilation through both backends: both print Shared Hello
and return 42 after a helper call. Existing native Hello/Unit/library/reference failures,
namespace cases and compiler-command execution pass. In the additional translated-System
run, the direct API case binds the selected projection and executes to 42, but the driver
binds the colliding host System.Math and rejects emission with NEOMETA001 (unregistered
System.Private.CoreLib dependency). Earlier driver runs passed; selection is not a reliable
contract. The author explicitly defers metadata loading to a future slice. No assembly-name
trick, reordered-reference workaround or silent native call substitution is added here.
The bounded Hello/helper case is the acceptance target for this codegen slice.
The shared implementation is a general shared-line candidate; reconcile it separately
from the native adapter when integrating the consumer branch into main.

### Shared Unit functions and entry points — 2026-09-30

The existing shared linear-body path now also serves eligible release-mode .NET
assembly-level functions and Unit-returning static methods. A Unit method enters this
path only when its already-created CLI method signature returns `System.Void`; a
value-bearing Unit representation stays on the general generator. This reuses existing
.NET declaration/signature construction rather than adding a second builder hierarchy.
The same immutable plan handles helper calls, explicit/implicit return and empty bodies.
Debug/PDB, captures, generic methods and unsupported bodies retain the established path.

The end-to-end probe emits each compilation through both backends. The Unit function
and static-method cases print Shared Hello and exit zero; an empty Unit entry also runs
on both runtimes. The Int32 helper case still returns 42. C# tests additionally verify
void signatures, release/debug behavior, assembly-function arithmetic and preserved PDB
sequence points. Native format/API and Runtime Contract configuration are unchanged.
Compared with CLI void, native no-result remains a backend representation choice; the
common lowering does not manufacture a Unit value or erase a value-bearing CLI result.
Metadata loading and the recorded optional System-driver collision remain deferred.

### Shared callable declarations — 2026-09-30

The supported Int32/Unit signature is now represented once in the compiler by
`Int32CallableSignature`. Declaration and body validation use that same description.
`ICallableDefinitionBuilder<TMethod>` returns a backend-owned method handle without
requiring native builders to inherit from Reflection.Emit's MethodInfo/MethodBuilder.
Both contracts are internal implementation details, not public metadata APIs.

The .NET adapter defines eligible static ordinary methods and functions on the existing
Reflection.Emit TypeBuilder. It retains the existing emitted name and method attributes,
resolves Int32/void through the compiler's target-aware resolver, and leaves parameter
names, custom attributes and other declaration bookkeeping with MethodGenerator.
Generic, captured, extension, extern and richer signatures keep the established path.
The native adapter defines either a method on a metadata TypeBuilder or an assembly-owned
function. Existing visibility/capability checks and metadata-library bounds remain in
force; unsupported metadata is not silently discarded. No native format change occurs.

Compared with the direct .NET builder calls, this extracts the common callable signature
while preserving differing ownership and handle models. CLI functions still use their
existing carrier types; native functions remain assembly-owned. The native subset still
lacks parameter-name/custom-attribute support, while the .NET path preserves it. Full type
construction, generics and richer signatures are subsequent work, not hidden behind this
small contract. No loader redesign or reference-selection workaround is included.

Validation covers public/private method flags, parameter names and types, Int32 and void
results, executable calls, generic fallback, selected System.Runtime core references
without host CoreLib leakage, and PDB preservation. Existing same-compilation .NET/native
Hello/helper cases and rvnc runtime checks validate the two concrete declaration adapters.
This compiler refactor remains a general shared-line candidate pending consumer-branch
reconciliation; native policy stays in the optional adapter.

## Compiler-lowered native bodies — 2026-10-01

The shared linear instruction planner now consumes `BoundTreeView.Lowered`, replacing
its source `IOperation` traversal. Both eligible release .NET methods and the optional
native backend use the existing compiler Lowerer before backend instruction encoding.
Implicit Int32 returns now work without a second return-rewriting implementation;
simple named calls whose lowered arguments fit the subset also work. Static qualified
calls treat a bound type receiver as a qualifier, not a runtime value.

Runtime Contract selection, semantic binding and the temporary CLI reference projection
are unchanged. .NET retains its carrier types and Reflection.Emit adapter; neoCLR emits
assembly-owned functions through the separate metadata API and loads PE/#Neo directly.
The format and runtime need no changes for this slice. Native debug output, general
signatures, locals/control flow and synthesized bodies remain unsupported; .NET debug,
PDB and unsupported bodies keep general codegen. Console permission/identity checks
remain target-owned. Metadata importer work and the known System driver collision stay
deferred. This internal refactor is pending shared-line reconciliation.

Validation: 19 focused C# compiler tests cover execution, implicit returns, named Unit
calls, fallback, PDB preservation and selected core identity. The metadata probe covers
same-compilation .NET/native Hello/helper/Unit execution and implicit Int32 returns,
plus native verification/loading, dependency failures, diagnostics and rvnc behavior.

See [the staged codegen migration](architecture/native-target-codegen-migration.md).

[Recorded binary/runtime probe evidence](../../tools/NeoClrMetadataProbe/validation.json).

## Callable identity resolution — 2026-10-01

The bounded .NET and neoCLR emitters now use a shared `CallableReferenceTable<THandle>`.
Compiler method symbols are the target-neutral identities; symbol equality preserves
owners, overloads and assemblies instead of relying on names. Each emission owns its
table and backend handles. Native definitions are registered before bodies; unresolved
references go through the existing explicit dependency/System import policy. Failed
resolution is not cached. Native local, imported CLI-projection and selected native
function handles are encoded inside the native backend, without casting between them.

The .NET adapter resolves through the existing metadata-proxy/runtime resolver and
caches only within its CodeGenerator. This preserves the CLI carrier representation of
free functions; native functions retain assembly ownership. Runtime Contract selection,
binding, metadata format and runtime loading are unchanged. No reference is shared
across emissions or compilations. Broader Reflection-dependent mappings, type/field
references and declaration traversal are still pending; this is the first callable
reference boundary, not completion of the general backend refactor.

Compared with the previous native per-call declaration-list search, the table centralizes
symbol identity and separates call encoding from common resolution bookkeeping. The cost
is a per-emission dictionary and backend handle objects; no execution-speed claim or
benchmark is implied. Focused C# runtime tests and the dual-runtime probe cover repeated
and forward calls, overloads and identical names across assembly functions and types.
Existing checks retain core identity, PDB/fallback, dependency diagnostics and failed-output
contracts. Native symbol loading and its known driver collision remain deferred.

Validation: 20 focused C# tests passed; [runtime and driver evidence](../../tools/NeoClrMetadataProbe/validation.json).

## Shared source callable plans — 2026-10-01

`SourceCallablePlan` now carries the supported source method symbol, declaration/body,
logical owner, metadata name and Int32/Unit signature. Both .NET method declaration and
bounded body emission consume that plan; native codegen consumes it for definition and
body emission too. An assembly-level function has no logical type owner even though its
CLI symbol may belong to a compiler-generated carrier. The .NET adapter retains its
chosen emitted name, owner, attributes and target-aware type resolver.

Native emission now collects and validates all supported source declarations first,
then creates type/callable definitions, registers references, and emits bodies. Empty
static types remain declarations even when they have no callable plans. The native
syntax/capability validator remains adapter-owned; it still rejects unsupported attributes,
visibility, namespace functions and richer type contracts rather than dropping metadata.

Compared with the previous inline builder creation, the plan separates source semantics
from backend lifetime/ownership and gives both adapters one callable definition/body
contract. The cost is an additional immutable source-plan object per eligible callable.
This does not unify the general .NET declaration traversal: synthesized methods,
accessors, state machines and unsupported signatures retain their established path.
General type/field handles and traversal remain later work. Runtime Contract and semantic
binding selection, binary format and the native runtime are unchanged. The public
metadata API remains a separate project, with no new public API in this slice.

Focused C# coverage checks source ownership and executable .NET output. The native probe
inspects actual metadata ownership, preserves an empty static type, and verifies/runs
the mixed assembly-function/type-method program. Existing overload, forward-call,
core-reference, diagnostic, PDB and rvnc cases remain part of focused validation.

Validation: 21 focused C# tests passed; [native metadata/runtime and driver evidence](../../tools/NeoClrMetadataProbe/validation.json).

## Shared static type plans — 2026-10-01

Both backends now consume `SourceStaticTypePlan` through a typed type-definition
builder contract for top-level public nongeneric static classes. The plan retains the
compiler symbol for owner lookup and a shared namespace/metadata-name mapping. The
native adapter stores these plans during declaration collection and creates native type
handles afterward. Empty static types still get definitions. Callable owner lookup uses
symbol equality, not a display name.

The .NET adapter creates its TypeBuilder with the existing TypeGenerator flags; base
resolution, custom attributes, members and completion remain in the existing generator.
Generic, nested, nonpublic and instance types keep the general .NET construction path.
Native syntax/capability validation still rejects unsupported contracts; the plan does
not silently approximate bases, interfaces or attributes. Runtime Contract selection,
reference binding, metadata format and runtime loading are unchanged.

Compared with direct Reflection.Emit and native AddType calls, this shares declaration
identity while keeping backend handles and type completion distinct. It costs a small
source plan/adapter allocation and remains a bounded type-definition contract, not a
general type-system or type-reference abstraction. The earlier paused prototype has been
adapted to retain symbol identity and preserve the .NET generator's computed attributes.
General field/type references and declaration traversal remain pending; metadata loading
is still deferred.

Focused C# tests check two same-named classes in distinct namespaces, public/abstract/
sealed flags, base types and executable calls, plus generic/nested/instance fallback.
The native probe retains empty types, namespaced library cases in both file orders,
assembly/type ownership, and actual runtime/driver loading and execution.

Validation: 23 focused C# tests passed; [native runtime and driver evidence](../../tools/NeoClrMetadataProbe/validation.json).

## Int32 locals and assignment — 2026-10-01

The shared lowered-body path now admits initialized Int32 local declarations, local
reads and standalone local assignments. Local symbol identity maps to slots before
backend emission; the .NET adapter creates locals with the selected target's Int32 type,
and the native adapter uses method-owned metadata local slots. No host type substitution
or source-operation rewrite is involved. The same Main/helper program with immutable
and mutable locals returns 42 on .NET and from a PE/#Neo assembly loaded by neoCLR.

The independent metadata library adds typed LocalDefinition handles and raw Ldloc/Stloc
operands, CLI local signatures and native format-5 local lists. Writes check ownership,
slot bounds, stack balance and stores before loads for the current linear body subset.
ClearBody retains declarations but resets initialization through revalidation. Older
producer artifacts with no locals remain readable; older experimental host readers may
reject newly written local lists. Runtime format-5 and binary transport schemas do not
change: neoCLR already implements these locals and instructions. Reference-only CLI
projections continue to omit executable body details.

Compared with .NET's general local/IL surface this is deliberately bounded to Int32 and
initialized declarations; arbitrary types, address-taking, control flow and debug local
scopes remain later work. The extra slot/initialization bookkeeping makes compiler and
assembler misuse fail at write time. Runtime Contract selection and the temporary symbol
loader are unchanged; native metadata loading continues directly in the runtime. See
the metadata API manual for the new public contract. General .NET debug/fallback emission
remains intact. No performance claim or runtime optimization is included.

Validation: 25 compiler tests, 33 metadata API groups and the API snapshot check passed; [runtime/driver evidence](../../tools/NeoClrMetadataProbe/validation.json).

## Shared comparisons and control flow — 2026-10-01

The shared body planner now consumes lowered labels/gotos and emits bound if statements
through the same backend-neutral instruction plan. Signed Int32 equality/less/greater
comparisons, Boolean constants, forward/backward branches and nested blocks support
ordinary if/else and while-loop consumers. Local symbol identities remain distinct
across lexical scopes. Disposal-bearing scopes, arbitrary Boolean/value signatures,
other comparison operators and exception regions remain outside the admitted subset.
The existing .NET general/debug/PDB path still handles unsupported bodies.

The independent writer adds method-owned BranchLabel handles and typed branch/Boolean
Emit overloads. A worklist validates stack types and definitely assigned locals across
joins and cycles; unmarked targets, incompatible stacks, path-dependent uninitialized
loads, reachable fallthrough and unreachable executable instructions reject before output.
CLI byte offsets and native instruction indices are computed separately, after native
Console expansion. Labels remain symbolic in the compiler and public writer API.

This reuses existing runtime format-5 operations; no runtime or binary schema change is
required. Compared with .NET/Reflection.Emit's broad branch surface, the metadata API
checks a bounded typed graph while retaining separate representations (native comparison
results are Boolean values). It costs graph state/initialization analysis during writing,
not a new runtime translation layer. Source syntax is not reparsed to implement loops;
the compiler Lowerer still owns loop rewriting. The historical LinearMethodBody name
now denotes this bounded body planner, including control flow, pending naming cleanup.
Runtime Contract selection, native symbol-loading deferral and explicit dependency
policy remain unchanged.

Validation: 27 compiler tests, 34 metadata API groups and API snapshot validation passed; [native runtime/driver evidence](../../tools/NeoClrMetadataProbe/validation.json).

## Negated comparisons and loop exits — 2026-10-01

The shared body plan adds `!`, `!=`, `<=` and `>=` for the admitted Boolean/Int32
conditions. Negation emits Boolean false plus equality on native bodies; .NET uses its
CLI Boolean stack representation. The metadata writer now accepts Ceq for matching
Boolean operands as well as Int32, while rejecting mixed operand types. This matches
the existing runtime's typed equality rather than conflating native Boolean with Int32.
The signature/local subset remains Int32 with Unit/no-result methods.

A consumer combines a constant-true loop, comparisons, negation, continue and break;
loop exits reuse the labels/gotos produced by the existing Lowerer. No source-level
loop rewrite or runtime change is added. Both runtimes execute the same source and
return 42. Short-circuit expressions, general Boolean signatures/locals and exception
regions remain separate capability work. Runtime Contract and reference loading remain
unchanged, and unsupported .NET bodies retain general emission.

Validation: 29 compiler tests, 34 metadata API groups and the API snapshot check passed; [native runtime/driver evidence](../../tools/NeoClrMetadataProbe/validation.json).


## Primitive callable signatures — 2026-10-01

The shared callable contract now carries ordered Int32/Boolean parameter types and
Int32/Boolean/no-result return types. .NET resolves each through its selected core;
neoCLR maps them to the independent metadata API's immutable primitive signatures.
Overload resolution/import matching uses parameter types, not just parameter count.
Runtime Contract selection and ordinary .NET defaults are unchanged.

Compared with CLR Boolean signatures, native metadata preserves the same source type
identity while validating Boolean evaluation-stack values distinctly from Int32.
No implicit Boolean/integer conversion is introduced. Native entrypoints remain
parameterless Int32/Unit. Locals and selected System inventory imports remain Int32-only.
The CLI declaration projection remains a temporary semantic-loader bridge: it carries
primitive declarations but no executable native body. Native semantic import, broader
types/conversions, fields/instances and complete target composition remain pending.

Validation: 31 focused C# compiler tests, 35 independent C# metadata contract groups,
and the native probe cover same-source execution on both runtimes plus separately
compiled Boolean library imports and same-name/same-arity Boolean/Int32 overloads.
The binary assemblies are verified and run by neoCLR. General changes remain shared-line
candidates on the consumer branch until independently integrated.


## Typed primitive locals — 2026-10-01

The shared lowered-body plan now carries each local's primitive type. .NET resolves
its selected core Int32/Boolean type; neoCLR declares the matching typed metadata
slot. Boolean predicate results can be stored, reassigned, loaded and compared for
equality/inequality. Both backends share source lowering and instruction planning.

The existing Runtime Contract and CLI symbol projection remain unchanged. Compared
with CLI's integer evaluation-stack representation, the native writer enforces a
separate Boolean stack type; stores must match their declared local type. The cost is
explicit type validation. No implicit conversion, uninitialized local, disposal,
nonprimitive local or new System inventory contract is introduced. The .NET general
fallback remains in place. Native metadata/backend replacement of the temporary
symbol projection is still pending.

Validation adds a predicate-local program on both runtimes, C# Release/Debug coverage,
and metadata contracts for reflected CLI local types, native projection and invalid
cross-type stores. All 35 metadata groups and 33 focused compiler tests pass.

This consumer also exposed the assignment parser bypassing logical negation on its
right-hand side. General fix `762bebad0` restores full expression parsing and retains
right-associative chains; 480 parser/assignment tests pass. This fix is independently
validated for the shared line, not a native representation workaround.


## Short-circuit Boolean expressions — 2026-10-01

The shared body planner emits built-in Boolean &&/|| with symbolic branches and a
Boolean stack value at the join. Operands are evaluated left to right; the right side
is skipped when the left determines the result. Nested expressions, local assignment
and value returns reuse this plan on both backends. Overloaded operators, nullable
logic and general conversions remain outside this subset.

Runtime Contract configuration and primitive CLI projection are unchanged. This
matches ordinary CLR Boolean short-circuit behavior; neoCLR uses its existing branch
instructions and distinct Boolean stack type. No native schema or runtime change is
required. The temporary projection is still owned by the metadata library and used
by the compiler's .NET semantic provider; native semantic import remains deferred.

Validation: 35 focused compiler tests, including Release/Debug skipped-operand cases,
and the dual-runtime native probe. Console side effects prove that precisely two of
five possible helper calls execute, while the program returns 42. Existing 35 metadata
contract groups cover the unchanged branch/Boolean encoding and validation.


## Statement-call result handling — 2026-10-01

The shared body planner now permits Int32/Boolean-returning calls in statement
position. It emits the call followed by a stack discard, preserving argument/call
side effects. No-result Unit calls emit no discard. The .NET adapter still handles
an imported inhabited Unit representation according to the actual CLI signature;
the native Console literal mapping retains its existing explicit Void-value discard.

Compared with CLR pop, the native metadata API's Pop has the same stack effect but
participates in native typed-flow validation. Empty-stack discards reject before
writing. The compiler shares result-use planning; each backend owns instruction
encoding. No native schema, runtime implementation, Runtime Contract configuration
or temporary reference-projection change is required. This removes a bounded emitter
restriction, not a language rule. Nonprimitive results remain outside the shared
subset; native semantic import and broader target composition remain pending.

Validation: 37 focused compiler tests, 35 C# metadata contract groups, and the binary
native probe cover local Int32/Boolean statement calls, no-result calls, imported
Int32 calls, preserved side-effect order and rejected pop underflow. Both .NET and
neoCLR execute the same source and return 42.


## Int64 primitives and signed conversions — 2026-10-01

The shared signature/body contract now includes Int64 parameters, results, constants
and locals. Int32→Int64 widening sign-extends; Int64→Int32 narrowing retains the low
32 bits. Existing compiler-bound numeric conversions select these operations; unsigned,
floating-point, checked and user-defined conversion support is not implied. Matching
Int64 arithmetic/comparisons reuse the shared operators. Mixed source arithmetic relies
on the binder's explicit operand conversions, not native stack reinterpretation.

.NET uses its selected core types and ordinary CLI integer opcodes. neoCLR uses the
independent metadata library's typed signatures, locals and existing native integer
operations. The metadata validator now tracks explicit primitive stack types rather
than Boolean tags; this costs a wider internal tag but preserves width at calls, local
stores and control-flow joins. No runtime or native schema change was required.

Runtime Contract configuration and the temporary CLI declaration projection are
unchanged. The metadata API owns the projection and preserves Int64 declarations;
Raven still binds them through the .NET semantic provider. Native semantic import is
pending. Entrypoints remain Int32/Unit and the selected System inventory stays Int32-only.
Older experimental host readers may reject Int64 declarations.

Validation: 41 focused C# compiler tests including integral-cast regressions, 36 C#
metadata contract groups, and the dual-runtime probe. Cases cover signed widening,
low-bit narrowing, long locals/arithmetic, extrema, an imported Int64 helper from a
separately compiled native library, and rejection of Boolean conversions/mixed widths.


## Signed unary integer operations — 2026-10-01

The shared body planner now handles built-in unary +, - and ~ for Int32/Int64.
Unary + evaluates its operand unchanged; negation and bitwise complement preserve
width. Logical Boolean ! remains separate. .NET uses its existing neg/not semantics;
neoCLR emits the corresponding native operations through the independent metadata API.
Negating the minimum signed value wraps to itself on both targets, matching the
existing general .NET emitter and native numeric implementation.

This slice changes no Runtime Contract configuration, symbol projection or metadata
schema. The writer checks integer operand type/stack presence before encoding. Checked,
unsigned and floating-point unary support is not implied. Native semantic metadata
import, broader types and full target composition remain pending. The CLI reference
projection stays temporary and metadata-library-owned.

Validation: 43 focused C# compiler tests and 37 independent metadata contract groups,
plus the native binary-assembly probe. Release/Debug cases cover both widths, extrema,
identity and complement. The same source executes on .NET and neoCLR, checks wrapping
at both signed minima and returns 42. Writer tests reject Boolean operands and underflow.


## Shared primitive type contract — 2026-10-01

Callable signatures and local declarations now carry EmissionPrimitiveType rather
than passing semantic SpecialType values to backend builders. Shared classification
admits Int32, Int64 and Boolean values and explicitly distinguishes NoResult. Unit/void
are normalized only in return position; Unit parameters/locals, nullable and other
unsupported types are not silently converted into no-result or primitive values.

Both declaration and body builders use IEmissionTypeMapper<TType>. The .NET mapper
resolves every type through the caller's selected-core resolver; it never substitutes
host typeof handles. The native mapper lives independently of the callable builder
and maps to the separate metadata library's PrimitiveType contract. Native local
emission no longer depends on the callable declaration builder for type mapping.

This is a bounded type boundary, not general nominal/array/generic type support.
Compared with passing SpecialType through each builder, the benefit is one shared
value/no-result admission rule and explicit target-owned representation mapping. The
cost is a small internal type vocabulary and mapper implementation per backend. CLR
Reflection.Emit handles remain in its adapters; native metadata handles remain in
its adapters. Ordinary .NET behavior and Runtime Contract configuration are unchanged.
The temporary CLI declaration projection and deferred semantic import are unchanged.

Validation: 44 focused C# compiler tests include rejection of unsupported signature
shapes and selected-core inspection of Int32/Int64/Boolean locals and callable
signatures. The existing native probe exercises both mappers by executing all supported
primitive cases on .NET and binary assemblies loaded by neoCLR. The metadata format
and API did not change; the prior 37 metadata contract groups remain applicable.

## Partial static declarations — 2026-10-01

The experimental native backend accepts public nongeneric partial static classes.
Raven's existing binder supplies one type symbol; native declaration collection now
coalesces that identity before creating a metadata type and collects methods from
every part. Empty parts do not add definitions. All parts retain capability checks;
an unsupported property/field/member rejects emission at its source location before
writing output. Partial methods and general instance/generic types remain unsupported.

No Runtime Contract configuration or semantic binding changes: ordinary .NET emission
remains the default, while native emission uses the explicit backend override/rvnc
neoclr command and the existing primitive bootstrap. Both targets erase source-only
partial boundaries into one type. Native format 5 and its temporary CLI reference
projection need no new encoding; native symbol-provider replacement remains deferred.
The independent metadata library and runtime loader are unchanged.

C# PartialTypeChecks exercises cross-part overload calls, an empty part, both file
orders, one projected type with three methods, and rejection in either file order.
The same source compilation executes to 42 on .NET and from binary PE/#Neo in neoCLR.

## String values and computed console output — 2026-10-01

The bounded shared emitter now carries String literals, parameters/results, initialized
locals, assignments, calls, discarded results and control-flow joins. It preserves
binder/lowerer ownership of language semantics. Both backend type mappers recognize
String; .NET resolves the configured core type instead of substituting host typeof.
Native metadata uses String signatures and existing ldstr instructions. Separately
compiled native libraries project those signatures for Raven's existing importer.

Console policy remains explicit: the registered Console reference's one-string
WriteLine overload can consume a supported expression, rather than only a literal.
The shared plan emits the argument first; .NET calls its resolved method, while the
native adapter uses the metadata API's stack-consuming WriteConsoleLine and discards
bundled System's inhabited Void result. Other overloads are not implicitly mapped.

Runtime Contract configuration is unchanged. Ordinary .NET remains the default;
native output still requires the experimental backend/rvnc neoclr and hosted primitive
binding. The independent metadata library owns encoding and validation, and the
existing runtime loads/executes the binary payload without schema changes. CLI reference
bodies still throw; they are not executable translations of native method bodies.

This is built-in text support, not general reference/nominal type support. Native
null literals/nullable strings, string equality, concatenation and instance members
remain unsupported; ordinary .NET falls back to its established generator. Native
writer literals must be valid Unicode within 64 KiB UTF-8, rejecting unpaired UTF-16
surrogates rather than replacing them. No cross-target interning guarantee is made.

Validation covers C# metadata contracts, Debug/Release .NET text helpers, selected-core
String signatures/locals, Unicode console output and an imported String overload
from a separately compiled binary library. See tools/NeoClrMetadataProbe/validation.json.

## Target-neutral emission contract — 2026-10-01

The author reaffirmed a shared abstraction that fits .NET and neoCLR without tying
the compiler to either metadata implementation. Keep symbol-based type/member
references, declaration plans and logical body operations in the compiler-owned
layer; concrete handles, metadata encodings and instruction selection belong to
target adapters. Additional instruction families and metadata categories should be
exposed through explicit target capabilities, with unsupported output diagnosed
before writing, rather than forcing one target's representation onto every backend.
The existing primitive mapper, callable table and bounded body planner implement
only part of this boundary; general declarations/types/fields and composable
capabilities remain work to do. Ordinary .NET behavior remains the default.

Standard CLI metadata and instructions remain the baseline for ordinary constructs,
with explicit neoCLR extensions. The current native payload plus throwing CLI
reference projection is a bridge, not interchangeable executable .NET/neoCLR
artifacts; reconciling that representation remains open. Transport readability
by .NET metadata tools does not establish executable compatibility.

The follow-up metadata Starg operation is not a new Raven source feature. Ordinary
parameters are immutable, and var/val modifiers outside primary-constructor promotion
are rejected by the current binder. A stale parameter-spec paragraph was corrected
to match those diagnostics. No compiler adapter or language workaround was added.

## Backend capability admission — 2026-10-01

The bounded planner now accepts an immutable, compiler-owned EmissionCapabilities
contract. Backend adapters explicitly list their logical instructions and built-in
types; newly added operations are not automatically enabled. Profiles are copied
once and reused. Signature, call, local and expression types are checked, and a
completed instruction plan is admitted before it is returned to a backend. Standalone
planner tests can omit a profile to inspect target-independent lowering; production
SourceCallablePlan body lowering requires an explicit profile.

Signed Int32/Int64 division is the first asymmetric case: shared lowering models it,
the .NET adapter selects standard div, and the native adapter reports NEOMETA001
with the division expression's source location because its metadata writer does not
yet expose that instruction. This restriction belongs to the current producer, not
neoCLR's language/runtime semantics. General .NET fallback remains available for
operations outside the planner; Debug/PDB keeps the existing generator. Native
emission preflights every body before allocating assembly/type/method builders.
Dependency binding and full writer validation still occur afterward, before output.

This adds no Runtime Contract setting or public target API. Ordinary .NET remains
the default and native emission still uses an explicit backend override with hosted
primitive/projection binding. Source binding and metadata formats are unchanged.
Console reference matching remains separate target-specific policy. General nominal
types, fields, metadata category capabilities and coherent target composition remain
open; this bounded contract is not a complete runtime feature inventory.

The static profiles avoid rebuilding capability collections per method. Preflight
retains all admitted native body plans until emission and adds instruction admission
checks; its memory/time cost is not benchmarked. The recorded phase/allocation
measurement remains necessary before claiming a performance improvement.

Validation: 48 focused C# tests pass, including signed division results/faults,
restricted profiles, selected-core types and ordinary fallback. The binary native
probe and rvnc command pass; native division reports its capability rejection at the
source expression and preserves output. Evidence: tools/NeoClrMetadataProbe/validation.json.

## Declaration-category admission — 2026-10-01

Backend-owned profiles now explicitly admit logical AssemblyFunction, StaticMethod
and StaticType categories as well as built-in types and instructions. Omitting the
category set admits no declarations. Source plans can still be collected independently
for analysis; production .NET/native declaration collection passes the target profile,
and body lowering rechecks callable admission. The profiles live with each backend
rather than its body emitter and are immutable snapshots reused across declarations.

These are source/semantic categories: .NET's assembly-function admission still emits
its existing carrier method, while native metadata retains assembly ownership. No new
physical CLI metadata category or format extension is introduced. Public nongeneric
static-type shape remains bounded; generic/nested/instance .NET definitions keep the
existing generator and are not claimed as shared categories. Visibility and broader
nominal/member categories still need explicit contracts.

Runtime Contract configuration, native backend override, hosted semantic binding and
the current CLI-reference/native-payload bridge are unchanged. The independent metadata
library is unchanged. Tests check category isolation, rejected plans, body-boundary
admission and copied configuration, with ordinary declaration/runtime regression
coverage and native binary/driver execution. No performance claim is made.


### Internal static helper emission — 2026-10-01

Both backend profiles admit Public/Internal top-level static types through the shared
source type plan. The native adapter maps logical accessibility to the independent
metadata builder's TypeVisibility; ordinary .NET retains its existing TypeDef flags.
No new Runtime Contract setting is required. Native writer output uses the runtime's
existing internal visibility and matching origin flag. The temporary CLI reference
projection preserves NotPublic so external source callers cannot access the helper.
The reference projection still has throwing bodies; native execution uses #Neo.
Public static methods, primitive signatures and existing body limits remain in force;
nonpublic methods, instance types and general metadata loading are subsequent work.
Validation covers internal helper execution on both targets, a separately compiled
native public facade using an internal helper, and rejected external helper access.

The external-access case exposed a general binder gap: qualified type expressions
could bypass the existing accessibility check. Both type-expression resolution and
namespace receiver lookup now use that check. This is ordinary .NET semantic behavior,
not a neoCLR mapping. A separately emitted .NET library/consumer regression covers
internal rejection and public acceptance. This independently validated fix is a
candidate for the shared compiler line; its inclusion in this integration branch does
not make it an experimental language rule.

Validation: 90 focused codegen/accessibility tests and the binary runtime/driver probe.
A deliberately emitted external call to the internal type is rejected by native
verification with `type access denied`, independently of Raven's source diagnostic.


### Native signed division — 2026-10-01

The native backend now admits the existing shared Divide operation for matching
Int32/Int64 operands. The independent metadata API validates the typed stack and
writes CLI div or native div. Results truncate toward zero; zero divisors and the
minimum signed value divided by -1 fault during execution. No new Runtime Contract
option, signature encoding or runtime instruction is introduced. This supersedes
prior native-division rejection; restricted-profile tests still prove selective
admission. The adapter rejection probe now uses unsupported shifts. Unsigned and
floating operations remain outside the bounded writer. CLI reference bodies remain
placeholders and native bodies remain in #Neo; metadata importer redesign is deferred.

Validation: 36 focused shared-body/capability baseline tests remain applicable (no
shared planner change); the complete native binary/rvnc probe passes, including four
zero/overflow fault cases and signed quotient execution against .NET. The paired
independent writer passes 41 C# contract groups and its API snapshot check.


### Shared signed remainder — 2026-10-01

Int32/Int64 '%' now passes through the shared lowered-body planner, both capability
profiles and both instruction adapters. The independent writer supports Rem and the
Remainder helper using existing CLI/native rem encodings. No Runtime Contract option,
binder rule or metadata category is added. Ordinary results keep the dividend's sign;
zero divisors fault. Native minimum/-1 faults match the tested CLR; that CLR edge is
platform-sensitive and universal host equivalence is not asserted. The .NET Debug
fallback remains tested. Unsigned/floating arithmetic, exceptions and broader metadata
loading remain future work. CLI reference projection/#Neo limitations are unchanged.

Validation: 38 focused shared-body/capability tests pass, covering Release shared
emission and Debug fallback. All 42 independent metadata C# contract groups pass.
The binary runtime/rvnc probe passes both result programs and eight Int32/Int64
zero/overflow fault programs across division and remainder. Every faulting native
assembly first passes verification, then faults during execution as intended.


### Integer bitwise emission — 2026-10-01

The shared lowered-body planner and both backend profiles now admit matching Int32/
Int64 AND, OR and XOR. Each adapter selects its existing instruction encoding; no
Runtime Contract configuration, binder rule or signature format changes. The separate
metadata library adds And/Or/Xor opcodes and BitwiseAnd/BitwiseOr/BitwiseXor helpers,
with typed stack validation. Negative values retain their fixed-width bit patterns.
At that integer-only checkpoint, Boolean/enum bitwise operations remained outside the producer; .NET's
general path retains its existing support. The CLI projection/#Neo bridge is unchanged.

Validation: 41 focused shared-body/capability tests, 43 independent C# metadata
contract groups and the full binary native/rvnc probe pass. The paired bitwise case
covers all three operators at both widths, sign bits and wide values.


### Shared signed shifts — 2026-10-01

The bounded planner now handles Int32/Int64 << and signed >> with an Int32 right
operand. Both profiles explicitly admit the logical operations; adapters select
shl/shr. The independent writer validates the distinct value/count types and exposes
Shl/Shr plus ShiftLeft/ShiftRight helpers. No Runtime Contract setting or binder
rule changes. Ordinary .NET retains raw CLI shift behavior, including unspecified
out-of-range counts; neoCLR retains its existing width-masked count rule. This does
not promise a new portable language rule for negative/oversized counts. In-range
counts, discarded bits and sign extension are tested on both targets, including
Release shared emission and Debug fallback. Unsigned right shifts and native-sized
integers remain outside the producer. The reference projection/#Neo bridge remains.
Unsupported floating conversion now supplies adapter/fallback rejection tests because
integer shifts are supported.

Validation: 45 focused shared-body/capability tests pass, including Release and Debug
shifts. All 44 independent C# metadata groups and the API snapshot check pass. The
full binary runtime/rvnc probe executes the paired shift program to 42 with zero,
31/63-bit boundary counts, sign extension and discarded high bits; unsupported
floating conversions retain precise diagnostics and unchanged output.


### Static method visibility — 2026-10-01

Shared callable plans now carry declared access and require explicit method-visibility
admission from each backend. The native backend supports public/internal/private static
methods, mapping them through the independent metadata API to CLI Public/Assembly/Private
and existing native public/internal/private access. The .NET adapter retains its existing
source and physical carrier attributes; ordinary .NET behavior remains the default.
No Runtime Contract setting is added. Assembly functions retain their existing bridge
policy; protected/native instance methods and a native semantic importer remain deferred.

The temporary CLI reference projection preserves method access so the existing Raven
binder rejects inaccessible dependencies. Native verification independently checks the
actual target definition, including callers constructed directly with the metadata API.
This reuses CLR-style access semantics rather than introducing a new access model; native
ownership and encoding remain backend responsibilities. The shared contract adds no
per-call reference lookup or performance claim. The compiler adapter owns source admission;
the metadata library owns serialization and the runtime owns verification. Native symbol
loading will eventually replace the CLI projection without changing declared access.

Validation: 83 focused compiler tests pass. The complete binary native/rvnc probe
passes with the independent metadata library at `5ccc41e8` on
`codex/extended-cli-metadata`. Paired .NET/native execution returns 42; both source
orders of a separate library preserve private/internal access, with compiler and
runtime rejection of external callers. See
[recorded probe evidence](../../tools/NeoClrMetadataProbe/validation.json).


### Shared expression-bodied callable plans — 2026-10-01

Callable plans now retain either a source block or arrow clause. For arrow clauses,
the bounded body planner uses the same original bound block and compiler Lowerer as
the established .NET generator. Return conversions, Unit expression statements and
source mapping remain binder/lowerer responsibilities, not backend rewrites. Release
.NET emission can use this shared path; Debug and unsupported signatures/bodies retain
the general generator. No Runtime Contract setting, public compiler API, opcode or
metadata format changes. Native source admission follows in a separate slice.

Validation: 48 focused C# declaration/shared-body/expression-body tests pass. New
Release/Debug cases inspect shared planning and execute Int32/Int64/Boolean/String
results, implicit widening and Unit calls; existing expression-body regressions pass.


### Native expression-body emission — 2026-10-01

Native source admission now accepts block or expression bodies for existing top-level
functions and static methods. Both forms use the shared callable plan and existing
compiler lowering. No metadata API/opcode/schema or Runtime Contract configuration
changes are needed. CLI reference projections keep declarations and throwing bodies;
native #Neo bodies retain existing instructions. Native semantic loading remains the
future replacement for the projection. Generic, async, instance and unsupported body
operations remain outside this producer; unsupported arrow expressions retain their
source span and leave output untouched.

Validation: 48 focused compiler tests pass. The complete binary runtime/rvnc probe
passes with metadata `5ccc41e8` on `codex/extended-cli-metadata`, including paired
.NET/native arrow entry/helper calls, primitive results and widening, Unit console
output, separate-library methods in both source orders, and precise unsupported
conversion rejection. [Probe evidence](../../tools/NeoClrMetadataProbe/validation.json).


### Eager Boolean operators — 2026-10-01

The shared planner now admits built-in Boolean &, | and ^ with matching Boolean
operands. Existing backend instruction capabilities and adapters select and/or/xor;
left and right expressions are evaluated in source order. This preserves existing
Raven/.NET eager semantics; && and || retain their separate short-circuit lowering.
No Runtime Contract option, binder change, new instruction or metadata category is
introduced. Enum/nullable Boolean and user-defined operator support remain outside
the bounded producer. The metadata API preserves exact Boolean stack types, and
native verification/runtime require Boolean bit-operation support (`fa25609d` on
`codex/extended-cli-metadata`); older runtimes reject these operands. CLI reference
projection and native importer replacement remain unchanged. There is no performance
claim; no synthetic conversions or branch expansion are required.

Validation: 52 focused C# shared-body/capability/declaration tests and all 45 independent
metadata test groups pass. The full binary runtime/rvnc probe passes against metadata
`83200ad6` and runtime `fa25609d`, including all Boolean truth tables and left/right
console markers for each eager operator. Existing short-circuit cases still pass.
[Recorded probe evidence](../../tools/NeoClrMetadataProbe/validation.json).


### Primitive conditional values — 2026-10-01

Shared body planning now accepts value-producing if/else with Boolean conditions,
matching Int32/Int64/Boolean/String branches and single-expression branch blocks.
It emits existing branch instructions with one value at the join; only the selected
branch executes. The binder owns expression context and type conversions. This is
ordinary conditional control flow on both .NET and neoCLR, with no new Runtime
Contract setting, metadata extension or runtime instruction. Unit/missing-else values,
nonprimitive joins and multi-statement value blocks remain outside this slice; .NET
retains its general fallback. CLI reference projections and the native importer
replacement remain unchanged. No performance improvement is claimed.

Validation: 32 existing shared-body/block-expression tests and both new Release/Debug
conditional tests pass. The complete binary runtime/rvnc probe passes against neoCLR
`83200ad6` (runtime code `fa25609d`), including all primitive joins, nested values and
skipped faulting/side-effecting branches. [Evidence](../../tools/NeoClrMetadataProbe/validation.json).


### Local computation inside value blocks — 2026-10-01

Primitive value blocks now permit initialized locals, local assignments and supported
calls before the final expression. The shared statement path owns those operations,
including discarding call results; each branch keeps distinct symbol-based local slots.
Only the chosen branch executes and writes its outer locals. Disposal and nonlocal
control flow inside value blocks remain explicitly rejected; this is a bounded native
producer limitation, not a Raven language restriction. Existing .NET fallback remains.
No Runtime Contract, metadata API/schema, runtime instruction or semantic importer
change is introduced. Both adapters use existing local/branch stack validation, with
ordinary CLI behavior and native execution payload/reference projection unchanged.

Validation: all 52 focused shared-body/block-expression/capability C# tests pass. The
full binary runtime/rvnc probe passes, including both local-computation branches,
outer assignments, discarded calls and unsupported prefix-loop rejection with no
output writes. [Recorded evidence](../../tools/NeoClrMetadataProbe/validation.json).


### Internal control flow in value blocks — 2026-10-01

Value-block prefixes now reuse shared statement emission for lowered if/else and
loops, including breaks/continues targeting labels inside the block. A preflight
walk checks statement blocks and discarded block expressions before emission: returns
and jumps outside the value block are rejected. This prevents an exit from bypassing
completion of an enclosing expression with operands already on the stack. Pure Unit
expression statements are no-ops. Disposal remains unsupported. General .NET fallback
is unchanged; source semantics and loop lowering remain compiler-owned.

The ordinary CLI and native adapters use existing local/branch instructions and
stack joins. No Runtime Contract setting, native metadata API/schema or runtime change
is needed. The temporary CLI reference projection/native importer boundary remains.
The control-flow scan adds planning work; no throughput or allocation improvement is
claimed. Extending exits requires an explicit enclosing-expression stack contract.

Validation: 55 focused C# shared-body/block-expression/capability tests and the full
binary runtime/rvnc probe pass. The new case retains an earlier operand through
loops, internal break/continue and conditional assignments. A conditional return
inside a value block rejects the shared plan and leaves native output untouched.
[Recorded evidence](../../tools/NeoClrMetadataProbe/validation.json).


### Assembly-function access — 2026-10-01

Shared capabilities now admit function visibility independently from type/method access.
Native declaration emission preserves public/internal source access through the separate
metadata API; default internal functions are no longer widened to public. Explicit
public/internal modifiers are accepted, including library helpers. Private ownerless
functions remain unsupported because native private access requires a declaring type.
Ordinary .NET retains its established carrier/visibility policy. No Runtime Contract
setting or binder rule changes; console entry selection can name an internal function.

This corrects an earlier bridge information loss: callers relying on accidentally
public default functions may now fail native verification. The native owner remains
absent, while CLI projection uses global methods with Public/Assembly access flags.
The updated reader/writer must be paired; older bounded readers reject internal globals.
The compiler owns access admission, the metadata library owns encoding, and the existing
runtime checks resolved module identity. Public static facades support library consumers;
direct Raven source import of projected globals remains deferred to metadata-loader work.
No synthetic native owner or runtime instruction is added, and no performance gain is claimed.

Validation: 51 focused Raven C# tests, 46 independent metadata groups and the full
binary runtime/rvnc probe pass with metadata `03472bef`. Libraries in both source
orders retain public/internal/default-internal function access. A separate consumer
calls a public static facade to 42; native verification rejects raw references to
the internal helper. Existing console entries still run with preserved internal access.
[Recorded evidence](../../tools/NeoClrMetadataProbe/validation.json).

## Class-library emission acceptance — 2026-10-01

The author prioritizes compiling actual Raven runtime-library source before broader
metadata loading, then using a broad consumer to drive missing codegen/metadata.
`NeoClrMetadataProbe --class-library-emission <runtime/raven/src> <fresh-output>`
now records source hashes, exact selected source, diagnostic phase and emitted byte
count. It uses the existing host-core primitive bootstrap; no Runtime Contract or
production target configuration changes. Ordinary .NET emission is unaffected.

The first run attempts unchanged Math, UnicodeScalar and GC files. They stop in
binding because native Result/error/RuntimeServices dependencies are absent. This
is not evidence that their bodies or metadata are supported. Selecting the original
Int32 Min/Max/Sign declarations with their System.Math namespace, excluding unrelated
imports/declarations, binds successfully and stops at NEOMETA001: native function
namespace metadata is missing. All failures leave the output empty. No runtime
execution or completed class-library assembly is claimed. The probe reports current
outcomes rather than asserting that unsupported features must remain unsupported.

Next: preserve namespace identity for native ownerless functions and define its CLI
projection explicitly, then emit and execute the selected real Math declarations.
Use neoCLR's order-collections application as the broader acceptance case: it spans
constructors/properties, generic collections/interfaces, arrays/iteration,
lambdas/delegates, Option/Result/patterns and shared reference identity. Compile its
actual library dependency sources as coverage grows; do not substitute fake library
contracts. JSON is a complementary UTF-8/inheritance case; neither sample covers
all language/runtime features. Metadata importer expansion remains deferred.

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

Validation: 53 focused C# tests and the full binary/rvnc integration probe pass, alongside the dedicated real Math execution checks.

## Order consumer frontier and shared property identity — 2026-10-01

The broad acceptance seed is neoCLR's
`docs/experiments/raven-target/samples/application-order-collections.rvn`.
`NeoClrMetadataProbe --consumer-emission <sample.rvn> <fresh-output>` now inventories
the unchanged full source and its exact global Order declaration. It records original
and selected hashes, selected source, diagnostic phase/count (first 32 messages),
and actual semantic members. It uses host-core references only; it does not replace
native collection/LINQ/union dependencies with stubs. Full-source binding errors are
not assertions about emission coverage. No Runtime Contract setting or target default
changes, and this inventory does not claim native object execution.

The isolated Order declaration binds with zero errors and reaches the native
nonstatic-class gate. It contains two instance properties, two backing fields, four
accessors and a constructor. Repeated binding exposed a general accessor/backing-field
identity bug, now fixed in the shared member binder and independently validated by
ordinary .NET execution. The producer must consume those canonical symbols, not
filter duplicate names as a backend workaround.

Next implementation sequence: shared nominal type/receiver references and nonstatic
type definitions; primitive instance fields and constructor/accessor method contracts;
property-to-accessor associations; then allocation, constructor calls and instance
field access. Use the selected real Order declaration plus creation/mutation/aliasing
checks on both targets. Preserve ordinary CLI Field/Property/MethodSemantics concepts
where applicable; add explicit native mappings behind target capabilities. Do not
strip source properties into an ad hoc field-only contract. Generic collection and
union/delegate coverage follows that first object case; broad native symbol importing
remains deferred.

Validation: 49 focused property/binding C# tests pass, including the independently failing identity regression and .NET execution. The unchanged consumer inventory now enumerates exactly nine Order members.


## Shared root and instance declaration contracts — 2026-10-01

SourceTypePlan replaces the static-only type plan and exposes separate StaticType and
RootClass categories. The bounded root shape is a public/internal nongeneric top-level
source class with System.Object base, no interfaces, records, abstract or closed
hierarchy semantics. SourceCallablePlan distinguishes InstanceMethod from StaticMethod
and admits nonvirtual ordinary methods of those roots with primitive declared
signatures. Capability admission remains explicit at declaration and body boundaries.
The receiver is not a declared parameter: shared argument slots shift by one only for
instance bodies. Instance calls, constructor/accessor bodies, object locals and field
operations are not admitted by this change.

The .NET adapter uses shared root and method definitions while retaining TypeGenerator's
physical flags/base policy and existing Reflection.Emit method attributes. Release
primitive instance bodies use shared lowering; Debug and unsupported bodies retain
the general generator. No syntax, semantic-model API or Runtime Contract configuration
changes. Ordinary .NET remains the default target. This general abstraction is not a
structural Function experiment.

Native adapters now map root and instance definitions to the separate metadata API's
AddClass/AddInstanceMethod. The native admission profile intentionally still rejects
source root classes until constructor and complete member-body contracts are supported;
it must not silently omit constructors, fields or properties. Thus Order's native
source emission remains a gap despite metadata API object execution. The temporary
PE/#Neo native-body/CLI-reference bridge is unchanged; broader native metadata/backend
support will replace it, not a new CLR-specific shared contract.

Validation: 54 focused pre-change tests and 56 post-change tests, including independent
Release/Debug .NET construction and Int32/Int64/Boolean/String instance results, target
capability denial, canonical Order property symbols and existing shared primitive bodies.
The native probe rebuilt against the matching metadata library and passed its
existing supported emission cases; this is not native instance source coverage.


## Unchanged Order source executes — 2026-10-01

The source class gate above is now opened for a bounded root object contract:
public/internal nongeneric top-level roots without base lists, interfaces or additional
type contracts; explicit primitive constructors with block bodies and no chaining;
mutable primitive instance auto-properties without initializers; and ordinary primitive
methods. Every constructor must be represented. Implicit constructors, readonly/custom/
indexed/static properties, user attributes and field/property initializers still reject
before output writes. Explicit field declarations and nominal signatures/locals are
not supported in this producer path yet.

Shared logical bodies now carry receiver, field-load/store, instance accessor-call and
new-object operations using compiler symbols. Auto-accessors reuse
Compilation.TryGetSynthesizedMethodBody and its lowered bound body; no native-only
getter/setter body synthesis was introduced. Root constructor assignments reuse the
normal lowered body; native roots require no base call. The existing .NET constructor
path keeps its base initialization. .NET Release auto-accessors consume the shared
field instructions; Debug keeps the general path. Physical field handles, constructor
handles and accessor calls stay in backend adapters, with capability admission before
builder allocation. .NET uses callvirt for accessor receiver checks; this native subset
only constructs non-null receivers or loads self. General nullable object calls remain
outside it.

The native adapter preserves canonical property/backing-field/accessor identities,
private mutable backing storage, property associations, primitive signatures and source
access. Synthesized .NET debugger/compiler-generated annotations are not projected by
the bounded metadata library. The separate library owns CLI Field/Property/MethodSemantics
projection and native references. No Runtime Contract configuration changed. The native
PE/#Neo execution payload and throwing CLI reference projection remain the temporary
bridge; this does not claim native symbol import or ordinary CLI-body execution.

`NeoClrMetadataProbe --order-runtime <order-collections.rvn> <fresh-output> <neoclr>`
extracts the unchanged global Order declaration and adds a separate Main. Both file
orders execute Boolean cases and Int32 boundaries and return 42 on .NET and neoCLR.
The reference projection retains two properties, two backing fields and five methods.
The complete consumer still has 49 binding errors under host-core bootstrap, from
missing native library dependencies. Native runtime e8611966 needed no source changes.

Validation: 52 focused C# compiler tests (51 baseline), including shared auto-accessor
planning, restricted field-capability rejection and Release/Debug mutation of the real
Order shape. The executable probe verifies/runs both file orders and rejects implicit
constructors, property initializers and nominal locals without output. Existing native
emission regression probes also pass. Object locals and aliasing are the next gap.
[Runtime evidence](../../tools/NeoClrMetadataProbe/order-runtime-validation.json).


## Root object locals and aliasing — 2026-10-01

The next slice adds a shared local declaration containing either a primitive kind or
a nominal compiler type symbol. The logical plan does not carry System.Type or native
builder handles. Both adapters explicitly enable root-class locals; .NET resolves the
symbol to its existing CLR type, while neoCLR resolves a declared source type to the
independent metadata library's class handle. Local loads/stores reuse existing control
flow and definite-store validation. No boxing, erased Object fallback or Void sentinel
is used. Declaration capabilities alone do not imply nominal-local admission.

The metadata dependency must include root locals (212b422b or later). Its development
LocalDefinition.Type property is nullable, and ClassType identifies nominal slots.
Shared source signatures still use primitive parameters/results. Nullable, inherited,
external and generic object locals remain unsupported, as do uninitialized locals,
implicit constructors and property initializers. Ordinary .NET fallback remains for
unsupported shapes; no Runtime Contract option or semantic-model behavior changed.
Native PE/#Neo encoding still carries executable bodies and a throwing CLI projection;
full native symbol import is deferred.

The unchanged Order plus a separate Main now holds an object in two locals, writes
Number and Pending through the alias, and reads both through the original. Both source
orders return 42 on .NET and neoCLR e8611966. The probe retains its boundary cases and
metadata association checks and rejects nullable locals without output. 54 focused
compiler tests pass, including independent Release/Debug alias execution and a profile
that admits root declarations but denies nominal locals. Existing native emission
regression probes pass. The full consumer's dependency frontier is unchanged.
[Updated evidence](../../tools/NeoClrMetadataProbe/order-runtime-validation.json).


## Ordinary instance calls — 2026-10-01

Shared lowering now admits ordinary nonvirtual source instance calls, using the same
symbol reference table and InstanceCall adapter contract as accessors. It evaluates
the receiver once before arguments and preserves left-to-right argument evaluation.
Implicit self calls, private methods, nested calls, primitive results and no-result
methods use existing instance signatures. .NET retains callvirt receiver checking;
the admitted native receiver set is still constructed objects, self and owned locals.
Virtual dispatch, nullable receivers, imported instance methods and nominal signatures
remain outside this bounded path. There are no Runtime Contract or metadata schema
changes, and .NET fallback for other calls remains intact.

The Order executable probe now adds an independent Counter helper to test private
nested calls, Unit mutation and argument side effects on the same receiver. The Order
source remains unchanged, and no native library dependency is stubbed. Both file orders
return 42 on .NET and binary neoCLR. 56 focused C# tests pass (54 baseline), including
Release/Debug instance-call behavior; existing native emission probes pass. The next
larger contract is nominal parameters/results before generic consumer coverage.


## Private primitive storage — 2026-10-01

Native source collection now honors Raven's existing `private var` field-only
implementation: mutable primitive storage without an initializer becomes one private
instance field, with no property or accessor rows. The binder's canonical backing-field
symbol is the shared instruction operand. Qualified `self.field` reads now use the
same shared field-load contract as unqualified reads; backend adapters retain ownership
of field handles. Existing .NET semantics and the public semantic property symbol are
unchanged. No Runtime Contract setting, metadata API or schema change is needed.

The executable Order consumer uses private Counter storage, mutates it through private
calls, and reads it through `self.Number`. Both source orders verify/run to 42 on
.NET and binary neoCLR; projection checks require one field, zero properties and seven
real methods. Independent C# tests cover Release/Debug with both property and private
storage variants. Private storage initializers reject before output, alongside the
existing incomplete-contract fixtures. Readonly storage, explicit field declarations,
initializers, nominal signatures and broader type contracts remain later slices.
This follows CLI field representation; native field identity remains in the temporary
binary execution payload until the full native metadata/backend replacement.


## Accessible setters on val properties — 2026-10-01

General Raven binding now honors an accessible ordinary setter for assignment,
compound assignment and increment/decrement on a `val` property. Previously its
public read-only contract incorrectly blocked even an explicit private setter inside
the declaring class. The semantic symbol remains `IsMutable == false`; inaccessible
setters still reject outside writes, and init-only/constructor rules are unchanged.
This is a shared compiler correction, not a neoCLR-only relaxation. It needs no
Runtime Contract setting or metadata encoding change. 60 focused property binding,
property execution and setter regression tests pass on modern .NET. Native explicit
accessor emission is a separate follow-up slice.


## Computed properties and implemented accessors — 2026-10-01

Shared callable plans now admit primitive property expression bodies and explicit
get/set block or arrow bodies. The native collector preserves property associations,
accessor visibility and optional canonical backing fields, and body emission reuses
ordinary instance calls and field instructions. The .NET Release adapter uses the same
plans; Debug/general emission remains available. This adds no Runtime Contract option
or metadata schema: ordinary CLI Property/MethodSemantics rows remain the reference
representation, with matching properties in the native execution bridge. A computed-only
property does not acquire storage. The setter-binding correction above is a prerequisite
for invoking a private setter on `val` from its owner.

The separate Gauge consumer tests computed getters, block getters/setters, private arrow
setters and the `field` keyword, including constructor assignments and branch-dependent
mutation. Both source orders verify/run to 42 on .NET and binary neoCLR alongside the
unchanged Order declaration. CLI projection checks require two Gauge fields, four
properties and nine methods, including a private setter and a getter-only computed
property. Independent C# Release/Debug tests check runtime results and metadata shape;
the 60-test focused codegen set passes. Explicit accessor lists without bodies currently
reject before output; implicit auto-properties retain their earlier support. Initializers,
init-only accessors, indexers, virtual members and nominal signatures remain separate
contracts. Full native metadata/backend replacement and collection dependencies are
still pending. [Evidence](../../tools/NeoClrMetadataProbe/order-runtime-validation.json).


## Expression-bodied root constructors — 2026-10-01

The native collector now admits explicit root constructors with arrow bodies as well
as blocks. Existing callable plans and compiler lowering already describe these bodies;
there is no backend-specific expression rewrite. Primitive overload signatures, receiver
slots and new-object reference resolution are unchanged. Arrow assignment and Unit helper
calls preserve initialization effects. The .NET constructor generator remains responsible
for its normal base initialization; this change does not opt constructors into the .NET
shared-method fast path. No Runtime Contract setting, metadata schema or runtime change
is required. CLI constructors retain their existing encoding and the native payload uses
the existing constructor/call contract.

Two focused C# tests pass in Release/Debug, asserting shared planner admission, overload
metadata and execution. The 10-test expression-body/instance-declaration baseline passed.
The expanded Order probe executes both constructor overloads, including two argument calls
that mutate the same receiver; it verifies evaluation order and exactly-once execution.
Both source orders verify/run to 42 on .NET and binary neoCLR. Explicit base chaining is
rejected without output, alongside five existing unsupported-contract cases. Implicit
constructors, chaining, property/field initializers and nominal signatures remain open.
[Updated evidence](../../tools/NeoClrMetadataProbe/order-runtime-validation.json).


## Default constructors and primitive initialization — 2026-10-01

The shared FieldInitializationPlan enumerates canonical source fields and their bound
initializers in compiler member order. The .NET constructor generator now consumes this
helper; native constructor lowering prepends its lowered assignments before the body.
Root classes with a synthesized parameterless constructor receive the same plan with an
empty body. Mutable private storage and auto-property initializers are supported within
the existing primitive body capabilities. No Runtime Contract setting or metadata schema
changes. .NET base initialization stays with its constructor driver; native roots require
no base call. Chaining, lifecycle initialization blocks, readonly storage and primary
constructors remain separate unsupported contracts, rather than silently omitted effects.

44 focused constructor/property/expression tests pass, including independent Release/Debug
initialization tests. The Order executable probe checks implicit initialization and
initializers preceding explicit constructor mutation; both source orders verify/run to 42
on .NET and binary neoCLR. Existing unsupported chaining/accessor/nullable fixtures reject
without output. [Evidence](../../tools/NeoClrMetadataProbe/order-runtime-validation.json).


## Owned nominal callable signatures — 2026-10-01

Shared CallableSignature/EmissionType now describe primitive and owned root-class
parameters/results without Reflection.Emit or metadata-library handles. Both adapter
profiles explicitly admit root-class signatures; the capability defaults to false for
other profiles. The .NET adapter resolves CLR types, and the native adapter resolves
predeclared TypeBuilder handles through the new independent MethodSignature API
(neoCLR `b2333489`). Constructors can accept those nominal parameters too. Primitive
import matching remains a separate restricted contract. No Runtime Contract setting or
semantic import mode changes; ordinary .NET fallback remains available for broader types.
The native reader preserves CLASS/TypeDef signatures through its temporary reference
projection, while binary execution uses existing Named records. No new runtime opcode
or schema is needed; full CLI-body execution remains a later replacement.

63 focused C# codegen/declaration tests pass, including Release/Debug nominal behavior
and capability denial. The expanded consumer passes factory-returned Order objects
through identity functions, discards a nominal result, mutates aliases, returns self,
passes objects to constructors and resolves nominal overloads. Both source orders verify
and run to 42 on .NET and binary neoCLR. The prior native primitive/import regression
probe also passes; its obsolete nonstatic-class rejection was updated to reject abstract
classes, since default root constructors are now supported. Metadata validation retains
exact class identity and rejects foreign-builder signatures or wrong-class values.
[Evidence](../../tools/NeoClrMetadataProbe/order-runtime-validation.json).

This completes the current bounded nominal-signature/default-constructor/primitive-
initialization slices. External nominal imports, nullable references, generic signatures,
nominal fields/properties, inheritance, readonly storage and constructor chaining remain
explicit future contracts. The full order-collections application still needs its native
collection/LINQ/union dependencies; no substitute dependencies were introduced.


## Explicit primitive instance fields — 2026-10-01

The native collector now accepts explicit `field` declarations on supported root classes:
mutable Int32/Int64/Boolean/String instance fields with public, internal or private access.
Canonical IFieldSymbol identity feeds the existing shared load/store instructions and
FieldInitializationPlan. Backend field definitions preserve access rather than treating
all storage as private. No property or accessor is invented for an explicit field. This
matches existing .NET field emission and the metadata API's CLI/native field contract;
no Runtime Contract option, metadata schema or runtime change is needed. Ordinary private
storage should still use `private var`; explicit `field` is appropriate when field identity
is intentional, such as a public field surface.

Two focused C# Release/Debug tests pass after a seven-test constructor/reference-field
baseline. The executable consumer checks default-constructor initialization, Int32/Int64
mutation through an alias and a private Boolean field read. It verifies public/internal/
private flags and absence of property rows. Both source orders verify/run to 42 on .NET
and binary neoCLR. Static and nominal field declarations reject without output; readonly,
by-reference, attributed fields and broader storage contracts remain unsupported by this
collector. Existing private storage/property backing fields retain their behavior.
[Updated evidence](../../tools/NeoClrMetadataProbe/order-runtime-validation.json).


### Owned nominal field storage — 2026-10-01

The shared field load/store plan now admits owned root-class types through the same
logical type/capability contract as callable signatures. Native declaration collection
accepts mutable explicit fields and private `var` storage, including initializers;
all owned types are declared before mapping fields. Ordinary .NET uses its existing
field definitions. Public nominal properties, external imports, nullable contracts,
readonly/static fields and generic fields remain unsupported in the bounded backend.
There is no new Runtime Contract option or language-binding rule.

The independent neoCLR metadata API now uses SignatureType for FieldBuilder.FieldType
and AddField. Primitive calls remain source-compatible through conversion; rebuild
consumers and inspect Primitive/ClassType. CLI signatures retain CLASS/TypeDef encoding;
native fields retain existing Named records. The binary #Neo payload/reference projection
is still a temporary bridge; eventual native extended-CLI emission replaces the physical
encoding, not the shared logical field operations. No fake dependency types are introduced.

Validation: ExplicitFieldEmissionTests passes all four Release/Debug cases. The Order
probe also exercises a separate Holder with explicit/private nominal fields, an initializer,
replacement and stored-object alias mutation. .NET and binary neoCLR verify/run with
result 42 in both source orders. Five rejection fixtures remain, now including nullable
nominal fields. See tools/NeoClrMetadataProbe/order-runtime-validation.json for hashes;
tested runtime revision e8611966 is on neoCLR's codex/extended-cli-metadata branch.
The whole broad consumer and native metadata symbol loading remain incomplete.


### Owned nominal property emission — 2026-10-01

The native adapter now maps owned root-class property types through the shared logical
signature contract and independent metadata API. Auto-properties, computed getters,
block/arrow accessors and private setters reuse the existing body plans and canonical
backing fields. No Runtime Contract option is added; ordinary .NET remains the default.
The host API's AddProperty/PropertyType now use SignatureType: rebuild consumers and
inspect Primitive/ClassType. Standard CLI property/CLASS signatures and existing native
Named property/setter-parameter records preserve identities and accessor visibility.
The #Neo executable payload with CLI reference projection remains the temporary bridge;
native extended-CLI emission will replace its encoding, not these logical source contracts.

A .NET-only C# regression exposed stale auto-property initializers when the referenced
constructor was declared later. The canonical field could retain a provisional error
expression after binding succeeded. Rebinding now updates its initializer without
replacing the field. This general compiler correction is independently tested on .NET;
it is not a native-target fallback or fabricated initialization.

Validation: 14 focused C# tests cover Release/Debug property, field, constructor and
symbol-stability behavior. The Order probe returns 42 on .NET and binary neoCLR in both
file orders with nominal property replacement, alias mutation, private setters and a
forward-declared constructor initializer. Projected getter/property types and accessor
flags are checked. Six unsupported shapes reject before writing, including nullable
nominal properties. See tools/NeoClrMetadataProbe/order-runtime-validation.json for
hashes; runtime revision e8611966 is on neoCLR's codex/extended-cli-metadata branch.
Indexed/generic/nullable property contracts, external nominal imports, readonly/static
storage and constructor chaining remain outside this bounded backend.


### Explicit root base initialization — 2026-10-01

Shared callable admission now accepts explicit `init(): base()` (also with an expression
body) only after binding proves a parameterless System.Object constructor with no
arguments. This is the same root initialization contract as an implicit base call:
ordinary .NET emits its bound base call; the independent CLI writer supplies its existing
Object constructor prologue; native roots have no base to initialize. No extra call,
metadata extension or Runtime Contract option is introduced. User-defined base classes,
base arguments and general constructor delegation remain unsupported in the bounded
native target. An unresolved explicit initializer is rejected, not silently discarded.

The consumer also exposed stale private-storage initializer identities. All stored
properties now reuse their canonical backing field during rebinding, extending the
existing auto-property rule. The binder refreshes the initializer on that same field.
This is a shared compiler correction, independently checked in C# on .NET.

Validation: 16 focused Release/Debug constructor, property and field tests; repeated
private-storage binding preserves field identity and a resolved forward initializer.
Side-effecting initializers execute in declaration order before block/arrow bodies.
The Order probe verifies/runs to 42 on .NET and binary neoCLR in both source orders;
six rejection fixtures remain, with explicit user-base construction replacing the now
supported root-base case. Evidence is in tools/NeoClrMetadataProbe/order-runtime-validation.json;
runtime e8611966 on neoCLR's codex/extended-cli-metadata is unchanged.


### Readonly instance storage — 2026-10-01

The neoCLR adapter now admits private `val` storage and stored `val` properties,
passing the canonical field's IsReadOnly flag to the independent metadata API. Existing
shared field initialization and constructor-body plans perform the permitted writes;
property associations preserve getter-only shape. Primitive and owned nominal storage
are covered, including mutation of an object referenced by a readonly field.
Ordinary .NET remains the default; no Runtime Contract option is added.

The matching metadata API adds FieldBuilder.IsReadOnly and optional AddField(isReadOnly),
so host consumers must rebuild. CLI uses ordinary InitOnly; native uses the existing
field_readonly bridge array. The updated runtime enforces direct writes by declaring
constructor identity and narrows managed field addresses outside construction to readonly.
This is shallow storage protection, not deep immutability or an unsafe-memory guarantee.
Old runtime binaries (including e8611966) do not enforce these flags during execution.
The tested runtime contains the readonly-field implementation committed as 75403431
on codex/extended-cli-metadata;
tools/NeoClrMetadataProbe/order-runtime-validation.json records its exact binary SHA.
Native semantic field attributes should eventually replace the bridge's origin-array
encoding without changing the logical storage contract.

Validation: eight constructor C# tests (Release/Debug), and the Order binary probe on
.NET and neoCLR in both source orders (42). Projected fields retain InitOnly and stored
val properties have no setter. The independent API's separate binary tests reject direct
and managed-address writes during both verification and execution. Static/literal fields,
explicit readonly field syntax, generic/nullable storage and external nominal imports
remain outside the compiler's bounded native collector.

## Shared vector emission — 2026-10-01

Raven now emits one-dimensional zero-based arrays of admitted primitives and owned
root classes through the shared logical type/instruction plan. Both target adapters
explicitly admit vectors; signature, local, field and property mappings preserve exact
element identity. Literal allocation (including empty literals), Int32 indexing, element
assignment and Length are supported. Evaluation is receiver/index/value order; aliases
retain the same array and contained objects. The unchanged Order declaration and its
three-element batch expression from the broad consumer execute in a separate array
consumer on .NET and binary neoCLR, in both source orders, returning 42.

Ordinary .NET remains the default and no Runtime Contract option is added. The native
adapter consumes the independent metadata API on codex/extended-cli-metadata (8e2a56ed);
compiler work remains on codex/metadata-consumer. CLI uses SZARRAY and standard typed
array instructions; the temporary native execution payload uses ArrayRef/newarr/ldelem/
stelem/ldlen. Length normalizes to Int32 explicitly, matching the source API. A later
native metadata backend replaces payload serialization, not this shared logical plan.
The host core is still the binding bootstrap; this does not implement general native
metadata imports or compile the complete collections application/System library.

Fixed-length type contracts, nested/multidimensional arrays, spread/comprehension expansion, covariance, imported
nominal elements, spans and element addresses remain outside this bounded shared path.
.NET keeps its general fallback. Array iteration is covered below. Backend primitive
type tokens are cached per output, and literals allocate directly without intermediate
collections; no performance measurement or speedup is claimed. C# shared-plan tests
cover Release/Debug execution and capability rejection; the executable ArrayChecks
probe verifies native binaries and storage projections in both source orders.

### Shared array iteration

Ordinary rank-one array `for` loops with an exact element local now lower into existing
bound locals, Length/index operations and branches, before either backend. Collection
evaluation occurs once. Nested loops, labeled continue/break, ordinary break/continue
and empty arrays retain their source semantics. Loops left for general .NET codegen
keep ownership of their unlabeled transfers, even inside a lowered vector loop.
There is no backend-specific language rewrite or additional metadata category.
Discard/converting iteration, generic enumerators and multidimensional arrays remain
outside this bounded native path; .NET uses its existing general path where needed.

Compared with the previous .NET generator-owned array loop, this shares lowering and
control flow across targets at the cost of temporary bound nodes and locals during
compilation. Runtime iteration remains indexed with no enumerator allocation. Performance
has not been benchmarked. C# tests independently validate .NET Release/Debug, target
admission, mixed enumerator/vector nesting and existing range/async/iterator loops
(39 tests passed). An additional fixed-length signature rejection check also passes;
this prevents silently erasing shape metadata. The expanded Order-array probe verifies and runs binary neoCLR
output and .NET output in both source orders (42), including side-effecting collection
calls and labeled continue. `array-runtime-validation.json` records source and runtime
SHA-256 identities. The full collections application and System build remain incomplete.

## Shared indexed property emission — 2026-10-01

Raven now admits implemented root-class indexers through an explicit IndexerAccessor
capability. Both .NET and neoCLR share getter/setter body planning, including declaration
arrow getters, and receiver/index/value evaluation. Calls use ordinary accessor methods;
the native adapter associates their existing signatures with indexed Property rows in
the independent metadata API. Overloaded index types, multiple indices and read-only
indexers preserve signatures/accessor associations in native CLI reference projections.

Ordinary .NET remains the default, with general fallback for unsupported shapes; no
Runtime Contract option changes. The metadata API on codex/extended-cli-metadata
(00752f25) uses standard CLI indexed Property signatures and existing native property
parameter lists. Native execution still uses the temporary #Neo payload plus CLI
projection; eventual native metadata/backend replacement must retain these associations.
The tested native runtime is identified by SHA-256 in indexer-runtime-validation.json.
No runtime opcode or indexed introspection GetValue/SetValue API was added.

The bounded native collector rejects interface/virtual/static indexers, ref/default/
variadic index parameters and unsupported element types. Imported symbol loading and
DefaultMemberAttribute synthesis for other CLI compilers remain separate contracts.
The .NET backend retains its ordinary DefaultMemberAttribute behavior. No parameter
boxing or intermediate array is introduced by shared indexed calls; performance has
not been benchmarked. Eleven focused C# tests pass, covering shared planning and
Release/Debug execution plus existing struct/imported-interface fallback. The native
probe verifies and runs overloads, two-index properties and read-only getters on both
runtimes in both source orders (42).

### Indexed Order collection acceptance

The expanded separate consumer retains the original Order declaration/batch expression
and wraps the array in a concrete OrderBuffer. Its indexers return and replace objects;
mutations remain visible through the original array and aliases. Nominal and array
index parameters, overloads and multi-index properties preserve their associations.
Side-effecting receiver/index/value calls execute once in order. Both file orders
verify/run on .NET and neoCLR to 42. An out-of-range indexed read verifies successfully
and propagates IndexOutOfRange on both runtimes. Unsupported fixed/nested-array index
signatures reject without writing output. C# Release/Debug tests also check two-index
assignment order independently. Evidence fields now name the actual indexer checks,
replacing copied array-probe labels from the initial snapshot. This is a bounded
collection consumer, not the generic ArrayList/HashMap implementation or full sample.

### Owned generic call emission (2026-10-01 development)

The shared callable/type plan now admits unconstrained static method/function type
parameters, locals, vector signatures and instantiated calls through explicit generic
capabilities. Native mappings use the separate metadata API's GenericMethodInstance;
.NET uses its existing generic declaration registration and runtime-symbol resolution
with the shared Release body plan. Debug and general .NET emission remain available.
No Runtime Contract setting is added: target adapter capability admission selects this
bounded subset. Native imports, constraints, generic types and generic instance methods
are deferred. Ordinary .NET generic metadata uses GenericParam, MVAR and MethodSpec;
native execution uses equivalent method parameters and call arguments in the temporary
PE/#Neo payload, with a CLI reference projection. General native symbol importing and
replacement of that execution bridge remain separate work.

`NeoClrMetadataProbe --generic-runtime <application-order-collections.rvn> <fresh-output>
<runtime>` tests generic forwarding, locals, static array access and Order aliasing on
both targets in both source orders (42). Use the matching metadata API/runtime branch
`codex/extended-cli-metadata`, including generic static class-method admission; Raven
support here is on `codex/metadata-consumer`. C# shared-plan tests exercise Release/Debug
execution and capability denial; reference emission regressions remain covered.

The expanded generic probe also checks inference, recursive calls, two generic parameters,
overloads, typed vector construction and iteration, conditional values and retained
object aliases. Shared value blocks and conditional joins admit exact supported value
types; explicit generic arguments require their own target type capabilities even when
absent from parameters/results. Binding-valid generic types, instance generics,
constraints, unsupported argument types and nested vectors reject with NEOMETA001
source diagnostics and no output. [Generic execution evidence](../../tools/NeoClrMetadataProbe/generic-runtime-validation.json)
records runtime/source hashes and both source orders; the full original collection
consumer is not yet supported. Validation: 46 focused shared-generic, shared-linear and
reference-emission C# cases; metadata/native validation is recorded in neoCLR.

### Generic instance receivers (2026-10-01 development)

The shared callable contract now records instance ownership and separately admits
instance generics through `AllowsGenericInstanceMethods`. Both adapters opt into
ordinary unconstrained generic methods on owned root classes; existing receiver-first
body/call emission handles slot zero independently from method parameter ordinals.
Generic locals and forwarding preserve receiver mutation and object identity on .NET
and neoCLR. No Runtime Contract option is added. Native virtual generic dispatch,
generic owners, constraints and external generic references remain unsupported.
The PE/#Neo bridge carries ordinary instance calls with explicit generic arguments;
CLI uses standard MethodSpec. Use matching neoCLR producer/runtime commit `6a7a0dd2`
on `codex/extended-cli-metadata`. Raven integration remains on `codex/metadata-consumer`.
Focused C# Release/Debug shared-plan tests and the binary generic Order consumer pass
on both runtimes in both source orders. General native symbol importing remains deferred.

The expanded receiver probe also verifies generic no-result methods (copy/reverse),
recursive instance calls, receiver/argument order (123) and independent receiver state.
Both source orders return 42 on .NET/native. See the refreshed
[generic evidence](../../tools/NeoClrMetadataProbe/generic-runtime-validation.json).
Unsupported generic owners, constraints and virtual dispatch remain explicit limits.

### Typed default emission (2026-10-01 development)

Shared body planning now admits BoundDefaultValueExpression for supported non-Void
primitive, owned class, vector and method-parameter types through the explicit
DefaultValue instruction capability. The .NET adapter initializes and loads a scratch
local; the native adapter uses the metadata API's LoadDefault. Both use ordinary
ldloca/initobj/ldloc semantics: numeric zero, Boolean false and typed null reference
values. No Runtime Contract switch or native schema change is added. General byref
signatures, nullable-source signatures, generic owners and constrained dispatch remain
outside this slice. Scratch locals are backend-owned and do not shift shared local
indices. Release/Debug C# tests cover shared admission, capability denial, generic
primitive/reference defaults and array clearing. The binary Order consumer also clears
primitive vectors and loads generic reference defaults on both targets/source orders.

The final default-value probe additionally clears Order references, verifies the binary
and checks a null-reference fault on both runtimes when reading a cleared element.
[Recorded evidence](../../tools/NeoClrMetadataProbe/generic-runtime-validation.json)
includes runtime/source hashes. Matching producer API: neoCLR `8e7fada6` or later;
receiver runtime: `6a7a0dd2` or later on the metadata feature branch. Validation includes
13 focused C# generic/default tests and both source orders. No full-library support
is claimed.


## Static generic owners (2026-10-01 development slice)

Unconstrained static generic classes now use the shared SourceTypePlan, callable
signature and body paths. An explicit GenericStaticOwners capability admits owner
parameters separately from method parameters, including arguments absent from a call's
value signature. No Runtime Contract configuration change is needed. The native
adapter uses the separate metadata producer's AddGenericType/MakeConstructedReference;
.NET preserves ordinary VAR/MVAR, TypeSpec/MemberRef and optional MethodSpec semantics.
The source-method resolver now projects the declaring type before constructing a
method, including open calls inside a generic type.

The temporary binary bridge retains native open/constructed owner records and a CLI
reference projection. It does not flatten owner parameters into method parameters.
Generic object layouts, fields/properties on generic owners, constraints and imported
generic owners remain deferred; the native metadata/backend replacement must preserve
both scopes and identity. This is feature-branch support, not main/released support:
Raven codex/metadata-consumer with neoCLR codex/extended-cli-metadata producer 0da5a3b0
or later and receiver runtime 6a7a0dd2 or later. C# Release/Debug checks cover shared
planning and target capability rejection; the binary Order consumer verifies and runs
42 in both source orders on .NET and neoCLR, including generic owner defaults, arrays,
method forwarding and object aliases. See the adjacent recorded probe evidence.

The expanded owner acceptance also permutes two owner arguments and forwards a
method parameter into a callee owner, keeping simultaneous substitution independent
of parameter ordinal/name. C# capability tests reject unsupported owner arguments even
when the method signature contains no use of the owner parameter. The recorded
binary evidence includes both checks for cross-scope and reordered owner forwarding.


## Generic instance storage (2026-10-01 development slice)

Shared source type/signature plans admit unconstrained generic root classes through
GenericClassOwners, distinct from static generic owners. Constructed signatures retain
the original definition and recursively mapped arguments. Native declarations use the
separate producer's AddGenericClass; constructor and ordinary/generic method references
bind both owner and method arguments. No Runtime Contract configuration changes.
.NET constructors retain the general emitter; ordinary method bodies, object creation,
value signatures and capability checks use the shared paths.

The bridge emits standard CLI GENERICINST/VAR/member references and existing native
Constructed owners/fields. Private var/val storage uses declaring-type VAR. External
constructed field references require a separate explicit capability, enabled by .NET
and denied by neoCLR until typed field-reference emission exists. Generic property and
indexer metadata, inheritance, constraints and external generic owner imports remain
unsupported by native emission; these are bridge limits, not permanent platform rules.

Matching feature branches: Raven codex/metadata-consumer and neoCLR
codex/extended-cli-metadata producer 62bf5931 or later, runtime 6a7a0dd2 or later.
C# Release/Debug regression tests cover shared methods, nested Box<Box<int>> storage,
mutation through generic aliases and independent generic methods. The binary Order
consumer verifies/runs 42 on .NET/native in both source orders with primitive, Order
and nested generic fields. Unsupported generic properties and external field access
produce source diagnostics without an output assembly. See
[recorded evidence](../../tools/NeoClrMetadataProbe/generic-runtime-validation.json).
Full class-library emission is not yet established.


## Generic properties and indexers (2026-10-01 development slice)

Generic instance properties/indexers now reuse shared accessor planning and constructed
method references. The native adapter emits property associations through the separate
metadata library, with declaring-type VAR preserved in value/index signatures. Existing
.NET behavior remains the default. No new lowering, Runtime Contract configuration or
native schema is required: CLI uses Property/MethodSemantics; the native bridge uses
canonical open Constructed accessor owners and matching reader validation. External
constructed field handles, constraints and generic imports remain distinct limitations.

C# Release/Debug tests cover shared accessor bodies and nested constructed values.
The expanded binary consumer verifies/runs 42 on both runtimes in both source orders,
including setter/getter calls, indexed mutation, Order aliases and independent generic
key/value parameters. The producer also
validates static generic associations and generic index parameter signatures. This is
feature-branch support on Raven codex/metadata-consumer and neoCLR
codex/extended-cli-metadata with generic-property producer/reader dbe03b1a or later;
the existing receiver runtime (6a7a0dd2 or later) is sufficient. See
[recorded evidence](../../tools/NeoClrMetadataProbe/generic-runtime-validation.json).
Native compiler support for static source properties is not added by this slice.

Constructed-field integration (development): the native adapter opts into the shared
ConstructedFieldReferences capability and resolves substituted fields to their original
definition plus owner arguments. CLI uses open-signature Field MemberRefs; native field
indices operate on the exact constructed receiver. No Runtime Contract or schema change.
C# tests and the binary consumer exercise public generic storage from outside its type
on .NET/native. Cross-assembly field imports remain unsupported. Matching field producer
on neoCLR codex/extended-cli-metadata and Raven codex/metadata-consumer required.

Nominal type bounds (development): a source owner parameter may have one owned
nongeneric root-class bound. Shared SourceTypePlan/signature capabilities retain the
contract; the native adapter declares all types before mapping bounds, preserving source
order independence. CLI GenericParamConstraint and native TypeBound agree on this
subset. No Runtime Contract or schema change. C# Release/Debug tests check emitted
bounds and invalid binding; the Order binary consumer verifies/runs 42 on both runtimes
and in both source orders. Open constrained member dispatch, interface/dependent bounds,
method bounds and class/struct/new/nullability flags remain separate work. Ordinary .NET
constraint behavior remains available through its general paths. Matching constrained
metadata producer/reader on neoCLR codex/extended-cli-metadata is required.


### Special type constraints checkpoint (2026-10-01)

Shared source/signature capabilities now admit class/struct/new type-parameter
requirements. The neoCLR adapter declares owners before assigning flags/bounds and
uses the independent metadata producer's SetSpecialConstraints API. Ordinary .NET
struct emission now includes both value and default-constructor attributes. Runtime
Contract configuration is unchanged; unsupported source contracts still diagnose
before output rather than losing requirements.

Standard CLI GenericParam flags project the requirements; native execution uses new
ReferenceType/ValueType/DefaultConstructor kinds. These preserve .NET's distinct
categories instead of equating them with native notvoid/notreference. New outputs
require the matching neoCLR codex/extended-cli-metadata runtime; the runtime hash is in
[consumer evidence](../../tools/NeoClrMetadataProbe/generic-runtime-validation.json).
Raven remains on codex/metadata-consumer. The metadata API stays a separate project.

The expanded consumer verifies/runs 42 on both runtimes in both source orders; 17
focused C# compiler tests pass. Class constructor checks are deferred until the producer
has all definitions. The requirements do not enable new T(), open constrained dispatch,
method constraints, interface/dependent bounds or notnull. The latter has nullable
semantics rather than an equivalent special CLI flag. General runtime-class-library
compilation and native symbol import remain unproven. Native payload plus CLI reference
projection is still a temporary bridge; replacing it must preserve these contracts.

After this round the author requested an assessment. The checkpoint is a working
bounded integration, not a completed emission story. Next select an unmodified library
source unit and let its concrete gaps drive work; the full order-collections sample
still requires imported collections, interface dispatch, callbacks and union handling.
Keep the neoCLR assessment in docs/experiments/extended-cli-metadata/state-assessment-2026-10-01.md
as the detailed cross-repository record.


### Unchanged Language source and static property accessors (2026-10-01)

The whole runtime/raven/src/System/Globalization/Language.rvn file now compiles without
source edits. Shared lowering represents static getters/setters as ordinary static
calls; instance accessor receiver evaluation is preserved. The native declaration
adapter admits implemented static properties on owned classes and static types,
including constructed generic owners. Storage-backed static properties still reject
before output: static initialization/storage is a separate capability, not synthesized
per-call storage. No new metadata API, native opcode or runtime change is needed.

The independent producer owns ordinary CLI Property/MethodSemantics and native
accessor associations. Runtime Contract configuration and the explicit console bridge
are unchanged. The --whole-library-runtime probe verifies and executes the binary on
neoCLR and the ordinary CLI image on .NET, in both source orders: und/sv/he and result
42. It also exercises a static setter and generic static getter, checks metadata,
and rejects unsupported static storage. Five focused C# tests pass (including Debug
and Release static-property coverage). [Evidence](../../tools/NeoClrMetadataProbe/whole-library-runtime-validation.json).
This uses the host-core bootstrap, not a completed native metadata symbol loader.

The refreshed class-library inventory shows Comparer<T> and EqualityComparer<T>
bind successfully but require interface declaration emission. ArrayList<T> still
requires native library dependencies before its emission frontier can be measured.
These are next acceptance gates, not newly implemented interface/collection support.

Author clarification: neoCLR follows .NET behavior and the same instruction semantics
for supported facilities unless another choice is explicitly made. Unsupported
features such as exception handling are coverage gaps. Physical #Neo payload transport
is a temporary bridge, not a separate instruction-set design; its future replacement
must retain ordinary CLI semantics and documented extensions.


### Comparer interface declarations (2026-10-01)

SourceInterfacePlan separates bodyless declarations from shared callable body plans.
Explicit Interface/InterfaceMethod and generic-interface capabilities admit invariant
owned interfaces containing public abstract instance methods with supported signatures.
The native adapter uses independent AddInterface/AddGenericInterface/AddInterfaceMethod
APIs. .NET retains its existing general interface emitter; C# tests compare its metadata
with the shared declaration plan. No Runtime Contract option or semantic-binding change.

The producer writes ordinary CLI interface/abstract flags and bodyless virtual new-slot
methods; the native bridge uses existing Interface identity and abstract method flags.
No opcode/runtime change. It does not substitute classes or throwing method bodies for
abstract contracts. The matching reader retains this shape in CLI reference metadata.
Inherited interfaces, properties, variance, default/static/generic methods, interface
value signatures, implementations and dispatch remain bounded-adapter gaps.

Both complete unchanged Comparer.rvn and EqualityComparer.rvn files load/verify on
neoCLR and reflect as interfaces on .NET in both file orders. The independent entry
returns 42; [evidence](../../tools/NeoClrMetadataProbe/interface-library-runtime-validation.json)
explicitly disclaims dispatch. Thirteen focused C# tests pass; full collection/library
compilation remains open. Native symbol import is still a separate integration task.


The iterator declaration extension adds explicit InterfaceProperty and
InterfaceInheritance capabilities. Bodyless accessors share method signatures and
producer property associations; all interface definitions are declared before their
base edges, preserving source-order independence. Only owned nongeneric base interfaces
are admitted. Unchanged Disposable and Iterator<T> now join the comparer acceptance:
both runtimes load/verify the declarations, with an independent entry returning 42.
13 focused compiler tests pass; no dispatch result is claimed. Generic inherited
interfaces and interface-valued signatures remain subsequent work. No Runtime Contract,
semantic binding, runtime instruction or ordinary .NET emitter change is required.


Interface reference extension (2026-10-01): unchanged Iterable<T> now emits beside
Comparer, EqualityComparer, Disposable and Iterator. Owned interface identities and
constructions share CLI CLASS/GenericInst and native Named/Constructed signatures.
The shared compiler descriptor is nominal rather than class-specific, with an explicit
interface-signature capability; nullable reference annotations retain binder semantics
and map to the same reference storage. Parameters, results, locals, defaults and arrays
execute on .NET and binary neoCLR in both source orders (42). The host-core bootstrap
and Runtime Contract configuration are unchanged. No runtime or ISA changes are needed.
Interface invocation/implementation remains the next author-directed acceptance gate;
generic interface inheritance remains deferred. Seven focused C# interface tests and
68 metadata API test groups pass. The evidence exercises null/default reference flow,
not dynamic dispatch.


## Owned interface dispatch — 2026-10-01

Raven now admits nongeneric root classes implementing owned nongeneric interfaces,
including inherited contracts. Shared InterfaceImplementation and InterfaceCall
capabilities gate admission; implicit reference conversions retain normal binder
semantics. Interface method/property calls use ordinary CLI callvirt (0x6f), and the
native payload uses existing callvirt/implements contracts. The separate metadata API
owns implementation validation, InterfaceImpl rows and public virtual/final/new-slot
CLI implementation flags. Native concrete methods use existing implicit implementation
lookup. No runtime code or instruction-set changes are required. Runtime Contract
configuration and the host-core bootstrap remain unchanged.

The C# producer rejects missing/incompatible implementations, foreign/generic dispatch
targets and unrelated receivers; direct calls to abstract contracts remain invalid.
CLI and native execution select two implementations through an inherited contract (42),
and null receiver calls fault. Raven additionally executes interface method/property
calls and interface-array alias updates in both source orders on .NET and neoCLR (42).
[Evidence](../../tools/NeoClrMetadataProbe/interface-dispatch-validation.json). Twelve focused C# compiler tests and 69 metadata API groups
pass. Owned nongeneric implicit implementations are the bounded target contract;
generic interface dispatch, explicit/default methods, external imports and class virtual
overrides remain future adapter work, not alternate platform semantics. Both feature
branches remain experimental. Native symbol loading remains a separate compiler gate.


## Source/sample readiness inventory — 2026-10-01

`NeoClrMetadataProbe --readiness-inventory <neoclr-root> <fresh-output> <runtime>`
reports unchanged source attempts under the existing host bootstrap and the real
CompilationOptions.NeoCLR profile with Self. It hashes the API declaration snapshot,
sources and runtime; it never executes failed emission. A separate ordinary CLI output
is retained as a nonexecuted reference-core control, for the legacy bridge to consume.
[Inventory](../../tools/NeoClrMetadataProbe/readiness-inventory-2026-10-01.json) and
[bridge runtime controls](../../tools/NeoClrMetadataProbe/readiness-bridge-controls-2026-10-01.json).

All twelve selected apps bind and emit ordinary CLI, but direct emission rejects the
neoCLR profile (NEOMETA002). No guard was bypassed. Three unchanged application controls
run through the current legacy bridge and existing generated System library. The
order-collections control emits None followed by Option<Order>'s Some constructor in
PendingOrder, then fails importer stack validation; this is a general compiler candidate
requiring isolation before a fix, not permission to weaken verification. Full-library
binding with the consumer snapshot lacks implementation services and has runtime-contract
identity conflicts. It is not a complete implementation bootstrap.

Recommended larger milestones: real target-profile/seed contracts; generic collection
interfaces and imported nominal/generic member handles; then union/value/callback and
propagation lowering driven by unchanged order-collections. Reuse the semantic importer
behind a metadata-source contract and existing shared lowering. Keep Runtime Contract
options explicit; ordinary .NET remains default. No production options or semantics
change in this assessment/tooling slice.

## Validated native target-profile emission (2026-10-01)

The direct backend now accepts `CompilationOptions.NeoCLR` with the profile's
`NeoCLR.CoreProbe` reference snapshot. `NeoClrBindingContract` validates the existing
profile and exact primitive/Unit core identity before native mapping. The host-core
bootstrap and ordinary .NET default remain unchanged. No binder or .NET generator
semantics change in this slice.

Native intent: primitives, Unit/no-result, assembly functions, arrays and owned
interface dispatch use existing native signatures/instructions. Temporary binding:
CLI declarations supply symbols via the existing PE provider; Unit uses System.Void.
The adapter currently requires that provider to inspect full core identity. It neither
loads native symbols nor authenticates snapshot contents. Implementation services,
imported nominal/generic members, native Self metadata and broader bodies remain
separate work; the reference snapshot is not a full class-library build seed.
The compiler target contract owns binding semantics; the adapter owns admission and
mapping; the independent metadata library owns encoding; neoCLR owns load/verify/run.
Replace the PE declaration dependency with a metadata-source identity contract and
native symbol provider later, preserving the same core validation.

C# `NativeProfileChecks` exercises Hello World through another function, a Unit entry,
array iteration and owned interface dispatch using no host core reference. It verifies
and runs each native binary, and checks wrong core name/version leave output untouched.
The existing host-bootstrap dispatch control still verifies and returns 42 on both
.NET and neoCLR in both file orders. Run:

```sh
dotnet tools/NeoClrMetadataProbe/bin/Debug/net10.0/NeoClrMetadataProbe.dll \
  --native-profile-runtime /absolute/neoclr /tmp/fresh-native-profile /absolute/neoclr/target/release/neoclr
```

The preceding readiness inventory is historical evidence; its profile-wide NEOMETA002
gate is now replaced by explicit validation, not proof that every listed app emits.
[Shared-codegen parity audit](architecture/neoclr-refactor-parity.md) records the
independent .NET checks and unresolved imported-carrier/loop-capture issues.

## Imported carrier binding correction (2026-10-01)

Independent Raven fix `46491585e` is integrated into local main and consumed here.
Argument contextual typing previously offered Some<T> as a target for None because
both belonged to the same union family. Concrete case parameters now require the
requested case name; carrier parameters retain family lookup. The selected constructor
is correct before lowering, so .NET and native consumers share the correction.
No CLI encoding, Runtime Contract or codegen fallback changes are involved.

The C# regression covers four None spellings, both declaration orders and both
optimization modes, checking semantic selection and ordinary .NET execution.
All 16 cases and 323 surrounding cases pass on the shared line. The unchanged
order-collections sample now imports through the legacy bridge; the previous
None-to-Some constructor stack failure is resolved. Direct metadata emission still
rejects Register's imported collection signatures; this is a separate backend gap.
The prior audit's carrier issue is superseded; loop capture remains open.

### Focused sample reproduction

Use the existing inventory machinery for one unchanged application, without running
the whole-library sweep:

```sh
dotnet tools/NeoClrMetadataProbe/bin/Debug/net10.0/NeoClrMetadataProbe.dll \
  --readiness-sample /absolute/neoclr /tmp/fresh-order /absolute/neoclr/target/release/neoclr application-order-collections
```

The report identifies its selection and records independent direct-emission and CLI
control results. The historical full inventory is preserved. A successful report
process does not imply successful direct emission; inspect its recorded diagnostics.

## Primitive-vector library/application boundary (2026-10-01)

The direct metadata adapter matches static nongeneric primitive and primitive-vector
signatures through the shared CallableSignature representation. It admits Int32,
Int64, Boolean and String vectors in parameters/results and retains exact overload
matching. Target checks reject nominal/generic dependency contracts explicitly;
no Reflection.Emit or ordinary .NET codegen behavior changes.

Configuration remains CompilationOptions.NeoCLR with the matching NeoCLR.CoreProbe
CLI declaration reference, explicit core identity and NeoClrMetadataDependency
bindings. The independent metadata API owns the standard SZARRAY encoding and
external references; Raven owns symbol matching and target capability checks.
Symbols still come through the temporary CLI declaration projection. Native semantic
data loading must replace that projection later; it cannot represent every future
neoCLR category and is not the executable image. Both emitted implementation binaries
are loaded directly by neoCLR, without a CLI-to-JSON body translation.

The C# VectorLibraryChecks probe emits the library and app separately, verifies and
runs them with the real neoCLR profile (result 42), checks array alias mutation across
calls and all four element overloads, and rejects absent dependency registration or
an incompatible overload snapshot without touching output. See
[recorded validation](../../tools/NeoClrMetadataProbe/vector-library-validation.json).
This is feature-branch evidence on codex/metadata-consumer and neoCLR
codex/extended-cli-metadata; imported generic collections remain pending.

## Imported static generic methods (2026-10-01)

The target adapter now imports unconstrained static generic definitions on nongeneric
owners and instantiates them with concrete primitive/vector arguments. It matches the
original definition, generic arity and scoped parameter/result types using the shared
CallableSignature model. Owned generic calls keep their existing path. The independent
metadata API encodes standard CLI MethodSpec over MemberRef; native execution uses the
existing generic call format. No binder, shared .NET codegen or runtime opcode changes.

Runtime Contract configuration remains CompilationOptions.NeoCLR with the matching
CLI declaration core and explicit dependency/core bindings. CLI MVAR/GenericParam is
a temporary symbol-input representation; a future native semantic loader must retain
scope and identity. Nominal arguments, generic owners, constraints and forwarding
caller-scoped generic arguments remain outside this import contract. The metadata API
owns decoding and substitution; Raven owns symbol mapping and capability checks.

C# GenericLibraryChecks emits separate library/app binaries and verifies/runs them
on neoCLR (42), checking concrete Int32/Int64/Boolean/String and array arguments,
void calls, alias mutation and absent registration/declaration rejection. See
[validation](../../tools/NeoClrMetadataProbe/generic-library-validation.json) for
feature branches and tested core/runtime hashes. The metadata API's C# CLR test
also resolves same-signature overloads by generic arity and executes a MethodSpec.

Exploratory follow-up: when Raven declarations Choose<T>(T) and Choose<T,U>(T) differ
only in generic arity, Choose<int>(7) reported RAV0121 against both candidates on the
integration branch. The executable Raven probe uses different value-parameter counts;
no binder workaround was added. Confirm the intended partial-type-inference rule and
reproduce on main before treating this as a general compiler fix. Any independent fix
belongs on the main-based fixes branch. This result does not establish a regression.

Author direction remains .NET metadata as the baseline and a Cecil-like inspect,
edit and create API. Bounded imports are implementation coverage, not a permanent
alternative metadata model; arbitrary loaded-assembly editing is still open.

## Raven external reference signatures (2026-10-01)

Raven's neoCLR adapter now opts into imported public top-level class/interface
signatures through an explicit shared capability. The default .NET portable admission
is unchanged. The adapter resolves symbols against registered dependency snapshots,
checks definition name/arity, and uses the independent metadata API's external type
references. Repeated definitions are cached for the emission. Standard CLI TypeRef,
GENERICINST and TypeSpec shape and native dependency scope are preserved.

Configuration remains CompilationOptions.NeoCLR, a matching projected declaration
core and explicit NeoClrMetadataDependency registrations. This supports signatures,
nullable reference locals/defaults, interface arrays and owned method-generic forwarding
with external constructions. The C# --external-signature-runtime probe produces a Raven
library and consumer; neoCLR verifies both and returns 42. Missing bindings or missing
snapshot types reject without modifying the output. No Runtime Contract change.

This is tested on codex/metadata-consumer with codex/extended-cli-metadata (7f084f4c),
not a published runtime capability. The unchanged collections sample now passes
Register declaration admission and stops at PendingOrder's Option<Order> return
contract; its ordinary CLI control still emits. Imported value/union types, external
constructors/member calls and translated-System origin-to-native identity mapping
remain open. Synthetic CLI core identity is not native implementation identity.
The temporary CLI symbol source must eventually be replaced by native metadata loading.

See [probe evidence](../../tools/NeoClrMetadataProbe/external-signature-validation.json).

Validation for this slice: 20 focused C# capability/nominal tests pass, including
Debug/Release ordinary .NET execution; native probe verify/run returns 42.

## Consumer-scoped imported generic calls (2026-10-01)

The next bounded integration removes the primitive-only instantiation gate for imported
unconstrained static generic methods. Arguments are validated in the consuming assembly,
while the definition retains its dependency identity. Caller method and declaring-type
parameters are checked at emission; foreign output types and out-of-scope parameters
reject. This preserves normal .NET MethodSpec/MemberRef semantics and introduces no
metadata format, runtime opcode or Runtime Contract change.

Raven maps these arguments through its existing target type mapper. The C# native probe
now executes consumer-owned Order arguments with alias mutation, caller method/owner
parameter forwarding and an external Box<int> construction. Both emitted binaries verify
and execution returns 42. The metadata C# checks additionally execute the consumer-owned
argument on the CLR. The CLI projection remains the temporary symbol source; explicit
core/dependency configuration is unchanged. These feature-branch results do not advance
the collections Option<Order> gate: imported value/union signatures, generic declaring
type members and translated-System native identities remain open. Native loading of
compiler symbols remains future work.

## Imported nominal method signatures (2026-10-01)

The feature-branch metadata API and Raven adapter now import static methods on
nongeneric owners whose signatures contain dependency-local reference types,
constructions, vectors and unconstrained method parameters. Raven compares the fully
remapped signature against the bound symbol rather than matching module-local tokens.
The metadata API owns bounded decoding, exact snapshot identity and reference validation;
Raven owns explicit dependency selection and overload matching. Ordinary .NET defaults
and Runtime Contract configuration remain unchanged.

The native intent is ordinary cross-assembly calls. CLI declaration input and emitted
TypeRef/MemberRef/MethodSpec are a temporary bridge; no native encoding or opcode change
was required. CompilationOptions.NeoCLR still requires matching core and registered
NeoClrMetadataDependency snapshots. The native implementation dependency must use the
producer's format-5 identity naming; translated System still needs an origin-based map.

The C# --external-signature-runtime probe now emits a factory returning Box<Order>,
forwards it and retrieves a consumer-owned payload while preserving aliases. Interface
vectors, nominal overload matching and missing-method rejection also pass. Both binaries
verify and execution returns 42; 74 metadata contract test groups pass, including CLR
factory/reader execution and malformed signature rejection. The existing generic-library
probe still verifies/runs (42). Tested branches are codex/metadata-consumer and
codex/extended-cli-metadata; evidence pins core/runtime hashes.

The unchanged collections sample still stops at PendingOrder's Option<Order> result,
which is a value-type union in this CLI bridge. Do not admit it as a reference type.
Next work requires value-type category preservation in signatures/native projection,
then appropriate union operations and imported instance/generic-owner members. The
current projection also omits nullable annotations; the factory boundary above uses
nonnull references. Native semantic loading remains future work. Primitive-only reader
recognizers/MemberReference resolution remain narrower than this producer import API.


Metadata authoring migration (2026-10-01): the independent metadata library now shares
assembly/type/field/method declarations with builder facades and permits direct global
function declarations. The existing CLI function projection and native encoding are
unchanged; no new bridge representation or loss is introduced. Raven owns target
mapping, the metadata library owns authoring/encoding, and neoCLR owns loading and
execution. Native body-definition and loaded editing work remains pending. See the
[dependency validation checkpoint](api/neoclr-emission.md#definition-first-metadata-dependency-checkpoint-2026-10-01).

Direct static type-method metadata construction also preserves the existing bridge;
Raven’s rebuilt external-signature runtime probe returns 42. This is authoring API
compatibility validation, not new target admission or a collections-gate resolution.

Root-class constructors and instance methods can now be declared directly in the
metadata library. They reuse the existing constructor/receiver encodings; no new bridge
loss or representation is introduced. Raven facade compatibility still verifies/runs (42).

Direct interface authoring preserves the existing InterfaceImpl, abstract-method and
callvirt projection/native contracts. Raven facade compatibility still verifies/runs
(42); no compiler semantic change or new bridge encoding is introduced.

The metadata dependency now owns interface relationships and body storage in definitions.
Existing helpers/writers retain the same CLI/native encodings. Rebuilt Raven execution
returns 42; this migration introduces no bridge representation or semantic changes.

The authored property migration shares definitions/accessors without changing CLI/native
property encoding. Rebuilt Raven execution passes (42); no new bridge restriction or
compiler semantic change is introduced.

Generic definition authoring now shares parameter names and constraint storage with
builders; CLI GenericParam/GenericParamConstraint and native encodings remain unchanged.
The rebuilt Raven probe passes (42), without Runtime Contract or admission changes.

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


### Terminal API naming correction (2026-10-02)

The neoCLR namespace action is `System.Fail(message)`; Fault names the runtime outcome.
The transitional `NeoClrCliCompatibility` contract now recognizes Fail with the same
exact NeoCLR.CoreProbe assembly, TopLevel namespace-container marker, static/nongeneric
shape, String parameter and void/Unit result. It remains non-returning for control flow,
output assignment and emission. The old Fault method name does not qualify, and ordinary
.NET methods named Fail do not qualify. Runtime Contract configuration is unchanged.

Migrate source and use a matching reference/compiler/runtime bundle; the public old-name
alias is not retained. The native host Fault result and UserFault code are unchanged.
The CLI void signature cannot independently express terminal behavior; this temporary
identity check remains owned by target compatibility until general target-owned
non-returning semantics/native metadata are available. Compiler-generated propagation
failure guards remain a separate native-emission gap; this rename does not admit throws.

Validation: all eight focused `NeoClrFaultControlFlowTests` pass on .NET 11. They cover
qualified/imported Fail calls, unreachable code, ref/out termination, wrong assembly,
wrong container/marker and the old Fault spelling without terminal treatment. The neoCLR
bridge also passes four source-export admission cases, including old-name rejection.


## Imported constructor checkpoint (2026-10-02)

The portable plan exposes `AllowsExternalConstructors`, disabled by default and
selected by the native adapter. Public top-level class/value constructors now use
one callable resolver for source, imported and constructed imported owners. The
metadata library encodes CLI Newobj and the existing native constructor operation;
value construction uses a managed construction receiver with definite field
initialization. No binding, semantic-model or Runtime Contract changes are required.
The ordinary .NET fallback and capability defaults are unchanged.

The CLI reference snapshot remains the temporary symbol source, owned jointly by
the compiler bridge and metadata importer. It cannot flatten nested owners without
losing identity. Native symbol loading and explicit nested definition/reference
scopes must replace this limitation. Byref constructor parameters, constructor
chaining and type initializers remain outside this bounded capability.

Validation: 29 focused capability/parity C# tests pass on .NET 11, including an
ordinary DateTime constructor consumer. A Raven consumer of separately authored
value and generic-value constructors verifies and executes to 42 on neoCLR; five
existing native controls pass. The metadata library additionally executes imported
value, generic-value and class constructors on both CLR and neoCLR (42). The
unchanged collections sample still emits a 7168-byte CLI control and rejects native
emission at Option<Order>(None), whose parameter has the nested identity
System.Option.None. The application has not run; accepting top-level constructors
alone does not close that gap.


## Nested imports (2026-10-02)

Native emission selects `AllowsNestedExternalTypes`; ordinary .NET defaults remain
unchanged. Imported reference/value signatures, local storage, constructors and receiver
calls retain their explicit enclosing metadata identity. Matching uses a union case's
physical metadata container when it differs from its semantic union carrier. This is a
symbol-to-metadata mapping correction, with no binder or Runtime Contract changes.

The independent metadata reader/writer preserves nested TypeRef scopes, NestedClass
rows and native owner identities. Generic nested value types under nongeneric owners
are supported; capturing outer generic parameters remains unsupported. CLI projections
are still the temporary symbol-loading bridge, owned by the metadata producer/importer
and compiler target adapter; native symbol loading must eventually preserve the same
scope graph directly. No name-flattening convention is used.

The C# metadata fixtures execute nested imported constructors on CLR and neoCLR (42).
The Raven native fixture constructs, mutates and reads nested values including a generic
owner across a dependency boundary (42). The unchanged collections sample advances to
the Single callable signature, with a successful 7168-byte CLI emission control. It has
not executed; extension/delegate signatures and native System bindings remain open.


## Coordinated Function runtime integration (2026-10-02)

The author authorized reusing the structural Function feature branches. neoCLR's
metadata branch integrates runtime/library revision a081c6e3 from codex/structural-types,
retaining System.Fail, binary PE loading and the metadata API. Raven already carries
the needed target-owned inhabited callback-result policy in CliRuntimeContract; ordinary
.NET remains delegate-based. No binding or Runtime Contract configuration changed.

The combined runtime/reference/bridge bundle passes existing structural callback,
closure, async and comparer CLI-import controls. These remain temporary CLI Func/Action
encodings owned by the bridge. The native metadata API and shared emission plan must
now expose structural Function signatures and checked binding/invocation directly;
that direct path is still incomplete for the unchanged collections application.
Main does not gain structural Functions from these feature-branch checks.

### Native Function producer checkpoint (2026-10-02)

The metadata adapter opts into `AllowsFunctionValues` and logical Function bind/invoke
instructions. It maps core Func/Action-shaped compiler symbols to structural native
signatures, independent of the selected method. Static owned nongeneric method groups
can be bound, stored in locals, passed as parameters, and invoked. A direct PE consumer
returns 42 on neoCLR; the other five native profile controls remain passing. The shared
admission test also executes the equivalent ordinary .NET consumer to 42.

Runtime Contract configuration and semantic binding are unchanged. Ordinary .NET does
not opt into these operations and retains the existing delegate generator. CLI metadata
uses Func/Action solely as transport; native bodies use `function.bind` and Function
invocation. The compiler owns lowering and capability admission; the separate metadata
library owns encoding, and the runtime owns invocation. Noncapturing lambda synthesis,
captured receivers, imported/generic binding targets and inhabited-Void callbacks still
need producer support. The runtime feature integration is on
`codex/extended-cli-metadata`, not evidence of main-branch availability.

The subsequent noncapturing-lambda slice prepares all lambda bodies before metadata
materialization and gives them internal assembly-function definitions. Parameter
slots use logical static ownership without changing the compiler's .NET lambda
symbols or closure policy. The shared planner retains each synthesized body and
uses the existing lowered expressions. Two direct native lambda callbacks return
42; captured environments, async/iterator bodies and generic lambda targets are
still rejected. No Runtime Contract or semantic-model change is introduced.

The native adapter also opts into `AllowsLoweredExtensionCalls`. This only admits
static callable signatures; the existing shared lowerer remains responsible for
receiver evaluation and argument placement. Unlowered extension receivers are still
rejected by the portable plan. Runtime Contracts, binding and .NET defaults are
unchanged. The collections consumer now reaches union-pattern emission.

### Imported union-case branches (2026-10-02)

The native profile opts into `AllowsCasePatterns` for conditional union-case and
union-member patterns with exact typed bindings or discards. Shared planning emits
TryGet calls and payload getters, branching directly on failure so successful-path
locals retain their assignment state. It preserves the same checked extraction used
by the .NET generator; unrelated pattern categories remain on that generator or
produce a native diagnostic. Compiler-generated non-exhaustive-match throws now carry
the existing terminal-failure annotation; ordinary .NET still emits the original
exception. User throws are unchanged. Concrete imported value overrides use an
addressed direct call, while nonfinal class overrides still require future dispatch
support. No Runtime Contract or source binding change is introduced.

34 focused compiler tests pass, including imported Some/None extraction and ordinary
.NET execution. The unchanged collections sample progresses to reference conversions;
this checkpoint does not establish execution of that sample on the native backend.

### Reference conversion and physical case identity (2026-10-02)

The native adapter explicitly admits `ReferenceConvert` for bound implicit reference
conversions between supported reference signatures. It emits the metadata API's
checked `castclass`; ordinary .NET does not opt into the instruction and keeps its
existing path. This provides the native interface representation transition without
requiring the metadata validator to invent imported inheritance relationships.

Case pattern storage comparisons retain semantic identity by default, but recognize
union-case symbols with the same assembly, physical metadata name and exact type
arguments. A nongeneric `Option.None` case can therefore be consumed across different
semantic carrier views. This changes codegen storage matching, not the binder's symbol
equality or Runtime Contracts. The seven native profile cases remain passing; the
unchanged collections sample next reaches retained iterator `for` statements.

### Portable reference enumeration (2026-10-02)

`AllowsReferenceEnumeration` enables a shared lowering helper for bound generic loops
whose enumerator is a reference and whose Current result exactly matches the iteration
local. It evaluates GetEnumerator once, checks MoveNext, loads Current and retains
break/continue transfers using bound labels. It reuses compiler-selected members and
implicit receiver conversions rather than choosing names in the backend. The default
.NET generator is unchanged; no disposal/exception policy is added or removed. Async,
value enumerators, element conversions and unresolved labeled transfers remain outside
this bounded path. Accessor calls generated by the helper use the same checked callable
path as ordinary methods. No Runtime Contract or binding change is introduced.

The unchanged collections application now finishes native body planning and reports
its first unregistered class-library type, MutableMap. Linking the matching translated
System implementation remains necessary before native output can be executed.

### Explicit translated System linkage and collections acceptance (2026-10-02)

The metadata feature integration now runs the unchanged neoCLR
`application-order-collections.rvn` through direct native PE emission and native execution
(exit 0, exact expected stdout). Configuration uses `CompilationOptions.NeoCLR`, the
matching CoreProbe reference, the explicit native Self marker, and
`NeoClrMetadataDependency(reference, definition, coreLibrary, nativeImplementation)`.
The new optional `NativeLibraryDefinition? NativeImplementation` property pairs the CLI
declaration snapshot with the translated native implementation. Omitting it preserves the
metadata writer's existing dependency naming contract. No ordinary .NET defaults change.

The emitter registers this binding before importing declarations. The independent metadata
API validates selected native names, generic arities, public type/member categories,
parameters/results, managed receivers and outputs; diagnostic candidate details now show
the rejected contract. CLI references keep their exact identity. Native code uses the
implementation's module/name conventions and carries manifest mappings for PE reference
projection. Core-local TypeRefs and Func/Action carriers retain exact scope. Inhabited
System.Void storage differs from no result; the API handles the translated library's
result convention and adjusts native branch offsets when a discarded result is required.

The application body is produced directly by the metadata backend. The System library is
still translated, and CLI metadata still provides compiler symbols. This bridge does not
implement a native semantic symbol loader, complete native class-library source emission,
or general captured/generic/imported Function binding. Its cost is explicit matched-bundle
maintenance and restricted imported signatures; the intended replacement consumes native
declarations and implementation metadata directly. Raven owns target/symbol mappings, the
metadata library owns encoding/projection, and neoCLR owns runtime admission and execution.

A runtime Function access fix was also required: specialization with an application-internal
type must not revoke a generic library's permission to invoke a supplied callback. The
runtime still rejects explicit foreign internal-type references and checks binding access.
The reference System/runtime bundle includes this feature-branch fix; compiler support does
not imply a corresponding main-branch runtime release.

`NeoClrMetadataProbe --readiness-linked-sample <neo-root> <fresh-output> <runtime>
application-order-collections <System.neox>` saves hashed evidence and compares the linked
sample against its checked-in `.expected.txt`. Failed emission, verification, nonzero exit,
stderr or stdout mismatch fails the command. The acceptance covers map insertion/lookup,
Option/Result propagation, noncapturing callbacks, query composition, shared object identity
and iteration. See neoCLR's `docs/experiments/extended-cli-metadata/collections-end-to-end-2026-10-02.json`.
Focused validation also retains the seven native profile controls, 91 C# metadata groups,
and runtime binary-fixture/access tests. This is a working application checkpoint, not a
claim that all source constructs or the full runtime library use the native backend.

### Constructed collection interface inheritance (2026-10-02)

The shared declaration plan now admits owned invariant constructed interface bases
behind `AllowsConstructedInterfaceInheritance`. CLR/native adapters opt in; other
profiles retain their previous admission. Native mapping preserves original definitions
and substituted owner arguments, including owned constructed interface calls through
Callvirt. Ordinary .NET emission remains on its existing path.

The matching metadata library validates definition cycles, owner scopes, transitive
method substitution and exact public implementations. The unchanged Disposable,
Iterator<T>, Iterable<T> and Collection<T> files compile with an executable consumer;
inherited Count dispatch returns 42 on CLR and native, in both source orders. The native
runs cover host bootstrap and CompilationOptions.NeoCLR with the explicit CoreProbe Self
contract. No new runtime instruction or format category was necessary: this follows
CLI's existing InterfaceImpl/TypeSpec and generic callvirt contracts.

The C# probe `--collection-contract-runtime <neo-root> <fresh-output> <runtime>` records
source/core/runtime hashes and results. It combines source interfaces and consumer in
one assembly, not a separately bootstrapped System implementation. CLI metadata still
provides core symbols; native symbol import remains a separate future layer. Producer
limits include nongeneric implementing classes, no variance/default interface bodies
or MethodImpl mappings. Sequence<T>'s interface indexer and implementation-only seed
contracts for ArrayList remain next. Eight shared-interface tests and the unchanged
collections application's exact-output assertion also pass; see neoCLR's
`docs/experiments/extended-cli-metadata/collection-contracts-2026-10-02.json`.

### Sequence interface indexers (2026-10-02)

`InterfaceIndexer` is a separate shared declaration capability, enabled by the CLR and
native profiles. The plan accepts public bodyless instance indexers with supported
getter/setter signatures. Native emission reuses property definitions and ordinary
accessor methods; index parameters remain in CLI property signatures and native calls.
No new metadata category or runtime instruction is introduced. Default .NET emission
retains its existing generator.

The unchanged Sequence<T> source now joins the collection-contract probe. A consumer
calls inherited Count (40) and the indexer (2), returning 42 on CLR and neoCLR, with both
source orders. Native checks cover host bootstrap and CompilationOptions.NeoCLR with
the matched CoreProbe Self contract. The probe also checks the projected getter-only
property association. Shared C# tests cover getter/setter signatures, mutation and
capability rejection in Debug and Release. The focused interface tests total 29 passes.

The run exposed general [source declaration completion and abstract indexer fixes](source-interface-completion.md),
isolated in commit d360b964f and independently exercised through ordinary .NET emission.
The source/consumer assembly still obtains core symbols from the CLI snapshot; native
symbol loading is the eventual replacement. Native implementing classes remain
nongeneric, and the probe does not bootstrap the full System implementation. The next
source-emission boundary is ArrayList's RuntimeServices/CheckedStorage implementation
seed. Bundle evidence is in neoCLR's
`docs/experiments/extended-cli-metadata/sequence-contracts-2026-10-02.json`.


### Generic implementation owners and library authoring seed (2026-10-02)

`AllowsConstructedInterfaceImplementations` defaults off; CLR/native shared profiles
opt in. Generic root classes can implement owned interfaces with constructed arguments.
Native mapping uses the original interface definition plus owner arguments, rather than
looking up a constructed symbol as a declaration. Open root classes use the existing
nonsealed root-class representation. Derived class bodies, variance, explicit MethodImpl
and default interface methods remain outside this admission.

`--generic-collection-contract-runtime <neo-root> <fresh-output> <runtime>` compiles
unchanged collection interfaces through Sequence<T> with generic provider/iterator
implementations. Constructors store T, inherited Count/indexer/iterator calls return 42,
and both source orders execute on CLR/native. Native checks cover both host bootstrap
and CompilationOptions.NeoCLR with the CoreProbe Self contract. 32 focused interface
C# tests pass, including Debug/Release generic implementation and capability controls,
plus recursive owner argument admission (Box<T> implementing EchoContract<Box<T>>).
The broad collections sample retains its exact stdout and exit 0.

Use the bridge producer's `--reference-library-core <seed.dll>` for authoring library
implementations; the consumer core intentionally omits RuntimeServices and CheckedStorage.
`--library-source <neo-root> <fresh-output> <seed.dll> <System.neox>` binds unchanged
ArrayList plus its source interface hierarchy with the real target profile, then reports
native emission and a nonexecuted CLI emission control. It registers the seed's imported
Option declarations against the explicit translated System implementation. It neither
executes reference bodies nor treats an emission diagnostic as successful runtime support.
Seed/System/source hashes and the exact boundary are written to validation.json.

The current boundary is CheckedStorage.Reserve<T>: it is a bootstrap intrinsic, absent
as an ordinary native System type. Its intended operation is checked uninitialized array
reservation; replacing it with initialized newarr would change observable behavior.
A native producer mapping and explicit target contract remain to be implemented.
Core symbol loading still uses the CLI authoring seed, with native semantic importing
as the eventual replacement. These changes do not compile or execute full ArrayList yet.
See neoCLR's `generic-collection-contracts-2026-10-02.json` and
`array-list-authoring-seed-2026-10-02.json` under docs/experiments/extended-cli-metadata.


### Checked storage authoring intrinsic (2026-10-02)

`NeoClrEmitOptions` now accepts optional `MetadataReference? bootstrapReference = null`
after `systemSymbols` and exposes read-only `BootstrapReference`. Existing source calls
remain valid; binary host consumers must rebuild for the extended constructor. Null disables the
intrinsic. The exact reference must be registered in the compilation and supply its
Int32 core declaration; existing target-core identity validation still applies. This
is explicit backend configuration, independent of normal .NET emission and consumer
reference contents. The compiler still uses the shared generic call/body path.

For that exact assembly, the native adapter recognizes only public static
`System.Runtime.CompilerServices.CheckedStorage.Reserve<T>(Int32) -> T[]` on its static
owner, with one unconstrained method parameter and no byref argument. It preserves the
actual element argument, including a caller generic parameter. The metadata API emits
native `array.reserve`; the runtime retains checked unreadable slots until stored.
Same-named calls outside the bound core do not acquire intrinsic semantics. Invalid
configuration yields NEOMETA002; unsupported/malformed calls yield NEOMETA001, with
output unchanged. The helper's CLI reference body is never executed by native emission.

The host metadata API's `ReserveArray(SignatureType)` and typed raw opcode are native-only.
Its executable CLI writer rejects the operation rather than mapping it to newarr, whose
initialization contract differs. Native PE/#Neo still includes the ordinary reference-only
CLI declaration projection. See neoCLR's `docs/reserved-array-capacity.md` for the existing
.NET comparison, tracked-state cost and native semantics; no performance claim is made.

`NeoClrMetadataProbe --reserved-storage-runtime <seed.dll> <fresh-output> <runtime>`
compiles a generic Raven helper and checks stored-slot result 42, unread-slot faults,
default-disabled mapping and unregistered-reference rejection. Metadata C# checks also
cover raw emits, generic substitution, invalid element/scope/stack contracts, CLI refusal,
and native container/projection loading. All 94 metadata groups pass. The source inventory
now gets unchanged ArrayList past Reserve and stops at System.Fail's imported namespace
container, which still needs a namespace-function dependency mapping. Full ArrayList
native emission/execution is not yet complete. CLI symbols and translated System remain
the temporary bootstrap; native symbol importing remains future work.

### Unchanged ArrayList source execution (2026-10-02)

The array-length lowering recognizes System.Array or the exact configured
RuntimeIterationContract.ArrayShapeTypeName definition. It requires an instance Int32
Length getter without parameters and an admitted array receiver. No arbitrary name-only
intrinsic or binder semantic change is introduced.

The host metadata library now binds public static methods on public abstract sealed
nongeneric namespace containers carrying the exact configured core TopLevelAttribute.
Native emission uses namespace.method without an owner; CLI references retain their
container. The scoped marker is a temporary CLI representation, owned by metadata binding;
native semantic imports will eventually replace it. Raw CLI global imports remain outside
this bridge. The compiler supplies explicit declaration/native System dependencies.

The --array-list-source-runtime probe compiles unchanged ArrayList and its source
interfaces with the implementation seed and BootstrapReference. Growth, copy independence,
iteration, callback predicates/searches and Option results return 42 on native execution.
Negative capacity and invalid index reach expected System.Fail faults. A separate
namespace-function consumer verifies dynamic messages and a successful branch.
Thirteen array/default tests and two independent required-result Debug/Release tests
pass; the broad collections application retains exact output. Full library source
compilation and native semantic loading remain open; translated System still provides
dependencies. Bundle hashes and source consumers are recorded in neoCLR's
docs/experiments/extended-cli-metadata/array-list-source-2026-10-02.

### Source callback comparer fields (2026-10-02)

Explicit instance field declaration and mapping now use NeoClrCapabilities.Shared,
matching body and signature admission. Stored core Func/Action transport types map
to existing native structural Function signatures, including owner type parameters.
No new metadata encoding or shared .NET policy is added. Nominal delegates and capturing
closures retain their existing restrictions; this check uses noncapturing callbacks.

The unchanged Comparer, EqualityComparer, FunctionComparer and FunctionEqualityComparer
sources now emit native PE, verify and execute ordering, equality and hash calls through
concrete/interface receivers (42), using the same explicit implementation seed and
translated System dependency bundle as ArrayList. Native semantic importing remains
the replacement for CLI transport signatures.

### Source HashMap execution (2026-10-02)

The --hash-map-source-runtime probe compiles thirteen unchanged library source units
together using the existing explicit implementation seed, BootstrapReference and native
System dependency binding. HashMap, ArrayList, the source map/sequence interfaces and
FunctionEqualityComparer execute native PE (42), including collision chains, growth,
duplicate rejection, insertion/update, key snapshot independence, inherited interface
dispatch and absent/present Option results. A custom policy constructor also verifies
equality-equivalent keys. The isolated shared interface signature capability correction
admits imported Option<V>; no new native encoding or runtime behavior is introduced.

All 14 focused interface tests pass, and source ArrayList's success and failure cases
still pass. neoCLR's docs/experiments/extended-cli-metadata/hashmap-*-2026-10-02.json
record source/consumer and bundle hashes. CLI seed signatures and translated System
remain temporary dependencies until native semantic importing and full source bootstrap;
capturing closures remain unsupported. This does not imply a feature merge to main.

### Reference payloads and query bootstrap boundary (2026-10-02)

The unchanged source collection checkpoint now also uses an internal Order reference
type. Nine entries force map/list growth; filtered list storage retains the same objects.
Mutation through a map lookup is visible through the original and filtered lists, while
replacing the map value leaves their earlier reference intact. Iteration observes the
mutated payloads. Numeric and reference consumers both verify and execute (42). No
compiler/runtime encoding change is needed for this additional evidence.

The broad application is assessed unchanged with the thirteen source collection units.
Translated query extensions target the seed Iterable rather than the newly source-defined
Iterable, so Single is not found. Adding unchanged Operators.rvn resolves that method
binding but retains RAVT001: the iteration contract selects the seed assembly while
metadata-name lookup finds the source iterable/iterator declarations. Arrays and their
configured shape still belong to the seed. Do not erase these assembly identities or
rewrite the sample to conceal the mismatch. A coherent source bootstrap contract/core
projection must be established before claiming full source-built application execution;
source extension declaration emission and the rest of Operators remain unverified.
Native semantic importing is still deferred; the normal translated-System application
checkpoint is separate from this source-library assessment.

### Direct native import takes priority (2026-10-02)

The author now directs reading native metadata into Raven's semantic model, developing
the independent metadata library according to its already recorded definition-first
architecture. See [the import alignment](metadata-import.md#direct-neoclr-metadata-importing-next-integration-work-2026-10-02).
Existing CLI projections remain controls; new projections are not the implementation
route. Native declarations must feed compiler-owned symbols through a native loader,
then preserve identity into codegen. This is planned work; Runtime Contract settings,
current importer behavior and ordinary .NET defaults are unchanged.

### Native definition read API available (2026-10-02)

The independent library's direct native function reader now supplies shared definitions
without a CLI projection. See [the reader foundation](metadata-import.md#reader-foundation-available-2026-10-02).
This is the next importer input, not a completed Raven native loader. Current contracts,
.NET loading/emission and the existing CLI bridge are unchanged. Both-target load/emit
support remains the scope; a later Cecil investigation is deferred.

### Native function symbols (2026-10-02)

A native function library can now populate Raven's semantic model directly through
NeoClrMetadataReference.ReadAssembly. Namespace functions retain native ownership,
primitive signatures and exact assembly identity. The current target still uses its
explicit CLI core for primitive symbols and runtime contracts. This is the only CLI
bootstrap dependency required by the small semantic probe; the native library itself
is not projected. Broader native declarations and native call emission remain pending.
See [the provider contract](metadata-import.md#first-native-reference-semantic-provider-2026-10-02).

Missing/duplicate/mismatched native dependencies and wrong target selection use RAVT003.
Default CLI emission and unsupported native calls fail before writing output. Both-target
loading/emission remains the scope, with ordinary .NET behavior unchanged for CLI inputs.

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

### Direct native local class signatures (2026-10-02 development)

Native function/method/constructor signatures can now refer to another supported class
in the same native assembly. The metadata library owns immutable nominal references;
Raven maps them to the same compilation-owned types returned by namespace/type lookup
and imports output operands through the metadata API. The existing CLI primitive core
and explicit native dependency/core bindings remain required; Runtime Contract settings
are unchanged. No CLI projection of this dependency is generated, and no shared binder
or .NET loading/emission behavior changes.

The Raven source library exposes a factory, namespace/static/instance identity calls and
a constructor accepting a class. Its native consumer preserves aliases and returns 42;
all four runtime consumers pass. C# metadata checks pass 98 groups, including CLR execution
of the corresponding imported nominal signatures. Ordinary .NET regression evidence from
the preceding shared-layer slices is reused, not claimed as rerun here.

Local nongeneric root classes only: native signature dependencies on other assemblies,
nominal fields, generic/interface/value types and full System import remain pending.
The metadata library owns future native shape/resolution support; Raven's target provider
owns symbol mapping. Existing translated System runtime input and CLI core bootstrap are
still temporary and require native core/reference loading and full source emission to
replace them.

### Direct native local class fields (2026-10-02 development)

Native fields may now refer to another supported class in the same module, including
forward/cyclic declaration references. The metadata reader publishes immutable nominal
field signatures; Raven lazily maps them to canonical native type symbols and the
existing field import contract creates output-owned operands. A source-library consumer
replaces a stored object, mutates the replacement and proves original-object independence
in neoCLR (42). This follows ordinary CLR nominal field/alias behavior; PE/#Neo encoding
and runtime instructions are unchanged.

99 C# metadata groups pass, including .NET execution from native snapshot imports,
wrong-type and readonly-store rejection. All four native runtime consumers return 42.
No shared binder or .NET provider changes were made. Existing CLI primitive core and
explicit dependency bindings remain required; Runtime Contract configuration is unchanged.
No CLI projection is used for these native library symbols. External signature types,
generic/value/interface/array field profiles and full System import remain pending.
The independent metadata library owns those reader/import extensions; Raven owns symbol
mapping. Native core loading/source emission remain the replacement for bootstrap inputs.

### Explicit native signature dependencies (2026-10-02 development)

Native method/constructor/field signatures now refer to supported classes in explicitly
supplied native dependencies. The metadata library retains immutable assembly-scoped
TypeReferences and resolves them through IAssemblyResolver; Raven maps the resolved
definition to its canonical compilation-owned symbol. Emission supplies the exact
registered dependency snapshots to the new resolver-taking import overloads. Missing,
wrong-version or missing-type dependencies diagnose; no CLI projection or reflection
loading is used for these native references. Runtime Contract settings are unchanged.

A Raven-produced payload library, holder library and consumer now compile and execute
as three native assemblies (42), including constructor parameters, nominal method
results/arguments and class-valued field replacement. All five runtime consumers and
100 metadata C# groups pass. Equivalent imported signatures execute on .NET too.
Existing .NET provider behavior and CLI cross-dependency decoding remain unchanged.
No PE/#Neo schema or runtime instruction change is required.

The CLI primitive core and translated System remain explicit bootstrap inputs. Generic,
value/interface/array signatures, type forwarding and full System native import remain
open; the independent metadata library owns reader/import support and Raven owns symbol
mapping. Native core loading and source compilation remain their bootstrap replacement.

### Direct native array signatures (2026-10-02 development)

Native field/method/constructor signatures now admit one-dimensional zero-based arrays
of supported primitives and local or explicitly resolved external classes. The metadata
library owns immutable element shapes and recursive output import; Raven shares cached
signature-to-symbol mapping between methods and fields. Array symbols preserve canonical
element identity. Existing explicit dependency/core bindings and Runtime Contract settings
are unchanged. No CLI projection is used to load these native library symbols.

The Raven-produced payload/holder/consumer case now stores an external-class array in a
field, replaces an element through an alias, and passes a primitive array across libraries;
neoCLR returns 42. All five runtime consumers and 101 metadata C# groups pass, including
.NET execution of equivalent imported vectors and wrong-element-type rejection.
No shared binder or .NET provider change is made, and the format/opcodes are unchanged.

Jagged/multidimensional arrays, covariance, generic/value/interface elements, broader
properties and full native System import remain outside this slice. The explicit CLI
primitive core and translated System bootstrap still require native core loading and
source compilation for their eventual replacement.

### Direct native non-indexed properties (2026-10-02 development)

Native properties now load into canonical metadata definitions and Raven property
symbols, with associated getter/setter methods and lazily resolved primitive, class
and vector signatures. Static/read-only/write-only accessors retain visibility and
identity. Emission imports the existing method operands; no property format or opcode
changes are required. Property signature admission uses explicit NeoCLR capabilities;
shared static property lowering allows external owners through the external-reference
capability. A separate general binder fix rejects inaccessible setter writes, proved
with an ordinary C#/.NET fixture. Runtime Contract configuration is unchanged.

The Raven payload/holder/consumer case reads/writes native nominal and array properties,
reads a static property, preserves aliases and returns 42 in neoCLR. All five native
consumers and 102 C# metadata groups pass. Read-only/private setter writes diagnose.
The native dependencies are not projected to CLI. The primitive CLI core and translated
System remain explicit bootstrap inputs; the metadata library owns broader reader
support, Raven owns symbol mapping, and native core loading/source compilation remain
their eventual replacement. Indexers, generic/value/interface owners and full native
System loading remain open.

Validated with metadata commit 7368716c on codex/extended-cli-metadata and the
existing runtime bundle recorded in neoCLR native-properties-2026-10-02.json.
The independent binder fix is 23161cffb (76 focused .NET tests); it is isolated
for shared-line integration, not merged into main by this slice.

### Direct native indexed properties (2026-10-02 development)

The independent metadata reader now retains indexed-property signatures, and Raven
imports them with canonical getter/setter symbols and cached index parameters. Source
indexers use explicit NeoCLR signature capabilities; emission imports the existing
accessor method operands. No metadata schema, opcode or Runtime Contract changes are
needed. Libraries are read directly from native definitions without CLI projection.

The payload/holder/consumer case now replaces and reads an external-class array element
through an imported indexer. An overloaded String indexer reads the nominal property;
private-setter and wrong-index-type assignments diagnose. All five native consumers
return 42. The CLI primitive core and translated System remain explicit bootstrap
inputs. Setter-only indexers can be inspected but source access remains a binder gap;
full native System importing and broader owner categories remain pending.

Validation: 102 metadata groups, 75 focused .NET indexer/accessibility tests and five
native runtime consumers pass. General Raven corrections are isolated in 9ee5aad97
and 2df6f3f6d. Bundle hashes are recorded in neoCLR's native-indexers-2026-10-02.json.

### Setter-only native indexers (2026-10-02 development)

Raven now assigns through setter-only indexers loaded directly from native definitions.
The shared binder uses the property parameter contract, already provided by the native
reader, and emits the existing setter call. No metadata schema, runtime instruction,
Runtime Contract or bootstrap configuration changes are required. Reading a setter-only
indexer and compound assignment remain diagnostics because a getter is required.

The payload/holder/consumer case includes a Boolean setter-only indexer with a nominal
external value type, verifies its parameter/accessor identity in both reference orders,
and replaces an object through it before reading via a separate indexer. All five native
consumers return 42. The independent shared compiler correction is Raven e07477274,
with 79 passing .NET indexer/accessibility tests. The unchanged metadata library retains
its prior 102-group evidence. CLI primitive core and translated System remain explicit
bootstrap inputs; native interfaces/generics/value owners and full System import remain
pending. No native library is projected to CLI for symbol loading.

### Direct native interface import (2026-10-02 development)

Native nongeneric interfaces, local inheritance and root-class implementations now
load into canonical definitions and Raven symbols. Interface method abstract/virtual
flags and inherited properties drive the existing callvirt emission path. The metadata
importer validates reference-to-interface conversions against exact native relationships;
no CLI projection, runtime opcode or Runtime Contract change is involved.

A native library exposes two implementations through factories returning a derived
interface. Its separately compiled consumer invokes inherited method/property contracts
and neoCLR returns 42. All six native consumers and 103 metadata groups pass. Local
relationships are supported; external implementation edges, generic/value owners and
full System loading remain pending. The CLI primitive core and translated System remain
explicit bootstrap inputs, to be replaced by native core loading/source compilation.

### External interface storage validation (2026-10-02 development)

The native interface case now spans a contract/implementation library, a storage library
and its consumer. The storage library imports interface-valued fields, arrays and
constructor arguments directly from native metadata; its method dispatches through a
stored interface. The consumer replaces values through a shared array and confirms
original-reference independence (42). Unrelated native classes do not convert to the
interface. Both reference orders preserve canonical external interface symbols.

All six runtime consumers and 103 C# metadata groups pass, including CLR field/array
store execution using imports from native definitions. This is additional validation of
existing shared paths: no new encoding, runtime opcode, compiler policy or Runtime
Contract change. CLI core/translated System bootstrap dependencies remain explicit.
Generic/value owners and external implementation edges remain pending.

Resolved shared diagnostic gap (2026-10-02): the previously deferred incompatible
expression-bodied return was reproduced independently with .NET references. Diagnostic
traversal now binds the complete arrow body through MethodBodyBinder, reusing its return
conversion checks instead of inspecting only the expression. Functions and methods now
report CannotConvertFromTypeToType before emission, including after GetTypeInfo and on
repeated GetDiagnostics calls. The focused .NET regression/return suite passes 106 tests.
This is a general compiler fix, with no Runtime Contract, metadata encoding or target
policy change; native import/bootstrap limitations remain unchanged.

### Native generic metadata boundary (2026-10-02 development)

The independent metadata library reads/imports unconstrained static generic methods
and namespace functions, preserving names, arity and parameter/vector signatures. An
initial explicit rejection boundary kept Raven safe during that library expansion;
the subsequent symbol integration now admits this profile.

Native method parameters are owned by their declaration and compilation. Parameter
vectors are cached per method; ordinary nominal/primitive vectors retain module-wide
identity. Construct uses the shared ConstructedMethodSymbol, inference/substitution and
callable lowering. No .NET reflection objects or CLI projections are created for these
native declarations. Existing generic metadata imports emit the calls; no backend,
Runtime Contract or encoding change was needed.

The seventh native consumer compiles its library from Raven, imports it directly, and
executes inferred/explicit namespace identity, generic forwarding, static generic
methods, overloads with different arities, Int64 vectors and reference/array aliases (42).
C# checks validate canonical parameter ownership, vector identity, compilation isolation,
both reference orders and invalid argument diagnostics. All seven consumers pass.
Generic owners, constraints and instance generic imports remain outside this profile;
the explicit CLI primitive core and translated System bootstrap remain required.

### Native generic root class import (2026-10-02 development)

Direct native references now include unconstrained generic root classes whose member
signatures use positional owner parameters and vectors. NativeNamedTypeSymbol preserves
simple and metadata names, arity and declaring-type parameter ownership. Fields,
properties and methods resolve through the owner's scope; method parameters remain
independent. Construct uses the shared ConstructedNamedTypeSymbol and ordinary member
substitution. No Reflection/Reflection.Emit dependency or new codegen route was added.

The native generic library/consumer case now exercises Box<int> construction, Set and
Current, Box<Item> reference aliases and generic-owner vector arguments/results. C#
checks assert names, owner identity and constructor/property substitution. All seven
native consumers compile and execute (42); 105 metadata groups and the standalone
CLR/native generic-owner consumers pass. No Runtime Contract or encoding change.

Constraints, constructed nominal signatures in imported declarations, generic interface
inheritance and direct field emission on constructed imported owners remain unsupported.
The CLI primitive core and translated System bootstrap are still required.

### Closed native constructed signatures (2026-10-02 development)

Native parameter/result signatures now include local closed generic root classes such
as Box<int>. The metadata model exposes immutable definition references and arguments;
NativeModuleSymbol resolves them through the existing signature cache and shared
constructed-type symbols. The generic consumer now calls native CreateBox and EchoBox
namespace functions instead of allocating its integer box locally. All seven consumers
execute (42); the C# metadata/CLR counterpart passes. No Runtime Contract, codegen
abstraction or metadata encoding change was needed. Open/external generic constructions,
constraints and the full native core/bootstrap remain separate work.

### Scoped native constructions (2026-10-02 development)

Open local signatures such as Box<T> now resolve recursively through the declaring
method or type's cache. Closed signatures retain module caching. Scope checks remain
in the metadata reader; no synthetic parameters or reflection objects are created.
Shared constructed-type/member substitution and inference handle OpenBox/OpenBoxes
namespace functions and Box<TItem>.Same without new emission logic.

The C# consumer checks exact method/owner parameter identity and incompatible generic
return diagnostics; arrays of open constructions preserve aliases at runtime. All seven
native consumers execute (42), and metadata CLR/native generic import consumers pass.
No Runtime Contract or encoding changes. External generic constructions, constraints
and full native core/bootstrap remain pending.

### External native generic constructions (2026-10-02 development)

NativeGenericBridge now exposes closed and method-scoped Box<T> signatures owned by
NativeGenericLibrary, including vectors. The consumer resolves the exact original
definition in both reference orders and retains bridge-method parameter ownership.
Missing dependencies diagnose. The existing importer, recursive substitution and emitter
needed no changes: this slice expands the metadata reader and executable coverage.
All seven consumers execute (42); 106 C# metadata groups pass, including CLR forwarding
through three assemblies and missing/wrong-version resolver rejection.

No Runtime Contract, metadata encoding or core bootstrap change. Constraints and generic
inheritance remain pending. During development, qualified calls
GenericBridge.Forward(...) and GenericBridge.ForwardArray(...) reported RAV0234 for
generic namespace functions, while imported unqualified calls compile and execute.
This is an observed lookup candidate, not yet independently reproduced on .NET or
attributed to a specific binder path. Follow it up separately; no workaround was added
to the importer or emitter.

### Qualified native namespace functions resolved (2026-10-02)

The earlier RAV0234 lookup candidate was narrowed with independent .NET source controls:
qualified inferred/explicit generic calls already passed on .NET. The shared namespace
member query only collected functions promoted from container types; native providers
also expose methods owned directly by a namespace. It now includes those static methods,
using the same deduplication and namespace-import option gate. Binding uses a null
receiver for ownerless functions instead of inventing a containing type.

This is a provider-neutral contract fix, not a .NET generic inference regression.
A focused 156-test namespace/generic/completion/codegen run passes, along with four
qualified-call controls (three overlap that run). An incompatible explicit type argument
still diagnoses. Native inferred/explicit qualified calls now compile and execute; all
seven consumers return 42. No Runtime Contract, metadata encoding or runtime change.
Constraint import and full native System/bootstrap remain pending.


### Independent importer and emitter contracts (2026-10-02 direction)

The importer must populate Raven symbols; emission must reconstruct references from
those symbols without reusing loader definitions, resolvers or handles. Raven's shared
emission contracts are separate from the Cecil-like library's proposed body-generator
API. See [ownership, current violations and migration slices](metadata-backend-boundaries.md).
The current native path has not yet completed this separation. Explicit Runtime
Contract selection, CLI primitive bootstrap and translated System requirements are
unchanged; existing execution evidence does not prove the proposed separation.


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


Native public root-class identities now also reconstruct from symbols and artifact
values, including unconstrained generics. Other type profiles and member references
remain reader-backed. See [scope and validation](metadata-backend-boundaries.md#symbol-only-root-class-references-2026-10-02).


Namespace functions with external root-class signatures now reconstruct references
from symbols too, including open/closed constructions and arrays across dependencies.
See [the bounded contract](metadata-backend-boundaries.md#nominal-namespace-call-reconstruction-2026-10-02).
Type-owned members and richer type profiles remain reader-backed; bootstrap and
Runtime Contract requirements are unchanged.


Public nonvirtual root-class methods and constructors now author references from
symbols, including generic-owner substitution. See [scope and validation](metadata-backend-boundaries.md#root-class-member-contracts-2026-10-02).
Fields and richer dispatch/type profiles remain reader-backed; Runtime Contract and
bootstrap requirements are unchanged.


Supported native root-class fields now emit from symbols and an explicit compiler-owned
layout ordinal. See [layout contract and limits](metadata-backend-boundaries.md#explicit-native-field-layout-2026-10-02).
Artifact validation and Runtime Contract/bootstrap requirements are unchanged.


Nongeneric interface inheritance/implementation edges and abstract dispatch references
now author from semantic symbols. See [scope and evidence](metadata-backend-boundaries.md#interface-relationships-and-dispatch-2026-10-02).
Generic interfaces and broader inheritance remain pending; bootstrap and Runtime
Contract requirements are unchanged.


The NeoCLR body adapter now uses the metadata library's own IILGenerator while Raven's
shared emission interfaces remain independent. See [implementation and remaining audit](metadata-backend-boundaries.md#independent-library-body-generator-2026-10-02).
Runtime Contract/bootstrapping behavior is unchanged.


The library generator now owns body-authoring implementation; builder calls are
compatibility forwarders. [Validation and remaining responsibilities](metadata-backend-boundaries.md#generator-engine-ownership-2026-10-02)
record the unchanged runtime/metadata contract and reader-boundary limitations.


Native static containers now author method references from symbols as declaration
owners, while remaining excluded from signature value types. See [validation and limits](metadata-backend-boundaries.md#static-declaration-containers-2026-10-02).
Runtime Contract/bootstrap requirements and translated CLI compatibility paths are unchanged.


Native callable emission now fails closed on incomplete symbol contracts instead of
using a reader-definition fallback. See [boundary and native/CLI flag distinction](metadata-backend-boundaries.md#native-callable-fallback-removed-2026-10-02).
Translated CLI compatibility binding and bootstrap requirements are unchanged.


Native type and field reference fallbacks are now removed alongside callable fallback.
Unsupported semantic contracts diagnose; the emitter no longer creates a native resolver.
See [boundary, validation and remaining host/lifetime work](metadata-backend-boundaries.md#native-typefield-fallbacks-removed-2026-10-02).
Translated CLI and bootstrap requirements remain unchanged.


Native host binding checkpoint (2026-10-02): native dependencies now use
`new NeoClrMetadataDependency(nativeReference, coreIdentity)` without a separately
supplied reader definition. Seven native consumers execute (42); duplicate binding,
wrong core, unregistered reference and legacy snapshot mismatch reject without output.
The compiler reference retains lazy semantic reader state; explicit primitive core,
Runtime Contract and translated System bootstrap requirements are unchanged.
See the [API and compatibility contract](metadata-backend-boundaries.md#native-host-bindings-without-reader-definitions-2026-10-02).


Closed generic field checkpoint (2026-10-02): NativeGenericConsumer now loads, replaces
and aliases Box<int> and Box<int>[] fields from a separately compiled native BoxStorage
class. The holder references the generic library; both reference orders pass and the
runtime result is 42. Field owners remain nongeneric, with explicit symbol layout.
Runtime Contract/core/System bootstrap requirements and encoding are unchanged.


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
