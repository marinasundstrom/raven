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
