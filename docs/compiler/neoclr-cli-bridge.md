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
| Assembly-level functions / terminal Fault | CLI containers and a TopLevel marker stand in for assembly-level functions. Shared lookup consumes INamespaceMemberContainer; the PE provider interprets the attribute. Fault classification checks assembly, namespace, static nongeneric signature, one by-value string parameter and void/unit result. It currently also applies through legacy imported-assembly recognition. | Represent callable ownership and terminal behavior in the native contract. Avoid permanent dependence on CLR container spelling, probe identity or attribute encoding. |
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
visibility, assembly-level functions and richer type contracts rather than dropping metadata.

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

Raven now admits block/file assembly-level functions through a distinct shared target
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
container, which still needs a assembly-level-function dependency mapping. Full ArrayList
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
assembly-level-function consumer verifies dynamic messages and a successful branch.
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
NeoClrMetadataReference.ReadAssembly. Assembly-level functions retain native ownership,
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
RAV0500. Existing assembly-level-function and static-overload controls remain in the probe.

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
and assembly-level functions, preserving names, arity and parameter/vector signatures. An
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
assembly-level functions instead of allocating its integer box locally. All seven consumers
execute (42); the C# metadata/CLR counterpart passes. No Runtime Contract, codegen
abstraction or metadata encoding change was needed. Open/external generic constructions,
constraints and the full native core/bootstrap remain separate work.

### Scoped native constructions (2026-10-02 development)

Open local signatures such as Box<T> now resolve recursively through the declaring
method or type's cache. Closed signatures retain module caching. Scope checks remain
in the metadata reader; no synthetic parameters or reflection objects are created.
Shared constructed-type/member substitution and inference handle OpenBox/OpenBoxes
assembly-level functions and Box<TItem>.Same without new emission logic.

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
generic assembly-level functions, while imported unqualified calls compile and execute.
This is an observed lookup candidate, not yet independently reproduced on .NET or
attributed to a specific binder path. Follow it up separately; no workaround was added
to the importer or emitter.

### Qualified native assembly-level functions resolved (2026-10-02)

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

Native assembly-level functions with primitive, method-parameter and single-vector signatures
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


Assembly-level functions with external root-class signatures now reconstruct references
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
MethodGenericParameterTypeInfo now project assembly-level functions and declared methods,
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


### External interface declarations (2026-10-03)

The native target now admits implementation/inheritance edges to separately compiled
native interfaces through an explicit shared emission capability. Source identity checks
remain separate from referenced identity checks, and callable/local checks preserve the
active target capabilities. The ordinary .NET backend remains unchanged.

Raven authors complete external interface contracts from symbols: every direct method,
property accessor, inherited edge and generic substitution is supplied before completion.
The metadata writer validates implementations and computes CLI flags. No importer or
reader definition is reused by this path. The runtime contract still uses explicit
NeoCLR.CoreProbe bootstrap selection and exact native artifact bindings.

PE emission uses the metadata library's authored-graph binary container overload, so the
validated projection is retained rather than reconstructed from native bytes without
dependency contracts. The CLI projection remains reference-only; native format-5 and
runtime dispatch are unchanged. Reader-only CLI reconstruction for external relationships
rejects explicitly. Native semantic import has no CLI fallback.

`--dual-driver-external` now builds contracts, an implementation with a generic diamond,
and a consumer in separate invocations, removing library sources before downstream
compilation. Both .NET and native execution return 42 and check property/field mutation
through aliases and interface calls. The original paired driver/rejection checks and
seven native runtime consumers pass. Focused .NET InterfaceMetadataEmissionTests,
EmissionBackendTests and NominalLocalEmissionTests pass (11 tests).

Next: canonical bootstrap/source ownership and independently compiled iteration/collection
sources. The broad source-built application gate is not complete. Capability forwarding
is a shared-contract change with this feature's dependencies; this slice introduces no
independent binder fix requiring a separate main-based branch.

Validated metadata dependency: neoCLR `codex/extended-cli-metadata` commit `8c08829e`; runtime executable is unchanged from the recorded `101ab9c8` baseline.


### Source-library bootstrap ownership (2026-10-03)

Both `rvnc` and `rvnc neoclr` accept `--bootstrap-ownership manifest.json`. This opt-in
host configuration validates every listed type against the selected source/reference
catalog before emission: one semantic declaration, in its specified assembly. It does
not load extra references or change native artifact identity validation. Ordinary command
behavior is unchanged without the option.

Version 1 contains `libraries` entries (`assemblyName`, `sources`, `types`) and an
`iteration` RuntimeIterationContract. Optional `typeOf` and `propagation` select their
existing runtime contracts; null/omitted means the profile does not supply them. Source
paths are used by acceptance tooling, not automatically compiled by the driver. The
iteration interface names must have the same declared owner as the contract assembly.
Unknown JSON members/versions, duplicate owners, missing declarations and conflicting
core/source declarations fail before publishing output.

neoCLR's `docs/experiments/extended-cli-metadata/bootstrap/iteration-ownership.json`
selects `NeoCLR.Collections` for seven unchanged library interface sources. Generate the
small CoreProbe with the bridge's `--reference-primitive-core` mode; the existing expanded
consumer/bootstrap modes remain available unchanged. That minimal core retains primitives,
compiler markers and basic Console/Math declarations, but no competing library interfaces.
The new fixture selects no TypeOf/Propagation dependencies and references no retained
System seed. This stage does not establish full runtime-library ownership.

`python3 scripts/check-source-iteration.py --raven-root <Raven> --core <primitive-core>
--output <fresh-directory>` in neoCLR builds a library, removes its source copies, then
builds and runs a consumer with ordinary compiler commands on both targets. Collection
iteration, arrays, inherited interfaces, Count/Current/MoveNext/Dispose and alias identity
execute with exit 42. Negative ownership/version cases leave no output; selecting the
old expanded CoreProbe is rejected as a duplicate owner. No importer/emitter coupling,
metadata encoding or runtime behavior changes are introduced.

Next: source/seed ownership for ArrayList dependencies and native Self signature support.
The existing native Self runtime is not evidence that the Cecil-like metadata signature
API and native semantic importer already support it.

### Native Self metadata foundation (2026-10-03)

The separate host metadata library now preserves a distinct Self signature node and
canonical interface-scoped introspection view for bodyless instance-interface members,
including generic/vector signatures and canonical property accessors. It consumes no
VAR/MVAR slot. PE/#Neo native readers retain that signature directly; reference-only CLI
projection uses the existing fieldless Self marker in the explicitly supplied core scope.
Executable CLI emission rejects native Self signatures.

Raven Runtime Contract configuration, native symbol materialization and emission are
unchanged in this slice. Native mapping of these new views and symbol-owned implementing
type substitution/callself emission remain required. Do not treat metadata loading of a
contract-only fixture as Raven Self dispatch support. The library test fixture loads and
verifies in NeoCLR, with an independent entry point returning 42; 111 C# metadata groups
pass. The separate expanded API snapshot still has its recorded source-union refresh blocker.

### Ordinary-driver checked-storage bootstrap (2026-10-03)

`rvnc neoclr --bootstrap-intrinsics --core-reference <core>` now explicitly selects the
registered core reference as `NeoClrEmitOptions.BootstrapReference`. Missing core and
repeated flags reject before emission; omitting the flag keeps bootstrap intrinsics disabled.
The existing semantic signature checks restrict this to checked-storage reservation.
This does not change ordinary .NET codegen, metadata import or other Runtime Contracts.
The matching neoCLR bridge offers `--reference-storage-core` with only primitive/compiler
contracts and CheckedStorage, avoiding duplicate source collection/union owners.

The native adapter emits array.reserve; CLR newarr instead provides initialized default
values. The metadata-only bootstrap method body must never execute on either target.
A real .NET runtime-service adapter remains necessary for the unchanged ArrayList sources.
No runtime or metadata format change is introduced here.

neoCLR's `scripts/check-source-storage.py` runs the paired source-iteration regressions,
then ordinary driver commands compile a generic helper library, delete its source and
compile independent native consumers. Alias mutation returns 42 and an unread slot faults.
Negative bootstrap cases leave no output. This closes a host-configuration gap; it does
not claim ArrayList or broad application completion. The unchanged Option/Propagatable
subset compiles on .NET; native interface out-parameter admission is next, with Fail and
callback bootstrap gaps separately visible in the ArrayList inventory.


## Native ref/out interface step (2026-10-03)

The existing native `ByRef` signatures and `out_parameters` indices now survive declaration
materialization and metadata-only introspection. ParameterInfo exposes element type plus
an explicit Value/Ref/Out mode. Raven imports those facts into parameter symbols and authors
member references from them. The portable interface plan admits writable ref/out only when
the target's managed-reference capability allows it; readonly variants remain unsupported.
Complete external contracts retain output indices when substituting generic owner arguments.
No runtime/schema change, importer-object emission dependency or new Runtime Contract option
is introduced. Ordinary .NET Reflection/Emit remains in place. Native semantics match the
existing CLR ref/out calling contract: out must be assigned before normal return; ref input
must already be initialized. CLI In metadata is not silently treated as writable ref.

A C# driver fixture compiles contracts, implementation and consumer independently, removes
library sources, then executes generic inherited interface out assignment and ref mutation
on both targets (42). Incompatible ref/out implementations reject without output publication.
Reproduce with NeoClrMetadataProbe `--dual-driver-parameter-modes <rvnc.dll> <neoclr>
<CoreProbe.dll> <fresh-output>`. C# metadata tests cover both containers, definition/builder
parity, mode conflicts, readonly rejection and open/constructed signature substitution.
Evidence is recorded in neoCLR `docs/experiments/extended-cli-metadata/parameter-modes-2026-10-03.json`.

The unchanged source Propagatable interface now emits with the source-owned collection
contracts. Option no longer fails interface admission; it now reaches the native source
union/declaration emission rejection. This is the next source-library gate, alongside the
previously recorded Fail/callback bootstrap dependencies for ArrayList. No source stubs or
manual union carriers replace the runtime library, and broad application completion remains open.

## Union payload foundation (development, 2026-10-03)

The metadata producer now accepts direct nominal and constructed fields in owned
value types, including a `Payload<T>` embedded in a `Carrier<T>`. Builder calls and
manually attached field definitions share validation. Writing rejects recursive inline
storage and limits owned layout traversal to depth 64 and 4096 visited constructions;
references and vectors terminate inline traversal. Native input validates the same
owned layouts. External dependency layouts still require explicit dependency resolution
and runtime linking; this check does not load dependencies implicitly.

`AssemblyDefinition.ReadNativeAssembly` now materializes unconstrained top-level
value declarations, signatures, fields and supported constructors/methods. `IsValueType`
reflects the native category; sealed/sequential flags are preserved without inventing
a CLI `System.ValueType` dependency. ImportReference overloads retain the explicit
matching output core requirement. Nested declarations, constrained owners and generic
instance methods remain outside this native snapshot profile. Native snapshots remain
immutable and preserve their original bytes on Write.

This matches CLR inline value storage and copy semantics: an executable tag/payload
fixture returns 42 on both runtimes after mutating an independent copy. Direct native
imports of nongeneric/generic value constructors and methods also execute on both.
The fixture uses an Int32 tag and is a metadata contract test, not a replacement for
Raven union lowering or proof that source Option compiles.

Raven maps the introspection value category to Struct and its semantic ValueType base;
generic field substitution remains in introspection. It does not reopen imported
metadata in emission. Source value declarations, nested union cases, the Byte tag,
synthesized members and symbol-authored external value operands remain the next
compiler work. Runtime-library union sources are unchanged. No format version change,
CLI projection fallback, runtime implementation change or performance claim is needed.

## Source value declarations for union emission (2026-10-03)

The native adapter now opts into an explicit portable `ValueType` declaration category.
Top-level ordinary structs, including unconstrained generic owners, map to metadata
value definitions. Reference classes retain their existing path. Nested declarations,
value-interface implementations, ref structs and constrained value owners remain outside
this bounded source profile; no source unions are rewritten into classes or manual carriers.

Shared body planning preserves an addressed receiver for member access and takes a value
copy when `self` is used as an expression. Synthesized parameterless struct constructors
zero-initialize fields before declared initializers. The adapter uses the existing metadata
ILGenerator and value-type builder contracts. There is no metadata format/runtime change,
new bootstrap selection or importer-to-emitter dependency. Ordinary .NET emission retains
its existing Reflection/Emit path; its portable profile does not opt into this new category.

The C# `NeoClrMetadataProbe --source-value-driver <rvnc.dll> <neoclr> <core.dll> <fresh-dir>`
checks ordinary driver compilation and execution on both targets: generic inline payloads,
explicit/default constructors, accessors, self copies, local field mutation and independent
copies return 42 with empty stdout/stderr. Unsupported value-interface implementation
rejects with NEOMETA001 and no output. This is same-compilation source value coverage,
not separate native library consumption or source Option completion. Source and artifact
hashes plus command results are recorded by the harness.

Remaining union dependencies include nested case/companion declarations, the Byte tag,
synthesized union methods/relationships and symbol-authored external value references.
The source library and broad application gates remain open. No independent binder fix
was needed here; shared changes add an explicitly selected emission capability.

## Native nested case metadata (development, 2026-10-03)

`AssemblyDefinition.ReadNativeAssembly` now retains supported nested class/value
ownership beneath nongeneric declaring types, including generic nested value cases.
Local signature lookup keys include the declaring token; external TypeRef rows use
nested TypeRef scopes. Same-named cases beneath different companions remain distinct.
Nested public/internal accessibility, canonical declaring views and constructor/member
signatures survive direct native reading without a CLI projection. ImportReference
accepts these scoped native definitions with the existing explicit matching core policy.

Raven publishes nested symbols as members of their declaring type, preserving their
namespace and containing-type identities; namespace member and simple-name lookup do
not flatten them. Generic nested field substitution stays in introspection. Emission
continues to reject native nested operands outside its symbol-authored capability profile.

This follows CLI NestedClass/TypeRef identity semantics using existing native relationship
encoding; no schema change or implicit dependency loading is required. C# checks cover
same-named local/external payloads, multiple nesting levels, internal visibility, missing
dependencies and cyclic owners. Direct native nested generic/nongeneric constructor
imports execute on CLR and NeoCLR (42). Native semantic probes retain owner/field identity.

This is a reader/importer prerequisite for union cases, not source union completion.
Nested types that capture generic enclosing parameters remain unsupported. Source union
declaration collection, nested definition emission, Byte discriminators and complete
synthesized union contracts remain pending. Existing immutable snapshot behavior is unchanged.

## Nested source declaration emission (2026-10-03)

The NeoCLR adapter now opts into the compiler-owned `NestedType` declaration capability.
It collects nested class/struct declarations in owner-first order and calls the metadata
library's existing nested builders with empty child namespaces and lexical ownership.
Supported children are nongeneric root classes and unconstrained generic/nongeneric
values under nongeneric supported owners. Static children, generic enclosing-type
capture and generic nested reference classes remain explicit unsupported categories.
The ordinary .NET Reflection/Emit backend does not opt into the new portable category.

Nested lookup may provide a substituted accessor under a nongeneric owner. The native
callable resolver now reuses its original source declaration before considering external
references. This uses compiler symbols only, with no name-based member matching or
importer access. It is part of the new native emission capability; no independently
reproduced .NET regression or binder fix is claimed.

`NeoClrMetadataProbe --nested-value-driver <rvnc.dll> <neoclr> <core.dll> <fresh-dir>`
compiles equivalent sources through ordinary commands on both targets and executes 42
with empty output. It checks two same-named payloads with different owners, a generic
nested value, nested class construction, zero default storage and independent value
copies. It reads the emitted native snapshot to verify enclosing identities and rejects
generic enclosing owners and value-interface implementations without publishing output.
The explicit CoreProbe bootstrap is unchanged; no metadata schema or runtime change is
required. Existing library IILGenerator remains behind Raven's emitter boundary.

Generated union declaration collection, Byte discriminators, full synthesized union
contracts and symbol-authored external value/case operands remain pending. This test
contains ordinary nested declarations; it does not claim that unchanged Option or the
broad class-library consumer compiles yet.

## Byte discriminator prerequisite (2026-10-03)

The native adapter opts into compiler-owned Byte signatures and the ConvertByte
operation. Introspection's Byte view maps to the semantic System.Byte; emitted
references use those symbols, never importer handles. Byte fields, parameters,
returns and literals retain unsigned 8-bit identity. Numeric narrowing selects the
metadata library's `IILGenerator.Emit(OpCode.Conv_U1)`; widening uses existing integer
operations. The .NET portable adapter does not opt into Byte and retains its ordinary
Reflection/Emit path. No Runtime Contract or bootstrap selection changes are required.

This follows CLI unsigned small-integer storage with Int32 evaluation-stack values.
The metadata writer normalizes stack categories without equating `ref Byte` and
`ref Int32`. Existing native Byte storage and `conv.u1` implement truncation and zero
extension; no native format or runtime code change is needed. This does not add
floating-point narrowing, overflow-checked conversions or complete source-union support.

`NeoClrMetadataProbe --byte-discriminator-driver <rvnc.dll> <neoclr> <core.dll> <fresh-dir>`
checks ordinary source struct tags, byte literals, Int32/Int64 narrowing and widening,
then builds a byte-returning library, deletes its source and executes a separate
consumer using the emitted reference on each target. All programs return 42 with empty
output; -1 → 255 and 256 → 0 boundary assertions execute. Native library references use
the direct metadata path, with the explicit existing CLI primitive bootstrap retained.
The metadata C# checks additionally cover array/local truncation and exact byref identity.

Validation: 116 metadata test groups, native metadata fixture execution (42), paired
driver/source-absent import execution (42), and eight focused Raven emission/operator
regressions. Source union collection, synthesized union members and external value/case
operands remain pending. The pre-existing API snapshot regeneration blocker remains
recorded in neoCLR's api-docs/README.md; manual host API/XML documentation is updated.

## Source union declaration graph (2026-10-03)

The portable planning layer now discovers the complete bound union declaration graph:
carrier, optional nongeneric companion, case types, fields, properties and generated
methods. Accessor methods are deduplicated by symbol identity. Discovery includes unused
members rather than depending on application calls. Metadata owners are recorded
separately from semantic owners: generic union cases belong to the existing nongeneric
companion, including constructed case signatures. The native type adapter consumes that
physical owner without changing the language symbol's containing type.

Generated union callables with no source body may use the union declaration as a
syntax/model anchor. Their body still comes from Compilation.TryGetSynthesizedMethodBody;
the target adapter does not synthesize alternative language semantics. Ordinary .NET
Reflection/Emit remains unchanged and no Runtime Contract or bootstrap setting changes.

Native emission now preflights these declarations and reports the unsupported type,
field or callable contract. This is **discovery and admission groundwork**, not native
union emission. A final explicit gate prevents treating the carrier as an ordinary struct
and losing the union/case contract required by future native symbol imports. No metadata
format, public metadata API or runtime change is included in this slice.

`NeoClrMetadataProbe --union-declaration-driver <rvnc.dll> <core.dll> <fresh-dir>`
executes generic and nongeneric source union construction/pattern extraction on .NET (42),
then checks native NEOMETA001 rejection without publication. Both minimal native cases
now reach the synthesized ToString override contract. Unchanged source Option<T> instead
rejects its value-type interface relationship (Propagatable). This test labels native
execution as pending; a rejection check does not satisfy the end-to-end gate.

Next: value-type interface contracts/dispatch for Option, supported synthesized override
and display contracts, and native union/case metadata round trips through introspection
and Raven symbols. Do not omit unused generated members or turn off structural display
implicitly to bypass these gaps. The existing explicit CLI primitive bootstrap remains.
Seven focused declaration/backend tests cover scoped ownership, constructed cases,
canonical accessor enumeration, synthesized body lookup and backend validation; paired
ordinary-driver controls establish the current executable boundary. This is target
integration groundwork, not a demonstrated independent .NET bug fix.

## Value-interface declarations and constrained metadata calls (2026-10-03)

Raven's native adapter now opts into the compiler-owned ValueInterfaceImplementation
category. Ordinary source value types can declare the supported owned/external interface
relationships; concrete calls retain addressed value receivers. The .NET portable adapter
does not opt into this category and ordinary Reflection/Emit behavior remains the default.
No importer objects are used to author relationships, and Runtime Contracts/bootstrap
selection are unchanged. The old negative value-interface fixture is now a negative
boxed-conversion case, since a struct implementing an interface is no longer unsupported.

Separately, the metadata library's IILGenerator now exposes CallConstrained(receiverType,
target) and Emit(Callvirt, receiverType, target). This bounded profile admits owned
nongeneric value receivers and owned nongeneric interface targets. It emits CLI
constrained./callvirt and native borrowed callself, preserving exact addressed storage
without boxing. Definition and builder interface authoring share validation; native
snapshots/introspection retain value relationships and generic owner arguments. The
existing runtime executes the emitted assembly; no runtime or schema change is required.
Raven's general constrained-call lowering, external/constructed constrained targets,
open receiver parameters and boxed interface conversions remain future work.

Validation: 117 metadata C# groups; CLI/native constrained dispatch returns 42 while
checking mutation and independent copies; wrong addresses, unsupported operands,
unboxed virtual receivers and incomplete implementations reject. The ordinary
`--value-interface-driver` paired case compiles and executes source interface/struct
relationships with concrete calls on both targets (42), and rejects boxed conversion
before publication. Eleven focused Raven declaration/backend/local-emission tests pass.
The explicit primitive bootstrap is unchanged; no independently reproduced .NET binder
regression is claimed. API manual/XML coverage is current, while the previously documented
RavenDoc snapshot regeneration blocker remains.

Unchanged source Option now passes the value-interface declaration boundary and reaches
its synthesized ToString override. Next address generated override/display contracts and
native union/case metadata preservation; this slice does not complete source union emission.

## Generated union body planning (2026-10-03)

Portable callable planning now recognizes generated case constructors whose location is
a CaseDeclarationSyntax and generated payload getters whose location is a ParameterSyntax.
These symbols previously failed declaration admission even though Raven already provides
their bound bodies. The union anchor must match a declaring syntax reference of the actual
owning union. Authored methods keep their own syntax/body; an unrelated union cannot
supply the generated-body anchor. This is internal target planning, not a binder rewrite.

The existing synthesized-body factory and Lowerer remain the semantic owners. C# tests
now require successful shared lowering of carrier constructors, case constructors,
TryGetValue, payload getters and available deconstructors for generic/nongeneric unions.
Seven focused planning/backend checks and two existing .NET union execution regressions
pass. The .NET Reflection/Emit path, Runtime Contracts, primitive bootstrap and metadata
library APIs are unchanged. No native union execution is claimed from plan admission.

Investigation of the next blocker confirms that synthesized ToString must retain a real
Object virtual-slot contract. It cannot be emitted as an ordinary nonvirtual method.
The metadata declaration API now authors that override for CLI output; native runtime slot
validation also needs the explicit retained System.Object dependency and correct target
identity/name. The generated formatting helper additionally uses object/string/character
operations. Next add the bounded override/reference contract and formatting dependencies,
then preserve union/case metadata for native imports. The production union publication
gate stays closed throughout; supported core bodies are not a complete union contract.


## Separate metadata override declaration API (2026-10-03)

The NeoCLR metadata library now supports TypeBuilder.AddOverride and manually authored
MethodDefinition flags for public value-type ToString() -> String. CLI output reuses the
Object virtual slot, including an implementation that also satisfies an interface.
Ordinary and constructed generic values execute through boxed Object.ToString in C#;
the metadata library reports 118 passing groups.

This does not enable a Raven target capability. Native writing deliberately rejects the
new override profile until the explicit retained System.Object dependency and runtime
slot name are bound and validated. No importer objects need to cross the emission
boundary; future emission must author the override from symbol facts and host identities.
Runtime Contract selection, bootstrap selection, .NET Reflection/Emit codegen and the
source-union publication gate remain unchanged. Generated formatting operations and
native union/case metadata are still required after the native override binding.


## Native metadata override binding verified (2026-10-03)

NeoCLR metadata commit `b94bdf79` extends the value ToString declaration profile to
native encoding, native declaration materialization and imported direct calls. The host
must register exactly one retained System binding through the existing BindNativeLibrary
bootstrap bridge. Writing checks the CLI Object slot and native public virtual instance
String-returning slot, then records the dependency and preserves native override flags and
slot names. Missing/ambiguous/incompatible bindings fail before bytes are returned.

The C# `--native-value-override` integration mode uses the checked-in CoreProbe and a
freshly assembled real System bundle. A separate API-produced consumer imports the native
library and prints `native override`, returning 42. A neoIL harness consumes the same
unmodified library and checks boxed ordinary/generic Object.ToString dispatch, returning
42. Native reader/projection flag checks and rejection cases pass, alongside 118 metadata
groups. The harness supplies boxing instructions; it is not Raven-generated union code.
Runtime source and native format version are unchanged.

Compiler code/capabilities, Runtime Contract configuration and .NET Reflection/Emit remain
unchanged. The next Raven slice must represent the bounded override in shared callable
contracts and the NeoCLR adapter, using symbol facts and explicit host artifact identities.
Do not reuse importer objects or enable general virtual methods. Generated formatting
operations and native union/case metadata remain additional gates before source Option
or the broad application can be declared supported.


## Raven value Object overrides and explicit runtime seed (2026-10-03)

The shared callable plan now carries an ObjectToString override category derived from
bound source-symbol facts: a public nongeneric instance method on a value type, the
resolved virtual System.Object.ToString target, no parameters and a String result.
Reference-nullability annotations do not change the physical slot. Binding still owns
language compatibility. General virtual methods, class overrides and other Object slots
remain outside this target capability. The .NET adapter's default profile is unchanged.
The NeoCLR adapter calls the separate metadata library's AddOverride; bodies still flow
through Raven's portable instructions and the metadata IILGenerator. Direct addressed
calls on local ordinary/constructed generic values are supported; compiler boxing and
symbol-authored external override references are not established by this slice.

The normal driver accepts:

```text
rvnc neoclr --core-reference NeoCLR.CoreProbe.dll --runtime-seed System.neox -o App.dll Display.rvn
```

The optional seed is an explicit legacy bootstrap-to-runtime binding, not a semantic
import of the seed or fallback for native application references. It requires a core path,
module System, bounded images, a distinct output, and no --system-symbols selection.
The metadata writer validates the selected Object slot. If --bootstrap-ownership is
present, source-owned type names cannot also occur in the seed. Generic arities/nested
names are normalized to the retained native naming convention for this inventory check.
A full System seed consequently cannot accompany a manifest that rebuilds its collection
contracts; a filtered retained seed is required. Runtime Contract settings and intrinsic
opt-in remain separate and unchanged. Configuration/binding/encoding failures publish no
output. Supply the same seed to the runtime using --system.

The C# --value-override-driver check executes ordinary/generic Raven-authored struct
ToString implementations on neoCLR (stdout `native override`, exit 42), rereads native
slot flags and checks missing seed/core and conflicting ownership rejection. The same
source is attempted on .NET and its existing RAV0307 nullability diagnostic is recorded:
host Object.ToString returns string?, while this retained bootstrap says string. Raven
intentionally requires matching override return nullability; that rule was not changed
or hidden by rewriting the control source. This fixture is native execution evidence,
not a completed dual-target gate. Existing .NET union executions remain successful.

Union preflight now admits the generated override declarations and checks their shared
lowered bodies before the final publication guard. Ordinary/generic driver unions and
unchanged Option reach `union body ToString: lowered expression BoundConversionExpression`.
The --union-declaration-driver control still executes on .NET and checks explicit native
rejection/no output. Twelve focused tests cover planning/capabilities, other override
rejection, .NET union execution and return-nullability behavior. Next implement the required
conversion/formatting operations and preserve native union/case contracts; do not publish
plain structs as completed unions. No binder change or independent main-branch fix is part
of this slice. NeoCLR metadata dependency: b94bdf79.

## Typed boxing continuation (2026-10-03)

The shared BoxToObject operation carries the compiler-owned input type only. NeoCLR
explicitly admits it and calls IILGenerator.Box through its adapter. An existing conversion
from a value or generic parameter to System.Object can require boxing even when IsBoxing
is false; admission requires an existing non-user-defined conversion and supported input
signature. .NET keeps the established general conversion generator. No importer object
or Reflection handle enters the shared instruction. Metadata CoreObjectType belongs to
the explicitly configured core; native writing requires its matching System binding.

`NeoClrMetadataProbe --boxing-driver <driver> <runtime> <core> <seed> <fresh-output>`
compiles the same source through both ordinary commands, executes (boxed / 42), and checks
missing-core-registration rejection without output. This is a discarded-result smoke,
not proof of boxing semantics: C# metadata tests separately verify CLR method/owner scopes
and reference/null identity, and the native harness verifies generic value dispatch,
primitive display and reference identity. The runtime's existing box instruction is used;
no format or Runtime Contract setting changes. Application metadata import remains native.

The union driver still executes CLR controls and rejects native output; it now reaches
`union body ToString: value receiver requires an owned local or ref/out parameter`.
Generated payload addressing, Object virtual calls/formatting and union/case metadata
preservation remain pending. A direct Object.ToString invocation still rejects explicitly.
The independent [expression-body conversion fix](expression-body-return-conversions.md)
was exposed by these checks and validated separately on the main-based fixes branch as
c96305e50 (ten tests); it is not a dependency on experimental metadata for ordinary .NET.
Fourteen focused integration tests pass, plus the ten expression-body execution tests.

## Owned field-address continuation (2026-10-03)

The FieldAddress shared operation carries IFieldSymbol, while NeoClrFieldReference
selects the owned definition or constructed metadata reference inside the target adapter.
Mutable source fields can supply value-type getter/method receivers and explicit addresses;
recursive receivers borrow the original storage. No temporary value copy is substituted.
The metadata generator checks initialization, exact receiver types and ownership, then
emits standard CLI ldflda or the existing native ordinal operation. Imported/readonly
field addresses reject explicitly; .NET's profile remains on its general backend path.
No binding or Runtime Contract setting changes, and no importer handle is reused.

`NeoClrMetadataProbe --field-address-driver <driver> <runtime> <core> <seed> <fresh-output>`
compiles one unchanged source on each target and verifies nested struct mutation through
two aliases of a generic holder (42). C# capability tests ensure admission is explicit;
metadata tests verify alias mutation on CLR/NeoCLR and reject uninitialized/temporary
receivers plus readonly/foreign operands. Ten focused Raven tests and 120 metadata groups
pass. Unchanged source Option and ordinary/generic unions advance to the generated
<RavenFormatUnionValue> helper's BoundBinaryExpression/null comparison, still without native
output. Reference/null operations, remaining formatting and union/case metadata are open.

## Reference-test continuation (2026-10-03)

ReferenceIsNull and TypeTest are compiler-owned operations. NeoCLR maps them through
IILGenerator.IsNull/IsInstance and the existing Boolean operations. Non-user-defined
comparisons with a null literal use the bound operator facts, preserving overload choices.
Discard declaration patterns from reference inputs use a real type test, including null
failure; general binding/extraction patterns are not implied. Supported explicit reference
conversions use checked CastReference, including String. No importer objects are exposed.

`--reference-operations-driver <driver> <runtime> <core> <seed> <fresh-output>` runs one
source through both ordinary commands: boxed integer mismatch, null detection and String
matching return 42. C# metadata tests additionally check generic scopes and String cast
identity; the native API image verifies/runs 42. 121 metadata groups and ten focused Raven
tests pass; the nullable-source capability case is verified separately. Runtime Contract
configuration, native formats and runtime code are unchanged. Explicit core/seed binding
remains required for boxed objects and value-type tests; no native-reference fallback.

Union declaration controls advance to `union body <RavenFormatUnionValue>: invocation
virtual func ToString()`, still without native output. The remaining core virtual call,
formatting and union metadata contracts must execute before removing the publication guard.

Core Object display (2026-10-03): the shared body planner has an explicit
AllowsObjectDisplayDispatch capability, enabled by NeoCLR only. It admits the
public concrete instance System.Object.ToString() -> String slot (ignoring reference
nullability for physical representation). Other nonfinal virtual class methods remain
unsupported. No binder change or Runtime Contract change; .NET keeps Reflection.Emit.
The NeoCLR adapter imports the permitted explicit CLI core declaration and validates
its System runtime slot, then uses the metadata generator's CallVirtual. This is a
bootstrap binding, not a native application reference fallback. No native format change.
`--object-display-driver` executes generic boxed integer and String display on both
targets (stdout 42/text, exit42). C# capability tests reject other Object virtual calls.
Native union preflight next stops at get_Value's null literal; no union execution claim.

Typed null continuation (2026-10-03): shared lowering uses the semantic destination
of reference returns, locals, storage and arguments to emit the existing DefaultValue
operation for null. Bound reference-null conversions use their target type. No null
value type, runtime API or metadata encoding is invented; unsupported value destinations
remain explicit. The .NET backend and Runtime Contract are unchanged. Ordinary commands
execute null Object/String returns, local assignment and a null parameter on both targets
(exit42); 11 focused tests pass. Plain/generic union bodies now pass preflight and remain
blocked by native union/case metadata preservation. Unchanged source Option independently
reaches a BoundIsPatternExpression in TryGetOutput. These are not union execution claims.

Option body continuation (2026-10-03): conditional gotos and Boolean case-test values
reuse the existing shared pattern traversal and semantic TryGet methods. Branches retain
success-only payload assignment paths; no new case encoding is introduced. A BoundUnit
literal with an explicit RuntimeUnitContract emits a nominal default of that contract's
semantic RuntimeRepresentation. NoResult remains separate; ordinary unconfigured .NET
emission is unchanged. The existing NeoCLR adapter maps configured System.Void storage
to inhabited native Void; no new metadata operation is needed.

Validation: 14 existing/focused tests passed, then the corrected explicit-core setup for
the added configured unit test passed separately (15 total). The new .NET test executes
an out ValueTuple assignment. `--unit-storage-driver` executes native System.Void storage,
out assignment and a value argument (42); it is labelled native-only, not a paired test.
Unchanged Option/Propagatable sources now reach `native union/case metadata contract`
with no output. All source bodies passing preflight does not prove encoding or execution.

Next writer work preserves the existing .NET union attribute contract: UnionAttribute,
RavenUnionCaseAttribute's physical case name/logical name/ordinal, and
RavenUnionCompanionAttribute's generic carrier link. These need ordinary custom-attribute
model/writer/reader/introspection coverage and native symbol reconstruction. Reuse the
runtime's existing custom_attributes representation and constructor validation. Do not
remove the publication guard or replace these relationships with an unmarked struct.


Owned union execution (2026-10-03): native emission now collects every union/case/companion
member and authors UnionAttribute, RavenUnionCaseAttribute and RavenUnionCompanionAttribute
from source symbol facts. The metadata API writes existing attribute records and CLI blobs;
no importer objects enter emission. Output-owned native marker constructors are metadata
records without CLI System.Attribute inheritance, a current bounded native limitation.
The initial embedded profile does not add the backend-synthesized CLI IUnion interface;
that introspection interface is separate from case construction/matching and remains open.

The normal command requires --core-reference and --runtime-seed for generated boxing/display.
Exact core String static methods and Char type operands use validated canonical primitive
native mappings (metadata revision bf8be1f9); the .NET backend remains Reflection/Emit.
Portable lowering omits operations unreachable from the method entry while retaining label
identities. Arity-zero physical cases resolve through OriginalDefinition even when projected
through a generic semantic carrier. Union bound-body initialization is recorded separately
in [the compiler note](union-body-initialization.md).

`--union-declaration-driver <rvnc> <neoclr> <core> <seed> <fresh-output>` now executes both
plain/generic cases on both targets: exact stdout `Choice.Some(42)` / `Choice.None`, exit42,
matching both cases and retaining a copy after reassignment. Native attribute inspection
checks union/case preservation. Twenty-one focused .NET tests pass. Separate-library native
union import remains pending; this does not claim unchanged Option/Result completion.


## Native union imports (development, 2026-10-03)

The normal driver now imports separately emitted plain/generic union libraries through
native metadata. The facade resolves attributes and parameter names; Raven reconstructs
carrier, logical case and physical companion relationships. Case ordinals and ownership
must be consistent, and cases require one instance constructor. Corrupt relationships
diagnose before publishing output. Constructor parameter names are retained because
Raven's case payload-property association depends on them.

`NeoClrPrimitiveBootstrap.ReadAssembly(ReadOnlySpan<byte>)` captures an explicit CLI core
snapshot and exposes its matching `PortableExecutableReference Reference`. Hosts must
include that exact reference and select its assembly as the target core. The new
`NeoClrMetadataReference.ReadAssembly(image, bootstrap)` overload admits only that exact
core identity during native metadata resolution; application and rebuilt-library
references remain native. Conflicting bootstrap snapshots and missing dependencies reject.
The existing overload remains available for catalogs without that explicit bridge.

Emission uses compiler symbol facts and host identity/digest contracts to author external
value and physical nested owners. It does not reopen native importer objects. The current
Reflection/Emit backend remains unchanged. A small common union-companion symbol contract
replaces concrete PE checks in binding; language decisions still belong to Raven.

Validation: local and separate-library plain/generic cases compile through ordinary driver
commands and run on both runtimes (42). Local cases also print the expected Some/None
strings. Separate consumers use only emitted references, preserve an independent carrier
copy after replacing the original, and match the payload. Malformed duplicate ordinals
reject with no output. 21 focused .NET regressions and 124 metadata contract groups pass.

Limitations: native embedded marker types retain the documented bounded attribute profile;
CLI IUnion projection, imported ToString overrides and unchanged Option/Result completion
are not claimed. The source Option ownership probe now reaches an unregistered System.Void
residual type argument. This requires a coherent unit-value/bootstrap contract, not removal
of source declarations or a fallback to CLI library imports. The API documentation guest
snapshot remains stale for the previously recorded bridge issue.

The matching metadata implementation is neoCLR `1e426ff2` on
`codex/extended-cli-metadata`; all seven existing native consumers also execute after
this importer change. The union body initialization fix is separately validated on
`codex/compiler-fixes-from-neoclr` (`c64181a37`); main has not been merged.


Configured unit union execution (2026-10-03): overload argument validation now honors
an exact resolved RuntimeUnitContract instead of rejecting every SpecialType.System_Void
argument. Ordinary CLI void stays rejected without the contract. See
[unit argument resolution](unit-argument-resolution.md). The unit-storage driver now puts
an out-initialized System.Void into Residual<System.Void>.Present, matches its payload,
and passes it to a normal function; native verification and execution return 42.
No metadata/runtime encoding change was needed. This proves inhabited generic payloads,
not unchanged source Option completion: bootstrap/source ownership remains a separate gap.
The compiler fix is ca7164aa2 here and fc32e3b9e on the main-based
codex/compiler-fixes-from-neoclr branch; both pass 16 focused unit-contract tests.
Main has not been merged. Validation uses neoCLR eb8aab9b's metadata state and the
explicit core/seed hashes captured by the driver evidence.


### Unchanged source Option/Result execution (2026-10-03)

The bounded union bootstrap in neoCLR docs/experiments/extended-cli-metadata/bootstrap/README.md
now supplies executable primitive services without duplicate collection/union definitions.
The ownership manifest assigns iteration, Propagatable, Option and Result to the source
library. A generated storage core supplies explicit primitive symbols only; native library
references use the metadata importer. The seed's Object display adapter uses existing
native type-handle queries rather than constructing the guest introspection facade.

Compilation exposed two adapter gaps: authored value types could not retain interface
relationships, and imported methods could not accept the configured inhabited unit as a
value parameter. Metadata now accepts top-level value implementation edges while keeping
boxing requirements; Raven authors direct calls to concrete nonoverride value methods,
including CLI virtual interface implementations, with managed receivers. Unit parameter
positions map to the selected nominal unit representation; return positions preserve the
existing no-result mapping. Importer objects remain outside emission.

The unchanged sources compile into NeoCLR.Collections.dll. A separate consumer using only
that native reference executes with exact output `Option.Some(40)` and `Result.Error(7)`,
exit 42, and checks copies, output initialization and residuals. Missing-library and duplicate
seed ownership tests reject without output. This does not complete the executable .NET
adapter, broader collections or application-order-collections gates. See the reproducible
evidence in neoCLR docs/experiments/extended-cli-metadata/source-unions-2026-10-03.json.


### ArrayList source assessment (2026-10-03)

The next bounded bootstrap adds existing CLI callback declarations and the System.Fail
namespace marker to primitive symbols. The executable seed extends the union seed with
terminal failure; no collection implementation is replaced. Unchanged ArrayList and its
internal iterator now compile alongside the source union/iteration library. Source-included
execution passes alias mutation, copy independence, iteration and Find callback checks
(exit 42), and negative capacity terminates with its expected error.

The separate consumer still fails before publication: native declaration materialization
rejects the callback function-signature category used by ArrayList.Find and related methods.
This is the next reader/facade/importer task. The source-included run is a diagnostic control,
not a completed native-reference gate or the dual-target application gate. The existing
separate Option/Result consumer still passes. See the bootstrap README and
`docs/experiments/extended-cli-metadata/arraylist-source-assessment-2026-10-03.json` in neoCLR.
The CLI callback declarations are existing bridge transport; no new Function semantics or
integration of the separate structural Function branches is claimed.


### Separate ArrayList callback import executes (2026-10-03)

Native metadata reading now retains the existing function signature category. The
metadata-only FunctionTypeInfo facade owns generic substitution, canonical signature
views and dependency resolution. Raven maps value-returning shapes into its existing
callable symbols using the explicit primitive bootstrap; emission independently authors
callback operands from those symbols. There is no importer-object reuse, format revision,
Reflection backend change or integration of the separate Function language experiments.
Explicit no-result callback imports reject rather than silently acquiring an inhabited
unit result; the facade preserves both categories.

The ordinary driver compiles unchanged ArrayList, iteration and Option/Result sources to
NeoCLR.Collections.dll, then compiles its consumer with only the emitted reference.
Alias mutation, independent copies, iteration and Find callbacks execute (42). Negative
capacity also executes through a separately compiled consumer and fails as expected.
The assessment driver now requires native import success; the previous failure record
remains historical. Metadata contracts pass 125 groups, all seven existing native consumers
pass, and the separate source-union gate still passes. The .NET class-library adapter,
HashMap/comparers, queries and broad application gate remain open.

### Separate native HashMap and comparer library (2026-10-03)

With Raven `5a01a6008` and NeoCLR `a206aaae`, the unchanged runtime comparer policies,
map contracts and HashMap compile cumulatively with ArrayList and Option/Result. The
consumer references only that emitted native library, using the same explicit collection
primitive bootstrap and retained seed. No compiler or runtime modification was necessary.
The symbol importer and symbol-authored emitter support these signatures as implemented.

NeoCLR commit `e1043b40` adds the `--hashmap` acceptance workflow and ownership manifest
under `docs/experiments/extended-cli-metadata/bootstrap`. Its recorded
`hashmap-import-2026-10-03.json` contains commands, source/artifact hashes and revisions.
Execution checks collisions, growth, duplicate insertion, replacement, missing keys,
independent key snapshots, equality/hash and ordering callback dispatch, and shared
object mutation. Expected stdout is empty and exit status is 42. Terminal capacity
failure and missing/duplicate ownership publication guards also pass.

HashMap has no removal API. The full .NET executable class-library adapter, query
composition and unchanged broad application gate remain pending; these results do not
claim their completion. The .NET backend and structural Function experiments are unchanged.

### Native generic extension declaration and discovery (2026-10-03)

The native adapter now collects bounded instance extension methods. It follows Raven's
existing CLR lowering: receiver type parameters are lifted onto each method, and the
physical container is nongeneric/static. Shared callable/type plans distinguish that
physical shape from the source container's semantic arity. Emission uses only those
symbol facts and the existing builder APIs.

The container receives the standard ExtensionAttribute through the bounded native
embedded marker profile already used for unions. Introspection supplies marker facts;
NativeNamespaceSymbol implements INamespaceExtensionLookup so Raven's existing binder
owns discovery filtering, receiver inference and overload selection. No reflection,
reader-to-emitter coupling or format version change is introduced. The embedded marker
is a temporary nominal record without CLI Attribute inheritance, as documented for unions.

NeoCLR's `verify_source_unions.py --extensions` builds a cumulative source HashMap library,
a separate extension library and an independent consumer. Receiver-generic predicates
and method-generic selectors execute with exit 42 and empty stdout. The library sources
are absent from consumer compilation. Runtime Contract selection uses the existing
explicit collection primitive core, ownership manifest and retained seed. The .NET
Reflection/Emit backend and structural Function experiments remain unchanged.

The unchanged System.Linq Operators source now reaches OfType<U>'s object-to-generic
conversion, which remains unsupported and rejects before publication. This isolated
extension regression does not substitute for the runtime source library. Constrained
extensions, static extension members and extension properties remain unsupported by
this native declaration path. The next owning-layer work is supported generic unboxing/
conversion emission, followed by unchanged query and broad-application reassessment.
No independent binder fix was retained or requires porting to main in this slice.

### Generic unboxing unlocks unchanged query sources (2026-10-03)

Shared lowering now exposes an explicit UnboxAny operation for built-in object/reference
to generic/value conversions, gated by target capability. The native adapter calls the
metadata library IL generator; CLI/native writers encode their existing unbox.any operation.
The ordinary .NET emitter remains unchanged. Runtime Contract and bootstrap selections
remain explicit and unchanged; no importer state enters emission.

The cumulative unchanged runtime library, including the full Operators and SingleError
sources, now compiles and imports into a separate native query consumer. OfType, Filter,
Map, ToList and Single execute (42), checking boxed value extraction and retained object
identity. Incorrect unboxing faults with InvalidCast. Fifteen focused compiler capability
and .NET execution tests pass; the metadata library adds CLR generic/value/reference
execution and negative authoring checks (126 groups). No runtime implementation or native
format version change was needed.

The unchanged application-order-collections sample now stops in binding at Order[].Filter.
Array participation in the configured iteration contract's extension receiver inference
and conversion is the next bounded gap. No sample rewrite is used. Full broad-application
execution and .NET source-library adapters remain pending. Reproduce with NeoCLR's
bootstrap `verify_source_unions.py --queries`; the manifest owns the complete query source
set, and application compilation receives only emitted library references.

### Preserve nominal Array<T> backing (2026-10-03)

The author explicitly retains Array<T> backing for this integration; structural array
identity decisions belong to later structural-types work. Keep vector storage separate
from its nominal member/iteration shape. The existing RuntimeIterationContract supports
ArrayShapeTypeName, and the current shared array symbol delegates member/interface
projection to its provider. No structural array redesign is part of this slice.

NeoCLR's `array-ownership.json` selects the unchanged source System.Array<T> and its
iterator, together with source-owned Iterable/Iterator and Propagatable. The cumulative
library and a reference-only array query consumer compile. Runtime execution currently
fails interface implementation selection for the vector receiver. The next fix must
connect vector storage to the canonical nominal descriptor and source iterator; old
translated ArrayEnumerable naming must not become a silent native fallback.

The prior Filter lookup failure also exposed incomplete host configuration: the query
manifest did not opt arrays into Iterable. Separately, leaving propagation unset selects
ordinary CLR exception-capturing lowering. These observations do not justify globally
changing default .NET semantics. The ordinary query success gate remains unchanged;
this additional nominal-array case is explicitly an assessment, not an execution pass.

### Native vector backing selection (2026-10-03)

The NeoCLR adapter selects the output-owned ArrayShapeTypeName only when the configured
RuntimeIterationContract.AssemblyName matches the output identity. It passes the owned
builder to AssemblyBuilder.SetArrayBacking; absent declarations reject before publishing.
Selection uses source symbol contracts and host configuration, not importer objects.

The matching runtime links the explicit native descriptor identity and dispatches vector
interfaces through source Array<T>/ArrayIterator<T>. The private vector field aliases
storage, preserving mutation through MutableSequence<int> and the original array.
The checked-in separate consumer then calls Filter/ToList and returns 42. Reproduce with
NeoCLR bootstrap/verify_source_unions.py --arrays. The manifest includes source-owned
iteration and propagation contracts; primitive storage core and retained seed remain
explicit. Native metadata library/runtime versions must support the optional array_backing
execution field; earlier readers reject it. CLI vector encoding and ordinary .NET backend
are unchanged. Structural-array policy remains deferred. The full broad application and
.NET source-library adapter gate are still open.

Validated against the NeoCLR nominal-array slice based on ced73e9d, with matching native
metadata/runtime working changes. The unchanged broad application now rejects before
publication because the retained seed lacks Console.WriteLine(Int32); this is the next
bounded runtime-service binding gap, not an array lookup/dispatch failure.

### Broad native consumer and imported value overrides (2026-10-03)

Native MethodInfo.IsNewSlot now supplies the declaration bit needed to distinguish a
virtual inherited-slot method from a new slot. Native method symbols expose IsOverride
from these metadata facts; the NeoCLR emitter admits the bounded public value override
contract and passes it to CreateMethodReference. Reference authoring preserves the native
ToString override name and managed receiver. It uses semantic symbols and host identities,
without reopening an importer object. Other override categories remain explicitly rejected.
No shared binder/lowering or ordinary .NET Reflection/Emit behavior changes.

With the retained seed's real Int32 console adapter, unchanged application-order-collections
compiles against the separately emitted cumulative source library and runs with exact
expected stdout and exit 0. NeoCLR's bootstrap/verify_source_unions.py --application also
runs a minimal separate imported union display test (Multiple, 42). Primitive core, source
ownership and runtime seed remain explicit; native application/library imports use no CLI
projection fallback. This completes the native side of the broad gate. The equivalent
.NET source-library execution adapter work remains pending.

### Paired .NET source-library assessment (2026-10-03)

The native broad gate exposed a shared .NET binding limitation: Object.ToString's nullable
reference result prevented the unchanged nonnullable SingleError override. The bounded
same-underlying-reference strengthening fix is isolated on the main-based fix branch as
54fc1e0aa and validated by 13 diagnostic/executable tests. It changes language binding in
both targets, not a native emitter special case. Nullable value ABI mismatches and weakened
reference return promises remain rejected; parameter and property rules are unchanged.

The .NET assessment uses --emit-core-types-only to avoid competing Raven.Core union copies,
the same ownership manifest, real .NET CheckedStorage/RuntimeFailure adapters, and unchanged
source Functions.rvn. Library emission alone succeeds after the override fix. Separate
consumer emission exposes invalid System.Void in the imported Propagatable generic
instantiation. The CLR cannot use its void marker as a stored/generic value. Explicit
source-unit to CLR unit-value projection is the next requirement; array backing adaptation
also needs executable proof. No source rewriting or CLI-projection fallback is counted as
success. The driver harness records failing stages as failures.

### Explicit .NET unit storage closes imported union loading (2026-10-03)

Bootstrap ownership manifests may now carry an optional Unit contract. The .NET manifest
selects NeoCLR.DotNetServices/System.Runtime.CompilerServices.UnitValue and sets
MapClrVoidToUnit=true. Source System.Void binds to the language unit in declarations and
type expressions, while ordinary imported no-result returns keep CLR void. The target
value assembly is independent of the core; its exact artifact identity supplies emission
scope. Unconfigured and native unit profiles remain unchanged. Library sources are not
rewritten, and there is no metadata projection fallback during native import.

The shared API/binding/projection change is isolated on the main-based fix branch as
eb5df24b1. All 22 focused unit-contract tests pass there and on the integration line.
The native broad application still passes. Separate .NET union, ArrayList, HashMap and
query consumers now execute with their expected output and exit 42. The full .NET
application compiles and prints the expected prefix through PrintPending (303), then the
custom array-query path terminates with signal 10 on this macOS host. CLR arrays do not
implement the selected custom interfaces merely because semantic metadata says they do.
Next is an explicit .NET array adapter conversion (or rejection before emission when
unavailable); no full paired broad-application pass is claimed.


### Established .NET method emission restored (2026-10-03)

.NET MethodGenerator now always uses its established MethodBodyGenerator. Remove the
release-only ReflectionEmitLinearMethodBuilder and automatic portable-path selection.
The native LinearMethodBody planner, symbol operands, NeoCLR builder adapter and metadata
library remain in use; remaining ReflectionEmit capability/declaration helpers support
bounded comparison tests and are not automatic .NET body-emitter selection.

This reduces competing .NET paths rather than replacing its backend. Shared lowering is
unchanged in this slice, so this is not a complete return to the main implementation.
No Runtime Contract option changes. Native and .NET library identities remain distinct.

Validation: the same 94 tests pass before and after, with collection parallelism disabled:
SharedEmissionParityTests, SharedGenericBodyTests, SharedArrayBodyTests,
SharedInterfaceDispatchTests, PdbSequencePointTests (including matching macro PDB tests),
AsyncGenericCaptureTests, TryExpressionCodeGenTests and FunctionExpressionCodeGenTests.
These cover Debug/Release cases, calls, arrays, generic methods, callbacks, async capture,
exceptions and debug information. The rebuilt net10.0 native-enabled compiler also compiles
and runs unchanged application-order-collections with its separately built native library,
exact output and exit 0. No performance improvement is claimed.

The class-method loop-capture reproducer still distinguishes the two existing lines:
main `46491585e` prints 0, integration before this removal prints 333, expected 123. A
List<Func<int>> stores callbacks capturing each item in [1,2,3]; invoking them after the
loop exposes the lifetime problem. Top-level-function form prints/returns 0 on both.
This is independent of portable .NET emission (Debug also fails). It remains a general
closure/loop-storage defect plus a shared-lowering behavioral difference; do not restore
0 and call that a fix. Source/seed adapters do not address it.


Array-loop boundary update (2026-10-03): vector-for expansion is requested only by the
portable planner; ordinary .NET lowering retains its established for-loop path. No public
Runtime Contract, native metadata or nominal Array<T> change. The 86 focused .NET checks,
native broad application and native labeled-loop execution pass. The known lexical
closure-lifetime bug remains unresolved; see
[the parity audit](architecture/neoclr-refactor-parity.md#array-expansion-scoped-to-portable-planning-2026-10-03).


Native field/property facade adoption (2026-10-03) removes redundant reader-definition
lookups from those symbol classes. It adds no CLI transport, Runtime Contract option or
metadata encoding. Native references remain native; the explicit primitive seed and older
projection probes remain separately configured. All seven native consumers execute after
the change. See [the caller inventory](metadata-backend-boundaries.md#nativelegacy-consumer-inventory-2026-10-03).


Native callable import cleanup (2026-10-03): method/constructor and module-function
symbols now consume canonical introspection views exclusively. This removes a reader
wrapper from Raven without changing native assembly-level-function semantics, primitive
bootstrap, Runtime Contract configuration or CLI transport. All seven native consumers
and canonical constructor/accessor checks pass. Ordinary .NET loading and emission stay
on their existing paths. See [metadata import](metadata-import.md).


Native type declaration cleanup (2026-10-03) consumes the retained introspection view
for names, generic parameters and interface relationships. This requires the matching
host-only GenericParameterTypeInfo.Name member, not new CLI transport or native encoding.
The existing explicit bootstrap is unchanged. Seven native consumers pass; .NET behavior
is unaffected. Remaining type/union definition uses are documented in metadata-import.md.

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

### Native catalog rejection (2026-10-03)

Native metadata catalog construction participates in configuration diagnostic translation.
A native snapshot that claims the explicit CLI primitive bootstrap's exact identity now
produces RAVT003 (conflicting metadata snapshots), rather than an uncaught exception
before semantic setup. The introspection context remains responsible for exact-identity
validation; Raven converts its InvalidDataException/NotSupportedException into the
existing native-reference diagnostics. No fallback or identity rewriting is introduced.

The C# NativeCatalogChecks regression verifies both reference orders, GetDiagnostics,
and failed native emission preserving pre-existing output bytes and stream position.
The seven native execution consumers continue to pass (exit 42). Runtime Contract
configuration, normal .NET loading/emission and native encoding are unchanged. This is
native-adapter validation, not a general binder fix requiring a main-based port.

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

### Imported no-result callbacks and nested vectors (2026-10-03)

Native Function signatures marked no_result now enter Raven through an internal explicit
no-result factory. It preserves Action-shaped callable transport instead of applying the
source runtime policy that selects an inhabited Func<..., Unit>. Ordinary source function
construction and .NET target defaults remain unchanged. Native emission consumes the
resulting symbols; it never reopens the importer. This extends the existing callback
transport and does not integrate structural Function feature branches.

Portable emission admits recursively nested one-dimensional vectors within the existing
signature depth limit. Projected array receivers also convert to the exact configured
nominal Array<T> backing, not only its interfaces. The runtime accepts that exact backing
view without allocating or copying array storage. No covariance, rectangular arrays,
new opcode or metadata version is introduced; the writer uses nested CLI SZARRAY/native
ArrayRef signatures. Existing unsupported malformed/by-reference elements remain rejected.

The native driver gate now executes unchanged library-array-callbacks (7, 42, First,
Second), including its empty nested vector, and a nonempty nested-vector callback that
mutates the original inner arrays and returns 42. The broad application remains passing.
C# tests distinguish imported no-result and inhabited source callback identities and
exercise nested vector planning/.NET execution; metadata tests cover CLI/native round
trips, introspection, generic substitution, malformed elements and nesting limits.
The portable and factory changes are retained general candidates pending independent
main-line callers; no ordinary .NET behavior fix is inferred from native support.

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

## Native comparer bootstrap and source ownership (2026-10-04)

The explicit native profile now admits public virtual Object.GetHashCode() -> Int32,
with no generic arguments or parameters. Other class virtual methods remain bounded by
existing capabilities. The portable plan also emits binary operators already bound to
static method symbols; it does not perform new operator selection. Ordinary .NET body
emission remains unchanged. C# capability controls and ordinary content-equality/hash
execution pass alongside the native driver gate.

NeoCLR's comparer-ownership manifest adds unchanged System.StringComparer.rvn to the
separately built library. Primitive String/Int32/Object declarations and executable
runtime-service adapters remain explicitly bootstrap-owned; the host selects the new
comparer-storage core and comparer seed. The metadata adapter validates exact primitive
owner/receiver signatures and the Object hash slot. No importer objects cross Raven's
emission boundary, and no implicit CLI fallback is added for native library references.

The focused consumer covers ordinal/folded equality, content hashes, Object hash
agreement, Unicode scalar ordering, map replacement and overflow-free Int32.CompareTo.
The entire unchanged library-comparers sample now stops at integer-range loop lowering;
this focused consumer does not replace that gate. Full primitive source ownership and
runtime-service source compilation remain future work. Existing UTF-8 scalar ordering
intentionally differs from .NET UTF-16 ordinal ordering; this slice adds no new semantics.
The static operator emission fix is a deferred general candidate until another backend
caller justifies independent validation on main.

## Signed ranges and value-parameter receivers (2026-10-04)

The native profile explicitly enables signed Int32/Int64 range expansion using the
binder's start/end/step expressions and inclusive/exclusive flag. Bounds and step are
evaluated once in source order. Zero step skips the body, as in the existing .NET path;
positive/negative steps use the corresponding bound comparison. The portable lowerer
reuses ordinary locals, arithmetic and branches and preserves labeled/unlabeled loop
transfers. Each iteration's immutable local can be captured by the existing frame
profile. Increment uses existing signed add behavior; this adds no overflow policy.
Other range element categories remain unsupported by the native profile. Ordinary
.NET lowering retains its established range emitter and wider supported numeric types.

Owned by-value parameter receivers now use a logical argument-address operation when
explicitly admitted. The NeoCLR adapter emits IILGenerator.LoadArgumentAddress; it does
not copy the parameter into unrelated storage. Receiver offsets account for instance
and captured-lambda frames. Captured parameters remain outside this closure profile.
Metadata imports also retain the CLI Object signature's explicit core identity, enabling
Object.ReferenceEquals in the unchanged comparer consumer. No importer objects cross
emission boundaries, and Runtime Contract/bootstrap ownership is unchanged.

The unchanged library-comparers source now runs with exact output against the separate
source-built library. Focused range execution covers bound evaluation order, descending
and exclusive ranges, zero/empty loops, nested labeled continue, break, immutable loop
captures and Int64 bounds. 55 focused .NET tests and native acceptance pass. New generic
portable lowering remains a deferred general candidate until an independent backend
caller merits main-based validation; no .NET emitter refactor is required.

## Native integer sample bootstrap (2026-10-04)

With compiler `1fb1bbd45`, unchanged `library-integers.rvn` now compiles and executes
through the ordinary native driver. The comparer-storage primitive core explicitly
adds `Int32.Equals(Int32)` and a nonvirtual `Int32.ToString()` declaration. The retained
seed supplies readonly byref receivers: equality uses `ceq`, formatting calls the existing
Int32ToString runtime service. The sample checks local and parameter receivers, equality,
comparison and formatting at both signed extrema. The expanded `--comparers` acceptance
gate requires exact stdout and exit 0, alongside the separate native library/application.

This is explicit primitive bootstrap ownership, not compilation of `System/Int32.rvn`.
As with existing bootstrap primitive methods, ToString is a concrete direct member;
it does not establish .NET's virtual Object-slot override or boxed dispatch semantics.
Declaration-only CLI placeholder bodies never execute. The eventual native primitive
source contract must preserve those distinctions explicitly. Application/library references
remain native, and metadata/runtime encoding and both compiler backends are unchanged.
No new .NET regression run is needed for this bootstrap-only slice; the preceding
55 focused tests remain the compiler baseline. The full dual-target library gate stays open.

## Native value-result receivers (2026-10-04)

The portable body planner now gives supported value-returning property/indexer getters
and ordinary calls a temporary local address for instance calls. It evaluates the
receiver once, before arguments, and never writes the copy back. Existing local,
parameter and field receivers retain their storage addresses; parenthesized receivers
preserve that distinction. Existing managed-reference/local-address capabilities govern
admission. Byref results and other unsupported expressions are not guessed into copies.
No importer objects or target-specific builders enter the shared plan. The established
.NET body emitter is unchanged; C# controls verify getter-copy versus field mutation.

NeoCLR's explicit primitive bootstrap now includes Int64.CompareTo with exact-width
readonly byref receiver validation. CLI uses the ordinary Int64 member reference and
native output uses the existing primitive owner form; no format or VM changes. This is
not source-built Int64 and does not add arbitrary primitive virtual dispatch. Hosts must
regenerate the comparer core and matching retained System seed together. Native library
and application references still use direct metadata import.

The new value-result-consumer executes against the separately compiled native collection
library: ArrayList<long> copy/indexer behavior, getter/call evaluation order, signed
extreme comparisons, and mutable-struct copy versus stored-field mutation all pass.
Expanded application acceptance, seven native consumers, 129 metadata groups, dedicated
native binding execution/rejection checks, three new C#/.NET checks and 26 existing
range/function checks pass. The unchanged full library-generic-collections sample now
rejects only because Date is absent, before output publication. Date's source depends on
larger globalization contracts; no stub or modified sample substitutes for it.

This portable planner extension is a deferred general candidate for independent
main-based validation when another backend consumes it. The current .NET emitter
already implements temporary receiver behavior; no .NET repair is claimed.

## Source-built Duration foundation on both targets (2026-10-04)

The native acceptance tool now provides `--calendar-foundation`, which extends the
comparer ownership manifest with unchanged `ComparableTo<T>`, `EquatableTo<T>` and
`Duration` sources. These declarations belong to the source-built library; the retained
seed and primitive core are unchanged. A native consumer imports only the emitted
library and exercises ArrayList<Duration> storage, default values, copying, iteration,
equality and signed-extreme comparison. The existing broad native application still runs.

The same value-contract consumer also compiles and executes on ordinary .NET against
an independently emitted library containing those three unchanged sources. The .NET
control uses the net10 targeting pack and installed Microsoft.NETCore.App with embedded
compiler shims, no reference-only runtime service stubs. Both consumers return 42 with
empty stdout. This validates a bounded common source subset, not full collection-library
parity. No compiler, metadata, runtime or guest API implementation changed in this slice.

A compile inventory of unchanged Date/calendar/globalization dependencies reaches
binding errors for missing string indexing, RuntimeServices.SystemCultureName and
RuntimeServices.UnixTimeToLocal. The latter produces cascading invalid-index diagnostics.
No output is published. These are the first observed blockers, not an exhaustive list of
emission/runtime gaps. Date is not replaced with a stub or a source-edited approximation.
Next work must give those primitive/service contracts explicit owners and executable
adapters while preserving the shipped grapheme-based Char/indexing contract; existing .NET behavior is the control.

## Native calendar service and grapheme-length bootstrap (2026-10-04)

The comparer-storage declaration core and retained seed now expose the existing
RuntimeServices.SystemCultureName() and UnixTimeToLocal(long) contracts. The latter
returns the runtime's legacy Int32 value array; the seed adapter copies its eight fields
into a fresh nominal array reference, following the existing translated-library adapter.
That explicit conversion costs one allocation and eight element copies per call; no
performance improvement is claimed. The primitive core contains metadata-only declarations,
while execution calls the real runtime services. Rebuild the core and seed together.

The base seed also implements String.get_Length through StringGraphemeCount. Native
String.Length counts extended grapheme clusters, unlike .NET's UTF-16 code-unit length.
The preceding Duration integration note's reference to preserving scalar indexing was
incorrect: the shipped Char/indexing contract is grapheme-based. Scalar traversal is a
separate API. This slice does not yet add String's indexer or native Char signature support.

The calendar-foundation consumer checks combining/ZWJ grapheme length, host culture
service invocation without assuming a locale, eight date/time fields, positive and
negative fractional Unix ticks, fresh independent array storage and the unchanged
out-of-range fault. Expanded native application acceptance and paired .NET/NeoCLR Duration
consumption pass. An old seed fails member validation before output publication.
The unchanged Date/calendar/globalization inventory now has only two string-indexing
binding errors; the missing services and cascading array-index errors are resolved.
Later emission/runtime gaps remain unassessed while binding fails. No compiler, metadata
or runtime implementation changed, and ordinary .NET emission is untouched.

## Direct native grapheme character imports (2026-10-04)

The native emitter now admits the configured core Char in imported native callable
signatures and resolves it through the explicit host artifact binding. This keeps
character signatures in the symbol-to-emitter path; no importer object is reused.
Metadata decodes/encodes canonical CLI CHAR while native output uses the existing
grapheme Char representation, including the intrinsic method owner. The primitive
core/seed expose String's indexer and Char.ToString; regenerate the comparer core.

A separately compiled CharacterContracts library returns and accepts char. Its separate
consumer indexes combining and emoji ZWJ graphemes and preserves their full text through
native import, calls and ToString. The broad native gate, paired Duration controls,
seven native consumers and 130 C# metadata groups pass; a dedicated C# native execution
check covers reimport, projection and invalid character aliases/receiver contracts.
CLI C# controls preserve UTF-16 code units, including surrogates. The ordinary .NET
emitter and runtime are unchanged; CLI declaration projections cannot carry native
multi-scalar grapheme values as executable CLR char values.

The unchanged Date dependency inventory now passes binding and reaches unsupported
BoundPatternAssignmentExpression emission (discard assignments after propagation).
A separate character-array receiver test explicitly rejects before publication: the
portable emitter still lacks array-element addresses. The successful character consumer
does not claim that capability. Both gaps remain visible; next is the Date discard path.

## Discard and propagation emission (2026-10-04)

The portable planner now evaluates supported discard assignment operands and drops their
values, without popping a no-result call or emitting a discarded unit literal. General
lowering normalizes `_ = operand?` into the existing once-only operand/check/residual
return sequence; the unused success value is not loaded. This avoids teaching the target
emitter propagation semantics. Other pattern assignments remain unsupported in this path.

The focused native consumer imports the separately built Result library and checks that
success continues, failure returns early and side effects occur exactly once. The expanded
native application/character/calendar gate and paired Duration consumers pass. Eighteen
focused .NET/planner/propagation tests pass. The independent lowerer change and its tests
also pass six checks and are committed as `8e0f88eda` on the main-based
`codex/compiler-fixes-from-neoclr` branch; no experimental backend is needed for that fix.
The portable discard planner remains on the target integration line.

Unchanged Date/calendar sources move past discarded propagation but still reject an
unlowered propagation expression. A minimal `let value = Read()? + 1` reproduces that
remaining gap before output publication. Nested expression propagation is the next bounded
slice; Date execution and full-library completion are not yet established. No metadata,
runtime, bootstrap or public API changes accompany this slice.

## Eager binary propagation initializers (2026-10-04)

Shared lowering now expands propagation nested in eager binary local initializers,
such as `Left() + Read()? * Right()`, into statement-level checks. It saves each
left operand before evaluating the right operand, including local/field reads that
the right operand may mutate. Failure returns before subsequent operands or
statements execute; success uses the original bound operator and conversions.
This builds on the existing propagation contract, without target metadata handles.

The ordinary .NET emitter already handled these expressions. Shared normalization
lets additional emitters consume them without handling propagation themselves.
Short-circuit operators, propagation within arbitrary invocation arguments and other
expression categories are not extended by this slice; it is not a general expression
spilling pass. Focused tests cover both operand positions, nested arithmetic,
once-only side effects and early failure. No Runtime Contract or metadata change.

Native acceptance uses the separately compiled Result library, checking arithmetic
success, failure skipping the right operand and following statements, and a field read
whose original value must survive a mutation in the right operand. Date/calendar
compilation now reaches a verifier error in Date.ToString (local loaded before store
on some path); Date execution remains unproved.

## Terminal runtime calls in portable bodies (2026-10-04)

The portable planner now preserves the existing semantic terminal-call fact for the
configured legacy System.Fail namespace function. It emits the original call, evaluating
its message normally, followed by a compiler-failure guard if that call unexpectedly
returns. Ordinary void signatures in CLI metadata do not express non-returning behavior;
the guard closes that control-flow path without inventing defaults for pattern locals
or weakening the metadata verifier. Correct runtime calls preserve their original fault.

The existing assembly/namespace/signature identity check is unchanged; unrelated methods
named Fail remain ordinary calls. No new importer/emitter coupling, public API,
Runtime Contract setting, metadata format or runtime behavior is introduced.
The identity/control-flow and shared planner .NET tests pass (60 cases). Native
reference-only let-else consumers exercise successful binding and dynamic-message faults.
The unchanged Date/calendar/globalization source closure now emits; a separate consumer
executes leap-day creation, invariant formatting and AddDays through the native artifact.
This does not establish .NET execution of the complete calendar subset.

## Native globalization execution budget (2026-10-04)

The unchanged library-globalization sample now executes against the independently
compiled native calendar library using neoCLR run --instructions 1000000. The normal
100,000-instruction default is unchanged. This exposes existing host Limits through
the CLI; no compiler, importer, metadata encoding or runtime instruction semantics
changed. The native --calendar acceptance driver checks deterministic formatting,
host-locale line shapes and the final contract-success message, alongside its existing
application and paired .NET/NeoCLR Duration cases.

The next sample inventory identifies missing source-owned Clock/SystemClock and
time-zone/instant contracts. Those are library coverage gaps, not evidence of new
compiler failures. Full .NET calendar-library parity and array-element receiver
addresses remain open. No performance improvement is claimed.

## Source-owned Instant and clock integration (2026-10-04)

The native library ownership manifest now includes OverflowError, Instant, Clock and
SystemClock (47 sources cumulatively). The comparer primitive declaration core adds
only RuntimeServices.UnixTimeTicks; its matching retained seed forwards to the existing
host service. Regenerate the comparer-storage core and seed together. Old cores reject
the missing declaration; old seeds reject the missing method contract before output.

Instant.ToLocalDateTime now calls the source-owned internal
LocalDateTime.FromUnixTimeTicks directly. The old bridge-only RuntimeServices.LocalDateTime
alias already translated to that same factory. This removes a bootstrap declaration
that would otherwise reference a rebuilt-library type; it does not change the public
API or local-time semantics. Primitive bootstrap and imported library ownership remain
separate. No Raven compiler, metadata format or runtime instruction change is needed.

The unchanged library-clock sample runs through native Clock interface dispatch and
prints six valid local date/time components. A separate native consumer implements the
imported Clock contract, checks Instant value copies, signed extrema and Add overflow,
and invokes SystemClock through the interface. Full .NET clock/calendar execution is
not established; the paired Duration control remains the .NET source-library gate.

Legacy translated snapshot regeneration was attempted but --reference-library-core
fails in SourceUnionReferences.Project with RAV0103 ('None' is not in scope). No legacy
generated output or hashes were rewritten; its Instant source digest is now stale.
Native artifacts are rebuilt directly from source and do not consume that snapshot.

## Imported value-type static properties (2026-10-04)

The portable planner admits a static property on an imported value type when the
adapter explicitly enables external value signatures, in addition to the existing
property-accessor and signature checks. Reference-type owners retain their separate
external-reference capability. This matches the already supported static-call shape;
there is no receiver or importer handle to pass to emission. The ordinary .NET backend
is unchanged.

This closes the TimeOffset.Zero gap exposed by the independently built native fixed-offset
library consumer. C# contract tests cover separately emitted value/reference owners,
disabled/mismatched capabilities, and ordinary .NET execution. The regression also
runs through the native metadata adapter as part of the offset consumer.

This change belongs to the shared portable contract, which is absent on current main;
it cannot be cherry-picked independently without that abstraction. No binder or general
.NET behavior fix is deferred by this slice. Metadata, runtime and public APIs are unchanged.


### Inhabited unit in shared emission (2026-10-04)

The portable planner now resolves a UnitTypeSymbol in value position through its
explicit RuntimeUnitContract representation, subject to external-value capabilities.
It admits the same representation for locals and recursive generic arguments.
Callable unit results remain no-result; a return whose original signature is a generic
parameter remains value-bearing even when instantiated with unit. This distinction
prevents both a spurious pop after a void call and a missing pop after Echo<unit>.

The native stream gate selects NeoCLR.CoreProbe/System.Void as the inhabited value
and uses the existing native Void representation. No new format or reflection API is
introduced. Ordinary .NET uses its existing emission path; the C# control selects
System.Runtime/System.ValueTuple and executes, while existing default-unit tests remain.
38 focused ExternalSignatureCapability/RuntimeUnitContract/NeoClrUnitContract tests pass.

The neoCLR verify_source_streams.py gate compiles five unchanged stream sources into
Streams.dll, imports them into a consumer with sources absent, and executes read/write,
shared cursor/interface identity, Flush success and closed error. A second separately
compiled library exercises generic unit arguments/returns, discarded results, local
storage and arrays. Both consumers verify and exit 42 with empty stdout. Inputs and
commands are hashed in its validation.json. The source calendar/collections dependency
remains the prior native artifact. Combining all 53 sources into one PE reaches the
existing 1 MiB schema-2 envelope limit; splitting source libraries preserves explicit
ownership and does not remove that tracked whole-library size limitation.

These portable-contract changes depend on the integration architecture absent from
main; they are recorded as integration work, not an independently cherry-pickable
.NET compiler fix. No application/native library reference uses CLI projection.


### Erased Value import/emission and unit regression closure (2026-10-04)

The NeoCLR emitter admits the selected core's top-level nongeneric System.Value struct
as a symbol-only native signature operand. Identity is checked against the selected core,
not just a name. Metadata import/export maps it to the existing runtime erased carrier;
CLI declaration projection remains nominal and non-executable. No loader reader object
is reused by emission, and ordinary .NET mappings are unchanged.

The comparer bootstrap now includes exact IsValue<T>/UnpackValue<T> contracts plus the
representative ParseInt64 service. Generic seed methods use existing value.is/value.unpack
instructions, so Raven emits ordinary calls rather than introducing intrinsic-specific
compiler lowering. The separate ErasedContracts library/consumer verifies Int64 success,
Byte format/overflow status and runtime wrong-kind failure. Rebuild core, seed and dependent
libraries together; no mismatched dependency artifact is accepted as a fallback.

System.Value's intended role includes nominal and structural values outside Object's
hierarchy for metadata/introspection APIs. This slice proves the existing scalar outcome
boundary only. Runtime payload identity, storage, depth and lifetime checks still apply.
Universal payload conversion and full introspection integration are not claimed.

The cumulative native gate found two unit-related regressions after explicit storage
admission: transported function callbacks use their substituted result convention, and
an explicitly discarded no-result invocation must not pop an inhabited unit value that
was never returned. Both are corrected in the portable planner. Generic method T returns
remain value-bearing; callback tests, the discard-propagation consumer and the separate
unit array/storage consumer pass. 39 focused C#/.NET controls pass. These portable fixes
remain dependent on integration code absent from main.

Native evidence: /tmp/value-offset-final-1004/validation.json (including broad application),
/tmp/stream-value-final-1004/validation.json and /tmp/erased-gate-1004/validation.json.
The independent metadata library passes 130 groups plus explicit alias/round-trip checks.
The shared service ABI catalog is next; the seed helpers are not a complete service layer.


### Checked native bootstrap service profile (2026-10-04)

neoCLR's NativeServiceCatalog now derives comparer-core declarations and generated
service-seed.neoil from the same RuntimeServiceBindings inventory. Twenty explicitly
selected host services plus IsValue/UnpackValue have exact binding checks. Scalar,
erased-value, byte/int vector-result adapters and string worker callbacks are covered.
Unselected inventory members are not implicitly exposed. Source-owned nominal results,
wider numbers and inhabited-Void completion callbacks reject during catalog selection.
In particular, Action/no-result and fn<Void> must not be treated as identical signatures.

Offset ownership now explicitly selects NeoCLR.CoreProbe/System.Void as the inhabited
unit contract. Rebuild core, seed and dependent artifacts together. Nine unchanged source
IO/Text files compile into another native library; a source-free consumer executes UTF-8,
invalid bytes and real file create/write/flush/read/position with actual byte verification.
A separate worker-contract library exercises a string callback and erased result. This
is a service boundary test, not substitute source Tasks/Workers implementation.

The cumulative application/calendar/collection gate remains passing. Twenty-two C#
metadata binding checks and negative catalog-selection tests pass; earlier 39 .NET and
seven native semantic controls remain the shared compiler baseline. This slice changes
bootstrap authoring/tooling, not the compiler. Source TaskQueue ownership/completion
callbacks, enum emission and the single-PE 1 MiB envelope remain concrete blockers.
See neoCLR's system-compilation-strategy.md and the new catalog audit; raw diagnostic
counts remain unsuitable as a completion score. No projection fallback is added.


### Explicit library PE transport profile (2026-10-04)

`neoclr --library` now selects the metadata API's WriteLibraryBinary profile (required
schema 3). Applications continue using WriteBinary/schema 2. Native PE reference
preflight permits up to 16 MiB and delegates actual envelope/declaration validation to
RuntimeAssemblyContainer.Read, which retains 4 MiB PE/1 MiB envelope limits for legacy
schema 1/2. The redundant second 4 MiB JSON decode was removed; no permissive fallback
or importer/emitter sharing was introduced. Primitive CLI core bounds remain 4 MiB.

The library profile carries at most an 8 MiB envelope/32 MiB host JSON with existing row,
storage and depth constraints. Runtime must be rebuilt with schema-3 PE admission; old
readers reject the required schema. No .NET backend or CLI instruction behavior changed.

The combined 53-source native library now emits in one >1 MiB PE and its independent
MemoryStream and unchanged broad application consumers execute. A >4 MiB API-produced
library also imports through rvnc and executes (exit 42). C# metadata tests: 131 groups,
plus linked large-library execution; runtime envelope test passes. Evidence resides in
neoCLR's bootstrap README and /tmp/combined-library53-gate-1004/validation.json.

Adding four more IO/UTF-8 sources to the same compilation exposes byte[] -> Sequence<byte>
binding with a source-owned Array<T>, though those sources already execute in a separate
library. Investigate interface readiness/caching next rather than rewriting Utf8 source.
Full System and full dual-target library completion remain open.

### Combined UTF-8 library import (2026-10-04)

Source-owned nominal `Array<T>` interface projection now survives early imported
vector member lookup during declarations. This is a compiler caching correction,
not a structural-array change or a new bridge representation. The primitive CLI
core still declares service signatures; native libraries and consumers use native
metadata with the selected ownership manifest. The combined 57-source gate includes
UTF-8 and file streams and executes separate consumers plus the broad application.
Full System compilation and early-initializer interface conversions remain open.

### No-result scheduling callbacks (2026-10-04)

The selected native primitive core now offers ScheduleTask(Action) and
DrainEntryTasks(). Existing function transport represents Action as
`fn<noresult Void>`, distinct from inhabited `fn<Void>`; runtime scheduling accepts
either explicitly and checks the exact callback target. A separately compiled
native helper library schedules a bound receiver and its consumer observes count
42 after draining. The 57-source combined native gate still passes. There is no
compiler special case or CLR Object erasure. Source Tasks/Workers, TaskState enum
emission and source queue ownership remain the next integration gates.

### Native Int32 enum category (2026-10-04)

NeoCLR capabilities admit nongeneric top-level Int32 enums. Native symbol import takes
IsEnum/IsLiteral/Constant from the metadata introspection facade. Emission constructs
references from compiler symbols and host artifact identities, without reopening imported
metadata. Shared linear bodies represent integer-to-enum and enum-to-integer conversions;
the NeoCLR adapter delegates to metadata IILGenerator helpers. CLI enum encoding uses
System.Enum/value__/literal constants; native output uses its existing nominal enum
representation and instructions. No format version change is required.

The ordinary .NET Reflection/Emit backend is retained. Flags, other underlying widths
and nested enum declarations remain unsupported in the native capability profile.
The paired `verify_enums.py` gate in neoCLR compiles unchanged TaskState and a helper
library, imports only its artifact into a separate consumer, and executes both targets
with exit 42/no stdout. It covers mutation, arrays, named comparisons and unnamed integer
round trips. Metadata C# tests cover declaration authoring, round trips and rejection.
This is a prerequisite for source Tasks/Workers, whose queue ownership integration is
still open. Runtime Contract bootstrap ownership and primitive identities are unchanged.


## Source Tasks/Concurrency callbacks (2026-10-04)

The native target now compiles the six actual Tasks/Concurrency files into a separate
library consumed without source files. The gate covers generic continuations, interface
state-machine callbacks, cancellation, worker results and entry draining; it does not
claim full async language lowering. Primitive declarations still use the explicit CLI
core bootstrap, while application/library references use native introspection.

Portable lowering admits source-owned methods on constructed generic reference owners
and interfaces through explicit target capabilities. Emission creates output-owned
method references from symbols, without reopening importer objects. `Func<unit>` retains
its configured RuntimeUnitContract value result; Action remains no-result. For an
otherwise no-result target the native adapter emits a small callback wrapper producing
unit after invocation. The wrapper preserves receiver identity but allocates; it does
not merge the two native function signatures. Native function identity consists of
parameter/result identities and return convention, never nominal Func/Action identity.

Nullable reference locals, null/negated patterns, named reference patterns and bare
union cases now lower through shared semantic operands. The default .NET Reflection/Emit
backend remains unchanged; focused .NET execution regressions cover the affected patterns.
The queue ABI uses exact generic runtime-service signatures and source-owned TaskQueue
identity. A mismatched queue type or second registration faults rather than silently
substituting the seed's nominal type. Legacy service entry points remain supported.

Reproduction and artifact/source hashes are in neoCLR's bootstrap `verify_tasks.py` and
`tasks-native-2026-10-04.json`; the matching runtime/metadata branch is
`codex/extended-cli-metadata` (parent 055d9928). Full System compilation remains blocked
by incomplete service coverage, canonical primitive/Self ownership and wider capabilities.


## Native Self declaration/import gate (2026-10-04)

The version-1 bootstrap ownership manifest accepts optional `self` with `assemblyName`
and `typeName`, selecting the existing RuntimeSelfTypeContract. A selected marker must
resolve in that exact assembly; a missing or wrong marker fails before publication.
Omitting it preserves the old opt-in behavior. .NET defaults are unchanged.

Native introspection `SelfTypeInfo` maps to the configured semantic marker already used
by Raven's Self binding/substitution rules. Emission recognizes that exact symbol identity
and writes `SignatureType.Self`; it does not re-open the loader or emit a nominal marker
as native type identity. The marker is only bootstrap semantic transport. Introspection
retains the scoped contract; this slice does not replace Raven's existing marker-based
Self model or introduce generic arity for Self.

The actual System.Clonable source is compiled separately, then Counter implements it
from that artifact, and a third source-free consumer clones/mutates Counter and exits 42.
The metadata writer substitutes the implementing owner when checking local/external
interface obligations. It retains symbolic Self in the contract and concrete class
signatures in the implementation. Native encoding remains SelfType; executable CLI
Self contracts are still unsupported by the metadata library. Existing .NET behavior
and experimental CLI Self tests remain separate controls.

Reproduce with neoCLR's `bootstrap/verify_self.py` and the matching
`--reference-comparer-storage-core` bootstrap, now including its Self marker. This gate
uses concrete method calls: erased interface calls, constrained generic Self dispatch,
static Number contracts and source primitive ownership remain subsequent work. Validation:
16 existing Self compiler tests, 134 metadata groups, seven native consumers and driver
negative publication checks. See neoCLR `self-native-2026-10-04.json` for artifact hashes.


## Parsing family and primitive core ownership (2026-10-04)

The neoCLR bootstrap catalog now includes all eleven existing Boolean/integer/floating
parsers as String -> System.Value service contracts. This does not imply support for
emitting every primitive payload type. A separate actual Boolean/BooleanParseError
source library imports natively into a consumer that executes Boolean success/failure,
byte/Int32/Int64 boundaries and the other parsers' invalid-format outcomes.

Primitive symbols now prefer the explicitly selected metadata core, retaining
System.Runtime as the unconfigured default. Named library declarations remain separately
addressable; System.Boolean in a rebuilt library no longer displaces the core's bool
identity. The shared one-line policy fix and two regression cases were independently
validated on main as f749c1a75 (20 focused .NET tests). Seven native consumer controls and
the parsing gate pass. No native format, instruction or runtime parsing behavior changed.
Full numeric-source compilation still requires wider primitives, static Number/Self
contracts and additional checked service coverage; do not conflate catalog admission
with support for every source method. Reproduction: neoCLR bootstrap/verify_parsing.py;
artifact/source hashes: parsing-native-2026-10-04.json.


### Native floating-point integration (2026-10-04)

The native profile now maps selected-core Single/Double symbols to the metadata
library's primitive signatures in both import and independent emission. Portable
literal operands preserve IEEE bits; numeric promotions/conversions and unordered
comparison operations are compiler-owned and translated by the native adapter's
IILGenerator. `<=`/`>=` invert cgt.un/clt.un to retain NaN semantics. .NET keeps its
existing general Reflection/Emit body path; this slice does not replace that backend.

The primitive CLI bootstrap remains explicit and temporary, not a fallback for
native libraries. No Runtime Contract switch is added. Source-owned Single/Double
classes and static Number/Self dispatch remain open. The general unary primitive
binding fix was validated independently and fast-forwarded into local main as
`87ba62572` (integration `1c519d338`).

The neoCLR `bootstrap/verify_floating.py` driver compiles a separate library and
artifact-only consumer on both targets, then a native parser-payload consumer.
All exit 42 with empty output; coverage includes field/array mutation, generic calls,
casts, signed zero and NaN in both comparison operand positions. The 135 metadata
contract groups, seven existing native consumers and seven focused .NET operator/
conversion tests pass. Metadata changes expose existing clt.un/cgt.un operations;
no runtime instruction or format-version change is required. These tests do not
establish out-of-range numeric conversion parity or full primitive source ownership.


### Number declarations and operator import (2026-10-04)

The native adapter selects the StaticInterfaceMethod declaration capability. Shared
interface plans retain static/instance identity, static property accessors and operator
contracts; ordinary .NET capabilities keep their prior admission. Source operators
reuse callable body plans. Native imports classify op_ metadata names consistently
with the existing CLI importer. No loader object is reused during emission.

With explicit RuntimeSelfTypeContract configuration in the bootstrap manifest,
unchanged Number.rvn emits static identities/operators and ComparableTo<Self> inheritance.
Metadata introspection supplies Self scope and declaration facts; writers validate
complete static/instance implementations and author CLI static MethodImpl rows.
A separate Scalar struct and source-free consumer execute direct arithmetic and ordering.
This validates Number contracts, not replacement of numeric primitives. The actual
Single source now reaches `missing public interface implementation: get_Zero`: binder
Self substitution selects primitive float, while metadata still treats the owner as a
nominal struct. An explicit intrinsic primitive representation/ownership contract is
required. Generic Number callself emission is still unimplemented.

Validation: 136 metadata C# groups, a CLI constrained-static dispatch test, the native
three-assembly Number gate, and 29 focused .NET static-interface/Self/operator tests. The imported static property
conformance correction was independently validated with 27 tests and integrated into
local main as b88a8d19d; native invalid implementations now fail in binding.
The numeric feature's completion gate remains all ten actual source primitive types,
parsing and constrained algorithms; the Scalar fixture is not a replacement library.


### Native integer widths (2026-10-04)

The explicit native backend imports and emits all eight fixed-width integer
signatures. Compiler-owned operands distinguish unsigned division/remainder, shifts,
comparisons and floating conversion. The adapter selects standard CLI-equivalent
instructions; Int32/Int64 evaluation bits do not decide signedness. Signed Int32 to
UInt64 sign-extends, UInt32 to Int64 zero-extends. Field, property, vector and signature
types survive native import. Ordinary .NET capability admission stays unchanged.

The neoCLR `bootstrap/verify_integers.py` gate compiles a library and artifact-only
consumer on each target; both return 42 with empty stdout. Runtime Contract selection
remains the explicit primitive CLI bootstrap and retained runtime seed. Library
references use native import, with no projection fallback. This does not establish
canonical source primitive ownership; Number implementation identity/storage and
constrained generic calls remain open. See neoCLR's
`docs/experiments/extended-cli-metadata/integer-dual-2026-10-04.json` for commands/hashes.
Validation: 31 focused .NET static-interface/Self/operator controls and the existing
paired floating gate pass. Target-only capability expansion has no independent main
backport; general binder behavior was not changed.


### Explicit native numeric providers (2026-10-04)

The ownership manifest optionally declares `nativePrimitives`, for example
`{"System.Single":"FloatingNumbers","System.Double":"FloatingNumbers"}`.
Each entry must have the same owner in the existing library/type catalog. While
building that owner, primitive spellings use the explicit CLI bootstrap; the native
adapter validates a nongeneric top-level value declaration with exactly one private,
mutable `m_value` field of the matching primitive and no explicit constructors. It
marks the metadata definition as a native primitive, omits the pseudo-field and
implicit constructor, and emits scalar managed receiver loads/stores/addresses.
No field storage is lost: the runtime scalar is the entire representation.

Consumers use `MetadataImportOptions(coreAssemblyName, primitiveAssemblies)` to select
numeric special-type providers. The immutable map admits only numeric special types.
Keyword, metadata-name and namespace lookup select the same native declarations;
missing providers cannot fall back to bootstrap copies. Other primitive/Unit bootstrap
bindings remain explicit. The ordinary .NET target rejects native provider maps and
retains its Reflection/Emit backend. Native symbols derive primitive identity from
introspection facts. Emission authors primitive member contracts from symbol facts
and dependency identity/digest, without reopening reader definitions.

`NeoClrEmitOptions.PrimitiveImplementations` is the explicit output-owned primitive
set. This configuration is target-specific, not a general rule that every System-named
struct has special storage. Actual source Single/Double and NumberParseError now build
and execute via `bootstrap/verify_native_floating.py` in neoCLR. The artifact-only
consumer checks identities, scalar methods, parsing payloads/errors, NaN ordering and
arrays. Its missing-reference control publishes no file. Numeric source ownership for
remaining types and generic Number-constrained static dispatch remain open. The
native metadata API is kept separate; this does not replace .NET primitive types.


The next numeric slice rebuilds the cumulative source subset and Number plus all ten
numeric types under one native assembly owner. Earlier artifacts that called seed-owned
Int32/Int64 members must be rebuilt; dependency visibility is not relaxed to redirect
them into an unreferenced library. During provider compilation, exact bootstrap member
contracts resolve to the selected output-owned source member; missing or ambiguous
matches reject. The bootstrap reference is validated against Object, which remains in
the explicit core, rather than the replaceable Int32 provider. Default .NET is unchanged.

neoCLR's `numeric-seed.neoil` excludes Int32/Int64 declarations. Regenerate the comparer
storage core and service seed with the current checked catalog (which adds the existing
Int32ToString/Int64ToString services). `verify_native_numbers.py` rebuilds the cumulative
source library from its ownership manifest, then compiles a consumer with no library
sources. It checks all ten numeric identities, parsing boundaries, ordering, formatting,
checked division and arrays, returning 99 with no stdout. Runtime primitive members use
canonical names; native interface matching preserves exact signature/access checks.
Evidence: neoCLR `numeric-source-family-2026-10-04.json`. Generic Number constraints and
static dispatch through type parameters remain the next compiler gap. These are target
integration changes; no independent general .NET fix was identified for backport.


### Number generic-call prerequisite (2026-10-04)

The independent metadata IL generator now supports static CallConstrained/Emit(Call)
for concrete owned class/value implementations and owned nongeneric interfaces, with
Self stack-signature substitution. CLI output uses constrained./call and native output
uses nonborrowed callself; both execute in focused C# and native tests. This does not
change Raven's callable admission. A generic Sum<T> constrained by native Number binds
but still rejects at emission. Preserve that rejection until method bounds survive the
metadata writer/reader/introspection path and open constrained call operands are supported.
The remaining sequence is recorded in neoCLR's system-compilation-strategy.md, with
static-constrained-2026-10-04.json as the concrete-dispatch evidence. The .NET backend
and compiler importer/emitter boundaries are unchanged.


### Native method-bound import (2026-10-04)

The NeoCLR adapter now obtains owned nongeneric method interface bounds through
MethodGenericParameterTypeInfo.GetInterfaceConstraints. Lazy compiler-owned symbols
retain canonical ConstraintTypes, TypeConstraint flags, Interfaces and AllInterfaces.
Raven's existing binder accepts a conforming native value type and rejects an unrelated
primitive argument; no inference or accessibility policy moves into metadata resolution.
Runtime Contract and explicit bootstrap/seed selection are unchanged.

The metadata library writes standard CLI GenericParamConstraint and existing native
TypeBound records. Its legacy reference-only CLI projection preserves the same bounds,
while the probe uses direct native semantic import. This introduces no bridge fallback.
External/constructed method bounds and open constrained emission remain unsupported;
portable callable admission still rejects constrained calls before publishing bytes.
No general .NET change or main backport is needed for this native adapter change.

Validation: NativeGenericSymbolChecks checks canonical bounds, inherited interfaces,
valid/invalid binding and failed output publication. Seven existing native consumers
execute. Metadata C# tests and native bounded-method execution are recorded in neoCLR's
method-interface-bounds-2026-10-04.json, with metadata 43c1a96b and this compiler slice above Raven 3787f15fb. Generic Number execution remains the next goal.


External nongeneric method bounds now resolve through the same facade and compiler
mapping, with a separate contracts artifact in the explicit reference catalog. The
native probe checks canonical dependency symbol identity, valid/invalid arguments and
missing dependency diagnostics. Seven native consumers still execute. No Raven
production adapter or ordinary .NET change was needed for this extension; no general
fix is being backported. The metadata fixture itself executes on .NET and NeoCLR with
exit 42. Open constrained-call emission is still rejected. See neoCLR's
external-method-bounds-2026-10-04.json for artifact/revision evidence. The legacy CLI
projection explicitly rejects these bounds; native references use the direct importer.

Tested with metadata commit `a2450bfc`; this probe slice is based on Raven `b2a99bccd`.


### Open constrained-call prerequisite (2026-10-04)

The metadata IL generator now authors static calls through a method type parameter with
an owned interface bound, including inherited interfaces. Its separate typed/raw APIs
substitute native Self with that parameter. Standard CLI constrained./call and native
callself execute the focused generic fixtures with exit 42, without runtime changes.
This does not yet enable Raven constrained emission: CallableSignature.TryCreate still
rejects method constraints. External target operands, semantic bound authoring and the
portable open-call operation must be supported before admission changes. Runtime Contract
configuration and the default .NET Reflection/Emit backend remain unchanged. Validation
is recorded in neoCLR's open-constrained-methods-2026-10-04.json; no compiler fix or main
backport is claimed by this documentation slice.

Verified metadata implementation: `812417c7`; Raven implementation remains `56cfefbc0`.


### Generic Number integration completed (2026-10-04)

The native target explicitly enables method interface bounds and compiler-owned
ConstrainedCall operands. Callable admission accepts supported nongeneric interface
constraints; class, constructor, value/reference special constraints and generic interface
method bounds remain rejected. The default .NET capability selection is unchanged.
NeoClrCallableDefinitionBuilder authors bounds from semantic symbols. Lowering retains
the implementing method parameter and the abstract declaration for NativeSelf static
operators/accessors, plus a managed receiver for supported inherited instance calls.
The metadata adapter selects the local/external/constructed IL-generator overload.
External numeric scalar ownership and interface conversions are registered from semantic
facts and host artifact identities. Emission does not reopen the native importer.

The checked-in native-number-algorithms.rvn and native-number-generic-consumer.rvn in
neoCLR are now part of verify_native_numbers.py. The driver rebuilds the cumulative
source subset and all ten numeric types, compiles the generic library against Numbers.dll,
then compiles the consumer against those two artifacts without their sources. NeoCLR
verifies and executes every Number member: arithmetic, Zero/One, inherited CompareTo and
generic forwarding. Exit 42 and empty stdout are required; the concrete numeric consumer
still returns 99. Sum<string> diagnoses RAV0320 and publishes no assembly.

Runtime Contract configuration retains the explicit primitive core, numeric runtime
seed, native Self contract and bootstrap ownership manifest. No runtime format change or
new numeric semantics were required. The seven native consumers, 140 C# metadata groups
and 122 focused .NET generic/Self/static-interface/type-constraint tests pass. See neoCLR's
number-generic-end-to-end-2026-10-04.json for exact commands, revisions and artifact hashes.
No independent general binding fix was introduced, so no main backport is claimed.
This completes the bounded Number feature, not the full runtime-library gate. Structural
Function work, wider constraint categories and a .NET backend replacement remain separate.

Verified pair: Raven implementation `41b2573fa` and neoCLR metadata `ea7fdf77`.


### Source-order conversion stability (2026-10-04)

The source-built numeric/stream library previously failed assigning ArrayList<byte>
to List<byte> when an empty file preceded its sources. Shared binding had cached a
provisional failed conversion before interface declarations were complete. Such queries
now bypass conversion caching until declarations complete; target policies, Runtime
Contract configuration, importer/emitter boundaries and metadata encodings are unchanged.
The 70-source library builds with the empty file first and in reverse source order.
Separate native consumers execute numeric checks (99), generic Number dispatch (42)
and unchanged application-order-collections (exact output, exit 0). Adding the six
Tasks/Concurrency sources also compiles and its artifact-only consumer exits 42.
Incompatible generic arguments still report RAV0320 without output publication.
The focused ordinary .NET conversion/generic suite passes 139 tests. This is a shared
binding correction, not new CLI bridge behavior or completion of the entire library.


### Native text-service bootstrap prerequisite (2026-10-04)

NeoCLR's checked service inventory now selects the existing text-service family, with
Char/UInt32 and their vectors. Rebuild the explicit CLI primitive core and native seed
as a matched pair, then rebuild library artifacts; old incomplete cores reject missing
members before output. Runtime Contract settings and native semantic import remain
unchanged. The CLI core supplies signatures only; native calls execute the existing
Unicode grapheme/UTF-8 implementation. It does not imply .NET UTF-16 char parity.

With compiler 459856a71, a separate native library containing unchanged UnicodeScalar
and a text-service contract test executes through a source-free consumer (exit 42).
The numeric gate still executes using the expanded seed. Canonical source String/Char
ownership is not complete: String source reaches the adapter's explicit interface
Count-property rejection. Char source emission alone is not evidence of intrinsic
storage/member ownership. Implement accessor emission and native primitive ownership
instead of projecting application/library references into CLI metadata. No importer,
emitter or metadata library implementation was changed by this service-catalog slice.

### Native text ownership replaces the remaining text bridge behavior (2026-10-04)

For the source-owned text gate, the host removes String and Char from the retained seed.
Application/library references use native introspection and native emission. Only the
explicit primitive core/service signatures remain CLI bootstrap metadata. Their CLI Char
elements map to the configured native grapheme owner; they do not define runtime UTF-16
semantics. Unbound CLI Char continues to denote a code unit.

The old bridge synthesized String(Sequence<char>) and redirected its allocation to a
factory. String now declares that constructor in Raven source. The metadata API encodes
ordinary instance .ctor facts and newobj; the runtime executes construction through private
String storage. The unchanged sequence sample verifies named arguments, immutable copying
and merged grapheme boundaries. Source Char likewise supplies its real methods; native
symbol import retains its special type and the adapter uses explicit grapheme definitions.
No consumer stubs, competing seed copies or application CLI projection remain in this gate.
See [native emission contracts](api/neoclr-emission.md#source-owned-char-and-string-2026-10-04).

### Source encoding layer (2026-10-04)

Seven unchanged neoCLR encoding sources compile against the emitted native Char/String
library, then a source-free consumer incrementally encodes/decodes UTF-8 and checks
ASCII and malformed-input errors. Reference-field stores in the shared portable plan
now spill their receiver before evaluating the RHS and reload it with the result. This
preserves once-only, receiver-first evaluation and permits terminal failure in a branch
without a stray receiver on the evaluation stack. Value-type managed receiver stores
are unchanged. No new runtime contract, opcode, metadata encoding or CLI projection.

Validation: 20 EmissionCapabilityTests pass on .NET 11; native encoding, field-order and
terminal-failure consumers execute. Rebuilt native Char/String acceptance passes.
Runtime evidence: neoCLR `docs/experiments/extended-cli-metadata/source-encoding-2026-10-04.md`.
The cumulative source build still binds String through the limited primitive bootstrap;
this gate builds a separate encoding library against the completed native provider.
StreamReader/Writer next reject a BoundRequiredResultExpression statement; JSON and
ordinary class inheritance remain subsequent work.

### Text stream control flow (2026-10-04)

The portable body adapter now unwraps required-result expressions at statement
boundaries and normalizes return expressions to return statements. Local initializer
blocks without disposal/fixed storage are expanded into their ordered prefix statements
and final initialization. This admits early returns in match initializers, as used by
StreamReader.ReadPart, and discarded match blocks in StreamWriter.Pump. A nested value
block with earlier live operands still rejects before output; control-flow scanning
includes initializer blocks and wrapped returns. No importer, runtime contract, format
or opcode changes are needed. Full arbitrary expression-exit normalization remains open.

Five unchanged stream sources compile separately against native Encoding.dll and the
source-owned text/collection library. Consumer execution verifies Unicode line and
whole-stream I/O, byte counts, leaveOpen, EOF, read bounds and invalid UTF-8. The minimal
match-return consumer checks both early-return and ordinary-result paths. Ordinary
.NET EmissionCapabilityTests include corresponding observable control-flow coverage.
See neoCLR docs/experiments/extended-cli-metadata/source-text-streams-2026-10-04.md.

## Local class bases and constructor calls (2026-10-04)

The native adapter opts into `AllowsLocalClassInheritance`, bounded to ordinary
nongeneric source classes in the same output. The shared type plan carries the
semantic base identity; declarations are ordered by dependencies rather than source
order. Shared lowering carries `BaseConstructorCall` with the bound constructor and
argument expressions, before field initializers. The native adapter resolves its
own output method builder and emits Call through IILGenerator; it does not consult
metadata loader objects. Root Object initialization remains adapter policy.

The metadata library encodes CLI TypeDef.Extends and the existing native base
relationship. Native declared field slots include ancestor storage; CLI operands
remain field tokens. This adds no projection fallback or bootstrap mapping. Existing
primitive-core/seed/source-ownership configuration is unchanged. The ordinary .NET
backend retains its general generator and does not enable this portable capability.

The accompanying compiler fix resolves explicit constructor initializers after
source member registration and preserves canonical source constructor symbols.
It also fixes a .NET defect reproduced on main and is isolated independently; see
[constructor binding](constructor-initializers.md).

`bootstrap/verify_class_bases.py` in neoCLR provides the paired driver gate for
forward declarations, base and derived field initialization, inherited mutation and
alias identity. External/constructed class bases, abstract/virtual class contracts,
protected constructors and Raven closed-family metadata are not enabled by this
capability. Unsupported shapes must still reject before publication; JSON remains
outside this checkpoint.

Verified with metadata library `f02f893d`: both paired driver processes return 42,
including base-typed alias mutation, and the existing five-source text-stream library
and artifact-only consumers still pass. The integration branch also passes 34 focused
.NET 11 emission/constructor tests. Compiler/runtime binaries and bootstrap artifacts
are hashed in neoCLR's `class-base-driver-2026-10-04.json` evidence.

### Runtime hierarchy prerequisite and main reconciliation (2026-10-04)

Raven main now contains the independently validated constructor-binding fix and the
remaining isolated shared fixes at `4f95db536`; 148 focused .NET 11 tests passed on
the merged code. Integrated fix branches were retired; the experimental native backend
remains separate. The opt-in CLR source-void alias is documented as a Runtime Contract
extension and leaves ordinary .NET defaults unchanged.

The corresponding neoCLR runtime continuation admits `protected` instance constructors
and checks caller ancestry by resolved identity across binary dependencies. Closed class
roots require abstract/nonsealed reference representation and same-assembly/revision
direct children. An open local child remains extensible externally. This is runtime
validation only: native metadata builder/facade support and Raven capability admission
remain pending, with JSON still rejected before output. Source-file/permits validation
belongs to Raven. No importer-to-emitter coupling, bridge fallback, primitive-core or
source-ownership configuration change is introduced.

### Closed families and protected constructors (2026-10-04)

The native adapter now opts into separate closed-family and protected-constructor
capabilities. Source type plans admit nongeneric top-level closed roots; syntax gates
permit sealed/permits declarations, while binding retains source-file/permits rules.
Protected admission applies only to constructors. Ordinary .NET portable defaults
remain unchanged and its full Reflection/Emit backend still executes the case.

The metadata library preserves CLI Family access, Abstract roots and the existing
native closed-family flag. Native introspection supplies direct-family members and
actual BaseType identities to symbols. The emitter declares bounded dependency-local
class-base conversion facts from those symbols and explicit artifact identities; it
does not reopen importer objects. These facts authorize writer stack conversions,
while native verification checks actual linked definitions. No new source class can
yet derive from an external assembly through this authoring path.

Closed native PE carries closure in its authoritative native payload. Its nonexecutable
CLI projection currently carries Abstract but no closed-family attribute; metadata
library executable CLI Write rejects that unsupported representation. Raven's ordinary
.NET emitter retains its existing closed-family attributes. The author requested a
later design pass on richer direct closed-family metadata encoding. No opcode or format
fork is introduced in this slice. Primitive bootstrap, retained seed and ownership
configuration remain unchanged; native application/library references never fall back
to the CLI projection.

Validation: 35 focused .NET 11 capability/constructor tests, 146 metadata groups and
the existing 55-test runtime prerequisite pass. The paired driver builds a direct
program and separate library/consumer for both targets: all return 42, preserve mutation
and alias identity, and reject an external direct child before output. See neoCLR
`closed-family-driver-2026-10-04.json` for commands, revisions and hashes. The unchanged
JSON source group next rejects BoundPropagateExpression during emission, without output.
General protected members, class virtual methods, generic/nested closed roots, external
base declarations and JSON execution remain open.


### Local assignment propagation (2026-10-04)

Shared lowering now normalizes direct/eager-binary local assignment propagation
before either target emitter. Native execution of the reduced success/failure consumer
returns 42 against the existing source-built libraries; no Runtime Contract, ownership,
bootstrap mapping or metadata change is needed. The independently validated fix is on
main at `9faabb1a2` (24 focused .NET tests); this branch passes 16 focused tests.
The unchanged JSON group still rejects a remaining nested propagation expression and
publishes no output. This is partial lowering coverage, not JSON completion.
See neoCLR `local-assignment-propagation-2026-10-04.json` for executable evidence.


### Conditional propagation and source JSON library (2026-10-04)

Shared lowering now exposes branch-local propagation checks at statement boundaries
for conditional initializers, including eager binary operands. The main-based fix is
integrated as `e1df355a2` with 29 focused .NET tests; this branch passes 21 focused tests.
The native four-outcome consumer returns 42. No Runtime Contract, primitive mapping,
bootstrap ownership or metadata encoding change is required.

The five unchanged JSON document/syntax/value/error sources now compile into a native
library and pass runtime verification. A separate artifact-only consumer returns 42,
checking numeric parsing, duplicate-field rejection and collection alias mutation.
The public serializer/object mapper remains outside this gate: adding those unchanged
sources to the same catalog fails because introspection types and reflection runtime
services are absent; cascading diagnostics are not independent compiler bugs.
See neoCLR `conditional-propagation-json-2026-10-04.json` for commands and hashes.


### Native typeof emission contract (2026-10-04)

The portable plan now has an explicitly enabled LoadTypeToken operation whose operand
is a Raven type symbol. A configured RuntimeTypeOfContract lowers typeof through its
Current getter, the type token and its nonvirtual GetTypeInfoFromHandle resolver.
Signatures and operands must pass the target profile; unbound generic operands and
virtual/override resolvers reject. sizeof is not admitted by this path.

The NeoCLR adapter maps RuntimeTypeHandle to the metadata API's opaque primitive and
forwards the token to IILGenerator.LoadTypeToken. It authors from symbols and explicit
artifact identities, without reopening importer objects. The metadata dependency is
neoCLR `ad375d0b` on codex/extended-cli-metadata. CLI encoding uses the standard core
value-type reference and ldtoken; native encoding uses the existing runtime operation.
No new runtime format version is involved. .NET's Reflection/Emit implementation and
unconfigured typeof behavior are unchanged.

This enables the emission boundary, not a default introspection provider. Host-owned
bootstrap catalogs must still supply the real handle services, descriptor/context
identities and dependency ownership. No implicit projection/fallback is introduced.
The native JSON object-mapping gate remains open until those services and unchanged
source descriptors/mapper execute together. This target-specific slice is not an
independent main-branch compiler-fix candidate.

The focused contract provider now compiles as a native library, then an ordinary
artifact-only consumer executes typeof on a method type parameter and an external
nominal type through its interface. The real runtime TypeName binding supplies names;
verification passes and execution returns 42. The provider is a test fixture, not a
replacement implementation of System.Introspection. The reproducible driver is neoCLR's
`docs/experiments/extended-cli-metadata/bootstrap/verify_type_handles.py`.
17 focused C# typeof tests cover portable capability admission and existing .NET
configured/default behavior. The metadata suite separately exercises standard CLI
local/constructed type tokens and native PE execution.


### Native reflection handle-service signatures (2026-10-04)

The symbol-only native callable signature path now admits the selected core
System.Object alongside RuntimeTypeHandle. Object remains an output-owned nominal
reference resolved through the explicit core binding; it is not treated as an opaque
primitive or an erased System.Value. No arbitrary special type is admitted.

The native integration gate imports a separate provider whose GetObjectType accepts
Object and whose Create returns Object. Real runtime services recover type identity
and invoke a parameterless constructor; the consumer checks initialized state and
identity against typeof. Generic handle arguments, equality, shape, display names and
metadata tokens also execute. Missing constructors and unsupported constructed-generic
creation report existing runtime statuses; an invalid argument index faults.

This target adapter fix does not affect the ordinary .NET generator and is not an
independent compiler-main backport candidate. Public System.Introspection descriptors
and JSON object mapping remain open; the provider is only a focused boundary fixture.
See neoCLR's native-handle-reflection-2026-10-04 integration record for commands/hashes.


### Source-owned runtime-service declarations (2026-10-05)

The NeoCLR adapter now admits internal, nongeneric, bodyless assembly functions in
`neoCLR.Runtime`, explicitly marked `[MethodImpl(MethodImplOptions.InternalCall)]`.
The attribute must belong to the selected primitive core assembly; its single bound
argument must be exactly 0x1000 and it must have no named arguments or other attributes.
Only `internal`/`extern` modifiers are accepted. This is an explicit native adapter
contract, not a new default interpretation of extern or a change to .NET P/Invoke.
The ordinary parser, attribute binder and semantic method symbols remain authoritative.
No syntax, bound-model or editor grammar change is involved.

The adapter makes bodyless metadata definitions through SetInternalCall, skips IL
lowering for these declarations, and resolves local callers from the normal compiler
symbol table. It does not reopen importer objects. Public wrappers remain ordinary
source methods and can be consumed through native metadata in another compilation.
Public or type-owned runtime-service declarations and generic services are deliberately
outside this slice. Keeping services internal avoids promising cross-assembly runtime
service imports through symbol contracts that do not yet retain implementation flags.

The matching bootstrap supplies only the compiler-facing MethodImplAttribute and
MethodImplOptions declarations. Their marker becomes the implementation flag, not a
runtime custom attribute or an executable bootstrap method. The runtime library owns
`runtime/raven/native/RuntimeHandleServices.rvn`; its declarations use existing Object
and opaque RuntimeTypeHandle identities. No duplicate descriptor types are introduced.
Host configuration still supplies core identity, explicit seed and ownership manifest.
Runtime name/signature binding rejects unknown services, even when unused.

Validation: neoCLR's `bootstrap/verify_internal_calls.py` compiles those unchanged
runtime-library declarations with a provider, then compiles an artifact-only consumer;
verify succeeds and execution returns 42 without stdout. Six invalid declaration cases
reject without an output file. Unknown runtime binding rejects at runtime verification.
The existing four .NET extern/PInvoke semantic/emission tests pass. The runtime fixture
uses an explicit Object/RuntimeTypeHandle seed; it does not prove production bootstrap
ownership. Production descriptor factories, snapshot/vector ABI and JSON object mapping
remain open. Services and seed callers must move together: seed code cannot call an
internal service owned by another assembly.

### Descriptor prerequisites (2026-10-05)

NeoCLR admits nongeneric source sealed interfaces through AllowsClosedInterfaceFamilies.
The metadata adapter authors native closed interface definitions; ordinary CLI interface
flags remain Abstract/Interface. Runtime linking enforces direct-family ownership, and
reader introspection supplies permitted direct children to the symbol loader. .NET
sealed-hierarchy emission stays on its existing path. Generic sealed interfaces remain
unsupported in this native profile.

Static extension methods use ordinary existing semantic calls and output-owned methods.
The exact configured bootstrap RuntimeServices.TypeHandle<T>() declaration is a native
intrinsic: one unconstrained method parameter, public static nongeneric owner, no value
parameters, RuntimeTypeHandle result. Lowering emits the portable type token for the
semantic type argument, including open method parameters. No executable bootstrap stub
or reflection/importer object is used. The temporary CLI marker needs eventual
source-built core replacement.

The paired native prerequisites return 42 (sealed interface dispatch and generic token
equality/static extension calls). Reproduction and hashes live in neoCLR's
`docs/experiments/extended-cli-metadata/introspection-prerequisites-2026-10-05.md`.
The production build still needs reference-class Object overrides and descriptor
materialization before JSON object mapping is complete.

Validation: 46 focused .NET static-extension and sealed-hierarchy tests pass. Native
metadata/runtime prerequisite commit: `44df0c27`. This admission is target-specific;
no general compiler fix needs a main backport.

### Reference Object slots (2026-10-05)

ReferenceObjectOverride is an explicit native admission category. Source overrides must
resolve to the real System.Object Equals/GetHashCode/ToString slot and have the exact
supported signature. Imported overrides use equivalent semantic declaration facts; the
adapter constructs output-owned references and preserves virtual dispatch. Metadata
writers validate the host's System runtime binding, including the Equals slot newly
added to the retained seed. Generic reference owners/arbitrary virtual slots remain
unsupported. Base-qualified ordinary virtual calls are outside this portable profile.

The metadata CLI encoder now uses ELEMENT_TYPE_OBJECT for the configured core Object
signature; C# execution exposed the incorrect CLASS encoding of Equals parameters. This
is a metadata library fix, not a change to Raven's .NET Reflection/Emit backend.
Native reproduction/evidence: neoCLR
`docs/experiments/extended-cli-metadata/reference-object-overrides-2026-10-05.md`.

Validation: the separate native consumer returns 42, including inherited and overridden
slots, equality and display. 43 of 44 focused .NET override tests pass. The remaining
`Emit_StaticInterfaceImplementation_EmitsMethodOverrideMapping` test also fails on
unmodified integration HEAD `1a1759d43`, before this slice, with RAV1503 for returning
nullable `default` as Factory<T>. Main `e1df355a2` accepts that same test. The pre-existing
arrow-body binding difference in SemanticModel remains a separately assessable general
compiler candidate; it must not be reported as fixed or caused by native Object slots.
The next JSON blocker is preserving Flags enum metadata, then descriptor materialization.

### Flags enum declarations and import (2026-10-05)

The native adapter accepts the configured core's argument-free FlagsAttribute on an
Int32 enum. Other attributes still reject explicitly. Metadata keeps the standard CLI
marker in its reference projection and the existing native enum-info flag in execution
metadata. NativeNamedTypeSymbol obtains IsFlagsEnum from introspection and projects
AttributeData using the configured core's actual marker/constructor; emitters do not
inspect importer state. Int32 enum and/or/xor use portable enum/storage conversions and
existing integer instructions. Native representation remains nominal.

The production System.Introspection.BindingFlags source compiles unchanged, then its
artifact-only consumer returns 42. C# NeoClrMetadataProbe --flags-symbols checks semantic
attribute identity. 12 ordinary .NET EnumCodeGenTests pass. Reproduction and cross-repo
hashes: neoCLR `docs/experiments/extended-cli-metadata/flags-enums-2026-10-05.md`.
Next production blocker: params-array declaration facts and import for reflection
extensions. JSON object mapping remains in progress.

### Native parameter arrays (2026-10-05)

NeoCLR admits final by-value vector `params` parameters through an explicit capability.
The metadata facade supplies IsParameterArray; native symbol import sets IsVarParams,
and emission authors the final parameter marker from symbols. Normal call-site lowering
performs expansion, including zero arguments, while existing arrays retain their identity.
The configured primitive core and native seed must expose the canonical ParamArrayAttribute.
CLI Param rows/attributes and existing native parameter target_token attributes carry the
same semantic fact; no new physical calling convention or importer reuse is introduced.

Validation: the runtime repository's verify_parameter_arrays.py compiles an artifact-only
provider/consumer, verifies and executes to 42; 151 C# metadata groups and focused native
roundtrip/projection checks pass. JSON mapping remains in progress. .NET Reflection/Emit
behavior is unchanged.

### Native JSON control-flow prerequisites (2026-10-05)

Portable lowering supports reference null-coalescing with single evaluation and lazy
fallback. A direct return fallback is currently admitted at a local initializer's empty
stack boundary; nested operand contexts reject explicitly. Boxed value declaration
patterns test the target type before unboxing and binding the pattern local.
Conversion/required-result wrappers around terminal return statements are unreachable;
normalization retains the return operand's existing conversion. No importer objects,
new metadata semantics or changes to the .NET backend are involved.

The runtime repository's mapping-guards provider/consumer validates both Result match
branches, null/non-null guards, boxed int/bool and a mismatched string. Native verify/run
passes (42); eight focused .NET pattern tests pass. Full JSON mapping remains open.

### Production JSON emission gate (2026-10-05)

Conditional emission now follows AND/OR/not control flow directly, so pattern locals
are assigned on precisely the successful edge instead of losing that fact at a boolean
value merge. String receivers calling inherited Object slots use the existing explicit
reference projection; this preserves the native UTF-8 representation boundary.
The unchanged mapper/serializer and descriptor sources compile into a native library.
The runtime is still being adapted to materialize those source-owned descriptors.

The extended mapping-guards consumer verifies/runs to 42, including both failed AND
operands, successful dual extraction and String hash calls. Nine focused .NET pattern
checks pass. Metadata writer dependency also supports inherited public interface bodies
(runtime commit 32321722); no changes to ordinary .NET generation or bootstrap ownership.
These portable emission fixes do not alter the current .NET backend and are not claimed
as independent fixes already backported to main.

### Native JSON object mapping execution (2026-10-05)

The runtime repository's `bootstrap/verify_json_mapping.py` now compiles unchanged
production JSON/introspection sources into JsonIntrospection.dll, emits ResultOperators
separately, and compiles two consumers against those artifacts without library sources.
The new public serializer consumer executes nested reference objects, Boolean/string/int
properties, integer/jagged arrays, shared-object mutation, exact serialized output and
invalid-input validation before constructor/setter effects (exit 42). The unchanged
earlier Mapping.rvn/Main.rvn sample also passes its expected stdout (exit 0).

Portable code generation admits explicit empty auto-accessors backed by symbol fields,
static conversion declarations and bound user conversion calls with exact storage
parameter/result types. It does not reconstruct overload resolution or implicit coercion.
Native class emission retains ordinary class finality using the CLI Sealed flag; Raven
closed families retain their distinct semantics. Runtime Contract configuration explicitly
selects JsonIntrospection as Boolean and typeof provider for this incremental build.
The existing numeric/text library owns the other source primitives. No application
reference is projected to CLI metadata and emission never reopens importer definitions.

The native adapter supplies scoped descriptor materialization and an immutable internal
ParameterSnapshot wrapper around an owned ParameterInfo array. Runtime service adapters
use exact InternalCall signatures. Primitive bootstrap/retained service seed, Numbers,
Encoding and TextStreams dependencies remain explicit and hashed in the gate evidence.
Execution uses a 100,000,000 instruction budget; this is correctness evidence, not a
performance claim. This does not establish .NET source-library parity, full reflection
coverage, HttpContent/HTTP integration or full-System compilation.

Validation: eight focused .NET auto-property/conversion/Result tests pass. The previously
recorded integration-only static-interface default-value binding regression remains
separate. These changes extend portable/native emission and are not general .NET fixes
to backport; independently validated earlier binder/lowering fixes remain on main.
See neoCLR `docs/experiments/extended-cli-metadata/source-json-mapping-2026-10-05.md`
for commands, dependency ownership, cross-repository revisions and executable evidence.

### Source primitive member selection (2026-10-05)

A native ownership manifest previously excluded declarations owned by the current output
from PrimitiveAssemblies without retaining a source member selection. Thus primitive
`string` receivers saw only the bootstrap's method surface. String.SliceUtf8 already
existed in source and worked when imported, but failed in cumulative encoding, streams
and JSON builds.

`MetadataImportOptions(string coreAssemblyName, IReadOnlyDictionary<SpecialType, string>?
primitiveAssemblies, IEnumerable<SpecialType>? sourcePrimitiveTypes)` now records the
immutable `SourcePrimitiveTypes` set. Supported types are the same numeric, Boolean,
String and grapheme Char types as imported providers. Overlapping imported/source
providers or unsupported types throw ArgumentException. Existing constructors remain
available and select an empty source set. The option requires the NeoCLR target;
ordinary .NET configuration and lookup are unchanged.

The host derives this set from declarations owned by the current output. Exact named
member lookup selects that source type and completes its member signatures normally;
accessibility and overload resolution remain binder responsibilities. There is no
fallback to bootstrap-only members. Primitive expression/parameter identities remain
the explicit bootstrap's canonical scalar types; this is the documented temporary
source-library bootstrap split, not an added metadata alias or importer/emitter coupling.
A future source-owned core can remove that split. This change does not claim to replace
all intrinsic constructor/indexer handling with source lookup.

Validation: 19 existing provider/core tests passed before the change; those and six new
source-provider tests pass afterward (25 total). Tests cover method/property/static and
literal lookup, source order, canonical scalar identity, opt-in selection, missing
bootstrap-only members, copied/conflicting configuration and .NET rejection. The ordinary
rvnc command compiles the previously failing cumulative 109-file runtime library. Both
JSON consumers execute against its single native artifact without library sources.
See neoCLR's source-primitive-members-2026-10-05 integration record for hashed evidence.
This is an explicit native bootstrap fix, not an independently applicable .NET fix to
backport to main.

### Cumulative native library capacity (2026-10-05)

After source-member selection is fixed, adding Tasks/Concurrency reaches the independent
metadata library's older 256-type authoring limit. Matching writer/native-reader versions
now admit 4,095 declarations within the existing 4,096-row CLI snapshot budget (including
Module). The 115-file cumulative source set emits with the same Runtime Contract and
native instructions. Existing library envelope budgets remain unchanged. Older metadata
library versions reject these larger outputs; see neoCLR's cumulative-library type-budget
record for boundary and executable controls. This requires no general .NET/main backport.

### Portable value-block returns (2026-10-05)

The shared linear adapter now carries statement-boundary context through nested
blocks, branches, conversions and transparent wrappers. Returns from a match arm
at an empty evaluation stack are admitted; returns across pending outer operands
remain unsupported and ordinary .NET can retain its general emitter fallback.
No Runtime Contract option, binding rule or instruction/metadata extension changes.
The unchanged native storage sources and artifact-only storage sample compile and
execute. Module-function references returning external value types additionally
require the matching metadata library's authored function-signature update.

Validation: 55 focused SharedLinearBodyTests pass, including Debug/Release returns,
ordinary .NET match execution and pending-operand rejection. A separate test correction removes the stale numeric-conversion rejection
expectation; the complete focused group now passes 56 tests. Raven main e1df355a2 has no portable adapter, so this change
has no independent main backport; existing general .NET emission already handles
these source constructs. Native storage evidence lives in neoCLR's
`docs/experiments/extended-cli-metadata/source-storage-2026-10-05.md`.

### Source networking callbacks (2026-10-05)

The portable adapter materializes converted value receivers once into existing
managed temporary storage, allowing IPAddress's numeric formatting calls. It also
admits immutable by-value parameters wherever immutable local captures already work.
Native closure fields use symbol-provided types and captured parameter reads use the
existing LoadCapture path, including nested functions. Mutable bindings and ref/out/in
parameters remain rejected by this bounded native capture policy. No binding rules,
Runtime Contract switches or native function representation changed.

The unchanged `network-cancellation/Main.rvn` executes DNS, cancellation, loopback
accept/connect/send/receive and buffer assertions against separately emitted native
libraries. Native DnsAddresses uses the runtime's managed string-snapshot adapter.
59 focused shared-body .NET tests pass, including converted receiver capability
checks and captured parameter reads. Main's ordinary .NET emitter already supports
these behaviors; the portable adapter is absent from main, so no standalone main
backport applies. This does not complete HTTP compilation: its next diagnostic is an
unlowered BoundPropagateExpression. See neoCLR's source-network-2026-10-05 integration
note and executable evidence for exact dependencies and limits.

### HTTP condition propagation (2026-10-05)

Shared lowering now rewrites propagation reached through generated child traversal and
spills propagation in eager/short-circuit if conditions. The native consumer executes
eight skip/success/error combinations against imported Result metadata. No Runtime
Contract or metadata changes are needed. The complete HTTP source group advances to
BoundDelegateCreationExpression admission, which remains open. Validation: 21 propagation
and 65 focused shared-body/runtime-contract tests pass on the integration branch.
See neoCLR's `condition-propagation-2026-10-05.md` evidence and the shared
`propagation-temporaries.md` design note.

### Generic HTTP callbacks (2026-10-05)

Owned generic method groups now pass instantiated semantic arguments into native
Function bindings. Generic owner and method arguments remain separate; the metadata
adapter validates the substituted shape and constraints. Unsupported groups identify
the target and enclosing callable. No importer objects are reused during emission.
No new Runtime Contract switch is needed. The complete HTTP/network source group
emits; native header/base-address consumers execute against artifacts alone. Metadata
C# tests cover generic callback execution and native readback. Imported callback
binding and native async lowering remain separate capabilities; .NET uses its existing
emitter. The portable adapter does not exist on main, so this change needs no main
backport. See neoCLR's generic HTTP callback integration record.

### Propagation in call arguments (2026-10-05)

Shared lowering now spills supported value-call receivers/arguments in source order
when propagation can return a residual. Conversion and pattern wrappers retain their
semantics. Reference receivers and by-value arguments are supported; address-taking
receivers/ref arguments retain their existing lowering. No Runtime Contract change.
The unchanged routing consumer executes against native HTTP/library artifacts. All
23 .NET propagation tests pass, including receiver/argument order and error short exit.
This is independently applicable to main and is validated there before integration.

### Capturing the enclosing reference object (2026-10-05)

Native closures capture self through their existing fields, and explicit/implicit
receiver uses load the same reference. Frames are nested under their lexical owner
using existing metadata ownership. neoCLR private access follows enclosing identities,
so private state stays private. Value-type self and generic owner closures remain
unsupported. A native consumer verifies deferred private state mutation; the existing
HTTP status server serves eight valid responses and rejects two invalid responses.
No Runtime Contract switch or .NET behavior changes. Main already supports these
closures; this portable adapter change needs no main backport. Runtime revision
8569d729 contains the matching lexical-access implementation and tests.

### HTTP patterns and inherited Object calls (2026-10-05)

The native portable adapter now tests reference property patterns, evaluates getters
once in source order and binds successful payloads. Null and incompatible objects fail
without accessing properties; failed earlier members skip later getters. Integer,
Boolean and enum constants use their semantic storage types. Field/value-receiver
property patterns remain outside this bounded path. Inherited Object calls box value
receivers explicitly, including imported status enums; runtime enum dispatch retains
nominal identity and existing enum formatting rules. No importer handles or new
metadata categories are required. Native property/status consumers execute; 59 existing
shared-body tests plus a focused .NET property-pattern control pass. Main uses its
existing .NET emitter, so these portable-adapter additions need no main backport.
The shared call-propagation fix was independently integrated into local main as
 e33591945 (31 propagation/runtime-contract tests); its temporary branch was deleted.

### String[] native entry support (2026-10-05)

neoCLR metadata/runtime revision e076c118 admits ordinary no-parameter or String[]
entry signatures. Raven's existing entry authoring requires no wrapper or intrinsic.
The runtime passes user arguments after argv[0], while Environment retains the full
argv; bounds and ambiguous native entry names reject explicitly. All eleven existing
stream-upload cases execute through the ordinary driver and native references. No new
Runtime Contract switch or .NET startup behavior change. Full-System bootstrap and
native async state-machine compilation remain separate milestones.

### Source RuntimeTypeHandle ownership (2026-10-05)

The explicit native primitive catalog now accepts System.RuntimeTypeHandle. When its
owner is the output assembly, source signature lookup (including array/generic nesting)
uses the canonical bootstrap handle; the source declaration remains an authored type.
The native emitter requires an empty, nongeneric top-level struct without constructors
and designates runtime-owned handle storage. Consumers select the emitted native
primitive provider. No importer objects are used by emission, and ordinary .NET name
binding is unchanged. RuntimeUnit/typeof contracts are not weakened.

Remove the retained seed handle declaration when assigning source ownership; duplicate
ownership still rejects before publication. The metadata library must support the new
RuntimeTypeHandle primitive designation. Existing runtime handle values and CLI handle
signatures are reused; this introduces no guest storage fields, reflection layer or new
format version. Native-only primitive implementations still reject executable CLI output.
The 140-production-source combined library and separate JSON/Tasks consumers execute
with this ownership. Core Object ownership remains a separate blocker. Matching evidence:
neoCLR docs/experiments/extended-cli-metadata/source-handle-ownership-2026-10-05.md.

### Object service declarations across native assemblies (2026-10-05)

The runtime integration now supplies native source adapters for ObjectReferenceEquals,
ObjectEquals and ObjectIdentityHash. ObjectEquals is the nonvirtual base identity service
with a non-null receiver; ReferenceEquals accepts nulls. These adapters reuse existing
InternalCall metadata and runtime operations. They are internal library infrastructure,
not a substitute implementation of source System.Object.

A source library and the retained seed may each declare the same validated service.
The matching runtime loader preserves assembly definition identities, binds symbolic
calls to the caller's local declaration, and preserves explicit external references.
Duplicate declarations inside one module, incompatible signatures and inaccessible
external declarations still reject. No compiler or .NET backend change is required.
See neoCLR's object-services-2026-10-05 integration record for native execution evidence.
Source Object's canonical root identity remains a separate unresolved contract.

### Release requirement: native metadata editor workflow (2026-10-05)

The author requires working editor/language-server support with native NeoCLR references
for release, alongside the compiler/runtime gates. Use the existing symbol importer and
ordinary compiler emission for editor builds; do not create an LSP-specific metadata
writer. Validate project target/reference configuration, imported completion/hover/
navigation and diagnostics, reference-change invalidation, and an editor-triggered native
build/run with source-built library references. Preserve ordinary .NET editor behavior.
A read-only disassembler is a release candidate to assess, not a committed release gate.
These are recorded requirements, not claims of implemented editor support.

### Source Object root selection prerequisite (2026-10-05)

The runtime root investigation (neoCLR `object-root-ownership-2026-10-05.md`) shows
that matching a method to any loaded declaration named System.Object is insufficient:
it grants intrinsic hashing to an application lookalike. That runtime experiment was
reverted; no compiler or .NET backend behavior changes in this slice. The next contract
must select one root explicitly through bootstrap ownership, resolve keyword/named
Object and implicit bases consistently, and author boxing/slots from that identity.
The runtime must receive the same selection before the retained seed root can be
removed. Keep source Object compilation marked incomplete. Existing source-handle and
Object-service gates do not establish root replacement.

### Runtime host root selection available (2026-10-05)

neoCLR now exposes `LoadedProgram::with_modules_and_object_root` and a matching mixed
reader. The host selects an exact library type row plus module/revision, validated
against the unique System.Object definition and its three concrete virtual slots.
The retained seed can reference the supplied source-root library with dependency and
accessibility validation. Default runtime loading still rejects application lookalikes.
Selection is private load context and is not serialized; no metadata encoding changes.

This closes a runtime prerequisite, not Raven's source Object integration. The driver
must eventually derive selection from the validated artifact catalog; Raven binding and
the metadata writer must first agree on source-root identity, implicit bases, signatures,
boxing and slots. No Runtime Contract option or .NET emission behavior changes here.
See neoCLR `explicit-object-root-2026-10-05.md`: 86 focused runtime tests pass, including
external-root binary execution, wrong revisions and incomplete contracts.

### Source Object binding selection (2026-10-05)

The opt-in `MetadataImportOptions(..., useSourceObjectRoot: true)` now unifies the
producer's source System.Object across special-type lookup, signatures, implicit
bases and override binding. It resolves before member signatures and does not give
the root a bootstrap base. Invalid/missing roots diagnose; ordinary .NET and default
NeoCLR behavior are unchanged. See runtime-contracts.md for the complete API contract.

There is no temporary CLI encoding of this source root. Both current emitters explicitly
reject the option before publication until the separate metadata library can author
root definitions, Object signatures and boxing/slot references. The runtime already
has explicit load-context selection (neoCLR `4e9e4045`); that is not yet connected to the
compiler driver. The native probe `--source-object-root /path/to/Core.dll` checks actual
bootstrap binding and the native no-publication boundary, not runtime execution.

### Object declaration metadata and editor gate (2026-10-05)

The NeoCLR metadata API now authors baseless canonical Object declarations through
manual definitions or builders, with ordinary CLI Object signature bytes in the
reference-only projection. Its 156 C# metadata groups pass. This does not yet supply
native Object virtual-slot authoring, boxing/root-reference selection or driver wiring;
Raven's source-root emission guards remain required. No default .NET behavior changes.

The author clarified that the end-to-end scenario includes language-server and VS Code
support. Editing against artifact-only native references, project diagnostics and
build/run must share the ordinary compiler's target configuration and explicit dependency
catalog. Do not add an editor-specific importer or writer. Runtime execution and editor
integration remain separate acceptance evidence still to be completed.

### Native Object slots and sample release gate (2026-10-05)

The metadata API now authors Object's concrete virtual new slots and preserves their
flags through native introspection and CLI reference projection. A C# API-produced PE
executes all three slots and boxed display under explicit runtime root selection.
157 C# metadata groups and 13 runtime identity checks pass; see neoCLR's
`object-root-slots-2026-10-05.md`. Raven source-root emission remains guarded until
root signatures, boxing, overrides and driver catalogs use the selected identity.

The author additionally requires working release samples including Tasks and await.
Callback Tasks consumers and older translated async experiments are not native async
emission acceptance. Assess ordinary native compile/run of the sample inventory after
root wiring, sharing the existing compiler/lowering path and VS Code configuration.
Runtime suspension and green threads are deferred; no new scheduler is required by
this direction. Ordinary .NET async behavior must remain preserved.

### Owned-root boxing prerequisite (2026-10-05)

The metadata API now uses `AssemblyBuilder.ObjectType` for boxing and value-type
Isinst results. This selects an explicitly authored root or the existing bootstrap;
`CoreObjectType` retains its original bootstrap meaning. Native emission validates the
owned root's complete slots and can emit boxing/virtual dispatch without a legacy
System binding. An API-produced BoxedDisplay function executes in NeoCLR with "42";
157 C# contract groups pass, including mixed-identity/incomplete-root rejection.

This prerequisite was found while reviewing source-root emitter integration. Raven
source-root emission remains guarded: local override signatures, declaration capability
checks and host catalog wiring are still required. No compiler runtime-contract option
or ordinary .NET behavior changes in this slice. See neoCLR's
`object-root-boxing-2026-10-05.md` for reproducible artifact/runtime evidence.

### Owned Object overrides and remaining compiler guards (2026-10-05)

The metadata API now authors overrides using the exact local Object signature, retains
Virtual/reused-slot flags through native introspection, and executes protected base
construction plus all three virtual overrides from API-produced PE. Validation passes
158 C# metadata groups and 14 runtime root-identity tests. Legacy System binding remains
required when no local root is authored; default .NET override execution still passes.

Source-root emission remains guarded. The next Raven slice must explicitly admit the
baseless abstract source root in SourceTypePlan, its concrete virtual declarations in
SourceCallablePlan, and source Object identity in type mapping. Target capabilities
must contain these differences; the emitter consumes symbols and output-owned metadata,
not importer objects. Then connect the builders and exercise a source-emitted root.
Driver/consumer root selection and the production System/VS Code/Tasks-await gates remain
open. See neoCLR `owned-object-overrides-2026-10-05.md` for artifact/runtime evidence.

### Raven source Object emission executes (2026-10-05)

The native backend now opts into compiler-owned ObjectRoot/ObjectRootSlot declarations.
The source root stays baseless, its slots use native builders, and local constructor and
override relationships retain that same symbol identity. Bootstrap validation checks the
registered assembly identity directly rather than deriving it from Object. Metadata
support is neoCLR `9d880af0`; no importer objects are reused during emission.

The source-root probe emits a native PE with a source-derived class and its display
method. NeoCLR loads that PE under explicit host root selection and returns
"source root override". 47 focused compiler checks pass, including existing .NET behavior;
unsupported extra virtual slots, generic reference owners and unregistered bootstrap
references fail before publication. .NET's source-root guard remains. Ordinary driver
configuration, native consumer root selection and production System are not complete.

The author requests a metadata disassembler next, before resuming broader end-to-end
work. Native metadata CLI/VS Code support and Tasks/await samples remain release gates;
runtime suspension and green threads remain deferred.

### Source Object root driver bootstrap (2026-10-05)

`rvnc neoclr --library --source-object-root --core-reference <core.dll>` selects the
existing source-root semantic/emission contract. It requires explicit library/core
selection and cannot combine with the legacy System-symbol projection. Primitive
providers configured by an ownership manifest are retained. Root bootstrapping has no
implicit typeof service: an explicit manifest TypeOf contract is used when supplied;
otherwise that service contract is disabled. `--bootstrap-intrinsics` remains a separate
opt-in; selecting a root does not implicitly authorize bootstrap storage operations.

The metadata library authors the root, signatures and slots directly into PE/#Neo;
there is no new bridge representation or metadata format change. The native driver
continues buffering emission and refuses existing destinations. The .NET driver and
backend are unchanged. This is a bounded library-producer configuration, not imported
Object-root selection, complete System compilation, project/LSP configuration or support
for generic local reference bases.

`NeoClrMetadataProbe --source-object-root-driver <core> <rvnc.dll> <neoclr> <source.rvn>
<fresh-evidence-directory>` exercises ordinary compiler and runtime processes, exact
runtime result, metadata inspection, existing-file preservation and six failure-before-
publication controls. The runtime uses `--object-root <library>` with explicit module
and seed inputs. The caller is neoIL; this does not claim a Raven-to-Raven consumer gate.
The evidence records input/artifact hashes and command results. Sixteen existing
source-root compiler checks and 30 runtime/CLI checks pass. No independently useful
.NET behavior fix needs backporting from this target-specific driver slice.

### Explicit native async declaration provider (2026-10-05)

`MetadataImportOptions.WithAsyncAssemblyName(string? assemblyName)` returns an immutable
copy selecting the registered native library owning `System.Tasks.Task<T>` and
`System.Runtime.CompilerServices.AsyncTaskMethodBuilder<T>`. The `AsyncAssemblyName`
property reports that selection; null clears it and empty names throw ArgumentException.
Use this only with the NeoCLR heap-state-machine target. The native dependency catalog
continues validating full artifact identities and conflicts; this selector does not
load files implicitly or accept an unregistered CLI projection as an async provider.

The native importer assigns the Task/builder special classifications only inside that
selected assembly. Special-type resolution uses that owner without falling back to the
primitive bootstrap. Resolved configuration requires public generic reference-class
Task and builder declarations from a native artifact. The default .NET and unselected
native paths are unchanged. This establishes declaration identity, not validation of
every possible builder protocol; normal await binding checks the used awaiter pattern.

`rvnc neoclr --async-library <assembly-name>` exposes the selection alongside explicit
`--core-reference` and `--reference` inputs. It preserves ownership-manifest primitive
configuration. No new CLI bridge encoding, metadata schema or Task library implementation
is introduced. Signatures remain nominal native generic identities. The emitter consumes
symbols and artifact contracts, never importer objects.

The five native async/HTTP POC samples now pass binding, including HTTP client propagation.
They still reject at the explicit native async state-machine emission boundary and
publish no output. Next connect synthesized state-machine owners/fields/methods to the
portable declaration and body path, using existing heap lowering, and prove completed
and pending awaits before claiming working async compilation. Runtime suspension and
green threads remain out of scope. Full System and Object-root replacement do not gate
these retained-seed POC samples.

Validation: 32-test pre-change async baseline; 33 final focused .NET/option tests pass.
The C# `--native-async-symbols <core.dll> <native-library.dll>` probe checks selected and
unselected identity, generic GetResult substitution, async/await binding, malformed
interface/value-type providers, missing/bootstrap providers, .NET denial and unchanged
output streams at the emission boundary. No .NET behavior fix requires backporting.


### Native heap state-machine emission (2026-10-05)

The selected native async provider must also own the public, nongeneric
`System.Runtime.CompilerServices.IAsyncStateMachine` interface. Task, builder and
state-machine identities remain nominal references, not primitive storage mappings.
Top-level nongeneric async functions now use the existing heap AsyncLowerer. Portable
plans carry prepared bound bodies for the synthesized constructor, MoveNext and
SetStateMachine, with ordinary fields, interface relationships and IL generation.
Synthesized owners are internal top-level metadata types; Reflection.Emit retains its
existing representation. Separate AsyncMethod/AsyncStateMachine capabilities keep the
shared portable default conservative. No importer objects enter emission.

The native `native-async-state` driver case executes awaitless completion, already
completed and pending awaits, hoisted local preservation and cancellation. A synchronous
Main explicitly drains TaskQueue.Default after completing/cancelling promises; this is
not support for pending async Main. Class/extension and generic async methods remain
unsupported. No metadata schema change, suspension mechanism or green threads are added.

Pass an optional third input to `--native-async-symbols <core> <native-library> <seed>`
to verify emission and native reader materialization of two synthesized owners. C# checks
also prove class/generic async rejection preserves the destination stream. The seed
supplies the same explicit runtime bindings as the ordinary driver. Next implement entry
completion and class methods, then resume the unchanged async/HTTP samples. These are
native adapter changes; no independently useful .NET behavior fix needs a main backport.

Validation: 44 focused .NET async, option and portable-plan tests pass, alongside the
C# native emission/reader and rejection probe and the executable driver case.


### Async closure capture storage (2026-10-05)

Native immutable closure captures created inside a synthesized async body now read the
hoisted local field selected by AsyncLowerer. Previously portable closure creation tried
to load the original local slot and rejected `library-async` as an undeclared local.
The callback still receives its ordinary capture slot; reference identity is preserved.
Mutable captures remain guarded, and the Reflection.Emit shared-closure implementation
is unchanged. This repairs the new native path, not an independently reproduced .NET
regression requiring backport to main.

The native C# probe emits and reads a third state machine containing a Promise callback.
The executable `native-async-state` test proves the callback completes the same Promise
that the suspended method awaits and returns 42. The unchanged `library-async` now reaches
the separate async entry signature blocker; this is not an async Main completion claim.

Capture follow-up validation: 37 focused .NET async and portable-body tests pass, plus
the native C# emission/reader probe and exact-output runtime regression.


### Native class async methods (2026-10-05)

Nongeneric static and instance class async methods now use the same synthesized body
collection as assembly functions. A class method's machine retains its class metadata
owner, so existing nested-type access rules permit private receiver fields; assembly
function machines remain top-level. Generated types retain internal visibility. This
matches CLI nesting semantics without changing user member visibility or runtime access
checks. No metadata schema change or new bridge representation is introduced.

The executable regression suspends an instance method, resumes it, and checks both its
result and mutation of the original receiver's private field; an awaitless static method
also returns 42. C# round-trip checks assert retained declaring ownership and reject
generic owners/methods before publication. Extensions remain guarded. Both HTTP samples
now reach `value block cannot exit its enclosing expression`, exposing a separate portable
control-flow gap; they are not yet executable through the native compiler path.

Class-method validation: 38 focused .NET async and portable declaration tests pass,
plus native reader ownership/rejection checks and the pending receiver-mutation consumer.


### Native async propagation and HTTP execution (2026-10-05)

Portable field assignment preserves empty-stack statement context after spilling an
ordinary reference receiver. Return/branch exits from value blocks remain rejected when
prior operands are on the stack. Compiler-generated heap-machine self receivers are
instead loaded after the right-hand side: async dispatch may resume inside that expression
and must not depend on an earlier temporary store. Ordinary receiver evaluation order is
unchanged. Distinct synthesized locals use bound-symbol instance identity, matching async
lowering, even when names and source-less declaration identities are equal.

A focused native consumer checks pending Result success/error propagation and proves
error completion skips mutation. The unchanged HTTP JSON server and client now compile
through rvnc with native references and execute together: GET report, POST acknowledgement,
malformed JSON and missing route responses pass, then a native client performs its GET/POST
pair. Both processes exit zero; server completion output and client JSON are checked.
No native metadata encoding, library API, runtime access or scheduler behavior changes.
Async Main, generic async methods and extension async methods remain outside this gate.

Reproduce from neoCLR with `scripts/check-native-poc-samples.py --case
native-async-propagation --case http-json-server --case http-json-client` and the existing
explicit core/seed/ownership/native references, then `scripts/verify-native-http-json.py
--runtime <neoclr> --server <server.dll> --client <client.dll> --seed <System.neox>
--module <Numbers.dll> --module <Http.dll> --output <fresh-evidence.json>`.

Validation: all 36 focused async and portable-body tests pass (including the new
empty-stack field-return plan check). The two native execution controls and both HTTP
process scenarios pass with matching output and exit status.


### Native async entry completion (2026-10-05)

Console entries returning the explicitly selected native Task<unit> or Task<int> now
receive an internal assembly-level Int32 startup adapter. It forwards the source entry's
parameters, calls that entry, invokes the explicitly bound RuntimeServices.DrainEntryTasks,
then calls the native task's GetResult. A Unit value is discarded and returns zero; an
Int32 becomes the process exit status. The source function retains its Task signature.

This reuses the existing runtime entry dispatcher and retains the task on the startup
frame while registered work finishes. It does not poll or introduce suspension/green
threads. Cancelled tasks and tasks still pending without registered work fault through
GetResult rather than silently succeeding. Task<Result<...>> entry adaptation remains
unsupported; no success/error mapping is invented. Native service selection still uses
symbol facts and the host dependency catalog. Missing service bindings publish nothing.
The ordinary .NET entry bridge is unchanged.

Validation: both unchanged async Main samples execute with exact output, including worker
completion; a pending integer entry forwards String[] arguments and exits 23. Cancelled
and unresolved entries exit 1 with the expected faults. The native C# probe checks the
separate Int32 startup metadata and missing-runtime failure-before-publication. All 60
focused .NET async-entry, target-entry, entry diagnostics and async-method tests pass.


### Value auto-property constructor initialization (2026-10-05)

Native portable value constructors initialize an owned auto-property's backing field
when the receiver is implicit self or explicit self. Calling its setter on the construction
receiver was rejected by the metadata verifier. Direct initialization preserves definite
field assignment without weakening receiver escape checks. The auto-property has no user
setter body to skip. Ordinary writes, custom setters, other receivers and reference-type
constructors retain their existing paths.

The unchanged application-types sample executes with stdout `42\n99\n7\n42\n7\n`,
proving shared class identity, independent struct copies and collection copy behavior.
The C# source-value driver also runs on .NET and NeoCLR, checking both forms of self
initialization and later property mutation. Its old boxing rejection control is corrected
to the current missing-System-binding error; this test correction adds no boxing feature.
The .NET portable capability profile remains conservative for value owners, and ordinary
.NET struct properties continue through its existing emitter. No metadata/runtime API or
format changes and no independently needed main behavior backport arise from this fix.

Validation: 17 focused auto-property, constructor, value receiver and struct semantic
tests pass, plus the dual-target driver probe and two exact-output native consumers.


## Native local virtual hierarchies (2026-10-05)

The native adapter opts into `AllowsClassVirtualSlots`; the ordinary .NET portable
capabilities do not change and CLR emission continues through the existing generator.
Public nongeneric instance virtual/abstract methods and exact overrides on local,
nongeneric reference classes are admitted. Abstract class methods remain bodyless;
only explicitly selected runtime services receive InternalCall. Abstract owner flags,
new slots and overrides go through the separate metadata builder/definition API.

Lowering retains an explicit `DirectInstanceCall` for `base.Method(...)`. Ordinary
reference calls dispatch virtually, while base calls target the resolved concrete
body. Receiver/signature identities come entirely from bound symbols. No importer
handles or metadata reader objects cross into emission. Local base declarations and
inherited interface conformance use existing metadata/runtime contracts.

Runtime Contract/bootstrap configuration is unchanged: native driver acceptance uses
the explicit primitive core, retained System seed, ownership manifest and source-built
Numbers/Http artifacts. No CLI fallback for native references is added. External class
overrides, generic virtual owners, re-abstraction and new-slot hiding remain unsupported;
this does not complete every inheritance or native import/emission combination.

Validation: the unchanged `application-inheritance.rvn` compiles through ordinary driver
commands and executes with stdout `7\n42\n`, exit 0, empty stderr on both targets.
The native collections and interfaces controls still pass. C# capability tests exercise
Debug/Release CLR execution and opt-in portable admission, alongside existing inheritance,
abstract-instantiation and override-binding tests. The metadata/runtime work is recorded
in neoCLR `docs/experiments/extended-cli-metadata/native-inheritance-2026-10-05.md`.
These are target integration changes; no independent main-branch binder fix is involved.


## Shared native reference catalog (2026-10-05)

`rvnc neoclr` now obtains its explicit core/native inputs from
`Raven.CodeAnalysis.NeoClr.NeoClrReferenceCatalog.Read(corePath, nativeReferencePaths,
runtimeSeedPath = null)`. This development host API owns immutable snapshots, preserves
input order and shares the same primitive reference between native semantic import and
the optional retained-seed emission binding. Core identity comes from the snapshot;
the driver no longer rereads that file separately for identity and seed binding.

Public members are `Bootstrap`, `CoreIdentity`, immutable `References` and immutable
`Dependencies`. `ValidateSourceOwnership(metadataTypeNames)` rejects a retained-seed
copy of a source-owned declaration, preserving the existing manifest check. File limits
remain 4 MiB for core, 16 MiB per native PE and 8 MiB for seed. Read rejects duplicate
paths/identities, malformed inputs and a seed whose module is not System. Missing files
remain IO errors; unresolved semantic dependencies remain normal compiler diagnostics.
It performs no dependency discovery, execution, projection, file watching or caching.

Hosts must recreate the catalog on a reference change and construct a new compilation;
old catalogs/compilations retain their original snapshots. Source ownership, runtime
contracts, async provider and output options still belong to host configuration; this
class does not infer them. Existing native driver flags, bootstrap configuration and
failure-before-publication behavior are preserved. The legacy explicitly selected
System projection remains separate. Ordinary .NET commands do not use this catalog.

C# `NeoClrMetadataProbe --reference-catalog <core> <seed> <output>` checks native import
and emission, replacement at the same path, old snapshot stability, missing dependencies,
duplicate/conflicting identities, malformed input and seed ownership. Its separately
emitted library/consumer executes with exit 42. Inheritance and order-collections driver
controls still execute with expected output. A malformed driver reference produces no
output file. This is an editor integration prerequisite; MSBuild project evaluation,
LSP invalidation/navigation and VS Code build/run are not connected by this slice.


### Explicit native project metadata (2026-10-05)

`RavenTargetPlatform=NeoCLR` continues to support the legacy CLI bridge. A project
must additionally select `RavenMetadataFormat=NeoCLR` to request native import.
Omitted format or `CLI` retains the existing project loader, including ordinary .NET.

```xml
<PropertyGroup>
  <TargetFramework>net10.0</TargetFramework>
  <RavenTargetPlatform>NeoCLR</RavenTargetPlatform>
  <RavenMetadataFormat>NeoCLR</RavenMetadataFormat>
  <RavenNeoClrCoreReference>bootstrap/Core.dll</RavenNeoClrCoreReference>
  <RavenNeoClrRuntimeSeed>bootstrap/System.neox</RavenNeoClrRuntimeSeed>
</PropertyGroup>
<ItemGroup>
  <Reference Include="Library"><HintPath>lib/Library.dll</HintPath></Reference>
</ItemGroup>
```

`RavenNeoClrCoreReference` is required; `RavenNeoClrRuntimeSeed` is optional.
Both resolve relative to the project directory. Every native `Reference` requires
an explicit HintPath. Missing files, invalid catalogs and conflicting explicit core
names fail project loading; paths are not silently removed. Existing evaluated
Runtime Contract mapping properties remain effective. No ownership manifest or
async-provider inference is added here. Configure those separately when supported;
this first project gate uses a simple separately emitted library.

Host API: `IProjectMetadataProvider.MetadataFormat` identifies the opt-in format;
`Load(projectFilePath, assemblyName, options, properties, referencePaths)` receives
an absolute project path, evaluated assembly name/options, evaluated Raven-prefixed
properties and absolute artifact paths. It returns a `ProjectMetadataConfiguration`
containing `Options` and immutable `References`. Providers throw on invalid input.
The project system publishes no partial project after provider failure. Register
one through the optional `metadataProvider` argument on `MsBuildProjectSystemService`.
The shared project system has no dependency on the native adapter.

`NeoClrProjectMetadataProvider` is the native implementation. It reads the shared
`NeoClrReferenceCatalog`, requires the NeoCLR target, validates explicit core names,
and supplies native symbols plus the primitive CLI bootstrap. No host framework,
compiler-support references or generated host TargetFrameworkAttribute are injected.
Native ProjectReference now resolves prebuilt target artifacts (see the project-graph section below).
PackageReference and FrameworkReference still reject explicitly.
The provider returns semantic configuration only; it does not attach an emitter,
execute dependencies or load importer objects during emission.

Language-server builds supplied `NeoClrMetadataProject` register this provider;
ordinary builds remain independent of the adapter and reject native-format projects.
C# `NeoClrMetadataProbe --native-project CORE SEED DIRECTORY` verifies evaluated
relative paths, native binding, absent adapter, wrong target/core, unsupported project
references and missing-artifact transactional failure. NeoCLR's
`scripts/check-native-editor.py SERVER_DLL DIRECTORY/project OUTPUT_JSON` verifies
real stdio completion, hover and missing-member diagnostics against that fixture.
This is initial semantic editor integration, not VS Code release qualification:
reference invalidation, metadata navigation, source-built System/async configuration
and unified project build/run remain work ahead.


### Native VS Code POC acceptance (2026-10-05)

The next slice completes the bounded project/editor gate above. Both the optional
language server and `rvnc neoclr --project App.rvnproj` use evaluated source files,
references, target options and the same `NeoClrProjectMetadataProvider`. Native
builds atomically replace `bin/neoclr/<AssemblyName>.dll` only after validation and
encoding succeed. A failed build retains the previous artifact; it does not run it.
`--run /absolute/path/to/neoclr` explicitly launches the native runtime, passing the
retained seed and native dependency paths; its exit code is propagated. Library
projects cannot be run. Existing `rvnc neoclr` source-file commands remain supported.
The project command currently uses native assembly version 1.0.0.0, no PDB/publish,
no managed execution, and no automatic dependency search or package/project builds.

Additional evaluated host properties:

| Property | Contract |
| --- | --- |
| `RavenNeoClrBootstrapOwnership` | Optional project-relative manifest path; the existing ownership format, validation and Runtime Contract mappings are shared with the driver. |
| `RavenNeoClrAsyncLibrary` | Optional explicit native Task/builder assembly identity; no reflection fallback. |
| `RavenNeoClrBootstrapIntrinsics` | Boolean, default false; permits the selected CLI primitive bootstrap's checked intrinsic storage in the native emitter. |

`NeoClrProjectMetadataProvider.GetConfiguration(projectFilePath)` returns the last
successfully loaded host artifact configuration, or throws InvalidOperationException
if none exists. The returned `NeoClrProjectConfiguration` exposes its immutable
`Catalog`, read-only `ReferencePaths` and optional `RuntimeSeedPath`.
`Validate(compilation)` enforces ownership; `CreateEmissionBackend(assemblyName)`
creates an adapter from explicit artifact identities and configuration. It does not
read semantic importer objects or resolve emission operands through reflection.
The compiler owns symbols; introspection and emission remain separate boundaries.

`IProjectMetadataProvider.GetInputPaths(projectFilePath, properties)` returns extra
explicit metadata/configuration dependencies (default empty). The project system's
`IProjectSystemService.GetMetadataInputPaths(projectFilePath)` defaults to empty;
MSBuild's implementation combines evaluated HintPaths with provider input paths.
The language server recognizes changes to these exact paths, even in output folders,
and rebuilds the project snapshot while preserving unsaved documents. VS Code watches
native artifact/configuration extensions. It does not discover undeclared dependencies.
Workspace-external dependencies still require client watcher coverage or project reload.

Failed watched-file reloads publish `RAVP001` on the project, including the underlying
failure and notice that the last good editor snapshot remains in use. Restoring the
input clears that diagnostic after a successful reload. The command independently
revalidates inputs, so stale editor state cannot make an invalid build succeed.
Initial project-open errors still use existing host logging; this slice specifically
qualifies dependency deletion/replacement/recovery in an open workspace.

Definition navigation for imported native nominal types/members opens a read-only
`raven-metadata:` document, fetched with `raven/metadataDeclaration`. It renders
compiler-symbol declarations, not decompiled method bodies or original source.
Snapshots are content-addressed and bounded to 256 documents per server; navigate
again if an old snapshot expires. .NET definition navigation retains its prior path.
Expanded union notation such as `Option<T>(Some<T> | None)` remains valid; this work
does not redefine display formats. Type-position hover checks cover constructed
native Option and Result with union kind and substituted arguments.

Validation: native C# project/catalog checks; 8 declaration-navigation tests; focused
project option and watcher checks; and real VS Code 1.140.0 extension-host acceptance
on macOS arm64. The latter tests native hover/completion/navigation, replacement,
unsaved text preservation, missing dependency/recovery, unchanged collections output,
failed publication, .NET hover/diagnostics and unchanged Tasks/await execution.
NeoCLR's `scripts/prepare-native-editor.py` creates the project/tasks and separate
reference-only library fixtures; `scripts/native-vscode-acceptance.cjs` drives the
actual extension host. See the matching NeoCLR integration evidence for revisions,
artifact hashes and commands. Packaged release installation/publication and broader
platform qualification remain separate gates.

## Native IDE documentation (2026-10-05)

File-backed native references now use Raven's existing external documentation contract:
an adjacent `Library.docs/manifest.json` and member Markdown files take precedence over
`Library.xml`, with XML fallback per member. Type, method/constructor, field and property
symbols expose documentation through `GetDocumentationComment()`. Image-only references
have no sidecar search path. Missing or malformed optional documentation does not prevent
semantic import, matching the ordinary .NET documentation behavior.

`rvnc neoclr --project Library.rvnproj` honors project XML/Markdown documentation options.
Library defaults produce both beside `bin/neoclr/Library.dll`; custom XML paths and dedicated
`.docs` directories are supported, with checks against overwriting project inputs.
Generation follows successful native validation/encoding. Failed binding/encoding publishes
neither a replacement assembly nor documentation. Individual file replacements are atomic;
publication of the entire set is not an atomic filesystem transaction.

The editor displays prose in hovers and completion documentation. Explicit reference XML
sidecars and Markdown directory descendants participate in workspace reload, including
deletion (which exposes XML fallback). The VS Code workspace watcher covers these paths
inside its watched roots; externally located files retain the existing watcher limitation.
The catalog uses the existing documentation cache; Markdown is read lazily, so documentation
is not claimed to be a byte-frozen part of the semantic artifact snapshot.

This is API help, not a requirement to copy website guides into source comments. Long-form
guides remain independently authored; RavenDoc may reuse the same concise API descriptions.
Documentation must accompany the matching library in a distributable bundle. Undocumented
APIs still show signatures. Native metadata encoding and runtime execution are unchanged.

## Synchronous use cleanup (2026-10-06)

**Native intent:** `use` releases successfully acquired `System.Disposable` resources
when their lexical lifetimes end, including Result/Option propagation. The shared
compiler owns lifetime tracking, evaluation order and cleanup insertion. The neoCLR
profile owns the protocol selection; native and CLI emitters only encode
calls/branches.

**Temporary CLI encoding:** `RuntimeDisposalContract("NeoCLR.CoreProbe",
"System.Disposable", UseExceptionHandling: false)` binds the supplied interface and
emits explicit reverse-order Dispose calls. No try/finally handlers are generated.
Native emission consumes the same lowered body through its existing call, local and
branch capabilities; no new native instruction or metadata facility is required. The
assembly identity remains a bridge bootstrap detail and can be replaced by an explicit
native protocol owner without changing lifetime semantics.

**Restrictions:** synchronous functions only; async and iterator use report RAVT006.
No unwinding occurs on terminal faults or abrupt process termination. Dispose has no
recoverable error result; Closable is not implicitly selected. Outward/backward goto
exits clean up; jumps that skip a use initializer are rejected with RAVT007. Ordinary
.NET goto restrictions are unchanged. These are bounded implementation guarantees, not
permanent neoCLR language restrictions. Suspension-aware cleanup must account for
cancellation and resumed exits before lifting the async restriction. This does not add
automatic iterator disposal.

**Validation:** `ScopeExitCleanupTests` exercises observable disposal order, return
and block-result preservation, loop exits, nested functions, failed initialization,
Result and Option propagation, and unsupported-context diagnostics. It also checks
portable body admission and absence of emitted exception regions. Existing .NET use
coverage remains the default-policy control. Native consumer evidence is recorded
below.

The native `--scope-exit-cleanup-runtime` probe verifies and executes six consumers
(return, goto, loop exits, value block, None and error propagation), all exiting 42.
It uses `CompilationOptions.NeoCLR`, an authored disposal interface, and explicit
authored Propagatable fixtures to isolate cleanup from runtime-library packaging. The
standard `System.Disposable` profile mapping is covered by profile tests. Implementing
the bare bootstrap facade directly still hits the native metadata adapter's
pre-existing "external relationship requires an authored interface contract"
restriction; select an authored library protocol using the compiler API or manifest
`Disposal` entry.

See `tools/NeoClrMetadataProbe/scope-exit-cleanup-validation.json` for runtime/core
hashes and verifier results. The runtime checkout was `codex/native-system-bootstrap`
at `c23a2585`; no runtime implementation changes were needed. The 84 focused modern
.NET tests cover shared cleanup, ownership configuration, goto diagnostics and default
async resource behavior; .NET Framework and NanoFramework execution were not tested.


### Native callable nullable annotations (2026-10-06)

The native adapter emits explicit parameter/return transform facts from Raven symbols
before physical signature mapping. Native introspection exposes those facts; the loader
reconstructs nullable references, vector elements, generic arguments and method parameter
scopes without accessing emitter objects. Constructed method substitutions retain them.
The symbol-only call mapper uses the existing `AnnotatedUnderlyingType` ABI to erase
wrappers when creating physical references. There is no GC-specific compiler rule.

neoCLR metadata revision `119d2daf` is required. Its optional callable origin annotation
payload corresponds to explicit .NET NullableAttribute flags; standard CLI writing still
uses those attributes. Older native readers reject annotated artifacts. This is native
semantic import, not a CLI projection fallback. Primitive bootstrap/core ownership and
runtime-service bindings remain explicit and unchanged. No runtime null check, pointer
layout, instruction or GC policy is added. Nullable context defaults, fields and wider
signature categories are not covered by this callable slice.

Validation: `NeoClrMetadataProbe --nullable-symbols <core.dll>` emits a native library,
imports it with the explicit primitive bootstrap, checks reference/array/generic nullable
facts (including constructed method scope), admits null to an annotated parameter and
rejects it for an unannotated parameter. The ordinary driver GC gate compiles unchanged
GC library sources, then two consumers against the artifact with sources absent; both
verify and print `Native source heap passed`, including KeepAlive(null). The 17 focused
.NET nullable emission/storage tests pass. Evidence and commands are in neoCLR's
`docs/experiments/extended-cli-metadata/verify_source_heap.py` and matching gate record.


### Native source Environment boundary (2026-10-06)

neoCLR `881c4d01` compiles unchanged Environment sources using three internal service
adapters and executes an artifact-only consumer with Raven `d19c6e4a3`. No compiler
code or target setting changed. EnvironmentArguments now accepts Raven's managed
string-array result and returns a fresh managed snapshot with array/heap limits.
The older runtime buffer transport remains an internal compatibility detail, not an
inline-value-array API. The author reaffirmed managed arrays backed by Array<T> as
the supported model; possible inline interop arrays remain future work.

Validation covers all three APIs, argument mutation independence, Unicode, exact cwd,
present/empty/absent variables and invalid names. Four runtime tests and the source gate
pass. Explicit core/Numbers/seed ownership remains required; the consumer uses an
explicit Environment namespace alias alongside the bootstrap's legacy type. See neoCLR
`docs/experiments/extended-cli-metadata/source-environment-2026-10-06.md` for commands,
hashes and limits. Full-System compilation still has 48 diagnostics; this is not release
or full bootstrap qualification.


### Source Console acceptance profile (2026-10-06)

Raven a6ee91610 compiles unchanged Console sources and a separate consumer using native
library references. neoCLR's target Probe supplies `--reference-source-console-core`
(the comparer-storage primitive profile without System.Console). The acceptance script
removes the exact legacy Console type and WriteLine service from its seed and records
Console.dll as the native owner. This is explicit dependency selection; no compiler
lookup precedence change or native-to-CLI fallback is introduced. Existing default
.NET behavior is unaffected. Source no-result WriteLine requires the matching runtime
host-call fix; older inhabited-unit seeds remain supported by that runtime.

The consumer executes UTF-8 input/output, EOF, integral/boolean overloads, stderr and
independent closed wrappers with exact output and exit 42. Reproduction, artifacts and
hashes live in neoCLR's `docs/experiments/extended-cli-metadata/source-console-2026-10-06.md`.
The full-System binding audit drops to 14 errors across 190 inputs; encoding and linking
of the complete source library remain unproven. RuntimeFailure/NativeAllocation,
let-else termination and HTTP Task return binding are the remaining diagnostic frontier.


### Source failure execution and remaining flow contract (2026-10-06)

With Raven a6ee91610 unchanged, neoCLR now compiles System/Functions.rvn plus internal
RuntimeFailure adapters, then imports Failure.dll into a separate consumer. The explicit
`--reference-source-failure-core` Probe profile omits the old projected Fail declaration.
The native no-result `neoCLR.Runtime.Fail` service raises UserFault; the legacy inhabited
Fault service remains for prebuilt Numbers/seed bodies. Wrong new-service signatures
reject. The consumer verifies, exits 1 with the expected fault message, and never reaches
its following return 42. No .NET behavior or Runtime Contract configuration changes.

This closes execution/import ownership only. BoundNodeFacts still delegates terminal
recognition to NeoClrCliCompatibility's core-assembly check. An explicit source/native
terminal-function owner contract is needed for let-else and abrupt-expression semantics;
merely recognizing any method named Fail would be incorrect. Full System still emits no
artifact (12 binding errors across 192 inputs). See neoCLR's
`docs/experiments/extended-cli-metadata/source-failure-2026-10-06.md` for reproduction,
validation and remaining scope.


### Native terminal owner contract (2026-10-07)

The preceding source-Fail flow limitation is now resolved by RuntimeFailureContract.
The host manifest explicitly selects its source/native owner, namespace and function;
source and native-imported symbols carry the same terminal fact. .NET defaults and the
legacy CLI profile retain their behavior. No reader/emitter coupling or native reference
projection is added. Missing/wrong owners and incompatible declarations reject before
publication. The native runtime no-result service from neoCLR b933c32b is required for
this source implementation. See [runtime contracts](runtime-contracts.md#native-terminal-function-ownership-2026-10-07)
for API/configuration and the matching neoCLR flow evidence for executable validation.

This is target-contract work, not an independently useful .NET behavior correction;
no general fix needs cherry-picking to main from this slice.


### NativeAllocation service boundary (2026-10-07)

neoCLR now offers exact native InternalCalls for byte allocation, unsigned checked-size
multiplication and no-result release: `neoCLR.Runtime.NativeAllocate(UIntPtr)->Void*`,
`NativeMultiplyChecked(UIntPtr,UIntPtr)->UIntPtr`, and `NativeFree(Void*)->noresult`.
They use the existing execution-owned pointer heap and limits. This is the intended
runtime target for the source NativeAllocation helper, replacing the older bridge-only
instruction mapping without changing that mapping's semantics.

The remaining compiler/metadata work is explicit unmanaged pointer signature support:
CLI PTR encoding, native Ptr materialization, introspection facts, compiler-owned
emission operands and the target adapter. No helper stub or CLR projection substitutes
for it. The runtime has five native-container tests plus 17 pointer regressions; Raven
source NativeMemory acceptance and the four remaining binding errors are unchanged.
See neoCLR `docs/heap-and-pointers.md#source-nativeallocation-services-2026-10-07`.

### Pointer metadata contract available (2026-10-07)

neoCLR revision `8e93e39b` adds `SignatureType.PointerTo`/`PointerElement` and canonical
`PointerTypeInfo.ElementType` views. Scalar/Void and nested pointer callable signatures
round-trip through CLI PTR and native Ptr, import into output builders, and preserve
exact target identity through locals/calls/returns. Pointer generic arguments, vectors,
Function shapes and nominal/managed targets remain explicitly unsupported by this
bounded metadata API. The .NET Raven backend is unchanged.

All 162 metadata groups pass; an API-authored native allocation/free assembly verifies
and executes with exit 42 against runtime revision `709b2322`. Next map compiler-owned
pointer symbols/operands through the native loader/emitter and connect the source
NativeAllocation helper. No compiler source acceptance claim or implicit bridge fallback
is made. Full System still has four binding errors. Reproduction and hashes are in
neoCLR `docs/experiments/extended-cli-metadata/pointer-signatures-2026-10-07.md`.

### Source NativeMemory execution (2026-10-07)

The NeoCLR target now opts into bounded scalar/Void pointer signatures through the
compiler-owned `EmissionType.Pointer` and `AllowsUnmanagedPointers` capability.
The native importer projects metadata PointerTypeInfo into ordinary pointer symbols;
PTR VOID resolves to System.Void, not the callable no-result Unit interpretation.
Emission uses only those symbols and explicit host artifact identities. Namespace
function references, arguments, locals and returns retain exact target identity.
Unsafe source functions/methods and internal runtime service declarations are admitted;
unsupported pointer categories still reject before publication. The existing .NET
backend and its pointer behavior remain the default and are unchanged.

Unchanged NativeMemory source plus real native allocation adapters now compile into a
library. A separate source consumer imports only that artifact and executes both Alloc
overloads and Free, including an explicitly typed pointer identity function. Native
UIntPtr test inputs are emitted by the metadata API; source native-width literal/cast
semantics are not added by this slice. Double-free and checked-size overflow fault;
unsupported string-pointer signatures publish no output. See neoCLR
`docs/experiments/extended-cli-metadata/verify_source_native_memory.py`.

The full-owned-handle audit now passes binding for all 194 inputs and reaches the
explicit emission restriction on generic classes inheriting the source Object root:
Array<T> is first. Full System still emits no artifact. This is the next broad blocker,
not an Array API implementation problem. No new Runtime Contract option or implicit
CLI fallback is introduced. The metadata API requires the external pointer function
reference extension in the matching neoCLR slice.

### Generic classes over the source root (2026-10-07)

`AllowsGenericObjectRootBase` is an internal, opt-in emission capability. NeoCLR enables
it and authors the semantic source Object base for generic classes through the metadata
builder's bounded overload. Other shared-plan clients retain their earlier admission.
No new Runtime Contract configuration is required. Constructors still lower their bound
base initialization and generic argument storage; no loader handles enter emission.

The metadata library now validates manual/builder parity, open receiver construction,
canonical base identity and inherited root calls for constructed instances. A small
Raven fixture executes Box<int> storage and inherited GetHashCode through a separate
API-authored consumer. Ordinary Raven imported-root consumers remain unsupported and
are not counted as complete bootstrap acceptance. Generic bases other than the explicit
local native Object root remain outside the supported subset.

The 194-input full-System audit now proceeds past Array<T> and rejects enum attribute
ownership. See neoCLR `docs/experiments/extended-cli-metadata/generic-object-root-2026-10-07.md`.
During test reduction, a constructor with `self.value = value` and a same-named parameter
reported RAV0200; the minimal generic-root fixture uses distinct names. This is an
unresolved general binding candidate, not a claimed fix or a production-source rewrite.

### Attribute ownership with a source Object root (2026-10-07)

Native enum FlagsAttribute and runtime-service MethodImplAttribute admission now use
`NeoClrBindingContract.MatchesCore` with the host's explicit `NeoClrEmitOptions.CoreLibrary`
identity. They no longer infer primitive-bootstrap ownership from System.Object, which
can be source-owned. Namespace/name, constructor arguments and supported declaration
shape remain independently checked; same-named source attributes do not qualify.

No new Runtime Contract configuration, semantic annotation meaning, emission category,
metadata encoding or .NET backend change is introduced. The source-root driver regression
compiles production BindingFlags and invokes an attributed native WriteLine service;
it exits 42 with exact output. Both FlagsAttribute and MethodImplAttribute lookalikes
reject with no output artifact. The consumer uses the existing metadata API fixture;
ordinary imported-root Raven consumers remain unsupported.

The full-System audit passes these checks and next rejects UnionAttribute's external
System.Attribute base. Attribute-class ownership/inheritance is the next boundary;
complete System still emits no artifact. See neoCLR
`docs/experiments/extended-cli-metadata/core-attributes-source-root-2026-10-07.md`.

### Source attribute hierarchy and union marker ownership (2026-10-07)

The source-built System library now owns an abstract System.Attribute with a protected
constructor and the existing UnionAttribute subclass. Native union emission reuses
the output's public parameterless UnionAttribute constructor after callable definition.
It never creates a duplicate marker or reopens importer objects. Invalid source marker
constructors fail before publication. Outputs without a source marker retain the
existing embedded nominal-record encoding; that fallback still has no Attribute base.
No Runtime Contract option or format version changes. Compiler-facing FlagsAttribute
and MethodImpl remain owned by the exact configured bootstrap.

The neoCLR source-attribute driver fixture preserves Attribute -> Object and
UnionAttribute -> Attribute in introspection and resolves a union's custom attribute
to the same canonical source marker. A separate API-authored consumer executes the
source constructor chain and inherited GetHashCode with exit 42. Embedded-marker
control and invalid-constructor rejection are covered by the same harness. This does
not claim Raven imported-root consumer support. The explicit minimal seed supplies
String.Concat for generated union display through the real runtime service.
101 focused .NET root/constructor/union regressions pass. The full 195-input System
audit passes binding and next rejects NativeMemory.Alloc's pointer-to-source-Void
signature; no full-System output is published.

### Explicit source/native unit ownership (2026-10-07)

The NeoCLR RuntimeUnitContract can select System.Void in the current source assembly
or an explicit native dependency, independently of the primitive bootstrap. It still
requires an empty public nongeneric value type. Ordinary .NET unit policy and emission
are unchanged. Source declaration completion precedes contract validation; unit storage
resolves through the chosen symbol, while callable unit results remain no-result.
Pointers to that exact selected contract use CLI PTR VOID. Same-named unselected
source types do not become untyped pointers. Emission authors local/imported unit
operands from the selected symbols; it does not reopen importer objects.

Ownership manifests may omit iteration for libraries that do not use iteration.
An explicitly selected source/native unit replaces the compiler bootstrap's unit
scaffold in ownership checks; other duplicate declarations still reject. Runtime
seeds and native libraries must agree on the selected owner. Old libraries that refer
to seed-owned Void cannot simply be reused after deleting that seed declaration.

The NativeMemory source-unit gate builds production Void, NativeMemory and runtime
adapters, then separately compiles a consumer with no library sources. It executes
both allocation forms, Free and an inhabited unit parameter (exit 42), plus expected
double-free and overflow faults and failure-before-publication for unsupported
pointers. Its minimal seed and API-authored native-width inputs are explicit. The
ordinary bootstrap NativeMemory control is retained. 76 focused tests cover unit,
pointer, configuration and synchronous cleanup behavior. Two pre-existing failures
were stale diagnostic wording assertions; their correction is independently validated
on main. The full 195-input System audit clears binding and pointer admission, but
encoding still reports a missing/ambiguous bootstrap System.Void reference. No full
System artifact or complete ownership migration is claimed.

### Unit selection before source declarations (2026-10-07)

Source assembly metadata-name lookup can temporarily return a referenced declaration
before its own type shells exist. ResolveRuntimeUnitType now checks the returned
symbol's assembly against the explicit contract before allowing UnitTypeSymbol to
cache it. An unavailable source owner stays unresolved until source declarations
exist; it never becomes the bootstrap Void. No name-based emission remapping, new
Runtime Contract setting or metadata format change is involved.

Two C# regressions initialize unit before source declaration completion, then inspect
a Closable<E>-shaped interface returning Result<Void,E>, in both file orders. Both
failed before the fix and pass afterwards. 52 focused unit/profile/configuration tests
pass. The native source-unit NativeMemory gate additionally executes a separately
compiled interface consumer whose parameter is UnitBox<System.Void>; unit value
passing, allocation/free, fault checks and failed publication remain covered.
The full 195-input audit clears bootstrap Void import and next rejects a constructor
call that does not satisfy the direct-base contract. Full System remains unpublished.
This fix belongs to the source-unit integration introduced in 7abe0adf7; main does
not contain that source-owner path, so no independent main backport is needed.

## Source Object beneath closed families (2026-10-07)

The NeoCLR type-definition adapter now forwards `SourceTypePlan.ClassBase` for
closed classes, just as for ordinary classes. Previously it created a root closed
class and discarded the source Object base, so correctly lowered protected
constructor calls failed metadata validation (the full-System `JsonValue` case).
The metadata builder overload uses the existing definition validation and native
base encoding. No binder, importer, Runtime Contract policy or .NET emitter changes.

Validation uses the neoCLR `verify_generic_object_root.py --closed-root` fixture:
source Object, a closed abstract family, a concrete child with stored state and
virtual dispatch returning 42. An artifact-only metadata-API consumer executes the
Raven-emitted library; this is explicitly not ordinary imported-root acceptance.
Full-System compilation now passes this constructor frontier and stops at the
`ObjectTypeHandle(object) -> RuntimeTypeHandle` dependency contract. See neoCLR's
`closed-object-root-2026-10-07.md` for revision/hash evidence and next steps.

## Source Object handle adapter (2026-10-07)

NeoCLR's source Object.GetType uses the internal NativeObject.GetTypeHandle facade
and existing native ObjectTypeHandle service. An extension named like an existing
bootstrap static method does not supersede it; a distinct facade keeps the selected
source Object and RuntimeTypeHandle identities intact. Compiler `793220f33` is
unchanged, including dependency signature matching and the .NET target. No new
Runtime Contract option is needed; source Object and explicit native handle ownership
remain required. The focused native consumer executes identity/hash checks, while
the 196-input full build now stops at ReflectionConstruct. Evidence and limitations:
neoCLR `docs/experiments/extended-cli-metadata/object-handles-2026-10-07.md`.

## Source-owned parameterless reflection construction (2026-10-07)

NeoCLR's native library routes parameterless TypeInfo.CreateInstance through its
internal NativeReflection facade and source InternalCall declarations. Existing
source Object/RuntimeTypeHandle configuration applies; compiler emission contracts
and .NET behavior are unchanged. Native execution preserves constructor initialization,
new object identity and accessibility. The CLI bridge retains the equivalent two-method
facade with exact owner/signature validation; its reference scaffolding is not a .NET
runtime implementation or fallback for native metadata import.

The full-source audit omits the seed-only ObjectIntrospection extension, since source
Object owns GetType. It reaches System.Value source/imported ownership validation
inside Environment.GetCurrentDirectory. Evidence: neoCLR
`docs/experiments/extended-cli-metadata/reflection-construction-2026-10-07.md`.

## Source erased-value ownership (2026-10-07)

The bootstrap manifest may select `nativePrimitives: { "System.Value": "Owner" }`
with the same declaration listed under that source library's `types`. The emitter
requires a nongeneric, top-level, empty value declaration without constructors and
marks its metadata as native runtime Value storage. This is not a CLI SpecialType;
the manifest does not insert SpecialType.None into scalar resolution. Ordinary .NET
options reject native primitive ownership as before.

The metadata library retains the nominal System.Value CLI signature and emits the
existing native Value representation. Only the selected source carrier and the exact
core/System seed erased alias share evaluation storage. Generic IsValue/UnpackValue
helpers remain explicit retained dependencies; their core facade does not require a
second seed Value declaration when a source owner is selected. This is a bounded
bootstrap path, not arbitrary same-name type equivalence or a metadata importer
fallback. Separate source-Value import as a compiler consumer remains unqualified.

Validation: 17 focused ownership/unit/profile tests, metadata definition/builder
round trips, native environment payload type test/unpack execution (42), and fielded
carrier rejection. The full-System audit now reaches array backing-storage validation.
See neoCLR `docs/experiments/extended-cli-metadata/source-value-2026-10-07.md` for
source/dependency hashes, remaining limits and reproduction.

## Array backing and source Object (2026-10-07)

The NeoCLR metadata reader/runtime now accept the array backing class over the
explicitly host-selected fieldless Object root, matching existing Raven emission.
No compiler change or Runtime Contract option is introduced. Managed arrays retain
nominal Array<T> backing and aliasing; CLI arrays and ordinary .NET codegen are
unchanged. Nonroot or unselected bases remain rejected. The linked PE runtime test
allocates an array, mutates through an alias and returns 42 through the original.
The 197-input audit now stops at System.String -> System.Object base classification.
See neoCLR `docs/experiments/extended-cli-metadata/array-root-2026-10-07.md`.

## Intrinsic String source-root execution (2026-10-07)

The NeoCLR reader/runtime now preserve intrinsic String over the explicitly selected
fieldless source Object. Runtime constructor adaptation keeps UTF-8 text storage,
validates the original receiver identity and single chaining, and executes the actual
base body through an Object handle. No compiler change, Runtime Contract setting or
CLI bridge mapping is introduced; ordinary .NET emission remains unchanged.

Metadata-API authored native PE library/consumer execution returns 42; missing root
selection rejects and a faulting base-body fixture confirms the body executes.
The 197-source System audit advances to the binary library payload limit and still
publishes no output. This is not full source String/System execution. See neoCLR
`docs/experiments/extended-cli-metadata/string-root-2026-10-07.md` for commands,
compiler/runtime hashes, ownership and remaining limits.

## Larger native libraries and first aggregate emission (2026-10-07)

Metadata library writers now select required schema 4 above schema 3's 8 MiB
envelope, allowing 16 MiB. JSON/node/depth and total PE limits remain unchanged;
older readers reject schema 4 and small libraries retain schema 3. No compiler
code, Runtime Contract configuration or CLI mapping changes; ordinary .NET remains
unchanged. All 197 diagnostic System inputs emit successfully with this metadata DLL.

The aggregate owner is still Numbers, a diagnostic layout, not a shipped monolithic
System.Runtime. The author suggested separate System.Data, System.Networking and
System.Web assemblies; dependency/service ownership must be assessed before adopting
boundaries. Large-library linked execution passes, but loading the complete source
artifact rejects the nominal Array backing contract. See neoCLR
`docs/experiments/extended-cli-metadata/expanded-library-2026-10-07.md` for evidence,
artifact/compiler/runtime hashes, bounds and remaining work.

## Source-root seed ownership and optional libraries (2026-10-07)

The full-source audit now removes bootstrap Object and its methods from the retained
native seed after resolving its temporary source signatures. Explicit JSON preparation
and NEOX encoding artifacts are recorded; runtime/consumer inputs stay native metadata.
Source Object is the sole selected root. All 197 inputs still emit, and linking now
reports the next conflict: retained neoCLR.Runtime.WriteLine returns a Void value,
while the source declaration uses no-result. Reconcile that ABI before claiming load
or execution. Compiler code and ordinary .NET behavior are unchanged in this slice.

An emitted local-reference inventory supports investigating Runtime → no optional
libraries, Data/Networking → Runtime, and Web → Runtime/Data/Networking. Nested union
cases remain with their declaring owners. Native adapters and imported seed dependencies
need explicit ownership review; the Runtime remainder is not a final minimal-core list.
See neoCLR `docs/experiments/extended-cli-metadata/library-boundaries-2026-10-07.md`
for reproducible inventory, evidence and separate-compilation gates.

## Scoped native service result contracts (2026-10-07)

NeoCLR runtime validation now allows separate-module InternalCall declarations with
unit-value/no-result Void results only when both complete contracts pass its existing
native registry. Calls retain their own member identities and stack behavior;
same-module duplicates, ordinary conflicts and unsupported result signatures reject.
Separate native NEOX consumers produce exact WriteLine output and return 42.

No compiler code, Runtime Contract setting, CLI projection or .NET semantics change.
The full-source artifact now passes the WriteLine conflict and reaches DnsLookup's
callback result represented by the selected nominal source System.Void. Reconciling
that unit signature remains required before full-System loading/execution. See neoCLR
`docs/experiments/extended-cli-metadata/scoped-service-results-2026-10-07.md` for
contracts, focused checks and compiler/runtime/artifact evidence.

## Canonical native unit storage (2026-10-07)

NeoCLR void is the inhabited unit type, usable in value positions and generic arguments.
The separate unit representation needed by CLR void restrictions remains a .NET backend
concern. No additional NeoCLR Unit type is introduced. No-result calls still have a
separate stack convention; this is not another language-level type.

The existing RuntimeUnitContract explicitly selects System.Void. The native emitter
marks an empty, nongeneric, top-level source owner with native Void storage and applies
the same designation to output-owned references to the selected imported owner. Reference
creation uses semantic symbols and host artifact identities, not importer objects. The
metadata library retains scoped dependency aliases and canonicalizes unit signatures.
Wrong source storage or constructors reject before publication; arbitrary empty structs
are not treated as unit. CLI transport keeps nominal signatures in value positions,
where CLI void is invalid. Ordinary .NET lowering and Runtime Contract defaults stay intact.

Validation: 165 C# metadata groups; native callback/generic and separate-reference
consumers return 42; production NativeMemory compiled separately from its Raven consumer
passes unit-parameter/generic-interface execution and existing negative cases. All 13
RuntimeUnitContractTests pass on .NET. All 197 aggregate System inputs emit; loading next
reaches retained-seed dependency ownership, not a DNS unit-signature mismatch. See neoCLR
`docs/experiments/extended-cli-metadata/canonical-unit-2026-10-07.md` for binary/source
hashes, commands and remaining gates. These are target-specific changes, not a general
compiler fix awaiting backport.

## Full artifact admission and remaining native import (2026-10-07)

The NeoCLR full-source audit finalizes a separate runtime seed after emission, adding
an explicit module/revision reference read from the source-owned native artifact. The
compile-time seed remains recorded separately; Runtime Contract ownership is unchanged.
This closes the missing retained-service dependency without relaxing runtime validation
or introducing CLI projection fallback. A wrong dependency revision rejects, and invalid
translator references publish no output.

All 197 inputs emit. The combined native load set verifies 2,433 IL functions and runs
an API-authored control returning 42. That is artifact admission, not execution of every
System API. The unchanged application-order-collections consumer, with library sources
absent, now fails native import with `Requested value 'System_Value' was not found.`
NativeNamedTypeSymbol maps native primitive designations through SpecialType parsing;
the erased Value category has no such Raven enum member. Fix that semantic classification
next, retaining explicit erased-value ownership rather than inventing a CLR primitive.
See neoCLR `docs/experiments/extended-cli-metadata/retained-catalog-2026-10-07.md` for
reproducible commands and hashes. No compiler or ordinary .NET behavior changes here.

## Native erased-value semantic classification (2026-10-07)

NativeNamedTypeSymbol now treats the metadata library's explicit Value representation
as a nominal value type with SpecialType.None. CLR has no corresponding special type;
Raven's existing erased-value/ownership contracts remain responsible for its target
meaning. The importer preserves the declared assembly identity in return and parameter
symbols. No new CLR special type, reflection surrogate, Runtime Contract option or
emitter/importer coupling is introduced. Other native primitive classifications remain
unchanged, including numeric types.

`NeoClrMetadataProbe --native-value-symbols <core.dll>` creates a native provider with
Value and Int32, imports it, and asserts identity/category and method signature ownership.
It reproduced the System_Value Enum.Parse failure before the fix and now passes. Both
ErasedValueOwnershipTests pass; the compiler builds. Ordinary .NET loading is unchanged.

The unchanged application-order-collections consumer gets through binding against the
197-input source-built artifact and reaches NEOMETA001 for System.Collections.ArrayList`1.
Native type-capability diagnostics now include the rejected metadata name. The imported
library's generic classes use its source-owned Object base, while consumer root selection
still needs reconciliation with the primitive bootstrap. Investigate that contract next;
no inheritance validation has been relaxed, no application artifact is published, and
broad runtime execution is not claimed. See neoCLR's
`docs/experiments/extended-cli-metadata/value-import-2026-10-07.md` for evidence.

## Source-owned library consumer gate (2026-10-07)

The explicit imported Object contract now selects one semantic root while retaining the
CLI primitive bootstrap. Native symbols validate the root shape and preserve a null base;
shared lookup and the temporary bootstrap loader canonicalize Object facts to that selected
symbol. Native binding validation recognizes its exact artifact owner. There is no .NET
root override by default, automatic dependency discovery or importer reuse in emission.

`--object-library Numbers` selects the source-owned aggregate referenced by the consumer.
The unchanged application-order-collections now compiles, verifies and runs against that
197-input native artifact, with class-library sources absent. Exact output and exit 0
cover mutation, shared identity, callbacks, maps and query operations. Missing or conflicting
root selections reject without publication. The runtime still uses the finalized retained
seed and explicit --object-root artifact; this is the approved primitive-core/seed bootstrap,
not complete removal of bootstrap dependencies or execution of every library API.

47 focused regressions and the native root semantic probe pass. Metadata/runtime binaries
are unchanged. See neoCLR `verify_source_owned_orders.py` and
`docs/experiments/extended-cli-metadata/source-owned-orders-2026-10-07.md` for reproducible
commands, compiler/dependency hashes and packaging/editor follow-up. These are opt-in
native target contracts, not a general .NET bug fix requiring a separate main backport.

### Flags markers with an imported Object owner — 2026-10-07

Native flags-enum facts still project to the primitive bootstrap's FlagsAttribute.
Selecting an imported Object owner does not transfer that marker contract to the
Object assembly. NativeNamedTypeSymbol uses the explicit MetadataImportOptions
core identity; absent markers or public parameterless constructors still reject.
The flags-symbols C# probe covers ordinary and imported roots (including a root
without FlagsAttribute). No ordinary .NET loader or emission behavior changes.
Separately compiled Data now advances to its internal array-reflection service
dependency; optional-library packaging is not yet complete.

### Imported Object authoring identity — 2026-10-07

When `MetadataImportOptions.ObjectAssemblyName` selects a native root, emission now
creates its output-owned reference from semantic symbol facts and the host's exact
artifact identity before authoring callable signatures. `SetNativeObjectRoot` on the
metadata builder preserves the selected identity for Equals overrides and imported
bootstrap Object signatures. The importer is not reopened. This requires the matching
metadata API change on neoCLR's native bootstrap branch. An ordinary source Item.Equals
consumer compiled against System.Runtime verifies/runs with exit 42; API authoring and
manual-definition tests also pass. Networking advances to a separate System.Value
encoding failure, so optional-library execution is not complete. No .NET target change.

### Imported erased Value ownership — 2026-10-07

The native emitter registers System.Value from the selected Object-owner assembly
using the existing erased-carrier contract, before importing primitive-bootstrap helper
signatures. It remains a nominal semantic symbol with no CLR SpecialType. The metadata
adapter maps bootstrap Value references to that explicit external owner and preserves
the canonical Value storage tag with scoped aliases. No semantic-loader objects are
reopened. Source-free ParseInt32/IsValue/UnpackValue execution against Runtime and the
retained seed returns 42 for success/error checks. Networking advances to unsupported
imported virtual Object.ToString calls. This native-only fix does not alter .NET emission.

### Imported native Object slot calls — 2026-10-07

The native emitter admits public concrete virtual ToString/GetHashCode/Equals methods
on the selected imported System.Object root and authors an explicit Object slot
reference from symbol signatures. It does not label these new-slot declarations as
overrides or reopen metadata readers. The metadata API validates exact signatures and
requires Callvirt. The ordinary Raven consumer uses an object receiver and executes
all three derived overrides (42) against the independently built Runtime. Other imported
virtual class methods remain outside this bounded contract. Networking advances to
the retained/source CheckedStorage mapping; this is not full Networking acceptance.

### Complete imported Object contract for boxing (2026-10-07)

With explicit `--object-library System.Runtime`, the native emitter authors all three
concrete public Object slots from semantic symbols before bodies: ToString, Equals and
GetHashCode. It requires exactly one supported slot per name and metadata checks exact
signatures. Selection does not reopen importer objects or infer contract completeness
from call sites. Native writing rejects incomplete contracts before publication; actual
external definitions are still validated by runtime linking. Default .NET paths are unchanged.

Separately compiled System.Networking requires explicit `--bootstrap-intrinsics` for
CheckedStorage.Reserve; its ordinary consumer does not. The unchanged network-cancellation
sample compiles using only emitted Runtime/Networking references and executes with
`Network token cancellation checks passed` and exit 0. Metadata C# contracts cover
incomplete external boxing rejection and complete-contract round trips. The corresponding
neoCLR evidence is `docs/experiments/extended-cli-metadata/separate-networking-2026-10-07.md`.
This still uses the documented primitive bootstrap and finalized retained runtime seed.
Data's cross-assembly array-reflection boundary remains separate work.

### Named Object syntax with an imported root (2026-10-07)

When NeoCLR selects an explicit ObjectAssemblyName, type binding normalizes references
to primitive-bootstrap System.Object to the selected native root. This includes `Object`
resolved through a parent namespace, `System.Object`, and nested nullable/array syntax.
Only the explicit bootstrap assembly identity/name participates; user lookalike types
and ordinary .NET binding remain unchanged. No importer objects participate in emission.

A reduced namespace probe asserts all three spellings have the same selected symbol.
The unchanged JSON object consumer compiles against independently emitted Runtime/Data,
then executes nested models, scalar/jagged arrays, setters, identity and invalid-input
checks. The matching metadata writer also preserves canonical nonvirtual Object.GetType
references; the Runtime library exposes bounded ArrayReflection services. Explicit
primitive Core, ownership manifest and finalized seed remain required. Evidence lives
in neoCLR's `docs/experiments/extended-cli-metadata/separate-data-2026-10-07.md`.
This is a native ownership fix, not an independently applicable .NET behavior change.

## Separate Web integration evidence (2026-10-07)

The neoCLR bootstrap branch now has a native Networking-owned NetworkDeadline contract,
consumed by separately compiled Web. Compiler qualification uses Raven 65f554a49 with
explicit primitive Core, retained seed and Runtime/Data/Networking/Web artifact references.
Select System.Runtime through --object-library and, for async consumers, --async-library.
No importer objects are reused by emission and no CLI fallback is introduced.

Five source-free native consumers and header/body loopback HTTP cancellation pass.
New typed deadline CLI declarations are documentation-only; the legacy translator has
no corresponding mapping. This does not qualify every async form: the new Task<()> test
rejected a return conversion while explicit Task<int> passed. A fresh Web build also
intermittently fails native System.Void type validation; the identical command can succeed
on retry. Both compiler cases require investigation before reliable bootstrap qualification.
See neoCLR docs/experiments/extended-cli-metadata/separate-web-2026-10-07.md and its hashed
evidence on codex/native-system-bootstrap. Native project-reference support is the next
integration boundary after reliable builds; existing workspace checks explicitly reject it.

## Bootstrap Void lookup correction (2026-10-07)

Namespace lookup could select primitive-bootstrap System.Void before the explicitly
selected native owner. A generic field such as Promise<Result<System.Void, HttpError>>
then attempted to import a competing Void from the retained seed and failed intermittently.
BindTypeCore now recognizes the exact bootstrap core/type identity under a NeoCLR
RuntimeUnitContract and normalizes it to the selected unit symbol, as it already does
for direct lookup of the selected owner. Unit value storage and no-result emission remain
separate. No emitter fallback, importer-object reuse, metadata extension or .NET policy
change is involved. A missing selected owner does not trigger this normalization.

Two C# tests force bootstrap namespace lookup for Void and System.Void; both failed
before the change. All 29 focused unit-contract tests and six clean Web compilations
pass after it. Existing .NET unit contracts and pointer rules are included. This is a
native target-contract fix on the shared compiler branch, not an independent .NET fix.
The separate async bare-return conversion issue remains a different investigation.

## Native project Object ownership (2026-10-07)

Native projects consuming a source-built runtime select its canonical root explicitly:

```xml
<RavenNeoClrObjectLibrary>System.Runtime</RavenNeoClrObjectLibrary>
```

The value must match exactly one registered native Reference assembly name. The metadata
provider applies the same ObjectAssemblyName target contract as the direct command's
--object-library option. It exposes the selected artifact as ObjectRootPath on the immutable
NeoClrProjectConfiguration. The project driver passes this exact path as --object-root
when --run is requested. The language server uses the same provider and already watches
the referenced artifact. No namespace-to-path guessing, implicit resolution or CLI bridge
is introduced. Missing/ambiguous selections reject; an unset property preserves defaults.
An invalid native root declaration remains subject to existing semantic validation.

C# project checks cover semantic root identity, exact runtime path, watched inputs,
missing selection and competing versions with the same name. The unchanged HTTP headers
sample compiles and executes through --project/--run against separate Runtime/Data/
Networking/Web references; invalid ownership preserves the last successful assembly.
The primitive Core and retained seed remain explicit. This does not implement native
ProjectReference orchestration, source-project symbol references or shipping layouts.
General Raven release integration is owned by the author's separate release task.


## Native project graphs (2026-10-07)

`rvnc neoclr --project App.rvnproj [--run /path/to/neoclr]` evaluates the
explicit native project graph before building dependencies in topological order.
A diamond builds each project once. Every referenced project must select the same
metadata format and target and produce a library. Cycles reject before any build.
This is a native host operation; ordinary .NET project behavior is unchanged.

The shared `IProjectMetadataProvider.GetOutputPath(projectFilePath, assemblyName)`
contract owns artifact placement. NeoCLR uses `bin/neoclr/<AssemblyName>.dll`
relative to each project. `ValidateProjectArtifact(path, assemblyName)` checks
that a prebuilt native artifact has the expected assembly name; dependency identity
and digest validation remain in the native catalog. An artifact from another
project is never silently accepted merely because it occupies the expected path.
Providers without output support reject project references by default.

`MsBuildProjectSystemService.GetMetadataProjectBuildOrder` evaluates the graph
without reading outputs or executing builds. Workspace/editor loading instead
consumes existing artifacts: missing outputs require a command-line build. It
flattens explicit native artifact dependencies across the graph, deduplicates paths,
and watches project files, artifacts, documentation sidecars and provider inputs.
It does not import dependency sources or inject host CLI references. Each project
keeps its own explicit primitive-core, retained-seed and ownership configuration.
Object/async selection may name artifacts supplied transitively by the graph.

Publication remains atomic per assembly, not a transaction across the whole graph.
A later failure may leave earlier successful dependency builds; it cannot replace
the failing project's output. Package/framework resolution, incremental builds,
configuration-specific native output folders and native source-compilation references
remain outside this slice. Full live VS Code acceptance and shipped class-library
project layouts are separate gates.

Validation: C# native project checks cover prebuilt semantic import, watched outputs,
missing artifacts, wrong names, cycles and incompatible formats. The NeoCLR
`verify-native-project-graph.py` consumer builds a four-project diamond, exercises
cross-library generic object mutation and shared identity, and checks exact output,
cycle preflight and preservation after dependency binding failure.

All 67 existing MsBuildProjectSystemServiceTests also pass. The focused command used
`--no-restore -p:BuildProjectReferences=false` after building compiler dependencies;
the initial ordinary test build was stopped while rebuilding the unrelated macro library.


## Native source-root projects (2026-10-07)

`RavenNeoClrSourceObjectRoot=true` exposes the direct native command's existing
`--source-object-root` semantic contract in evaluated projects. Only library output
is accepted, and `RavenNeoClrObjectLibrary` must be unset. Invalid boolean values,
executable output and conflicting source/imported selection reject before publication.
An unset or false property preserves existing behavior.

The provider keeps primitive ownership, sets `UseSourceObjectRoot` and preserves an
explicit async owner. Runtime TypeOf comes from the ownership manifest, including an
absent contract; it does not invent introspection services for the bootstrap root.
Emission still consumes Raven symbols and host-owned artifact identities; no importer
objects are passed to builders. No instruction or metadata encoding changes are needed.

NeoCLR's checked-in `runtime/raven/projects/System.Runtime/System.Runtime.rvnproj`
uses this with bootstrap intrinsics, its explicit ownership manifest and host-supplied
`RavenNeoClrCoreReference` / `RavenNeoClrRuntimeSeed` paths. The 175-source foundation
builds through the ordinary project command. A retained seed finalized against the
emitted artifact allows unchanged orders to compile and execute in a separate project
with Runtime sources absent. This is not a seed-free bootstrap or a complete shipping
project layout. Ordinary .NET defaults and the legacy bridge project are unchanged.

C# native project checks cover the property, async-owner preservation, invalid values,
executable rejection and source/imported conflicts. Existing graph checks remain green;
the 67 ordinary project-system checks from the preceding graph slice are unaffected.


## Prebuilt project dependencies (2026-10-07)

`rvnc neoclr --project App.rvnproj --no-build-references [--run /path/to/neoclr]`
compiles only the selected project. Native project graph evaluation, cycles, target
compatibility, expected assembly names, catalog identity/digest validation and missing
artifact errors remain in effect. The switch does not import dependency sources or
fall back to CLI metadata. Without it, the host still builds dependencies first.
A duplicate switch rejects; an unsupported option continues to report usage.

This exposes existing workspace artifact-loading behavior to build orchestration,
similar to the separation of source and metadata references in the .NET project path.
It adds no new symbol or emitter contract and changes no .NET default. It is not an
incremental freshness check: the host explicitly owns dependency build order.

NeoCLR's staged class-library build uses this to build Runtime, finalize its retained
seed against that exact artifact, then build Data, Networking and Web once in order.
The initial approach that rebuilt Runtime through Web's graph changed Runtime bytes,
invalidating the finalized seed; the staging tool rejected it before publishing a
bundle manifest. Deterministic assembly output is not claimed by this change.

The executable native diamond verifies default builds and prebuilt execution with
broken dependency sources, confirms prebuilt artifacts remain byte-identical, and
checks missing-artifact rejection preserves the consumer. The four native class-library
projects compile; five source-free consumers verify/run, and the project HTTP consumer
passes exact-output and invalid-owner publication checks. Shared project-system tests
from the preceding slice are reused because only explicit native command parsing changes.


## Relocatable native bundle configuration (2026-10-07)

NeoCLR class-library staging now emits `NeoCLR.ClassLibrary.props`. A consumer imports
that file to obtain explicit native References and the matching primitive Core, retained
seed, ownership, Object and async selections. Paths use MSBuildThisFileDirectory;
relocation does not embed the original build checkout in consumer configuration.
Consumers still choose their project TargetFramework, output kind and assembly name.
The configuration explicitly disables source-root and bootstrap-intrinsic authoring.

The same evaluated project reaches the compiler and native-enabled language server.
`GetMetadataInputPaths` now includes evaluated .props/.targets imports for native
projects and their project-reference closure, alongside artifact and documentation
inputs. No source compilation reference, Reflection fallback or emitter access to
importer objects is introduced. Ordinary .NET paths are unchanged.

C# `NeoClrMetadataProbe --native-bundle-project BUNDLE FRESH_DIRECTORY SOURCE` copies
and relocates the bundle into a path with spaces, loads an ordinary project, verifies
native symbol ownership across Runtime/Data/Networking/Web and five references total
(four native, one primitive bootstrap), and checks configuration/seed/artifact input
paths. Invalid Object ownership and missing Web reject without a partial workspace.
The unchanged HTTP consumer then compiles and executes through the project command.
This is headless workspace and CLI evidence, not a new installed VS Code acceptance
claim or a promise of live file-event delivery from every client.


### Split native bundle documentation validation (2026-10-07)

The native bundle project probe now copies nested documentation directories when
relocating Runtime/Data/Networking/Web artifacts. When the Networking XML sidecar
is present, it asserts that native IPAddress symbols expose the authored summary.
The NeoCLR bundler ships generated XML and Markdown next to their owning assemblies
and includes their relative paths in its hash manifest. The same native importer
serves compiler symbols and editor documentation; emission remains independent.
Object/async ownership remains explicitly System.Runtime with the existing primitive
Core and retained-seed selections. No compiler or ordinary .NET behavior changes.
This validates transport of existing help, not complete API documentation coverage
or a merged RavenDoc website model. The integration evidence is recorded in neoCLR's
`docs/experiments/extended-cli-metadata/native-bundle-documentation-2026-10-07.md`.

## Console AOT producer lookup qualification (2026-10-08)

Shared fixes `b7a22b9e1` and `8b46ab9eb` keep source-assembly lookup local and
route qualified and namespace-imported types through compilation-level selection.
Their general equivalents are on main as `9c7db32a2` and `26cc6caae`. The native
backend and qualification build remain on codex/source-object-metadata-resolution.
Ordinary .NET regressions independently reproduce the lookup bugs on main; 68
focused import, namespace, alias and lookup tests pass on each branch. Earlier
lookup/closure/root checks pass 46 tests here and 22 applicable tests on main.

The Console producer had two declarations: a minimal CLI bootstrap Console and the
complete native System.Runtime Console. First-candidate namespace/import lookup and
source-assembly queries escaping into referenced CLI assemblies caused intermittent
missing-member diagnostics before emission. Source-path staging was an observation,
not the root cause. Closure Object lookup now uses the owning compilation's selected
special type, preserving native and .NET root contracts.

The explicit configuration is unchanged: --core-reference, --runtime-seed, native
--reference System.Runtime, --bootstrap-ownership and --object-library System.Runtime.
No temporary CLI encoding, native metadata schema, intrinsic, ABI or emitter fallback
is added. Compiler symbol lookup owns the fix; existing native metadata/CIL emission
and neoCLR AOT compilation consume the corrected bound program. Broader duplicate-type
policy and retirement of bootstrap facade declarations remain separate design work.

neoCLR's docs/experiments/aot-console/raven-lookup-validation.json records fresh
compilation of imported Console calls, nine interpreter/native reader cases, exact
broken-pipe fault/exit parity and standalone execution with an empty environment and
only macOS libSystem. No staged source or reused compilation is used. The driver
now hashes adjacent compiler DLLs and runtime/dependency configuration before reuse;
missing implementation hashes reject before copying artifacts. The bundle libraries
remain pinned; this does not qualify all native emitter features or other .NET targets.

## Console interpolation conversion qualification (2026-10-08)

Shared binder fix `9d2f6ae4e` (main `45650a975`) normalizes synthesized Concat
arguments through ordinary invocation conversions and params mapping. No Runtime
Contract option, CLI representation, metadata encoding or overload-selection change
is introduced. Native consumers retain explicit core, runtime seed, native Runtime
reference, bootstrap ownership and `--object-library System.Runtime` configuration.
Raven owns semantic conversions; neoCLR owns native receiver/codegen support.

Eleven focused .NET tests pass on both lines, including evaluation order and null
text. neoCLR's `docs/experiments/aot-console/interpolation-validation.json` records
fresh native CIL execution for Int32 endpoints, interpolation/addition and null text,
and standalone ARM64 String-only interpolation with UTF-8/NUL parity. The executable
runs alone with an empty environment and only libSystem as a dynamic dependency.
Boxed numeric Object formatting remains explicitly rejected without object publication
until a later native receiver/metadata profile. This limit is not a permanent language
rule or a new CLI bridge restriction. Compiler support on main does not ship the
experimental native backend there; it stays on codex/source-object-metadata-resolution.

## neoCLR StringBuilder and String.Join integration (2026-10-08)

The companion neoCLR main branch adds a development System.Text.StringBuilder with
fluent append, explicit UTF-8 byte quotas and immutable ToString snapshots, plus
String.Join(separator: string, values: string[]). These are runtime-library APIs;
Raven language syntax and ordinary .NET framework lookup are unchanged.

The temporary CLI bridge projects a sealed reference class, fluent returns and
ToString override, and maps the exact bootstrap service
StringJoinParts(arrayref<String>, Int32, String, Int32) -> String. The native target
uses ordinary source-owned CIL class metadata and a namespace InternalCall instead.
Runtime Contract settings for target platform, metadata format, core/unit identities
and checked array ownership are unchanged. The CLI importer was refreshed to accept
String's existing source sequence constructor and authored equality operators.
The runtime library owns public semantics; the neoCLR bridge owns CLI declarations
and import validation; Rust/native C services own checked joining and allocation.

The initial builder supports at most 65536 UTF-8 bytes; quota overflow is a user
fault, not Result propagation. Join preserves separators around empty elements and
requires non-null initialized inputs, unlike .NET's null-to-empty coercion. No
formatting/iterable overloads, mutable native-buffer ownership or stable C ABI are
implied. Native metadata/codegen replaces the temporary facade in native builds;
the documentation pipeline still uses the aggregate CLI reference.

neoCLR's same-source interpreter/AOT consumers use this integration branch at
2acfd40ecc88f5ae45ec4178e2f310c12cee8113 and a matching rebuilt Runtime/Data/Networking/Web
bundle. They validate Unicode, snapshots, reuse, separators and limit fault stacks.
The neoCLR backend additionally admits sealed-owner virtual calls with their null
checks, required by ToString. General virtual dispatch and native HTTP server support
remain incomplete. See neoCLR docs/design/string-building.md and
docs/experiments/string-building/README.md for evidence and performance limitations.

## Inhabited Void imports (2026-10-08)

The HTTP Result<Void,HttpError> consumer exposed an import ordering gap: native
signature encoding already maps the explicit core System.Void facade to inhabited
Void, but declaration validation first required a nominal seed type. The metadata
importer now admits only the empty, nongeneric, top-level value facade from the
selected core bound to native module System. Snapshot/core checks remain in force;
wrong-module bindings are rejected. No-result returns remain distinct from inhabited
Void results. Runtime Contract and unit configuration are unchanged, and ordinary
.NET import/emission retains its existing behavior.

This is a companion neoCLR metadata-library fix on main, tested with Raven's
codex/source-object-metadata-resolution compiler f88757da4 rebuilt against that
library. Raven main does not contain this native emitter; no shared .NET compiler
fix was identified. The CLI facade is temporary type identity for the existing
native Void primitive, not a new nominal runtime type or bridge opcode. The native
metadata path preserves the value/no-result distinction directly.

Validation: the focused --native-void-value metadata check reproduces the original
failure, then passes native serialization/reimport, value-result and wrong-core
checks. TaskResultList.rvn now compiles and executes interpreted; native admission
next rejects HttpError's 40 flattened lanes against the current 32-lane limit.
The HTTP app remains work in progress; no native server execution is claimed.

## Unprefixed async interfaces (development, 2026-10-08)

The neoCLR target uses `System.Runtime.CompilerServices.AsyncStateMachine` and
`TaskAwaiter`. The runtime owns these declarations; Raven's target contract maps
its internal state-machine special type to the unprefixed name for lookup and
CLI emission. The native metadata importer recognizes that name in the explicitly
selected async provider. The temporary CLI importer recognizes it in the configured
core/async provider. Ordinary .NET keeps `IAsyncStateMachine`. `TaskAwaiter` is an
ordinary runtime interface discovered through builder signatures, not a new compiler
special type. No async protocol or scheduling semantics change.

This intentionally changes neoCLR source and metadata identities: rebuild consumers
and all runtime/reference bundles together. Old prefixed artifacts are not aliases.
CLI bridge helpers retain their existing callback/heap state-machine restrictions;
the native backend emits the actual unprefixed metadata identity. A future removal
of the CLI bootstrap does not require another public interface rename.

### Unprefixed union bridge interface (2026-10-08)

The neoCLR Runtime Contract names the boxed-active-case CLI bridge interface
`System.Runtime.CompilerServices.UnionValue`. The existing explicit legacy
`NeoCLR.CoreProbe` target-core compatibility contract selects the same identity.
Lookup, generated interface definitions and imported interface reuse use this one
contract. Ordinary .NET retains `IUnion`; `IUnionSymbol` and related compiler API
interfaces are unchanged. Rebuild matched neoCLR references, runtime and consumers.
The native backend's native union metadata is unchanged; this naming correction
does not add native general boxing or reflection. Sixteen focused profile, .NET union-protocol and output-initialization checks pass.

Legacy explicit probe-core async lookup also uses `AsyncStateMachine`, including
CLI special-type recognition in the selected provider; ordinary .NET remains
unchanged. The companion neoCLR bridge refresh translates current union constructor
field defaults into checked native field writes and materializes typed null output
stores. Its regenerated union library now follows the existing shared bound-body
contract from `0d261d8be`: TryGetValue clears outputs on failure. The legacy union
and time/timezone samples pass, with ten error-union admission/rejection cases.
This corrects stale bridge behavior rather than changing Raven's union semantics.

Async interface signatures use target metadata alongside task/builder types during
persisted CLI emission; they are not resolved through the compiler host's runtime
assemblies. Regression checks emit a pass-through signature for explicit NeoCLR
and legacy probe-core profiles and inspect its return type and assembly scope.


### Discarded await at a statement boundary (2026-10-08)

The portable body planner now forwards the enclosing empty-stack statement context
when lowering `_ = expression`. A discarded await may suspend and resume before its
result is dropped, just like an awaited local initializer. A discard nested in a
larger value expression does not gain permission to exit while earlier operands are
still on the stack. The new regression covers both boundaries without asserting an
instruction sequence, plus ordinary .NET completed/pending awaits in Debug/Release.

This is an emission correction, not a binder, semantic-model or Runtime Contract
change. The neoCLR consumer still explicitly selects `System.Runtime` as its async
provider. Existing CLI state-machine representation and native metadata/CIL encoding
are unchanged; the portable compiler owns the missing control-flow context, and the
runtime owns queue entry draining. Native host-I/O entry waits remain unsupported.

The portable planner currently exists on `codex/source-object-metadata-resolution`
and is absent from main. This general planner fix should accompany that shared backend
when integrated; it is not a permanent neoCLR-only language rule. Do not transplant
the target backend to main solely to apply this local fix. Independent .NET controls
remain usable on main without the portable planner.

Validation: the 60-test portable-body baseline passed. Before the fix, both new
statement-boundary planner controls failed while five other new controls passed.
After the fix, 67 focused shared-body/.NET async tests pass. The native queued-await
consumer now prints `Queued` then `Resumed` in interpreter, sanitized native and
standalone execution. These are modern .NET and bounded neoCLR checks, not .NET
Framework/NanoFramework qualification or a benchmark.

### Math bootstrap collision correction (2026-10-08)

neoCLR's explicit source-runtime bootstrap now omits the placeholder System.Math type
in favor of System.Runtime's native Math namespace. Qualified/wildcard Sign calls
selected that placeholder in old inputs; an explicit namespace alias selected the
native function. No Raven binder, Runtime Contract option or native method mapping
changes. Ordinary .NET type/namespace precedence remains intact. Four focused namespace
metadata controls pass on integration 2c8c1f9de; comparison with main shows only the
unrelated union-companion abstraction in ImportBinder. This does not establish a general
lookup defect on main. Rebuild neoCLR Core.dll and the matching native bundle; old
artifacts retain their collision. The native-core replacement and assembly-level constant
support remain separate work. neoCLR records interpreted/native string-sample evidence
in benchmarks/native-web/math-lookup-validation.json.

### Native assembly-level Double constants (2026-10-08)

The native backend now collects public/internal namespace const declarations of
finite Double type and emits their values into neoCLR constants metadata.
The native reader exposes them as IFieldSymbol constants in the actual namespace;
ordinary GetConstantValue and qualified/wildcard lookup supply the value. The portable
body planner inlines Double field constants. No target Runtime Contract option is
added; ordinary .NET namespace literal-field emission remains unchanged.

The initial metadata contract is finite Double only. Other types/attributes reject
with NEOMETA001 rather than losing declarations. Native semantic snapshots carry
exact bits; native PE's incidental CLI envelope does not project assembly-level constants.
Standalone metadata-tool CLI projection rejects. neoCLR's documentation bridge exposes
its Pi/E/Tau API as literal fields separately. Use matching updated compiler, metadata
and runtime tools; older readers reject the additive metadata field. Consumers must
be recompiled after changing constant values, just as with .NET const fields.

Validation: namespace metadata controls and qualified/wildcard Double .NET execution,
plus neoCLR's separately compiled MathConstants consumer in interpreted mode.
AOT still rejects Double instructions; no compiled execution is claimed.
The portable emitter is not present on main, so its Double-inlining addition remains
with that pending shared-emitter integration; this is not a demonstrated main compiler
bug. Source constant syntax and .NET semantic behavior already exist on main.
neoCLR owns metadata/reader bounds and API documentation; Raven owns declarations,
namespace symbols and body inlining. Full native-core replacement remains independent.

Qualified native constant access also extends GetNamespaceMembers to include direct
constant fields and permits a null nominal receiver for such fields. The pre-existing
native direct-function path is not on main; these changes remain with that integration.
Ordinary .NET carrier lookup keeps its existing behavior and focused controls pass.

## Assembly-level members and qualified names (2026-10-08)

Author clarification: types, functions and constants can be assembly-level members.
Their names include namespaces. Assembly identity supplies ownership; the namespace
is part of the qualified name and supports source lookup/imports.

For example, System.Runtime declares the type `System.String`, the function
`System.Math.Sign`, and the constant `System.Math.Pi`. The metadata may store the
namespace and simple name separately; together they represent the qualified name.
Function overloads also retain signatures. A qualified name alone does not replace
assembly identity when resolving dependencies. Type-owned members and nested types
retain their declaring-type relationships.

Use **assembly-level function**, **assembly-level constant** or **assembly-level
member** in APIs and documentation. The existing `Namespace` metadata property and
namespace imports describe name qualification. This terminology does not change
CLI instruction semantics, runtime storage, overload identity or ABI conventions.

The unreleased constant adapter now uses AssemblyConstantDefinition,
AssemblyBuilder.AddConstant and ModuleDefinition.Constants. Native metadata uses
assemblies[].constants. Rebuild same-day prototype artifacts with matching tools;
old serialized names are not retained as aliases. Direct namespace symbol lookup
still supplies the qualified members; no source syntax or inlining behavior changes.

The matching metadata library also exposes AssemblyDefinition.GetMembers and
AssemblyInfo.GetMembers for assembly-owned type/function/constant views. Native
round-trip checks retain ownership, qualified names and overload signatures. Six
compiler emission/visibility controls pass using the renamed contract; the Math
consumer still passes interpreted execution and remains AOT-blocked on Double.


## Native-only core initialization frontier (2026-10-08)

neoCLR's metadata writer now permits a self-owned native core PE/#Neo container,
using local TypeDef handles in its reference-only projection rather than an external
self-dependency. This is producer support, not a new Raven target contract.
The reduced neoCLR `docs/experiments/native-core-bootstrap` probe passes only a
`NeoClrMetadataReference` for an authored primitive core. At compiler `bc3c500e6`,
GetDiagnostics reports RAVT004 because DotNetCompilationTarget still creates a
DotNetMetadataSession from portable references before native semantic initialization.
The probe uses the required NeoCLR.CoreProbe identity and emits no consumer.

Native intent is direct core symbol ownership from native metadata. Current catalog,
driver and project profiles still require the explicit CLI primitive bootstrap;
ordinary .NET behavior is unchanged. Do not supply a CLI projection as a purported
fix for the no-bridge release requirement. Next isolate native semantic initialization
behind an explicit Runtime Contract, then validate complete core ownership, native
emission, source-library builds and project/editor consumers. A metadata core with
three primitives is not a complete runtime contract. Compiler hosting on .NET is a
separate concern; no compiler implementation change is included in this checkpoint.


### Native-only core follow-through (2026-10-08)

The initialization failure above is now closed for the explicit compiler API mode
`MetadataImportOptions.WithNativeMetadata()`. Semantic references load directly,
without a DotNetMetadataSession or CLI projection input; native emission accepts the
exact native core identity for primitives and unit. Default .NET/CLI behavior remains.
The neoCLR reduced consumer now emits and returns 42 in interpreter and macOS ARM64
native execution; the binary depends only on libSystem. Fixture Object slots and an
empty native System seed are deliberate test inputs, not production runtime coverage.
See the runtime-contract API section for guards and limits. Driver/project,
complete source-runtime ownership and Windows/editor qualification remain outstanding.

The author also expects RavenDoc to stop exposing Probe artifacts as this migration
progresses. Its assembly input loader still creates portable references and adds
framework references; the symbol renderer already tracks actual declaring assemblies.
A native provider must reuse native semantic metadata and real ownership, not relabel
CLI CoreProbe declarations. Probe.dll is generator tooling; NeoCLR.CoreProbe.dll is
the current aggregate documentation reference. No RavenDoc provider change or new
release gate is implied by this compiler API slice.


### Native core catalog follow-through (2026-10-08)

`NeoClrReferenceCatalog.ReadNative` now composes native core/library semantic snapshots
and matching native emission bindings through an explicit API. Configure
`WithNativeMetadata`, the selected core, owned unit and Object contracts as documented
in runtime-contracts.md. Existing `Read` remains the CLI bootstrap path. No temporary
CLI projection is used as a semantic input, and no translated System implementation
is attached to the native core; runtime seed selection remains the execution host's
responsibility. Native PE/#Neo wrapping remains the reference transport format.

Raven owns loading/configuration; neoCLR owns core production and execution. Focused
snapshot/invalid-input/dependency checks and the existing CLI catalog control pass.
neoCLR's separate-library consumer returns 42 interpreted and ARM64 native. Full
core declarations, driver/project/editor integration and RavenDoc's native provider
remain pending; this does not remove their existing importer path or rename artifacts.


### Native core driver selection (development, 2026-10-08)

`rvnc neoclr --native-core-reference Core.dll --reference Library.dll -o App.dll App.rvn`
selects the native catalog and semantic loader explicitly. It configures the core’s
System.Void and Object ownership and disables the implicit typeof service contract.
`--object-library` and `--async-library` can select explicit native providers.
No host framework references or CLI Console binding enter this path. Existing
`--core-reference` retains CLI bootstrap behavior; the two selections are exclusive.
Runtime seeds remain execution inputs. This bounded consumer path rejects retained
seed/static projection, bootstrap intrinsics, source-root and ownership-manifest flags;
complete source-core bootstrapping and project/editor configuration remain pending.

The companion neoCLR verifier compiles a separate-library consumer through the driver,
checks conflicting options and preservation of existing outputs, then runs interpreted
and ARM64 native with result 42. The native image links only libSystem. Raven owns this
selection; native metadata/codegen and runtime contracts are unchanged. This removes a
CLI semantic input requirement for explicit driver consumers, not the .NET compiler host.


### RavenDoc native loader (development, 2026-10-08)

RavenDoc now accepts `--native-core-reference` for compiled native inputs, or
`nativeCoreReference` in site configuration and individual API groups. It reuses
`NeoClrReferenceCatalog.ReadNative` with explicit dependency references, owned core
Object/System.Void contracts and no implicit typeof service. No framework references,
adjacent assembly discovery or CLI semantic projection enter this path. The native
metadata adapter remains an optional build dependency selected by NeoClrMetadataProject;
ordinary .NET loading remains the default. Raven owns symbol loading and rendering;
neoCLR owns the produced metadata and documentation sidecars.

Grouped native libraries retain one namespace/type tree, original declaring identities,
documentation and cross-library links. Missing/invalid/conflicting inputs reject before
rendering. Native source/project inputs and the full neoCLR documentation bundle migration
remain separate work. See `docs/ravendoc.md` for configuration and limitations. This
reuses native compiler loading rather than adding a second metadata decoder or pretending
that current CLI CoreProbe declarations belong to production libraries.


### Assembly-level documentation ownership (2026-10-09)

RavenDoc and DocumentationCommentIdBuilder now support assembly-level function IDs
`M:Namespace.Function(parameter-types)` and constant IDs `F:Namespace.Constant`, with
no namespace prefix for global members. Type-owned .NET IDs remain unchanged. Native
constant symbols forward sidecar documentation through the existing reader. Type
selection filters no longer dereference a missing declaring type on these members.
This restores documentation semantics; no emitted IL or runtime contract changes.

The native-only fixture exercises namespaced/global functions, constants and their
comments. Existing .NET documentation tests remain controls. A separate production
bundle audit reuses its project metadata configuration, including the explicitly
retained CLI primitive bootstrap: four actual native library inputs now render 1,632
API pages with their real owners and no CoreProbe labels. This audit is not native-only
bootstrap qualification and does not silently expand the strict native loader's policy.
Full website coverage, comment association and route migration remain required.

The renderer assumption also exists on Raven main. The shared renderer/documentation-ID
fix is a general candidate; its reduced native regression depends on the integration
adapter. Carry the independent documentation tests with the shared-line reconciliation,
rather than treating this renderer fix as permanently target-specific.


### Native documentation identity follow-up (2026-10-09)

For a NeoCLR target, source members owned by the synthesized NamespaceMembers
container now receive native assembly-level documentation IDs: for example
`M:System.Math.Exp(System.Double)` and `F:System.Math.Pi`. XML and Markdown
sidecars share this identity builder. Ordinary .NET compilation retains the carrier
IDs because those match its emitted CLI declarations; user-authored types named
NamespaceMembers are not rewritten. Native metadata readers already use the
canonical IDs. No new Runtime Contract setting is required beyond the explicit
NeoCLR target; this does not change IL or native metadata encoding.

Historical bundle sidecars must be regenerated or explicitly mapped during migration.
The neoCLR website audit matches reviewed XML to actual native declarations before
recovering comments; that compatibility input is not a permanent loader fallback.
RavenDoc now includes assembly functions/constants in namespace navigation and
collects declarations from documented libraries rather than letting bootstrap
primitive equivalence hide Object. Full native-core bootstrap remains separate.

### Native nested unit ownership (2026-10-09)

When RuntimeUnitContract selects a source/native library's System.Void, native
nominal emission maps the explicit primitive bootstrap's Void value to that selected
owner. Imported callable substitutions can retain a bootstrap symbol inside nested
generic constructions; accepting it unchanged produced two incompatible
Promise<Result<Void, HttpError>> signatures in HttpContext's constructor. The
metadata verifier remains strict. This mapping is confined to the selected core's
Void and an explicit unit contract; same-named foreign types are not aliases.
No-result returns and ordinary .NET compilation are unchanged.

The remaining CLI primitive bootstrap is temporary. A fully native core must carry
the same explicit inhabited-unit ownership; this fix does not qualify bridge-free
bootstrap. `tools/NeoClrMetadataProbe/check-unit-owner.py --compiler <rvnc.dll>
--bundle <native-bundle> --output <fresh-directory>` checks nested field initializer
constructors with both System.Void and unit spellings. The nominal case reproduces
NEOMETA003 without the fix; both compile after it. The full neoCLR Web library also
builds, with the existing seven unit-contract tests retained as controls.

### neoCLR System.Time migration (2026-10-09)

The matching neoCLR library now owns civil values, clocks, durations, instants,
calendar policies and timezone mappings/errors in System.Time. TimeOfDay replaces
the former System.Time struct; LocalDateTime.Time remains a property. Consumers
use `import System.Time.*` and rebuild against a matching library bundle.
System.Runtime remains the assembly owner. System.Globalization retains formatting.
No new Runtime Contract setting or ordinary .NET type remapping is introduced.

The legacy neoCLR reference producer mirrors these declaration names. During
separate Instant slice authoring only, its bootstrap reference temporarily exposes
LocalDateTime.FromUnixTimeTicks(Int64); the importer restores internal visibility
and admits only the exact Instant caller. Normal consumer references omit that
factory; native source emission preserves its ordinary internal contract. This
bootstrap shim is temporary and is replaced by full native library compilation.
Validation includes regenerated time/globalization slices, calendar and timezone
consumers, renamed metadata/XML IDs and local API navigation. Timezone runtime
services and nonempty value boxing still limit AOT coverage.

## Declaration module foundation (2026-10-09)

Native intent: assembly identity owns independently named logical modules, which
own types, free functions and constants. Raven accepts `module` through the existing
file/block namespace grammar and exposes `INamespaceSymbol.IsModule`; imports and
qualified names retain their established binding behavior. The native backend writes
explicit module names, including empty declarations, and its loader marks native
module scopes without creating CLR container types.

Temporary CLI representation remains ordinary namespaces and the existing global
function/constant projection. It loses explicit module status and empty declarations.
No new Runtime Contract is required. The parser/binder owns source scope and naming;
neoCLR's metadata library owns the native table and validates owners; runtime admission
validates its version/names. Guest module discovery and module-private access have not
been implemented. Tests cover both syntax forms, nested imports, unchanged namespace
controls, metadata ownership and native scalar execution. Ordinary .NET behavior is
unchanged for existing source.

The language-server/VS Code presentation uses module labels for neoCLR target scopes,
including CLI projections; this is not a claim that the CLI image stores native
module records. Explicit module source uses that label on either target. Hover,
completion display and document/workspace symbol kinds follow this contract, while
ordinary .NET namespace spelling retains namespace presentation.

## Native-only project core selection (2026-10-09)

Set RavenTargetPlatform=NeoCLR, RavenMetadataFormat=NeoCLR and
`RavenNeoClrNativeCoreReference` to a project-relative native PE/#Neo core path.
Select exactly one of that property and the legacy `RavenNeoClrCoreReference`.
The shared project adapter calls ReadNative, sets native-only metadata import,
selects the core's Object and System.Void contracts and clears the default TypeOf
service contract, matching the bounded direct driver path. Core identity properties,
when supplied, must match the artifact. This does not establish a complete core API.

`RavenNeoClrRuntimeSeed` is optional and execution-only in this mode. It must exist
and differ from semantic references. The native core is included in ReferencePaths
and selected as ObjectRootPath for `rvnc neoclr --project ... --run ...`; it is also
a watched input. The seed is never attached as a translated core implementation.
CreateEmissionBackend uses Catalog.CoreReference and never asks a native catalog
for its CLI Bootstrap.

BootstrapOwnership, BootstrapIntrinsics, SourceObjectRoot, ObjectLibrary and
AsyncLibrary settings (with the RavenNeoClr prefix) reject when nonempty in this
bounded mode. No bridge encoding is introduced: all semantic core/dependency inputs
are native snapshots. The old explicit CLI bootstrap mode retains its existing
behavior and limitations. Shared project loading also supplies language-server
semantics, but installed-editor acceptance is a separate validation step.

The native-core-project probe covers native-only references, module binding, input
watching, emission, execution paths and nine rejected configurations, including
preservation of the last successful configuration on failed reload. The checked
neoCLR project verifier covers driver publication and interpreted/native execution.
Full source-runtime bootstrap and release package qualification remain open.

## Native bootstrap synthesized-string frontier (2026-10-09)

Unchanged Option/Result source reaches generated formatting after the native value
base and Byte tag prerequisites. An incomplete native String must now produce RAV1501
for missing String.Concat without output publication, rather than terminate the host
with a Sequence/InvalidOperationException. The body factory and emission boundary
own this shared diagnostic behavior; no temporary CLI encoding or substitute
formatting body is introduced. Ordinary .NET missing-Concat tests exercise the same
failure. Production native String/core completeness remains the next bootstrap task.

Shared-line follow-up: the synthesized concat helper and test file match Raven main
before this fix, so this is a general compiler repair, not a permanent neoCLR
divergence. The integration branch's emission boundary additionally dispatches an
external backend. Port the diagnostic wrapper to main's emitter boundary and run the
same .NET checks there independently; the current evidence is on the integration
branch only.

Native ValueType prerequisite: native-only core classification now recognizes the
selected core's nominal System.ValueType base independently of primitive storage
markers. Catalog controls include a same-named non-core type, which remains ordinary.
The checked copied-struct consumer runs without CLI semantic references; unchanged
production Option/Result reaches the missing String.Concat diagnostic. The minimal
core fixture adds Byte for union tags but is still not a production runtime library.


### Constructed value receivers (2026-10-09)

Portable emission now routes BoundObjectCreationExpression receivers through the
same single-evaluation temporary storage used by value-returning calls/getters.
For example, Number(40).TryGet(out output) no longer rejects as unaddressable.
Construction precedes method arguments; mutation applies only to the temporary.
The existing constructor/type/capability checks still apply; initializers and
unsupported construction shapes are not newly admitted.

No Runtime Contract configuration, native format or temporary CLI representation
changes. This is compiler lowering toward ordinary CLI value-receiver behavior,
not a neoCLR semantic divergence. The portable emitter is absent from Raven main;
there is no corresponding main-line file to patch independently. The fix belongs
to the shared integration line, codex/source-object-metadata-resolution.

Validation: --value-constructor-runtime and --nested-constructor-runtime check
immediate mutating/out calls on ordinary and generic values, with interpreter
result 42 and missing-dependency output preservation. The neoCLR native-core
escaping audit also executes an immediate union-case ToString call in project,
interpreter and ARM64 native modes. It intentionally records a separate shared
escaping defect: without String.Replace(string,string), synthesized quoting does
not escape quotes/backslashes. That library gap remains unresolved.

## Source String bootstrap dependency contracts (2026-10-09)

The explicitly selected native core classifies nominal `System.Array` and
`System.Enum` alongside ValueType. `RuntimeIterationContract.ArrayShapeTypeName`
and its exact assembly select rank-one array members/interfaces; rank-two arrays
are not projected. These native symbols now provide the same vector member lookup
contract as the temporary CLI provider. No System-name inference applies to an
unselected assembly, and ordinary .NET behavior remains unchanged.

Native function syntax uses the existing Function signature transport. Its internal
synthesized delegate symbol does not itself constrain public API accessibility;
parameter/result types still do. Explicit named delegates retain nominal identity.
Portable invocation lowering uses the Function result policy, including popping an
inhabited unit result in an expression statement. No new structural Function
language semantics are enabled.

When MetadataImportOptions.SourcePrimitiveTypes selects String, implicit `+` and
synthesized union display/escaping use that source member provider, while expression
storage remains the selected core primitive. This removes the need to put fake
Concat/Replace implementations on a bootstrap metadata declaration. The temporary
CLI bridge remains available, but this validation uses only native references.

Validation: ArrayTypeProviderTests plus NativeCallbackAccessibilityTests (7 tests);
NeoClrMetadataProbe `--native-array-shape <NativeCore.dll>`; neoCLR's
`docs/experiments/native-core-bootstrap` source String fixture compiles unchanged
production String/Array/unions and executes a separate consumer in the interpreter.
The native runtime owns primitive-reference resolution and AOT service bindings;
this compiler slice does not by itself establish full core bootstrap readiness.

## Native documentation extension visibility (2026-10-09)

NativeNamedTypeSymbol currently recognizes a container ExtensionAttribute, and
NativeMethodSymbol treats its parameterized static ordinary methods as instance
extensions. The metadata needs an explicit static/instance distinction and
receiver identity: the first ordinary parameter of a static extension is not an
instance receiver. ConsoleRuntimeServices.ConsoleFlush(error: bool) exposed this
limitation through native API documentation. Do not derive a future contract from
parameter names. Emitter and reader changes with native semantic tests remain
required; no Runtime Contract option or runtime binary changes in this slice.

RavenDoc now requires public enclosing types as well as a public member for
receiver contributions. Source/.NET metadata regressions cover instance/static
internal extensions and specialized generic extensions; the neoCLR production
audit verifies that internal console functions are absent from Boolean. This
shared presentation fix prevents the visibility leak but does not resolve the
native reader's broader extension classification limitation.

## Positional record storage for native map pairs (2026-10-10)

The native adapter explicitly enables `PositionalRecordStorage`: top-level positional
`record struct` declarations with `val` components and no additional members or
attributes. It emits ordinary native value storage, a primary/copy constructor,
getters and `Deconstruct(out ...)`. Portable lowering supports simple positional
bindings and discards through the selected instance deconstruction method; receiver
expressions are evaluated once. This is native metadata emission, not a new CLI
encoding. No runtime-contract configuration or ordinary .NET record behavior changes.

This capability is intentionally **not full record support**. The native surface
omits synthesized equality, hashing, formatting and init-only accessors. Calls to
unsupported generated helpers fail target admission; imported components are
getter-only. Native metadata does not yet carry a record marker or init-only accessor
semantics. Mutable positional components, record classes, extra members, attributes,
extension/nested/refutable deconstruction patterns are outside this bounded slice.
Use explicit key comparers for maps; the pair is a transport value, not an implicitly
hashable key contract. Full records require shared generated value equality/hashing
and formatting against native runtime contracts plus metadata support for initialization.
Those restrictions belong to the adapter, not permanent Raven language rules.

Ownership: Raven owns declaration admission, synthesized body lowering and diagnostics;
neoCLR owns value verification, copying, generic storage and metadata. The matching
neoCLR collection consumer checks imported generic pairs, getters, copying,
deconstruction, map/interface iteration and snapshots on interpreter and macOS AOT.
Focused portable-plan tests guard capability opt-in and deconstruction. The earlier
`71cafd353` compiler rejects the record declaration; development libraries require
the compiler revision containing this capability. Windows pair qualification is
tracked by neoCLR separately from its already-passing Queue/Stack/Set action.

Branch scope: this implementation depends on the existing native adapter on
`codex/source-object-metadata-resolution`; it is not on Raven main. The portable
instance-deconstruction lowering is a general integration candidate, validated with
an ordinary source struct independently of the native positional-record consumer.
Move it with its portable-emission dependencies when reconciling that line; its
classification as general behavior is not changed by this native consumer.

## Native init properties — 2026-10-10

This follow-up supersedes the previous positional-record getter-only limitation.
The explicit `InitAccessor` portable capability admits instance automatic and
implemented init properties, including positional record components. Shared lowering
uses the existing accessor body/call model. Native emission associates the setter
with a property carrying `init_only`; native import exposes `MethodKind.InitOnly`,
so existing Raven initializer/ordinary-assignment diagnostics work across assemblies.
Native imported accessors remain available through the property/token map and are
excluded from ordinary named-member lookup, matching PE import and rejecting direct
`set_Property(...)` bypasses. No syntax, language-service grammar or default .NET
emitter policy changes.

Requires matching neoCLR metadata/runtime support (neoCLR `84df378d` or later).
The Runtime Contract continues to select the native adapter explicitly. Native
semantics do not require a .NET IsExternalInit type: only the CLI transport/reference
projection emits that standard required return modifier. Native containers retain
the explicit property fact. Ordinary init calls, including reflection, are permitted
at runtime; the verifier grants declaring init setters the same own-field readonly
write privilege as constructors. This is not runtime freezing, full C# construction-
phase parity, native init indexers, or full record identity/equality/hash/display.

Validation: 62 focused property/object-initializer/record/portable tests pass, including
capability rejection and automatic/implemented init body lowering. A separate native
library with an automatic property and generic positional record compiles; its
initializer consumer runs in neoCLR's interpreter. The neoCLR init-accessor experiment
records successful macOS/Windows x64 native/interpreter and negative consumer
qualification. Native AOT requires neoCLR `a2d7eda4` or later to retain reached init
associations through specialization/trimming; metadata/runtime support alone does
not qualify that backend. This adapter remains on
`codex/source-object-metadata-resolution`, not Raven main; general portable support is
an integration candidate dependent on the existing shared native emission foundations.


## Scoped native enumeration cleanup (2026-10-10)

With explicit RuntimeIterationContract and RuntimeDisposalContract configured,
`UseExceptionHandling=false` now lowers disposable reference-iterator `for` loops
before the method-wide scope-exit pass. Iterator acquisition occurs once; successful
acquisition owns a scoped resource. Exhaustion, break, return and outward transfers
dispose it, in reverse lifetime order with surrounding and nested `use` resources.
Continue within the same loop retains its iterator. Ordinary .NET enumeration
keeps its existing default path and exception-handling behavior.

neoCLR source System.Runtime must override the bootstrap disposal owner in its
ownership configuration: assemblyName `System.Runtime`, interfaceTypeName
`System.Disposable`, useExceptionHandling `false`. Its source Iterator<T> implements
that source-owned protocol; the bootstrap interface is a different identity.

This is scope-exit cleanup, not exception unwinding: provider/callback terminal
Faults do not guarantee disposal. Only the supported reference iterator shape is
covered; arrays/ranges retain existing lowering. Temporary CLI transport uses
ordinary calls, locals and branches and carries no exception region. Native
metadata/codegen consumes those same semantic calls; future runtime unwinding
requires an explicit contract change, owned jointly by Raven lowering and neoCLR.

Validation: PortableEnumerationCleanupTests exercises exhaustion, break, continue,
return, nested use ordering, labeled break/continue and outward goto. All eight
regressions failed before this fix and pass after it; the 50 existing focused
cleanup/loop tests also pass. Native executable integration is qualified in
neoCLR's integration evidence. This adapter revision remains on
`codex/source-object-metadata-resolution`; it does not imply availability on main.

Follow-up qualification caught no-resource return expressions being needlessly
wrapped when the same method contained a scoped loop. Preserve the original return
expression when no cleanup is needed (notably `?? return` before JSON mapper loops).
Ten focused cases now check portable body admission and execution, including
match-arm returns inside loops. System.Data native emission passes this correction.


## Native attribute import and usage policies (2026-10-10)

The explicit native metadata provider now maps stored custom attributes to ordinary
Raven AttributeData on types, constructors, methods and module-level functions,
fields, properties and parameters. Enum arguments retain their enum symbol and
TypedConstantKind.Enum; named data retains its member name and typed value. No
attribute constructor, getter, setter or annotated body runs during inspection.
The configured primitive-core Flags fact is still synthesized as before.

There is no new Runtime Contract switch. This remains the existing neoCLR target,
UseNativeMetadata/CoreAssemblyName configuration and explicitly supplied primitive
bootstrap when used. Ordinary .NET import/binding is unchanged. The ordinary binder
uses the imported System.AttributeUsageAttribute identity to enforce valid targets
and AllowMultiple, including inherited policies and replacement defaults. Inherited
is retained as data; this change does not add an inherited runtime attribute query.
A similarly named application annotation is not recognized as the core policy.

The native reference boundary validates constructor signatures and public writable
named fields or public read/write non-indexed properties against its explicit catalog.
Invalid or unsupported metadata becomes RAVT003 instead of silently losing policy or
throwing late in binding. The matching host metadata reader now inspects bounded CLI
instance and nominal method signatures, needed to validate bootstrap attribute
constructors without runtime loading.

Native data remains authoritative. PE/#Neo keeps a temporary CLI reference projection
with equivalent constructor signatures and positional/named blobs. No CLI bodies are
used for native execution. The metadata library owns decoding/identity; Raven owns
symbol mapping and source diagnostics; neoCLR owns linked validation and execution.
Direct native emission/import must ultimately replace the bootstrap dependency.

Limitations: this qualifies import and source binding against metadata-authored
libraries, not native source attribute emission. The emitter still rejects ordinary
source annotations. Named inherited members, wider payload categories, strict native
System.Attribute base enforcement, assembly/module/return/generic-parameter targets,
events, guest named-data inspection and AOT discovery retention are not completed.
These are support gaps relative to .NET, not alternative semantics. The installed
runtime bundle is not automatically upgraded by a compiler source change.

Validation: `NeoClrMetadataProbe --native-attributes <Core.dll>` covers all supported
member kinds, typed enum/named data, nine valid/invalid usage consumers, lookalike
policy identity and three malformed-data diagnostics. The 20 ordinary AttributeUsage
regressions pass; native flags and the existing System.Runtime async-symbol consumer
also pass. The matching neoCLR host suite passes 168 groups. No Windows/AOT execution
claim is made by these metadata/binding checks.


## Native source annotation emission (2026-10-10)

Follow-up to native import: the native adapter now writes bound source annotations
on supported types (including interfaces/enums), functions, constructors, methods,
fields, properties/accessors and parameters. The payload preserves String (including
null), Int32, Boolean and nominal Int32 enum fixed arguments, plus primitive named
field/property arguments. Co-owned source attribute constructors retain local identity;
imported constructors retain dependency identity. Binding applies ordinary usage rules
before native emission. Inspection does not execute attribute code.

No Runtime Contract configuration changes: use the existing native target, matching
metadata dependency and explicit primitive bootstrap. The portable interface planner
has an explicit native custom-attribute capability; other adapters retain their
admission defaults. Native metadata owns declarations and typed values. Temporary CLI
projection uses ordinary constructor references and attribute blobs; it does not
supply missing target or payload support. Raven owns binding/emission, neoCLR owns
metadata validation and runtime inspection. Native core metadata will replace the
bootstrap dependency; no new permanent CLI encoding is introduced.

Assembly/module/return/generic-parameter and other unhandled targets, wider primitive,
type/array and named-enum payloads reject with NEOMETA001 before output publication.
Named members must be declared unambiguously on the attribute type. Existing flags,
InternalCall, nullable, params and union marker encoders remain in place. Synthesized
attributes are not mistaken for source annotations. Separate-library concrete
Attribute inheritance remains a native type-planning gap; co-owned inheritance is
covered, not a substitute for that missing contract. The packaged compiler remains
494dede84; source support here does not imply bundle or end-to-end AOT qualification.

A general binder correction also prevents untargeted property/event annotations from
being copied to backing fields. Explicit field targets still bind to those fields;
Raven field-only storage retains ordinary untargeted field annotations. This is a
shared .NET behavior correction, independently covered by AttributeUsageTests.

Validation: the native attribute probe round-trips source types, interfaces, enums,
fields, properties/accessors, constructors/functions and parameters, repeated/fixed/
null/enum/named values and a co-owned Attribute hierarchy. Negative wider-constant
and return-target cases verify diagnostics and unchanged output streams. The 21
ordinary AttributeUsage tests pass. AOT metadata-authored retention is independently
qualified by neoCLR 6ae823c0 on macOS ARM64 and Windows x64; this source emission
slice has not replaced that fixture with a compiler-produced AOT consumer.


## neoCLR named attribute data (2026-10-10)

The source runtime's CustomAttributeData.GetNamedArguments and
CustomAttributeNamedArgument (MemberName, IsField, TypedValue) use ordinary class,
getter and sequence signatures. No compiler behavior or Runtime Contract option is
added. Native metadata remains authoritative; neoCLR's temporary CLI reference
bridge declares the same signatures and validates the private snapshot array layout.
The shared interpreter/AOT recipe creates traced descriptors without calling attribute
constructors or setters. Raven owns ordinary compilation, neoCLR owns metadata,
snapshot ABI and compatibility. Source-native library compilation supplies the new
API; the independent legacy generated CLI snapshot stays fixed-only until its
ArrayReflection project-input regeneration blocker is resolved. Fixed-only libraries
continue working; named data requires the matching newer library and otherwise faults.

neoCLR's expanded source-library consumer passes macOS ARM64 native/interpreter
values, nulls, field/property kinds and snapshot-copy isolation. Its compiler bundle
remains 494dede84; the reference bridge is built with dfaa76145. This is not validation
of compiler-produced AOT annotations or expanded Windows named-data execution.
MemberInfo resolution and broader named constant categories remain .NET parity gaps.


## Source-owned AttributeUsage declarations (2026-10-10)

neoCLR's source runtime now defines AttributeTargets and AttributeUsageAttribute
alongside Attribute. AttributeUsage describes its own Class-only policy. This exposed
a general binding cycle: validation queried GetAttributes on the same declaration.
The shared compiler now separates bound data from usage validation; policy lookup
reads only internal bound entries. Public symbol queries remain validated. This does
not hard-code an AttributeUsage policy or skip diagnostics on recursive declarations.
Ordinary .NET and native source share the fix, with query-order, invalid-target,
repeatable-attribute and mutual-reference regressions.

No Runtime Contract setting or CLI representation changes. Native enum identity,
constructor references and named values remain native metadata; the temporary CLI
reference mirrors the .NET flag constants, constructor, ValidOn and Boolean options.
Raven owns binding/emission, neoCLR owns runtime declarations and metadata decoding.
Separate-library concrete Attribute inheritance and compiler-produced AOT attribute
inspection remain independent gates. This slice compiles the complete source-native
Runtime/Data/Networking/Web libraries; its runtime consumer checks usage defaults
and imported policy diagnostics. The tested compiler is the matching source build,
not the older 494dede84 bundle.


### neoCLR source usage bootstrap (2026-10-10)

The neoCLR development source library now owns System.AttributeTargets and
System.AttributeUsageAttribute. Its bootstrap preparation removes the duplicate
CLI scaffolds and their type-level usage annotations; its ownership manifest checks
single ownership. No Runtime Contract option or ordinary .NET lookup changes.
This is a temporary metadata-only preparation step owned by neoCLR, replaced when
the primitive CLI core is removed. Full API reference declarations remain intact.
Raven 0f09c350a provides bound-data-before-policy validation, including self-described
AttributeUsage, with 24 focused .NET tests. The neoCLR usage consumer checks flags,
defaults and imported RAV0502 rejection with macOS native/interpreter execution;
Windows qualification is pending. External Attribute bases, inherited guest queries
and discovery remain open. See neoCLR docs/experiments/attribute-usage/README.md.

### Fieldless external base integration (2026-10-10)

The neoCLR native capability now admits explicitly scoped, public, nongeneric,
top-level external class hierarchies with no instance fields, interfaces or extra
virtual slots. This enables user attributes to derive from source-built
`System.Attribute` in System.Runtime. The compiler validates imported symbol facts,
declares the metadata writer's fieldless-base contract and emits a direct imported
base-constructor call. Protected constructors are admitted only for this contract;
normal .NET emission remains unchanged and no Runtime Contract setting is added.

The temporary CLI projection records a normal scoped extends reference and base
`.ctor` call; native metadata records scoped base bindings. There is no synthetic
attribute carrier or name-based base fallback. General external storage/virtual
inheritance remains unsupported, owned by the native metadata/backend workstream.
Replace the bounded host layout declaration with general native hierarchy/layout
validation as that backend matures. A matched metadata library is required.

Validation: host metadata authoring checks actual CLR execution, native snapshots,
and rejected missing/double base initialization or protected allocation. A source
Raven attribute derived from the runtime library returns 42 in interpreter and
macOS ARM64 AOT. Compiler capability tests check explicit opt-in and reject a
non-fieldless external base. Windows qualification is separate.

### Canonical namespace type lookup (2026-10-10)

Attributed test registration exposed inconsistent base-type selection when the
bootstrap and native runtime both describe System.Attribute. Qualified/early
namespace lookup now canonicalizes imported definitions through the same metadata
lookup used by ordinary imports; source declarations keep precedence. The existing
assembly-affinity/explicit-contract selection policy is unchanged. This is a shared
lookup consistency fix, not a new neoCLR-only name mapping or Runtime Contract
option. The CLI/native base encodings and their bounded layout limits are unchanged.

Both qualified and imported spellings, both reference orders, and 83 focused
namespace/lookup/capability/AttributeUsage tests pass. The repository test-discovery
consumer additionally qualifies native source binding with typed registration
adapters and rejected generic/instance/parameter/result signatures.
