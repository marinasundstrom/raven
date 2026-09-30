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
format-5 application bytes from Raven's public operations. Top-level source functions
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
