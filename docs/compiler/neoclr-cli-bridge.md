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
