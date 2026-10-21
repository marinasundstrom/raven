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


### Shared friend-assembly checker; native integration pending (2026-10-10)

Raven recognizes source and CLI InternalsVisibleTo declarations in the shared
accessibility checker. This general compiler fix belongs on main. The existing
CLI assembly attribute is retained without rewriting types or members as public.
Runtime Contract configuration, attribute syntax and emitted call shapes are
unchanged. Separate-library .NET tests cover allowed and denied access, protected
combinations, one-way/non-transitive grants, public-signature restrictions and
full-key identity policy. See [the contract](friend-assemblies.md).

Native intent is the same assembly-scoped grant. The native provider still lacks
assembly attribute retention; metadata import admission and neoCLR runtime access
checks also need matching enforcement. AOT must validate the grant before lowering
static calls without running attribute constructors. Do not treat successful shared
compiler tests as native support, or hide the gap by making helpers public. Raven
owns binding and native emission; neoCLR owns metadata preservation/import and
resolved access checks. Native assembly-level metadata replaces the temporary CLI
attribute transport once those layers are qualified together. No information is
silently dropped and then treated as permission: absent grants deny friend access.

### Int32 attribute flag constants (2026-10-10)

Shared attribute constant folding preserves declared Int32 storage for bitwise
compositions, including Int32-backed enum flags. This is a semantic metadata fix
for all targets; no Runtime Contract option is added or changed. Native attribute
emission can pass the typed value directly to the strict metadata writer. The
temporary CLI projection keeps enum identity and the same 32-bit payload; native
metadata replaces that projection without changing the contract. Other numeric
widths and richer attribute payloads remain governed by their existing support.
Focused AttributeUsage semantic tests cover OR, AND and XOR storage and policy.
neoCLR's ObsoleteAttribute is the integration consumer.

## Shared override lookup for handler pipelines (2026-10-10)

An unqualified forwarding overload such as `Send(request, token)` previously saw
both a derived override and its abstract base declaration, reporting ambiguity.
Shared binder enumeration now suppresses the overridden chain by semantic identity,
retaining unrelated inherited overloads including substituted generic bases. Explicit
self lookup already selected the override. This is general .NET-compatible lookup,
with no Runtime Contract configuration change and no native-only encoding. Focused
semantic and .NET execution tests cover the selection; neoCLR handler pipelines are
an additional native consumer. Native metadata still retains declarations and override
relationships; no metadata or code generation replacement is needed for this fix.

## Async base receivers (2026-10-10)

The shared async lowerer now retains a base-dispatch marker around the hoisted
original instance receiver. Previously an async override could pass its state
machine to a base method; a normal .NET regression returned 1 instead of 42.
CLI emission loads the hoisted receiver and retains direct `call` dispatch.
Runtime Contract configuration is unchanged. This is a shared compiler fix, not
a temporary native mapping; the native backend must consume the same explicit
receiver while preserving direct dispatch. No metadata replacement is needed.
Focused CLR execution tests cover completed and suspending awaits with private
base storage and an overridden method. NeoCLR pipeline qualification remains
on its integration branch.


## Runtime scheduler projection experiment (2026-10-11)

neoCLR's source-included internal Scheduler prototype uses an authored InternalCall
`neoCLR.Runtime.SchedulerSubmit(fn<Void>) -> Int32` to acknowledge deferred runtime
admission. Its managed facade discards the zero acknowledgment; Task/Promise remain
managed. This uses existing Raven declaration/delegate emission and generated async
state machines. Runtime Contract configuration, semantic-model rules and lowering
are unchanged; ordinary .NET behavior remains the default. No compiler feature is
added on this shared line by this documentation update.

The Int32 acknowledgment and unit-callback shape are temporary bridge restrictions,
not the eventual native async ABI. neoCLR owns the exact binding, bounded rooted
admission, dispatch and teardown. AOT/JIT explicitly opt in with `--bind-scheduler`
plus `--compile-system` and `--native-gc`. A later native metadata/backend contract
must support runnable/resumption capabilities independently of managed Task layout
or a callback-only representation. Runtime suspension is not implemented here.

neoCLR reports nested submission, pending/completed awaits and terminal callback
faults passing in interpreter/AOT/JIT on macOS ARM64 using its matching integration
bundle. See `docs/experiments/scheduler-projection/validation.json` in neoCLR for
compiler/bundle hashes and executions. This is reported integration evidence, not
validation of arbitrary Raven-main binaries or Windows support; the compiler bundle
was not rebuilt from this docs-only shared-main change.


### Captured invocation destination extension (2026-10-11)

neoCLR's development scheduler projection adds exact InternalCall forms
`neoCLR.Runtime.SchedulerCapture() -> Int64` and
`neoCLR.Runtime.SchedulerSubmitTo(Int64, fn<Void>) -> Int32`. Runtime Contract settings,
Raven semantic analysis and state-machine emission are unchanged; ordinary .NET remains
the default. The native backend opts in through `--bind-scheduler`, `--compile-system`
and `--native-gc`. Its development JIT CLI supplies that binding set.

The native semantic intent is preserving a continuation's invocation placement identity.
The source-included internal awaitable captures the opaque token per registration,
retains the callback in managed storage, then submits to that destination at completion.
The runtime checks identity before retaining the callback; foreign/expired identities
fault instead of silently targeting the completing invocation. The token is non-owning,
not persistent or a security capability. No cross-invocation/thread routing, custom
scheduler capture, ExecutionContext flow or public Task API migration is implemented.

neoCLR owns token generation, scope lifetime, dispatch validation and GC roots. Raven
emits existing Int64/InternalCall/callable forms. The integer and unit-callback encoding
are temporary bridge restrictions; native metadata/backend work must represent placement
and lifetime without fixing these forms as language semantics. The neoCLR
`docs/experiments/scheduler-continuations` experiment records exact binary/bundle hashes,
all-mode consumers and native/interpreter rejection contracts. Reported execution is
macOS ARM64; Windows is not newly qualified. No Raven compiler binary changes in this
companion documentation slice and Raven main alone does not prove native support.


### Runtime-owned continuation registration (2026-10-11)

The neoCLR internal awaitable now emits existing InternalCall forms for
SchedulerRegister(Int64 destination, fn<Void>) -> Int64, SchedulerActivate(Int64) ->
Boolean and SchedulerCancel(Int64) -> Boolean. The scheduler backend opt-in is unchanged;
Runtime Contract options, Raven semantics/lowering and .NET behavior are unchanged.
The runtime owns dormant callback roots and reserved capacity. Activation transfers
ownership to runnable dispatch once; cancellation releases a dormant root once and
cannot retract admitted work. Unknown/foreign/expired handles return false on transition.
Dormant registrations do not keep an invocation alive; teardown abandons their roots.

Task remains the managed operation/state abstraction and Promise the producer-side
TaskCompletionSource-like completion authority. Registration cancellation does not
cancel that operation. The temporary Int64 token, unit callback and 64-slot prototype
bound are not permanent language contracts. Native metadata/codegen should express
registration ownership for callbacks or saved runtime activations. Cross-invocation
routing, race semantics and automatic abandoned-operation cleanup remain runtime work.

neoCLR reports six interpreter/JIT/AOT consumer runs and focused native/interpreter
lifecycle contracts passing on macOS ARM64, with evidence in its scheduler-continuations
experiment. Windows is not newly qualified. This is companion documentation only;
no Raven compiler binary or public Task API is changed.

### Private interpreter frame-yield probe (2026-10-11)

The source-included RuntimeAsyncYield experiment declares exact InternalCall
`neoCLR.Runtime.RuntimeAsyncYield()` with unit (or CLI no-result) return. neoCLR
binds it as TaskDispatch and retains ordinary interpreter frames during at most one
eligible queued callback. No-result encoding does not push a unit at resumption.
Raven emits an ordinary call; no Runtime Contract setting, async-method semantic
change or automatic await remapping is added. This private signature is interpreter
only, not a supported JIT/AOT intrinsic or stable native ABI.

neoCLR owns frame/root lifetime, budgets and dispatch. Raven owns future explicit
target capability checks and await intent emission. Its existing CLR runtime-async
AsyncHelpers/metadata route remains separate and unchanged. The temporary CLI call
cannot describe operation parking, saved activation layout, liveness or logical
await links. Native metadata/codegen must supply an explicit target contract for
those before compiler integration; do not emulate CLR helpers merely to enable it.
Validation uses the existing compiler bundle with a source-local declaration and
focused neoCLR contracts. Public Task/Promise and compiler-generated state machines
remain unchanged. See neoCLR `docs/experiments/runtime-async/README.md` for evidence
and limitations; no native suspension support is implied by compiler hook availability.

### Operation-wait proof of concept (2026-10-11)

neoCLR now experiments with source-local InternalCalls `RuntimeAsyncCreate() -> Int64`,
`RuntimeAsyncDelay(Int32) -> Int64`, `RuntimeAsyncComplete(Int64, Int32) -> Boolean`,
`RuntimeAsyncCancel(Int64) -> Boolean` and `RuntimeAsyncAwait(Int64) -> Int32`, all
under `neoCLR.Runtime` and classified as TaskDispatch. Raven emits ordinary calls;
no Runtime Contract option or automatic `await` mapping has been added. These are
private interpreter experiments, not public Task APIs or native ABI commitments.

The runtime owns an execution-context operation table and retained caller frames.
A pending wait returns to the host, releases managed graph access, and parks using
the durable scheduler wake latch plus a bounded cancellation/deadline timeout.
Queued callbacks may complete the operation, including after nested timer waits.
Completion/registration checks serialize on the same dispatcher, so this POC has
no cross-thread producer race. A future host completion ABI still needs atomic
publication and wakeup qualification. Handles are globally unique, context-local,
one-consumption tokens; they do not redefine Task's multiple-await contract.

Temporary limits: Int32 results, 64 unconsumed operations, LIFO retained chains,
no independent activation scheduling, no worker use, no legacy I/O callback pumping
inside the wait, and no JIT/AOT implementation. Cancellation faults the invocation;
unused operations are discarded on teardown. Native metadata/codegen must replace
the integer bridge with explicit operation/activation ownership, result rooting,
liveness and logical await links. Raven must use an explicit target capability
contract before mapping language await. Existing CLR AsyncHelpers lowering and
compiler-generated state machines remain unchanged.

Evidence: neoCLR `docs/experiments/runtime-async/README.md`, `Await.rvn`, the focused
runtime operation tests and consumer report. A bundled-compiler gap was exposed by
initializing the Operation property directly with the locally declared attributed
intrinsic: binding recurses to stack overflow, also with an explicit property type.
The consumer instead passes the handle to a constructor. This gap is deferred for
an independently minimized compiler regression; it is not a language restriction.

### Detached activations and default-await goal (2026-10-11)

The existing private operation-wait signatures now permit the neoCLR interpreter to
detach parked callback frames and resume ready parents independently. No CLI encoding,
Runtime Contract option or Raven lowering changes. GC roots, frame permits and array
charges remain runtime-owned while detached. A source-included Raven parent/callback
handshake completes without generated state machines; evidence is in neoCLR's
`docs/experiments/runtime-async/detached-validation.json`.

The author now directs runtime await as Raven's default, with demonstrated stability
and improvement. This is a goal, not a switched default. The next compiler contract
must explicitly describe supported neoCLR await lowering and managed result/fault/
cancellation projection, preserve state-machine fallback and keep ordinary CLR
AsyncHelpers semantics unchanged. Qualification requires ordinary async consumers,
root/lifetime stress and equivalent state-machine comparisons; JIT/AOT and Windows
remain unqualified for this POC. Native activation metadata, source/await links and
result rooting replace these private integer-only calls before production adoption.

### Async-body invocation bridge (2026-10-11)

Private interpreter service `neoCLR.Runtime.RuntimeAsyncStart(fn<Int32>) -> Int64`
(ordinary InternalCall, TaskDispatch capability) runs a checked body until completion
or its first pending wait, then returns the body operation handle. Runtime-owned
return completion cannot be overwritten by manual RuntimeAsyncComplete. A cancelled
operation retains terminal authority over late body return. Native/JIT support is
absent; Runtime Contract configuration and all Raven lowering/defaults are unchanged.

The source-local delegate is a temporary thunk representation. It loses general
result types, argument/receiver metadata, logical async method identity and the
public Task lifetime/outcome contract. Faults remain invocation-terminal. Future
compiler integration must explicitly lower body invocation plus Task construction
and await outcome projection; skipping generated state machines alone is insufficient.
Keep ordinary CLR AsyncHelpers/async metadata behavior separate. Evidence is neoCLR
`docs/experiments/runtime-async/start-validation.json`; ordinary Raven functions use
these private calls, not newly supported compiler-lowered async methods.

### Retained outcome and release bridge (2026-10-11)

Existing RuntimeAsyncAwait(Int64) now observes a retained result; it no longer
consumes the handle. Multiple parked activations may await the same operation.
New exact InternalCall `neoCLR.Runtime.RuntimeAsyncRelease(Int64) -> Boolean`
requires TaskDispatch and releases only terminal operations not used by any active
or parked waiter/body. Unknown/pending/in-use handles return false; reads after a
successful release fault. Runtime Contract options and Raven emission are unchanged.

This corrects a temporary single-consumption limitation toward .NET Task observation
semantics. Explicit release is private bridge ownership, not a public Task.Dispose
requirement: GC-linked managed ownership must replace it before production projection.
The 64-handle budget includes retained outcomes until release/teardown. Int32 results,
invocation-terminal cancellation/faults, interpreter-only execution and missing
compiler await integration remain limitations. neoCLR owns storage and release guards;
future Raven target mapping must project Task lifetimes/outcomes explicitly. Evidence:
neoCLR `docs/experiments/runtime-async/shared-results-validation.json`.

### Ordinary await adapter and readiness bridge (2026-10-11)

Private InternalCall `neoCLR.Runtime.RuntimeAsyncIsCompleted(Int64) -> Boolean`
(TaskDispatch) observes terminal readiness without consuming an outcome; stale/foreign
handles fault. Observing an expired timer fixes its terminal success. No Runtime
Contract options, Raven lowering or defaults change. The source-local Operation
awaiter returns itself and adapts OnCompleted by scheduling a callback that parks on
RuntimeAsyncAwait, then invokes the generated continuation.

The Raven consumer uses async Main and ordinary await over both the custom operation
and managed Task. Existing state machines remain emitted. Pending delivery, completed
fast path, repeated observation and completion between readiness and registration
pass on the interpreter with the existing compiler bundle. Its completion-only
adapter supplies IsCancelled=false for the current target protocol; cancellation,
managed lifetime, general result types, native/JIT and compiler-native await remain
open. The callback adapter is a temporary CLI compatibility path with allocation
and submission costs, not a selected native activation ABI.

The older bundled compiler required separate AwaitServices.rvn declarations to avoid
attribute-binding recursion from the readiness property. The shared compiler now
caches the compilation-unit scope before member registration, allowing property
getters/initializers to inspect attributed functions in later namespaces without
re-entering declaration binding. This is a general .NET/neoCLR binder correction;
Runtime Contract configuration, emitted signatures and await lowering are unchanged.
The .NET-target regression covers getters and initializers with obsolete diagnostics.
The native compiler remains on `codex/source-object-metadata-resolution`, while this
fix is on shared `main`; the native compiler and bundled consumer have not yet been
qualified with it. Keep the split source-file workaround until that integration is
validated. Shared main's current driver does not provide the native `neoclr` command.
See neoCLR's
`docs/experiments/runtime-async/language-await-validation.json`. No public Task or
awaiter type is added; author clarification favors the protocol over copying type
structure, retaining the self-awaiter until a separate policy wrapper is justified.

### Compiler branch ownership (2026-10-11)

The author directs keeping shared `main` and the neoCLR compiler line separate.
The native line is currently `codex/source-object-metadata-resolution`; its native
metadata/backend and `neoclr` driver command are not all on main. Carry general fixes
between lines deliberately, qualifying each target. This supersedes earlier plans
that treated a full merge into main as the default integration destination.
`codex/internals-visible-to-main` is already an ancestor of main and has no unique
work. Worktree directory names are historical: `raven-internals-visible-to` currently
hosts main, while `Raven` hosts the native line. No branches were merged or deleted.

The property/attribute recursion fix on main passes 74 focused attribute, obsolete,
namespace, import, alias, multi-file and file-scoped codegen tests on .NET 11.
Native bundle qualification remains separate and pending.

## Await in synchronous for loops (2026-10-11)

The shared compiler normalizes synchronous iteration before async capture analysis.
Array/range positions and iterators become bound locals; generated state machines
can retain them across suspension. Value-type receiver calls address saved fields,
and branches leaving protected regions use CLI `leave`. Finally blocks run only
on a source-scope exit, not on a suspended `MoveNext` return. This repairs ordinary
.NET execution and removes the RAV2712 blanket restriction under cancellation
propagation; awaiter-protocol and ref-safety checks are unchanged.

No new Runtime Contract option or metadata encoding is introduced. Ordinary .NET
uses protected cleanup. The native compiler line must select its existing explicit
scope-disposal contract and independently qualify state-machine and retained-body
execution; it remains separate from main. Native metadata/codegen should represent
iteration lifetime and suspension directly rather than relying on this CLI bridge.

Validation on shared main covers pending awaits in reference/value-type iterators,
arrays and ranges, cancellation/faults, early and labeled outer-loop exits, and
cleanup before a subsequent await. Native bundle revisions and runtime evidence
are recorded separately in neoCLR's integration documentation.

Hoisted `use` resources also retain source-scope ownership across branches. Leaving
such a scope disposes and clears its saved field before transferring control;
terminal completion/cancellation cleanup then observes an empty field. Focused
.NET regressions cover labeled `break`/`continue` with an additional scoped resource
inside the loop, as well as the loop iterator itself.

Mixed-loop exits use the same branch representation: enclosing `while` and `loop`
statements are normalized before async capture as well, so `continue outer` cannot
replace a hoisted scoped resource without first disposing it. A regression with
`use` inside an awaiting `for` and an enclosing labeled `while` verifies this on
ordinary .NET; native qualification must also cover mixed-loop iterator disposal.

### Shared ad-hoc carrier widening (2026-10-15)

General Raven conversion classification now admits exact alternative inclusion
between ordinary .NET `System.Union` carriers, including reordered alternatives.
The semantic conversion is implicit and participates in ordinary overload ranking;
strict subsets win and incomparable overlaps are ambiguous. Named unions remain
nominal. CLI emission saves the source once, probes its exact typed extraction
members and constructs the destination carrier for the active payload. A default
carrier received from a .NET caller stays inactive. This introduces at most one
probe per distinct source alternative and retains the existing carrier's storage
costs; it is not a claim of allocation-free .NET unions.

This shared compiler fix requires no Runtime Contract change and does not change
native structural signatures. The native target continues using its own explicit
structural conversion contract and backend. Payload-transforming conversions such
as `int | string` to `long | string` remain unsupported by the CLI widening rule.
Validation: 85 focused semantic/overload/emission tests on modern .NET, including
open generic transport, payload reference identity, single evaluation and a .NET
caller supplying an inactive default. Older .NET targets are not execution-qualified.

### Independent main expression qualification (2026-10-19)

neoCLR's portable-planner correction in Raven `bab04d508` motivated independent
ordinary .NET tests on main, whose ExpressionGenerator does not use that planner.
Main already preserves the observed behavior: positive/negative pattern variables
through logical values, short-circuit evaluation, and Result propagation inside
string equality after completed and pending awaits. Success invokes the enclosing
call once; an error returns before that call. Six baseline tests and fourteen
focused tests including five new cases pass on modern .NET 11. No main emitter
fix was necessary; older .NET targets are not execution-qualified by this run.

The tests remain independent of neoCLR imports and Runtime Contract configuration.
There is no semantic-model, CLI metadata or native backend change. The portable
planner and native verification/execution remain owned by the neoCLR line; no
branch merge or target-specific implementation was carried into main. These
results do not qualify the larger native JSON fixture, which has separate
specialization and interpreter failures recorded in neoCLR's integration evidence.

A follow-up JSON failure on the native line is reduced to a nullable Result
payload matched with `Ok(let value)`. Main passes three nullable-payload cases
(null, string and boxed integer), including narrower string-pattern rejection.
The native portable planner's payload binding inserted an unnecessary null-rejecting
type test; its correction and native qualification remain on codex/neoclr. Main's
new regression fixture contains no target-specific imports or Runtime Contract.

### General positional declaration semantics (2026-10-21)

Raven main owns irrefutable named declarations, inherited Deconstruct lookup and
qualified readonly static value-pattern binding. These do not depend on Runtime
Contract configuration or target type-name mappings. Ordinary .NET emission is
validated first. Record-struct typed/Object equality synthesis is also corrected
on main. The neoCLR line must carry the general change deliberately and validate
its native pattern backend separately; main support alone is not integration
proof. No additional CLI bridge encoding is introduced.

### Construction payload capture traversal (2026-10-21)

A shared bound-tree traversal fix visits ordinary construction arguments,
initializer entries and union-case payloads when discovering lambda captures.
This is a general Raven fix: outer locals inside `Some<object>(value)` must be
captured on .NET as well as neoCLR. The native emitter's undeclared-local error
exposed the missing traversal; no target-specific capture exception is added.
Runtime Contract configuration, CLI bridge representation, metadata and closure
ABI remain unchanged. The existing native captured-value support limits still
apply. Focused .NET tests check semantic capture identity and emitted execution;
the neoCLR DI activation consumer independently validates the native branch carry.
Native metadata/backend work does not replace this shared semantic requirement.


### Pointer conversion classification (2026-10-21)

General Raven now classifies explicit pointer-to-pointer reinterpretation and implicit
pointer-to-void erasure. The ordinary .NET emitter uses its existing `conv.u` path;
35 pointer semantic/codegen tests pass, including an address-preserving round trip.
No Runtime Contract configuration changes. Native lowering must independently
preserve the target pointer signature; this shared binder fix alone does not provide
native emission or grant ownership, bounds, alignment or lifetime validity.
