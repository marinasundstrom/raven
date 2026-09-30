# neoCLR Function migration integration

Current status (2026-09-30): native Function/structural-type work is deferred on
feature branches in both repositories. Raven `codex/neoclr-structural-types` carries
the experimental integration notes and future compiler work; neoCLR uses `feature/function-types`.
The notes below describe that experimental history, not supported main behavior.
Ordinary Raven function syntax and .NET delegates remain on main.

Development integration, 2026-09-28, on the isolated `neoclr` branch. neoCLR has
selected structural Function shapes and callable objects to replace its nominal
delegate feature. Named function types may follow later; alias versus nominal
identity is undecided. This is not a change to Raven's ordinary CLR delegate model.

The native neoCLR foundation accepts structural signatures, checked binding and
Invoke without nominal declarations. Raven callable imports and library signatures now use structural neoCLR shapes;
Func/Action CLI metadata remains a compiler transport detail. No new function syntax or Runtime Contract setting is claimed here.

The context-owned typeof contract still names System.Introspection.TypeInfo in
NeoCLR.CoreProbe and System.Runtime.RuntimeContext. TypeInfo now exposes DisplayName
and IsNominalType and no longer inherits MemberInfo. NominalTypeInfo extends both
TypeInfo and MemberInfo and owns FullName/Namespace; declaration names, module,
tokens and attributes come through MemberInfo. Semantic typeof results remain
TypeInfo, so clients must test/cast to NominalTypeInfo to use declaration metadata.
Arrays use a structural descriptor; current declared tuple and union families
remain nominal. Matching reference, source library, importer and runtime are required.

neoCLR's descriptor consumer compiles with the existing target compiler, verifies
and executes: nominal interface tests agree with IsNominalType, structural arrays
are not MemberInfo, and equality, object discovery and collection work. Native
shape tests additionally cover generic functions, receivers, lifetime checks and
no-result invocation. These are target observations, not .NET Framework or
NanoFramework validation. API documentation and full callable migration remain
tracked in neoCLR's `docs/function-types.md` and runtime/language tracker.

A general compiler candidate remains deferred: with revision `5dde32fbf`, an
expression-bodied Module getter before a later Handle field in RuntimeNominalTypeInfo
emitted a null result instead of the declared service call. Moving the getter after
storage and other methods produced the expected body, and the consumer passed.
The current library uses the validated ordering. Root cause is not diagnosed;
reduce the case and validate a general fix independently on main-based CLI fixtures
before integrating it. No compiler implementation fix is included in this checkpoint.

Author clarification: structural types can have members and extension members
without nominal declaration names. TypeInfo member queries remain common. RavenDoc
structural family pages (Array, Tuple, Union, Intersection, Function) should describe
shape signatures and member/extension contracts without inventing nominal metadata.
This is a documentation direction, not an implemented general renderer.

## Unit-returning function transport

With TargetCoreAssemblyName set to NeoCLR.CoreProbe, CreateFunctionTypeSymbol
uses Func with the selected inhabited unit as its return argument for unit/void
source function types. This makes `() -> ()` use the same transport as a generic
`() -> T` instantiated with unit. Other targets continue to select Action for
unit/void functions. The neoCLR importer maps the transport signature to structural
`fn<...>` metadata and adapts a no-result target method to an inhabited unit result.
No new Runtime Contract option or source syntax is introduced. Existing function
notation, hover/display and grammar apply; this is an isolated target policy,
not an ordinary CLR compiler change or a named function type feature.

Validation: 62 focused function syntax/diagnostic/inference tests passed before
the change; 65 pass afterward, including three target-selection cases. neoCLR's
Tasks and Array source slices compile and import with structural callback shapes.
Full library and executable callback migration validation remains in neoCLR.

## Generic construction binding

A general compiler bug independently reproduced on ordinary .NET silently omitted
`List<() -> ()>()` constructor expressions: the expression-side type binder lacked
FunctionTypeSyntax handling. The standalone fix is `e316703ca` on main-based
`fix/function-type-construction`, integrated here as `6b5418e57`. It binds the
signature through the normal type binder; constructor operations and runtime
initialization now work, and invalid result types produce diagnostics. Fifteen
focused ordinary .NET tests pass (three function-shape cases failed before the fix).
This is independent of Runtime Contract configuration and the unit transport policy.
The standalone fix was fast-forwarded into main after confirming the main
worktree was clean. This experimental branch must not be merged wholesale. Other target frameworks have not been rerun for this fix.

## Structural runtime replacement

neoCLR now rejects nominal delegate declarations, legacy binding encodings and
serialized Delegate representations. All library callback APIs use structural
shapes; compiler Func/Action carriers stay at the import boundary. Comparer adapters
are FunctionComparer/FunctionEqualityComparer. Existing function notation and Runtime
Contract configuration remain unchanged. The common/nominal descriptor split and
Function extension syntax are unchanged from the verified checkpoint above.

Function values compare shape/closed target/receiver and preserve shared captures.
Native null Invoke reports NullReference; constructors explicitly initialize callback
fields. Ordinary Object conversion, synthetic Invoke descriptors and general
structural member enumeration remain bounded follow-up work. This does not select
named function identity or change .NET target delegate behavior. Matching updated
compiler, reference, importer and runtime artifacts are required; see neoCLR's
Function fixture for final validation and API snapshot evidence.

## Signature descriptors and OfType (2026-09-28)

The matching neoCLR reference adds TypeInfo.IsFunctionType and FunctionTypeInfo with
Parameters, ReturnType and InvokeMethod. No common function-info interface is
introduced. GetMethods also returns the synthesized public instance Invoke.
MemberInfo/ParameterInfo Module and MetadataToken and MethodInfo.DefinitionIndex
are optional; synthetic descriptors have no declaration metadata. This changes the
target library contract, not Raven typeof's static TypeInfo result or Runtime Contract
configuration. Rebuild matching references, bridge, runtime library and applications.

The target library also adds lazy OfType<U>() on Iterable<T>, with the source type
inferred from the receiver, supporting module.GetTypes().OfType<NominalTypeInfo>().
It imports through existing generic query bindings, type tests and Object casts.
The executable neoCLR consumer validates filtering, null/boxed values, descriptor
narrowing, order, deferred evaluation and disposal. No ordinary CLR, .NET Framework
or NanoFramework behavior change is included.

The existing deferred getter candidate was observed again: the concise
InvokeMethod getter calling GetMethods()[0] emitted a null result. An explicit
getter with a local RuntimeServices.TypeMethods result passes the target consumer.
This observation is not a diagnosed root cause or compiler fix. General correction
still requires an independent main-based reproduction and validation. Structural
inheritance, named function types, Function-to-Object conversion and dynamic
reflection invocation of synthesized Invoke remain neoCLR follow-up work.


## Function objects, target inspection and Object (2026-09-28)

The neoCLR target reference now projects a read-only Function: MethodInfo property
and ToString onto its CLI callback transport types. The importer maps these to
synthesized structural Function members, not nominal delegate declarations. Callable
comparisons lower to value equality. The property reports the closed bound method;
FunctionTypeInfo.InvokeMethod still reports the shape's Invoke contract. A common
FunctionInfo interface over methods and module functions is explicitly deferred.

Function shapes inherit Object while retaining structural signature identity.
Object upcasts, exact-shape casts back, GetType and virtual Equals/GetHashCode/ToString
work through the native Function representation. Equal bindings share target and
receiver identity; separate creation identity remains observable through
ReferenceEquals. Source-qualified target names survive lowering for diagnostic
ToString output, together with the closed signature. Other structural families do
not acquire Object inheritance from this change.

This supersedes the earlier Function-to-Object limitation. Dynamic invocation of
synthetic descriptors remains unsupported; use typed calls and property access.
Ownerless module target attributes and dynamic reflection invocation remain bounded
by the existing type-based services. FunctionInfo, named nominal Function types,
common callable base/interfaces and structural introspection factories remain
future design. Runtime Contract configuration and ordinary CLR/.NET Framework/
NanoFramework behavior are unchanged; the compiler implementation is unchanged in
this slice. Matching bridge, reference and runtime artifacts are required.

Validation is recorded in neoCLR's executable Function consumers and focused native
Function/Object/reflection tests; the target documentation lives in its Function
feature page and manual API reference.


### Completion follow-up (2026-09-29)

The neoCLR importer binds ordinary instance methods directly when no return adapter
is needed, so Function reports the original member instead of a generated forwarding
method. Necessary adapters without source declarations expose absent optional
module/token metadata. This changes only the isolated target bridge/runtime contract,
not Raven's general CLI emission. The source consumer checks original member identity,
receiver-sensitive equality, unit target inspection and capture retention through
Object views. RavenDoc's existing authored-page navigation now exposes the target's
structural families; no general publisher or compiler change is required.


### Independent neoCLR main backport (2026-09-30)

The author selected callback function type syntax and lazy OfType for backport to
neoCLR main. This source/library subset retains main's nominal Func/delegate ABI,
artifact formats, comparer names and TypeInfo/MemberInfo contracts. Raven source
uses arrow-shaped callback annotations; matching compilation still emits the
existing Func metadata consumed by main's bridge. OfType adds a two-generic-argument
query binding and a Raven iterator using existing Object type tests and casts.
Runtime Contract configuration and general CLI compiler behavior are unchanged.

The structural Function runtime and nominal/structural descriptor split remain on
the neoCLR feature branch. Validation uses regenerated main library/reference
artifacts, query/delegate/Task native checks, compiled callback/async consumers and
the OfType query suite, as recorded in neoCLR's query API documentation.

## Main synchronization after Self integration

This feature branch includes shared main's target-gated Self. Production compiler
code currently matches main: the earlier inhabited unit-result transport also
serves neoCLR's nominal delegate ABI and is therefore shared. Structural Function
identity, assignability and native metadata work remain deferred here. The detailed
migration history above is experimental evidence, not a claim of main support.
