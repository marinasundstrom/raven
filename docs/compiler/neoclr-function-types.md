# neoCLR Function migration integration

Development integration, 2026-09-28, on the isolated `neoclr` branch. neoCLR has
selected structural Function shapes and callable objects to replace its nominal
delegate feature. Named function types may follow later; alias versus nominal
identity is undecided. This is not a change to Raven's ordinary CLR delegate model.

The native neoCLR foundation accepts structural signatures, checked binding and
Invoke without nominal declarations. Raven callable import and library migration
are still incomplete; existing Func/CLI delegate metadata remains the transport
baseline. No new function syntax or Runtime Contract setting is claimed here.

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
