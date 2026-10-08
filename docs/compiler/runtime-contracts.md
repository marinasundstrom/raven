# Runtime Contracts

The planned general model is a **runtime/platform contract** governing semantic
rules, available types, representations, supported features, compatible symbol
sources, and one or more code generators. See the
[selection and compatibility design](https://github.com/marinasundstrom/raven/blob/main/docs/compiler/architecture/runtime-platform-contract-design.md).
The CLI-oriented options documented below are existing implementation mechanisms;
they do not require every future symbol source to use metadata or CLI assemblies.

The neoCLR bridge is a temporary transport, not the native platform specification.
See the [bridge behavior and replacement inventory](https://github.com/marinasundstrom/raven/blob/main/docs/compiler/neoclr-cli-bridge.md) for current
encodings, limitations, semantic distinctions and branch-qualified exploratory evidence.

## CompilationOptions presets and planned configuration

The public configuration type remains `CompilationOptions`. The agreed direction
is TargetPlatform (coherent loader/codegen selection), LangVersion (Raven source
version), Contract (platform mappings), and Features (requested optional features).
See the [API direction](https://github.com/marinasundstrom/raven/blob/main/docs/compiler/architecture/runtime-platform-contract-design.md#agreed-compilationoptions-api-direction)
for ownership and validation stages, including language settings shared with
parsing. These four settings are not yet implemented as a unified public API.

`CompilationOptions.DotNet` is implemented as an explicit-reference preset. It
selects no framework version and resolves no packages. The supplied references
provide the core identity for both binding and emission. See
[metadata import](metadata-import.md#net-compilation-preset) for examples,
compatibility with existing constructors, and validation limitations.

`CompilationOptions.NeoCLR` and `RavenTargetPlatform=NeoCLR` select the experimental
CLI bridge profile described below. This still uses CLI metadata and emission;
a native loader/backend and a complete capability matrix are not implemented.

Known metadata-core initialization failures now report RAVT004 through compilation
diagnostic collection and Emit, without writing output or manufacturing a partial
semantic environment. This fatal error cannot be suppressed or downgraded.
Syntax-only diagnostics remain available without target references. Direct semantic
queries still require successful setup; later metadata import failures are outside
this initial diagnostic boundary.

## Configuration validation before loading

The .NET runtime contract checks configuration-only contradictions before
compilation diagnostic collection or emission opens a metadata session:

- An explicitly selected emission core must have compatible explicit import
  settings. In discovered-core mode, its actual identity is checked after loading.
- A unit contract must provide an assembly and type name and select that same
  assembly as the explicit target core.
- A typeof contract must provide assembly, type-info interface and context names.

These checks report RAVT003 before a missing-core RAVT004 can obscure the
configuration error. For the neoCLR profile, profile consistency is checked first. Remaining checks run
in core-selection, unit, then typeof order and
return the first error. They require no reference I/O or imported symbols.
Diagnostic collection stops on the contradiction, and emission writes neither
output stream, including when precomputed diagnostics are supplied. These
configuration failures remain errors even if suppression or severity overrides
are requested. Syntax-only diagnostics remain independent of target configuration.

Successful configuration validation does not establish target availability or
contract compatibility: core discovery, unit type shape, and typeof provider
members are still checked using loaded symbols. Named import/core matching retains
its existing policy. Direct semantic queries have not acquired a new configuration
validation API; this early boundary applies to diagnostic collection and emission.

## Current target composition

`Targets.DotNetCompilationTarget` is the internal composition point for the
existing CLI implementation: it creates the semantic-data loader, selects a
`DotNetRuntimeContract` or `NeoClrCliRuntimeContract` from `TargetPlatform`, and
invokes the existing .NET code generator. Each
compilation constructs its target from its immutable options; macro-plugin
compilations use their own target when emitting. Diagnostics and target-core
compatibility checks still run before emission writes output.

The contract currently owns special-type metadata names, the preferred
`System.Runtime` assembly for those types, the runtime tuple family, configuration
validation, and post-load unit/typeof contract validation. It resolves typeof's
result type, context getter and resolver method as semantic symbols. The .NET
target selects and validates the emitted core identity from its metadata session.
Compilation retains semantic lookup, per-snapshot symbol caches, diagnostic
reporting and the compiler-owned Unit symbol. Other protocol mappings and feature
policies have not yet moved into this contract. Existing experimental tuple and task mappings are
preserved on this branch; this is not a separate neoCLR target implementation.

This is deliberately a concrete .NET composition, not a public provider registry.
The .NET target owns metadata-session and reflection-core state; Compilation
retains forwarding reflection APIs, and loading and codegen still depend on .NET
reflection. A future replaceable target must remove those
shared-layer dependencies and select a coherent loader/contract/codegen trio.
Independent component selection and cross-compilation are outside current scope.
The development goal is one shared compiler line on main for .NET and neoCLR.
The current neoclr branch is temporary integration work; experimental policies
need explicit ownership and validation, not permanent branch separation. Native
neoCLR loader/backend completion is not a prerequisite for integration. See the
[main integration plan](https://github.com/marinasundstrom/raven/blob/main/docs/compiler/architecture/neoclr-main-readiness.md).

## Context-owned typeof (experimental, 2026-09-17)

`CompilationOptions.WithRuntimeTypeOfContract(new RuntimeTypeOfContract(
assemblyName, typeInfoTypeName, contextTypeName))` opts language typeof into a
runtime-owned descriptive interface. Projects use RavenTypeOfAssemblyName,
RavenTypeOfInfoType and RavenTypeOfContextType. All three are required together.
The neoCLR POC selects Probe, System.Introspection.TypeInfo and
System.Runtime.RuntimeContext; these names are configuration, not binder policy.

The contract names public nongeneric top-level types in one assembly: an interface
and a class with public static Current returning that class, and a public instance
GetTypeInfoFromHandle(System.RuntimeTypeHandle) returning exactly the interface.
Source providers and referenced providers are resolved through compiler symbols,
without loading a runtime implementation into the compiler. Invalid/partial
configuration reports RAVT003 rather than silently selecting the host System.Type.

Binding and semantic-model type information report the interface. Ordinary
expression emission acquires Current, loads the type token and invokes the
resolver. The receiver precedes the handle on the CLI stack. Existing generic
type-token handling is retained. Default .NET projects still use System.Type's
static factory. Compiler-synthesized System.Type expressions and sizeof retain
their existing behavior. Option copies preserve the contract, and changes to it
prevent reuse of incompatible incremental semantic state.

No syntax, grammar or TextMate change is required; editor semantic requests see the
same bound result type. This slice covers executable typeof expressions, not
custom-attribute or expression-tree representation with alternative descriptors.
The target owns provider identity, equivalence, lifetime and implementation hiding.

Compared with the ordinary CLI/.NET Type.GetTypeFromHandle contract, the context
path makes the execution universe explicit and separates descriptive API shape
from executable capabilities. It costs a new target contract and coordinated
runtime/reference migration; it is not a drop-in binary compatibility change.
The generic mechanism is a deferred candidate for independent main-based validation;
neoCLR policy and fixtures must not be merged wholesale from the experiment branch.

Validation uses RuntimeTypeOfContractTests for semantic type, emitted interface
return shape and observable execution, with TypeOfExpression tests protecting the
default behavior (23 tests passing on .NET 11, including a referenced provider).
The neoCLR integration adds an actual source typeof(Date)
execution test and handle-equivalence checks. These results do not claim .NET
Framework or NanoFramework execution or full cross-context Emit composition.

## Storage constraint validation (2026-09-30)

Storage binding validates ordinary generic argument constraints recursively
through constructed arguments, containing types, arrays, nullable types, and
by-reference types. Failures use the existing constraint diagnostic and an
error type; no new metadata encoding or Runtime Contract option is introduced.
This fix was extracted from the parked intersection experiment without its
syntax, semantic symbols, or target-specific policy. Focused tests use ordinary
CLI contracts on modern .NET; they do not establish execution compatibility
with .NET Framework, NanoFramework, or neoCLR.

In the current CLI implementation, a Runtime Contract option describes a
compiler-facing requirement supplied by a target's CLI metadata. A target profile
selects contracts and reference assemblies; it does
not require a separate binder or emitter for each framework. These options are
independent of source syntax and are opt-in. Unconfigured compilations retain
Raven's normal .NET behavior.

| Requirement | Compiler option | Default |
| --- | --- | --- |
| Metadata universe | `MetadataImportOptions` | Reference-pack selection with host-assisted dependency discovery |
| Emitted core identity | `TargetCoreAssemblyName` | Normal .NET emission |
| Iteration protocol | `RuntimeIterationContract` | .NET interface/pattern iteration |
| Propagation carrier protocol | `RuntimePropagationContract` | Existing Raven carrier conventions and .NET exception behavior |
| Unit value representation | `RuntimeUnitContract` | Raven's `System.Unit` value representation |

Read [metadata import and core selection](metadata-import.md),
[iteration contracts](runtime-iteration-contracts.md), and
[propagation contracts](runtime-propagation-contracts.md) for their validation rules.
Selecting a protocol does not select an exception model, disposal policy, array
variance rule, or generic array representation. Those require separate decisions.
Runtime-specific profiles and deviations should remain independently testable.

## Unit value contract

`RuntimeUnitContract(AssemblyName, TypeName, MapClrVoidToUnit = false)` selects a top-level, non-generic, empty value type in the
explicitly selected metadata/emission core. For example, a .NET reference set can
select `System.ValueTuple` from `System.Runtime`:

```csharp
options = options
    .WithMetadataImportOptions(new MetadataImportOptions("System.Runtime"))
    .WithTargetCoreAssemblyName("System.Runtime")
    .WithRuntimeUnitContract(new RuntimeUnitContract("System.Runtime", "System.ValueTuple"));
```

The corresponding evaluated project properties are:

```xml
<RavenMetadataCoreAssemblyName>System.Runtime</RavenMetadataCoreAssemblyName>
<RavenTargetCoreAssemblyName>System.Runtime</RavenTargetCoreAssemblyName>
<RavenUnitAssemblyName>System.Runtime</RavenUnitAssemblyName>
<RavenUnitType>System.ValueTuple</RavenUnitType>
```

The compiler and project-backed language server consume the same options. Partial,
missing, reference-type, or stateful unit selections produce `RAVT003`; they do not
fall back to `System.Unit`. Option copies retain the contract, and changes invalidate
incremental semantic-state transfer.

The language still has a unit expression `()`. When its value is needed, storage,
arguments and generic payloads use the selected value type. An ordinary no-result
call still returns CLI `void` and leaves no value on the evaluation stack. Using
such a call as a unit-valued expression materializes the selected empty value after
the call; discarding the call does not manufacture a value to pop.

The Reflection.Emit implementation currently uses an intermediate Unit structure.
Target emission replaces that structure's value references and literals, removing
its definition from the final assembly. This is compiler implementation machinery,
not an additional platform type. It does not change the default .NET representation.
Public members of the synthesized Unit implementation are not automatically members
of the selected type; unsupported use is rejected during target emission. Libraries
must be compiled with compatible contracts rather than assuming a consumer can
reinterpret another assembly's distinct Unit definition.

The general regression uses a normal .NET empty value type and executes the emitted
program on .NET. A target choosing a type with additional semantics must validate
those semantics and its runtime separately. In particular, this mechanism does not
make nominal Void values or Void generic arguments valid on the .NET CLR.

## Generic async unit results

`Task<unit>` and `ValueTask<unit>` bind explicit unit returns as generic payloads.
State-machine completion supplies the unit value to the generic builder;
awaitless Task methods use generic completion instead of `Task.CompletedTask`.
Expression-bodied async methods pass through the same lowering as block bodies.
These rules require no new Runtime Contract option and preserve the normal .NET
unit representation. Tests execute immediate, pending and awaitless methods on
.NET; they do not establish support for any other runtime's builder or unit ABI.

## Field receivers

Taking the address of an instance field uses the containing object's reference for
class owners and an address for value-type owners. Calling a struct member through
a class local, parameter or nested field therefore preserves the original field's
storage. No Runtime Contract option or semantic-model change is involved. Regression
coverage executes ordinary CLI types on modern .NET; .NET Framework and
NanoFramework execution require their own validation.

## Integration and documentation

Reference metadata, compiler binding, emitted signatures, and runtime execution are
separate validation layers. Contract tests should check diagnostics and semantic
symbols, final metadata, and observable execution where supported. Document the
selected API names and unsupported operations in the target integration as well as
here. General mechanisms belong in Raven; target-specific configuration and policy
must not be smuggled into ordinary .NET defaults.

This preview adds optional compiler-option constructor parameters. Source callers
can retain defaults, but compiler API consumers must rebuild.

Compiler-affecting changes must update the relevant Raven documentation, including
contract selection, diagnostics, semantic-model behavior, emission and limitations.
When an external runtime integration is affected, its repository must also document
the selected configuration and integration evidence. Keep implementation status
separate from proposed design and record changes in each repository’s changelog.

## Experimental neoCLR selection

The neoCLR profile selects `RavenUnitAssemblyName=NeoCLR.CoreProbe` and
`RavenUnitType=System.Void`, with that same explicitly selected core for metadata
and emission. The final assembly uses the core’s Void value representation and
contains no synthesized System.Unit definition. Ordinary calls still return CLI
void; values required by expressions, parameters or Result payloads use nominal
Void. This policy remains experimental and does not change the ordinary .NET target.

The metadata-only Void regression is separate from the general ValueTuple execution
test. Actual execution, including Result<Void, E> propagation, is checked by
neoCLR’s `docs/experiments/raven-target/verify_unit_contract.py`. Void storage and
generic arguments are neoCLR semantics; the .NET CLR is not expected to execute them.

The reusable mechanisms were independently integrated into Raven main through
`2d17199a1`. This branch retains its separate generic-array, nominal Void and
no-exception policies; those policies were not included in the main integration.

Generic method calls are projected through the method specification’s element
signature and its separate type-argument list. They do not have writable declaring
types of their own. The regression executes `Echo<int>` with both default Unit and
a selected ValueTuple contract. Generic storage of unit remains covered separately.
Generic unit-valued calls retain the value-bearing return signature of the original
method definition. Consuming a result uses that value directly; discarding it pops
it once. Raven does not synthesize another unit after a generic unit-returning call.
The regression covers generic methods, methods on generic types, assignment,
arguments, statement calls and no-result wrappers under default .NET emission,
explicit System.Runtime emission and the selected ValueTuple contract.

Constructed imported types can contain source method/type parameters in emitted
signatures. MetadataLoadContext cannot combine its types with Reflection.Emit
generic parameter builders; Raven uses a persisted signature representation for
that combination, as it already does for source TypeBuilder arguments. This applies
to ordinary CLI contracts and does not select target-specific collection semantics.
The regression compiles and executes a function using a separately compiled C#
`Box<T>` parameter/return with ordinary and explicit-core .NET emission.

Imported member proxies are rewritten per use in the emitting method context.
Source method/type parameters in constructed owners or method arguments retain
their CLI generic parameter positions, while imported definition signatures retain
their own parameters. This is general metadata emission behavior, independent of
target contract names. The independent `Box<T>.GetValue()` execution regression
covers ordinary .NET and explicit metadata-core compilation.

Namespace lookup combines source declarations with referenced sibling types before
falling back to global types. Constructor expressions and generic arguments therefore
resolve the same referenced types as annotations when a source namespace extends a
metadata namespace. The independent `Contracts.Box<T>` regression covers both import
and same-namespace use, executing under ordinary and explicit metadata-core emission.
This requires no target-specific binding rule.

Member-union case construction follows the admitted CLI carrier constructor contract.
A bare empty case in a return or explicitly typed local initializer constructs its
parameterless case value before constructing the carrier, even if the case itself
has no Raven union-case attribute. The independent C# `Choice.Empty` contract is
executed under ordinary and explicit-core emission; this is not a target naming rule.
Unannotated bare ordinary types still require explicit constructor invocation.


Explicit constructor type arguments remain bound when they are the containing
class's own parameters: `Box<T>(value)` inside `Box<T>` is a valid open construction,
not an omitted-argument inference request. Binding still validates constraints and
emission uses ordinary CLI generics. The execution regression covers qualified and
unqualified construction under default .NET and explicit metadata-core options.
No Runtime Contract configuration or target-specific policy changes.


Imported generic methods may be constructed with a source class or method parameter
under an explicit metadata core. Emission uses the existing temporary metadata-token
proxy and rewrites it to a CLI MethodSpec in the caller's generic context. It does
not call MetadataLoadContext.MakeGenericMethod with Reflection.Emit parameters from
a different context. This changes emission only: binding, type inference and Runtime
Contract settings are unchanged. The independent C# Helpers.One<T>(T) -> T[] fixture
executes Raven consumers on .NET 11 with default and explicit System.Runtime settings,
covering both type and method parameters. This validation does not claim execution
on .NET Framework or NanoFramework; no neoCLR-specific contract is involved.


Generic array elements preserve their actual CLI type parameter for loads and stores,
including literals and indexed iteration. An unconstrained parameter may instantiate
as a value or a reference, so reference-only array operations are insufficient.
This is independent of Runtime Contract settings and does not change array variance
or inference. The .NET 11 regression executes method and type parameters, literals,
indexed reads/writes and iteration with Int32, String and Decimal elements under
both default and explicit System.Runtime metadata configuration. The earlier emission
could crash the isolated .NET test process; the corrected tests preserve complete
values and execute normally. .NET Framework/NanoFramework execution is not claimed.

## Generic interface base scope — 2026-09-19

Interface base lists bind in the declared interface's type-parameter scope for
top-level interfaces and interfaces nested in classes or other interfaces.
The semantic model and emitted CLI base-interface signature retain that parameter
identity; an enclosing binder must not replace it or report it out of scope.
No Runtime Contract option or target-specific mapping is required. The regression
checks diagnostics, semantic symbols and reflected emitted metadata on .NET 11.
All 87 focused resolution, interface, accessibility and constrained-hierarchy tests
pass; the three reduced cases failed before the fix.
This does not establish execution on .NET Framework or NanoFramework.

## Experimental opaque library authoring (2026-09-19)

The neoCLR importer now admits Raven-authored String and message Error bodies
against its bootstrap reference assembly. This uses the existing explicit metadata
core, nominal Void and propagation configuration; no Runtime Contract option or
Raven semantic/emission change is introduced. A plain Raven `class String` is final
in CLI metadata; `sealed class` instead denotes an abstract closed hierarchy and
is not the required shape here.

The target checks a single private string storage marker, erases it to intrinsic
storage, and preserves String's existing mixed byref/value neoIL receivers. Error
has checked fieldless metadata; its placeholder reference-assembly layout does not
specify runtime storage. The importer projects its managed CLI receiver loads onto
the existing opaque value ABI. Native calls remain bootstrap-only; primitive string
equality avoids recursive calls to Equals. Direct opaque allocation/default Error
and String storage writes are rejected. These are target-owned admission rules, not
general CLR class layout or constructor semantics, and stay off Raven main.

Validation lives in neoCLR's `verify_opaque_library.py`, saved string/slicing/error
programs, and Rust string/error/interface tests. The source migration preserves
runtime behavior; String retains both its existing Raven named arguments and the different
descriptive names in runtime introspection metadata. Proposal API alignment and remaining union/descriptor/array
source migration remain separate work. No .NET Framework or NanoFramework execution
claim follows from these neoCLR checks.

## Unit contract assembly identity (2026-09-19)

RuntimeUnitContract validation resolves the named type from its explicitly
configured assembly. A source type with the same metadata name does not replace
that contract or cause an unrelated shape diagnostic. Ordinary source-name lookup
continues to prefer source declarations; only this configuration lookup is scoped.

The regression uses .NET's System.Runtime/System.ValueTuple contract alongside a
nonempty source System.ValueTuple. It checks diagnostics, the emitted unit local's
System.Runtime identity, preservation of the source declaration, and execution
returning 42. This is an assembly-identity fix with no new contract configuration or
target policy. All 19 focused unit/target-core checks pass; the new regression
failed before the fix. .NET Framework and NanoFramework were not executed in this check.

## Experimental typed error library authoring (2026-09-19)

neoCLR now authors seven existing typed error carriers in Raven. Its bootstrap
metadata exposes checked erased storage and empty nested cases. Bootstrap-only
pack/test/unpack calls lower to the existing target value instructions. The importer
checks the complete constructor CIL shape before replacing its single assignment
with neoCLR's by-value construction convention. Arbitrary payloads, constructor
side effects and fabricated defaults are rejected. This is a target importer rule,
not a change to Raven's CLI value semantics or Runtime Contract configuration.

The current explicit-core/unit/propagation settings remain unchanged. Fourteen
admission checks, 25 focused Rust tests and all 64 saved-program cases pass in
neoCLR. Generic unions, descriptor inheritance and runtime adapters still require
source migration. Unqualified nested case names rejected in a source signature
remain a candidate for independent scope-rule investigation; qualification works,
and no general compiler fix is claimed from that observation.

The follow-up Propagatable declaration is also Raven-authored. The target importer
checks exact generic positions and ordinary CLI out metadata before emitting its
existing readonly `out(true)` ABI. Failed extraction does not establish destination
initialization. This continues the existing target contract; it does not change
Raven's ordinary .NET out assignment rules. Five declaration admission checks pass.

## Experimental generic union authoring (2026-09-19)

neoCLR now authors Option/Result carrier and case bodies in Raven. The importer
checks complete generic families, matches storage and method signatures, preserves
public runtime case Value fields, and lowers checked constructors to the existing
value ABI. Compiler-only consumer recognition members remain metadata protocol
adapters, not additional runtime exports. Native pack/test/unpack primitives remain
bootstrap-only. The target's existing metadata core/unit/propagation configuration
is unchanged.

A bootstrap-only LeaveUnassigned(out T) marks failure paths in source; it never
executes its CLI stub. Import admits only the current conditional output address
followed immediately by false return, emitting no write. Literal Boolean returns
preserve runtime verification of true-only assignment; readonly receiver adapters
copy without requesting writable references. Invalid carrier/generic-case defaults
remain unreadable. This does not change ordinary .NET out semantics or introduce
a Runtime Contract setting. These representation policies stay off Raven main.

All 12 admission cases, 41 focused Rust tests and 64 saved-project cases pass in
neoCLR. General out-forwarding assignment was separately reproduced with source
and .NET metadata and fixed on main (`5f6e17347`); the feature cherry-pick is
`2d2a1d586`, with 41 parameter checks passing on each branch. The independently reduced same-name constructor arity fix is now on main
(`d7292b935`) and the target branch (`d833ef2f3`): 78 focused main checks and 18
feature checks pass; the ordinary .NET regression returns 42 instead of 0. No .NET Framework/NanoFramework execution is implied.


## Experimental descriptor authoring (2026-09-19)

The neoCLR importer now admits the exact MemberInfo/FieldInfo/MethodInfo/PropertyInfo
snapshot hierarchy from Raven sources. Ordered private field layouts and existing
runtime field names remain fixed. Source protected base construction is admitted
only in the checked derived constructor chain and emitted with internal visibility.
This is a bounded importer rule, not general application protected-member support.

The bootstrap-only ParameterSnapshot metadata view maps to the runtime's immutable
parameter vector. Its checked Length/Get operations are intrinsic; Raven owns array
allocation/copying and property-accessor visibility filtering. Consumer metadata is
unchanged and does not expose the view. No new Runtime Contract configuration,
compiler semantics or metadata emission rules are introduced. These representation
policies remain on the neoCLR branch; the normal .NET compiler is unchanged.


neoCLR's NativeMemory overloads are now Raven-authored as well. Bootstrap-only
NativeAllocation signatures map to checked native multiplication, allocation and
release instructions. Consumer pointer signatures and existing unit/void projection
are unchanged. No Runtime Contract setting or general compiler rule is added.
Five admission checks, 28 native/pointer runtime tests and a saved Raven allocation
program pass; these importer policies remain on the target feature branch.


System.Fault source authoring uses a bootstrap-only RuntimeFailure.Terminate binding.
Its existing no-result signature, dynamic diagnostic and terminal guest-failure
semantics are preserved. No compiler non-return analysis or Runtime Contract option
is added. Three admission cases, seven fault/query tests and a verified Raven Unicode
failure program pass in neoCLR. The host continues after a guest failure.


neoCLR now admits all five invariant Func declarations authored in Raven, validating
CLI runtime constructor/Invoke metadata and generic positions against its existing
consumer contract. Runtime invocation and capture lifetime remain unchanged. No
Runtime Contract setting, ordinary .NET delegate rule or target compiler mapping is
added. Six admission cases, 28 delegate tests and the Raven delegate sample pass.

## Enum backing-field metadata (2026-09-19)

The final PE metadata pass preserves `SpecialName | RTSpecialName` on the instance
`value__` field of CLI enums, as required by ECMA-335 II.14.3. Raven already requests
both bits when defining the field, but the host PersistedAssemblyBuilder masks
reserved field attributes. The correction applies to normal and explicitly retargeted
emission, including nested enum definitions, without changing source semantics,
Runtime Contract configuration or underlying integral storage.

An independent ordinary .NET regression inspects the emitted PE field attributes,
then loads the enum and checks its underlying type and literal value. It failed
before the correction; all 13 focused enum/target-core checks pass on .NET 11.
.NET Framework and NanoFramework execution are not established by these checks.
The final metadata normalization is necessary while the host emission layer drops
the bit; it does not admit malformed enums or add target-specific enum policies.

Sources: [CLI standard](https://ecma-international.org/publications-and-standards/standards/ecma-335/),
[Persisted field builder](https://github.com/dotnet/runtime/blob/main/src/libraries/System.Reflection.Emit/src/System/Reflection/Emit/FieldBuilderImpl.cs).


## Experimental enum declaration authoring (2026-09-19)

neoCLR's BindingFlags source is now a normal Raven Flags enum. The target importer
checks the exact Int32 enum declaration and supplies intrinsic enum lowering to
its existing nominal value ABI. Enum operations are not authored struct methods.
The six literals, unknown-bit behavior and reflection filtering remain unchanged;
no Runtime Contract setting or source-level enum rule changes. Seven declaration
admission cases and the compiled Raven flags/filtering sample pass. The general
backing-field metadata correction above is on main independently; this target
projection remains on the neoCLR feature branch.


neoCLR's subsequent Object/UnionAttribute slice checks empty source declarations
and exact base-calling constructors. Object remains the fieldless runtime root;
its compiler-facing reference members are not executable library bodies. The
ordinary CLI Attribute source declaration projects to the existing empty target
marker ABI. This is bounded target declaration lowering, not a general class/value
conversion or a new attribute execution model. Runtime Contract configuration is
unchanged. Nine declaration admission cases and the saved generic-union program pass.


The managed-array follow-up authors Array members, callback iteration and a private
iterator in Raven. Its importer checks one intrinsic T[] field, omits only a private
empty constructor and rejects backing-field writes or ordinary allocation. Bootstrap
Length lowers to vector length. The source instance GetIterator body projects to the
existing internal static ArrayEnumerable dispatcher ABI with argument zero unchanged;
public array exports and reflected capabilities are preserved. Static property metadata
retains its receiver kind. Eight admission checks, 25 array/collection tests and four
saved array programs pass. No Runtime Contract option or Raven compiler code changes.


The final handwritten TypeOf<T>.Of helper was removed at the author's direction;
existing typeof syntax supplies declared-type inspection. Rebuild callers against
updated neoCLR reference metadata. Its source-body ownership gate now finds only
generated declarations/bodies and explicit runtime services across 73 source slices.
Thirteen type/reflection checks and the saved reflection program pass; removed-helper
calls are rejected. Object.GetType remains a preview API alignment candidate, not a
member added by this port. The author permits deliberate development compatibility
breaks while using .NET as the ergonomic comparison baseline. No compiler syntax,
Runtime Contract configuration or .NET target behavior changes in this removal.

## Explicit editor references (2026-09-19)

When MetadataImportOptions selects an explicit CLI reference universe, the language
server uses those supplied references without adding host Raven.Core or macro
support assemblies. This matches compiler metadata import configuration and avoids
resolving editor symbols against assemblies outside the selected runtime. Normal
host-framework projects retain their existing support-reference behavior. No new
Runtime Contract option or emission policy is introduced.

A standalone .NET project with only System.Private.CoreLib reproduced unwanted
editor-added assemblies before the fix. The regression checks the exact reference
set and configured core identity. All 65 workspace integration tests pass on
.NET 10; this does not establish .NET Framework or NanoFramework execution.

## Value receiver deconstruction (2026-09-19)

A nominal deconstruction pattern whose input and receiver are the same known value
type stores a local copy and invokes Deconstruct on that copy. It does not box the
value to perform a redundant null/type test. The previous path generated an invalid
program for .NET ref structs, which cannot be boxed; the reduced execution case
failed with InvalidProgramException before this correction. An ordinary struct
retains its original field after a mutating Deconstruct, while a reference receiver
continues to observe mutation. Null and unrelated narrowed reference inputs fail
the pattern normally. Type parameters and genuinely narrowed inputs retain the
existing general path; no broader generic ref-struct support is claimed.

This is a general CLI emission correction, with no Runtime Contract configuration
or neoCLR policy. All 31 focused deconstruction/ref-field checks pass on .NET 11.
.NET Framework and NanoFramework were not executed. See Microsoft's
[ref struct restrictions](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/ref-struct)
and Raven's [deconstruction rules](../lang/spec/deconstruction-and-union-patterns.md).

## Completed neoCLR source-port branch audit (2026-09-19)

The long-lived integration branch is now `neoclr`. General interface scope, delegate
void bridges, unit assembly identity, out forwarding, constructor arity, enum field
metadata, explicit editor references and exact-value deconstruction fixes are on
main with independent .NET validation. Temporary fix branches were removed after
confirming their commits belong to main; the target branch was not merged wholesale.

Remaining production differences implement target array shape/covariance, nominal
Void value positions, target propagation and context-owned typeof, with their
configuration and tests. No new Runtime Contract setting is added by this audit.
Explicit project reference isolation already belongs to the shared project service;
the extra feature evaluator guard was redundant and is removed. Unit-contract
project coverage is retained alongside the target array cases. All 47 main and 52
neoclr project-system tests pass on .NET 11. .NET Framework and NanoFramework were
not executed in this validation. Parser and nested-case lookup observations remain
unclassified follow-ups, not implemented changes awaiting extraction.

The neoCLR library now has 73 source slices; native services and compiler-generated
adapters remain intrinsic. Local VS Code tooling uses matching copied binaries and
target metadata. Its saved-program task runs neoCLR; the ordinary Raven Run/Debug
commands retain their .NET workflow. Subsequent preview API alignment may break
compatibility deliberately, retaining useful .NET ergonomics without locking the
runtime to the complete .NET API. Object.GetType remains a candidate for that stage.

### Experimental Unicode scalar Char contract

The neoCLR branch adds `CompilationOptions.WithUnicodeScalarChar(true)` and the
project property `RavenUnicodeScalarChar`. The default is false. The explicit
contract retains `System.Char` metadata identity while selecting 32-bit scalar
array, indirect and numeric operations for runtimes with scalar Char storage.
It requires a matching target runtime/reference pack; it must not be enabled for
the ordinary .NET System.Char implementation.

The lexer represents supplementary character literals as `System.Text.Rune`
constant values. The semantic model still reports System.Char. Both literal
Unicode and eight-digit `\U` escapes are accepted with this contract; surrogate
literals are diagnosed. Ordinary targets reject supplementary Char literals.
Four-digit `\u` escapes also work. Numeric conversions pass through typed Char
storage so the target runtime can validate the scalar. Scalar patterns and
array round trips are exercised by the neoCLR integration sample.

This policy is experimental and is not a change to Raven's .NET Char contract.
Potential general lexer improvements must be reviewed and tested independently
before extraction to main; none of this target bundle is approved wholesale.

Validation on the development host: 76 lexer/literal/semantic tests and the neoCLR
ScalarChar/Primitives saved-project checks, including supplementary literal patterns
and arrays. Runtime invalid Int32 conversions fault through typed storage. Numeric
casts still narrow to UInt32 before validation; this does not promise checked
conversion from arbitrary wide numeric inputs. Unicode escape highlighting already
exists in the TextMate grammar. No .NET Framework or NanoFramework execution claim.

## Experimental grapheme Char target — 2026-09-19

The neoCLR branch exposes `CompilationOptions.WithGraphemeChar(true)` and project
property `RavenGraphemeChar=true`. This replaces the scalar policy selected by the
current neoCLR props; it is not enabled for ordinary .NET, .NET Framework or
NanoFramework targets. The earlier `WithUnicodeScalarChar` experiment is retained
for its older target, and must not be selected by the current neoCLR toolchain.

Character literals containing one extended grapheme cluster have semantic type
System.Char, including combining and emoji sequences. Emission calls the matching
core's static `Char.FromString(string)` factory, with typed Char array and indirect
operations instead of integer storage. Literal patterns compare typed characters.
Numeric conversions and arithmetic are rejected. The core reference metadata still
uses the CLI Char identity as an intermediate carrier; this experimental artifact
requires the neoCLR importer/runtime and is not executable as ordinary CLR IL.

String's ordinary iteration contract is `Iterable<char>` and Length counts clusters;
scalar traversal is an explicit Sequence<uint>. The runtime owns UTF-8 text for Char,
validates one cluster at every construction/storage boundary and implements ordinal
comparison without normalization. This borrows Swift's character abstraction while
retaining .NET-like names; it differs deliberately from .NET Char/Rune/StringInfo.
The cost is variable-size values, segmentation and current snapshot copying.

Literal diagnostics use host StringInfo segmentation; neoCLR uses pinned Unicode 16
rules and revalidates constructions. Host/target Unicode differences can therefore
produce a runtime rejection after successful compilation. Aligning diagnostic tables,
constant metadata, normalization/collation and cursor indexing remain development
work. The tested minimal surface uses ordinary let bindings, literal patterns,
arrays, iteration and FromString construction, not CLI literal fields.

Validation: focused Char diagnostics/type tests and ordinary target regression
checks; the paired neoCLR sample executes 4 graphemes / 12 scalars / 37 bytes,
combining/emoji patterns, arrays, direct and interface iteration. No claim of testing
this experimental contract on the CLR, .NET Framework or NanoFramework is made.

## Provisional async exception-capture policy (neoCLR branch, 2026-09-19)

`CompilationOptions.WithAsyncExceptionCapture(false)` opts compiler-generated
async method state machines out of their implicit exception-capture wrapper.
`CaptureAsyncExceptions` defaults to true, preserving ordinary .NET behavior.
This provisional compiler API is available on the neoclr branch; there is no
project property or CLI flag in this slice and no new stable builder ABI.

With capture disabled, AsyncLowerer does not create AsyncDispatchGuard or its
System.Exception catch; builder-member discovery does not request SetException.
Dispatch, await rewriting and ordinary completion are unchanged. T is opaque:
Task<Result<V,E>> receives no special lowering. Propagation already exposes early
returns before await rewriting; the same completion path handles those returns.
Option copies retain the policy and changes prevent incompatible incremental-state
transfer. No syntax, TextMate or editor presentation changes are needed.

This switch alone is not an exception-free target profile. It does not remove
source try/catch/finally, throw expressions or compiler-generated disposal regions.
Targets must diagnose or reject unsupported forms; neoCLR's importer must keep
rejecting handlers. Default .NET builders and awaiters remain selected, so complete
neoCLR Task execution still requires target builder selection, library contracts
and safe heap-owned state. Do not enable this policy for ordinary .NET application
code merely to avoid faulted tasks: an escaping exception may terminate its host.
The neoCLR target uses terminal Faults instead.

AsyncExceptionCapturePolicyTests compare the default and opt-out policies through
actual .NET execution: immediate/pending awaits, propagation before/after await,
skipped post-propagation side effects, unrelated union payloads, default task fault
capture and opt-out escape. Metadata checks require no handlers on the simple
opt-out machines, without fixing their instruction layout. Existing lowering,
propagation and resource-lifetime tests remain regression coverage. This is not
evidence of end-to-end neoCLR async execution or exception-free disposal.

A separate generic-unit gap surfaced while extending coverage: an explicit
`return ()` in `async ... -> Task<unit>` reports RAV2705 with either policy. Resolve
that independently; this change does not alter return binding or silently route
Task<unit> to a nongeneric Task. The generic opt-out mechanism is experimental
target policy, not a change merged into Raven main.

Validation on 2026-09-19 with .NET SDK 11.0.100-rc.1.26425.128: the pre-change
functions/async baseline passed 70 checks and the focused runtime/lowering baseline
passed 42. After the change, 61 focused checks (including 19 new policy cases) and
all 119 checks selected by the functions/async feature filter passed. These sets
overlap; they are not an aggregate unique-test count. The touched C# files were
formatted with dotnet format whitespace.

## Provisional heap async state machines (integration branch)

`CompilationOptions.WithHeapAsyncStateMachines(true)` makes synthesized async
state machines reference types with an object constructor. The default remains a
value type. Option copies preserve this setting and incremental reuse rejects
changes. Binding and Task payload semantics are unchanged; generated state metadata
and field receiver emission follow the selected storage kind.

For existing by-reference .NET builder methods, the class reference is passed
through an addressable local; the object itself retains state and awaiters. Tests
execute completed and pending two-await methods with forced GC on modern .NET,
plus default-policy and option-copy regressions. This is a provisional mechanism
on the neoCLR branch, not a promise of general runtime-owned suspension. There is
no project property or CLI switch yet, and neoCLR builder/importer integration
remains outstanding. Exception capture is an independent option.

Release-gate validation (2026-10-07) found a pending heap-state regression in
field-assignment emission: an emitter-local copy of `self` was bypassed by resume
dispatch. Stable self/base receivers are now reloaded after the value expression;
side-effecting receivers keep their original evaluation order. Seventeen focused
modern .NET tests pass, covering Debug/Release heap and value states, delayed task
completion, forced GC and reference-field early returns. Runtime Contract settings
and CLI bridge encoding are unchanged. This does not establish native neoCLR,
.NET Framework or NanoFramework execution support.

## neoCLR Task builder integration (2026-09-21)

On the neoclr branch, a target-core compilation with heap async states resolves
Task<T> to System.Tasks.Task<T>. PE symbols recognize that contract; emission uses
target Task and builder metadata rather than host BCL builder representation.
Builder calls honor metadata parameter passing: neoCLR reference state and awaiter
arguments are by value, while default .NET by-reference protocols are preserved.
Heap-state constructors receive captured receiver/parameters before publication;
this supports runtime constructors that reject uninitialized erased payloads.
Awaitless methods use the same heap builder path and do not require FromResult.

The neoCLR bridge enables WithHeapAsyncStateMachines(true) and
WithAsyncExceptionCapture(false). There is no stable CLI or project contract yet.
Result payloads remain ordinary values. No language syntax or TextMate changes
are introduced. Ten neoCLR source scenarios exercise queue scopes, pending awaits,
GC, composition, unit and Result propagation before/after await; 38 focused .NET
checks cover heap/default policy, unit, capture and field receivers. This is not
.NET Framework or NanoFramework validation. Generic async methods, async lambdas
and broad async disposal remain outside this PoC. Hoisted non-default aggregates
need additional validation. Nested ordinary-lambda capture failures are deferred
candidates for independent main-based investigation. Target policy stays on neoclr;
no wholesale integration into main is intended.

### Development project selection

The neoclr branch now reads `RavenHeapAsyncStateMachines` (default false) and
`RavenCaptureAsyncExceptions` (default true) from .rvnproj. The neoCLR development
props select true/false respectively, making project builds and the editor use the
same policy as the bridge. These remain experimental target contracts, not a
portable .NET recommendation. Two focused project tests cover explicit selection
and unchanged defaults, in addition to the existing async execution coverage.


### Provisional cancellation propagation (neoclr, 2026-09-23)

WithAsyncCancellationPropagation(true), or RavenPropagateAsyncCancellation=true in
an rvnproj, selects the target-only await protocol. Defaults remain false for .NET.
The project option is shared by CLI and workspace/editor evaluation and invalidates
incremental semantic reuse. Awaiters must expose a public instance bool IsCancelled
getter, and the generic builder a public parameterless SetCancelled with no result.
Missing members are diagnosed while binding await. neoCLR also selects heap state
machines and disables exception capture; Result is unrelated to this policy.

The shared immediate/resume path tests cancellation before GetResult, clears the
saved awaiter and exits to a separate cancellation completion label. That exit
uses source-scope disposal/leave machinery before publishing completion. Successful
completion and Result propagation retain their normal paths; no default payload is
used for cancellation. There is no new source syntax or highlighting change.

This provisional subset supports named Task<T> functions. Await within for loops
is rejected with RAV2712 after an integration probe exposed unsaved iterator state
and skipped disposal. Protected cleanup/async disposal and runtime-async lowering
are not validated target capabilities. neoCLR rejects async use declarations with RAVT006. Synchronous use is supported
by the scope-exit disposal contract described below.

Validation: normal .NET heap/value state and exception-policy regressions; option
copy, missing protocol and project-evaluation tests; neoCLR immediate/resumed int,
unit and Result cancellation scenarios with nested calls and side-effect checks.
This target policy stays on neoclr, not main. Existing research/comparisons are in
neoCLR docs/task-model-alignment.md and docs/async-api-design.md. Loop lowering is a
potential general Raven improvement requiring an independent main-based repro.


### Propagation temporary lifetime

When a propagation operand has no exception-conversion boundary, lowering binds
its temporary at initialization. In `(await operation)?`, this prevents an empty
carrier from being hoisted before the await has produced it. Ordinary CLI targets
and runtime propagation contracts use the same rule; exception-catching operands
retain their protected assignment. There is no new option or precedence change:
`await operation?` still applies postfix propagation before await. Validate both
completed and suspended Result operands when changing this lowering.

## Attributed custom union metadata

Typed-case carriers marked with `System.Runtime.CompilerServices.UnionAttribute`
are recognized without target-specific configuration. The [CLI union contract](../lang/spec/dotnet-implementation.md)
defines the required constructors and typed accessors. This affects imported
symbols and documentation; it adds no Runtime Contract option, storage rewrite or
new extraction lowering. Independent .NET class/struct fixtures cover recognition,
negative shapes and RavenDoc case grouping.

### Configured unit identity in imported interfaces (2026-09-23)

With RuntimeUnitContract explicitly selected, the language unit symbol retains its
source semantics but compares and hashes as the exact imported value type selected
by assembly and metadata name. This also applies inside generic signatures, so a
source List<unit> return can implement an imported List<System.ValueTuple> return
when System.ValueTuple is the configured unit representation. Previously equivalent
emitted signatures could fail semantic interface matching.

This does not equate an unconfigured unit with arbitrary empty structs, change
ordinary no-result method emission, or select a shadowing source type. No option
is added and default .NET behavior is unchanged. The regression checks opt-in
success, unconfigured rejection, symbol/hash equality and actual interface dispatch
on .NET 11. Existing unit storage, no-result and target-shadowing tests remain the
compatibility checks. No .NET Framework or NanoFramework execution is claimed.

### Imported array interface identity (2026-09-23)

Array symbol identity is structural: element type, rank and fixed length. The
namespace/container attached while constructing a source or imported array symbol
is not part of the array's CLI identity. Equality and hashing use the same rule.
Previously a source byte[] parameter could compare unequal to an imported byte[]
interface parameter, preventing implicit interface implementation flags and dispatch.

This general correction requires no Runtime Contract option, name mapping or
backend-specific policy. A regression imports an ordinary C# interface, compares
its array parameter with the Raven implementation, checks hash-set lookup, emits
the consumer, then invokes it through the interface on .NET 11. Existing tests keep
array element/rank/fixed-length distinctions. .NET Framework and NanoFramework
execution have not been tested; this is not a claim of new target support.

## neoCLR terminal Fault calls (2026-09-25)

On the experimental neoCLR branch, the resolved namespace function
`System.Fault(string)` from `NeoCLR.CoreProbe` terminates control flow like a throw
statement. Statements following the call receive the ordinary unreachable-code
diagnostic, and a path ending in Fault does not require a return value or assigned
out parameters. Qualified and imported calls have identical behavior.

Recognition uses the runtime assembly, System namespace, namespace-member
container contract and static nongeneric string-to-unit/void signature. Container
spelling is not significant. Unrelated Fault methods retain ordinary call behavior.
The invocation remains an invocation in the semantic operations API and emitted
metadata; it is not rewritten to CLR exception throwing. Lowering and emission
recognize the terminal statement without synthesizing a missing-return throw.
No new Runtime Contract setting is required. This policy remains neoCLR-specific.

Validation: all 40 focused control-flow/return-path tests pass on .NET 11,
including metadata-backed qualified/imported calls, cold `AnalyzeControlFlow`,
unreachable statements, out parameters, branch endpoints, emission and negative
identity/container cases. A real neoCLR consumer with a Fault-only non-unit
function compiles and imports successfully, reporting RAV0162 after Fault.
The end-to-end `verify_fault.py` gate stopped before execution because the runtime
library snapshot was stale for an unrelated `HttpClient.rvn` edit. No guest runtime
execution, .NET Framework or NanoFramework validation is claimed for this change.

## neoCLR directional interface identities (2026-09-25)

The neoCLR library/bridge now exposes System.EquatableTo<T> and
System.ComparableTo<T> in place of Equatable/Comparable, and adds invariant
System.ConvertibleInto<T>.Convert() -> T. These are ordinary CLI interfaces;
explicit implementations, semantic interface conversions and call emission need
no compiler policy change. Equality/ordering methods retain their behavior.
Conversion is explicit and does not enable return-type overload selection.

neoCLR's shared props now select ``System.EquatableTo`1`` for
RavenRecordEquatableType, retaining RavenRecordAssemblyName=NeoCLR.CoreProbe and
the existing HashCode setting. This records the target's requested configuration,
not proof that this compiler implements a record Runtime Contract: the currently
built compiler rejects generated-record assignment to both the old and new target
interface. That baseline limitation remains open; explicit implementations compile.
Default .NET IEquatable<T> behavior is unchanged. Rebuild neoCLR references, managed
library and applications together because the renamed CLI identities are incompatible.

Validation in neoCLR: 47 focused runtime tests, bridge signature admission/rejection,
API reference snapshot and full library artifact regeneration. A consumer covering
all three interfaces compiles and imports. Generated IL changes only the intended
old identities, plus the new conversion declaration. Full tests and website builds
were skipped by author instruction. This is target integration documentation only;
no neoCLR-specific compiler change is proposed for main.


### Member overload generic arity (2026-09-26)

Duplicate member checking, signature-skeleton reuse/cleanup and stale-candidate
filtering and member lookup preserve method
generic arity. Ordinary and generic methods with the same value parameters remain
distinct, regardless of declaration order. This follows the
[C# signature rule](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/basic-concepts#75-signatures-and-overloading)
(checked 2026-09-25). The semantic model retains the selected arity and emission
uses ordinary CLI generic methods; no Runtime Contract switch or target policy is
added. Explicit type arguments exclude nongeneric candidates, including when a
higher-arity candidate uses Raven's partial type-argument inference. Return-type-only
overloads are not enabled. Regression coverage uses
standard .NET references; execution is checked on .NET 11, not .NET Framework or
NanoFramework.


### Generic method groups in generic callers — 2026-09-26

Method-level construction can remain open over the caller's type parameters.
For example, inside `Run<T>`, `Convert<T>` can initialize `Func<object, T>` or
be passed to a generic higher-order method. Overload inference must preserve the
constructed method rather than try to infer its already-supplied arguments again.
Delegate compatibility checks use the constructed signature; they must not reject
all type-parameter arguments as unresolved. A method whose arguments cannot be
inferred from the delegate parameters still reports a diagnostic.

This follows [C# method-group conversion](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/language-specification/conversions#108-method-group-conversions)
(retrieved 2026-09-26): explicit generic arguments and inference both participate
in delegate binding. The fix uses ordinary CLI generic method/delegate metadata,
without a Runtime Contract switch or a runtime-specific convention. Public semantic
queries use the same binder selection; no language-server or syntax changes are
needed. Regression tests execute typed and inline method groups with reference and
value instantiations on .NET 11, and reject an uninferred method. This does not
claim execution validation on .NET Framework or NanoFramework.

Validation: all 399 focused overload-resolution checks pass before and after the
fix; five new focused runtime/diagnostic regressions pass (four failed before).
A standalone C# .NET 11 comparison also preserves identity and the integer result.

### Terminal-flow diagnostic ownership correction (2026-09-27)

The neoCLR integration now reads a trailing expression from its bound statement,
including expressions bound with a contextual return type. The flow walker no longer
calls GetSymbolInfo recursively when its noncontextual expression cache is empty.
That recursive query could replace an executable binder and lose its diagnostics:
`func Main() { missing() }` incorrectly compiled successfully. Ordinary missing-name
and noninvocable-value errors now remain compiler errors and prevent emission.
Cold and warm AnalyzeControlFlow still recognize only the configured neoCLR Fault
identity. This is a correction to the experimental branch's terminal-call policy;
Raven main correctly rejected the same minimal program before this change.

No Runtime Contract setting, target metadata shape, guest Fault behavior or .NET
exception behavior changes. Validation: 55 focused invocation, terminal-flow,
return-path and control-flow tests passed on .NET 11, including cold flow queries,
failed emission and ordinary nonterminal Fault methods. Packaged MSBuild stale-output
validation is performed by neoCLR's release gate. No new .NET Framework or
NanoFramework execution claim is made.

### Nongeneric cases of generic unions

A generic union's companion may contain both generic payload cases and nongeneric
empty cases. Target-metadata emission preserves the actual CLI arity of each case: a
constructed symbol for an empty case does not make its metadata type generic. This
applies to ordinary separately compiled unions and does not require a target-specific
Runtime Contract setting. Focused imported-union tests cover both emission paths.

## Shared encoding reference surface — 2026-09-27

The neoCLR development reference adds System.Text.Encoding, Decoder, Encodings and
EncodingError plus selected StreamReader/StreamWriter constructors. EncodingError
uses standard Raven union metadata; InvalidEncoding cases are appended to stream
error unions without renumbering existing cases. The bridge admits exact interface
and constructor signatures and exports the matching authored Raven library bodies.
Application codecs implement the same interfaces as built-ins. No Runtime Contract
configuration, compiler semantic rule, opcode, metadata convention or native VM
service changes. Match reference and System library artifacts; Preview 10 does not
contain this surface. Stateful Encoder and UTF-16 buffer-style conversion are not
part of this contract.

Validation: focused production consumers pass for UTF-8 defaults, strict ASCII,
owned split-input carry, decoder lifecycle, decoded line boundaries, independent
custom providers, limits, partial-write failures, flushing and leaveOpen. Existing
reader checks pass, including 65536 input bytes. Only necessary snapshot/contract
checks run; no full compiler/runtime suite, website build or platform matrix. See
neoCLR's docs/experiments/text-boundaries/encoding-validation.json for evidence.

## Encoder reference and writer completion — 2026-09-27

The development neoCLR reference adds Encoding.CreateEncoder, Encoder.Accept/Drain,
EncoderProgress read-only properties, a standard EncoderState union and
StreamWriter.Finish. EncodingError appends Busy without changing existing case order.
Custom Encoding implementations must add the factory. Exact interface, constructor
and getter signatures are admitted by the bridge; progress uses private mutable
storage with an immutable public surface to fit the existing library profile.
Internal provider helpers remain instance methods under that profile. No new
Runtime Contract setting, compiler semantic rule, native opcode or general importer
admission is introduced. Rebuild references and System together; this is not Preview
10 compatibility. TextWriter itself does not gain Finish.

Validation in neoCLR: four focused public Encoder/writer runs and three existing
encoding/line runs pass, including independent state, scalar/output boundaries,
custom final bytes, failure handling and a 65536-byte write. Larger fixtures use the
established measure_async host budget; runtime defaults stay unchanged. API/library
snapshot checks accompany the implementation. No full suite or website build. See
neoCLR docs/experiments/text-boundaries/public-encoder-validation.json.

## neoCLR casing and Int64 reporting — 2026-09-27

The target's development reference adds String.ToUpperInvariant/ToLowerInvariant,
Int64.Parse/ToString, static MinValue/MaxValue getters and standard Raven
Int64ParseError. Exact target catalogs admit these public signatures and the
internal casing/parse services. No Runtime Contract configuration, language
semantics, compiler emission or general importer admission changes. Primitive
ToString retains the existing byref receiver; static bounds are properties rather
than literal fields. Parsing returns an ordinary Result<long,Int64ParseError>.

Unicode 17 full default casing intentionally differs from .NET invariant casing;
strict Int64 parsing follows neoCLR's existing ASCII Int32 grammar. Rebuild matching
native runtime, System library and reference artifacts. The archived Neo bootstrap
omits this new Parse/union surface, avoiding a new legacy manual carrier. The current
Raven profile exposes the complete API. Focused neoCLR source/native checks, a .NET
10 comparison and 16 exact signature checks are recorded in
neoCLR docs/experiments/casing-integer; no broad compiler suite or website build.

## neoCLR reflection member integration — 2026-09-27

The target bridge adds ConstructorInfo/GetConstructors and Result-based extensions
for argument-taking typed/untyped activation, method invocation and instance-field
access. Exact target signatures and source access metadata are required. The importer
retains public nongeneric static IL methods with declaring-type ownership and source
names, and public read-only fields with existing source-store checks. Field execution
requires original field access/read-only flags; older origins are conservatively denied.

No Raven Runtime Contract option or compiler semantic/emission change is required.
The target's TypeInfo source slice uses a private TypeHandle<T> bridge intrinsic emitting
existing ldtoken, because typeof(T) in that self-defining slice falls back to the host
System.Type factory. ParamArrayAttribute is an internal target reference marker.
Typed construction checks result assignability before user code. Exact scalar/reference
matching, public nongeneric class execution and terminal user Faults remain deliberate
limits. neoCLR docs/reflection-members.md and its reflection-members executable consumer
own the contracts and focused validation. This note belongs to the neoclr feature branch.


### Target-owned Main completion (2026-09-27 development)

With an explicit TargetCoreAssemblyName and UseHeapAsyncStateMachines enabled,
entry selection uses the same System.Tasks.Task<T> identity as async lowering.
Unit/int and Result<unit|int,E> payloads are admitted. The compiler retains the
selected static Main and its return type in the intermediate PE instead of emitting
a synchronous CLR bridge with host Task/Console dependencies. The neoCLR importer
adapts arguments, drives pending default-queue/host work and maps success/error to
process status and stderr. Target unit tasks are Task<unit>; there is no separate
nongeneric target Task. Other CLR targets retain their existing bridges.

Runtime Contract settings are unchanged: named unit, heap state machines,
cancellation propagation and disabled exception capture. Target images are import
inputs, not CLR executables. This is neoCLR target policy, not a general main-branch
metadata fix. See neoCLR docs/experiments/entry-results/README.md for limits and
executable validation, and TargetEntryPointTests plus the existing entry-point
suites for compiler selection/regression coverage.


### Array arguments in target-metadata generic signatures — 2026-09-27

Target-metadata emission resolves an array recursively from its target element type
before constructing an imported generic signature. For example, a reference-only
`Box<int[]>` must use the same metadata context for Box, Int32 and the array type;
using the compiler host's Int32 array caused MakeGenericType to throw. Jagged
arrays and nullable reference element annotations follow the same rule. Source
array syntax, binding and semantic-model types are unchanged. No Runtime Contract
setting is added or changed, and emitted CLI array/generic signatures remain
ordinary metadata rather than runtime-specific contracts.

The focused TargetMetadataEmissionTests compare emitted parameter/return metadata
using independent reference-only contracts and .NET 11 reference assemblies.
Integer, string and jagged array regressions failed before the fix; nullable-element
coverage preserves existing behavior. This does not claim execution validation on
.NET Framework or NanoFramework.


### Union case attributes (2026-09-27)

Authored case-declaration attributes now participate in the ordinary source-symbol
attribute lookup and semantic diagnostic walk. The existing type emitter writes
those attributes on generated nested case types, including generic companions;
constructors do not acquire duplicate attributes. Case symbols expose the same
metadata through GetAttributes. AttributeUsage is checked against the case type,
so a class-only attribute on a value case reports an error.

No Runtime Contract option, union storage layout, metadata target selection or
backend policy changes. Focused ordinary CLI metadata tests cover empty/payload
cases, nongeneric/generic unions and invalid targets on .NET 11; this is not an
execution claim for .NET Framework or NanoFramework. Downstream tools can inspect
attributes without constructing attribute instances.

### Constructed source types in target signatures (2026-09-27)

Target-metadata emission accepts imported generic signatures whose arguments are
constructed source types, such as `TaskCompletionSource<Holder<string>>`. A source
`Holder<>` definition belongs to Reflection.Emit, even when its argument is a
metadata-loaded `string`. The emitter recognizes that definition when deciding to
use a persisted generic signature instead of asking MetadataLoadContext to load a
foreign type. This is a general CLI emission correction: no syntax, semantic model,
Runtime Contract option or platform-specific mapping changes.

Validation: `TargetCoreGenericSignatureTests`, `AsyncGenericCaptureTests` and
`GenericArrayElementTests` pass all 16 checks on ordinary .NET. The new regression
executes a completed task holding the correct constructed source class in both
normal and target-metadata modes. This is not .NET Framework/NanoFramework runtime
certification. It does not resolve generic-containing-type async arity.

A separate reduced observation remains open: in target-metadata mode using
`System.Runtime`, an actual async method can fail while looking up the
`AsyncMethodBuilderAttribute(System.Type)` constructor with reflection types from
different contexts. The ordinary mode control succeeds. This attribute lookup
failure is distinct from constructed-source signature resolution; it was observed
while reducing the integration case, and is not claimed fixed here.

### Generic async owners and instance field writes (2026-09-27)

Nested state-machine signatures now carry all enclosing generic arguments before
method-derived state arguments. A nongeneric async method inside a generic class
also constructs its state type. Both ordinary local signatures and projected generic
method arguments use the same runtime-arity selection, fixing invalid
`AwaitUnsafeOnCompleted<TAwaiter,TState>` state arguments. Union-case companion
ownership remains separate and is covered by existing regression checks.

Async lowering redirects implicit instance field assignments through the retained
original receiver, matching explicit `self.field` access. Previously a write could
address the state machine while the subsequent read observed the unchanged source
object. The regression checks both generic and nongeneric source classes.

Validation: 35 focused ordinary .NET signature, generic-array, capture, generic-owner
and sealed-hierarchy/union checks pass. The generic-owner cases include nongeneric
and generic static async methods, captured locals, instance methods and two enclosing
generic types. `docs/compiler/development/async-generic-containing-type.rvn` is now
a positive 42-result example. Runtime Contract configuration, public compiler APIs,
syntax and semantic intent are unchanged; these repairs implement the normal CLI
contract. No .NET Framework/NanoFramework execution is claimed. Target import
admission still belongs to the target, independently of this compiler correction.

Separately observed during validation: explicit `self.value` assignment to a
private `var` member in a generic class can incorrectly report RAV0200. The reduced
`docs/compiler/development/generic-private-var-assignment.rvn` records this remaining
binding candidate. The field-write regression intentionally uses a declared field
to test physical receiver semantics independently of private-variable projection.

### Generic extension closure ownership (2026-09-27)

The generic async owner correction must distinguish semantic and emitted owners.
Raven generic extension containers emit as nongeneric static CLI types, with their
parameters moved onto methods. Nested closures use their own method aliases and
must not inherit the semantic extension's parameters a second time. The SDK's
Raven.Core WithContext bootstrap exposed this boundary. The existing ordinary CLR
ResultWithMessage success/error consumers reproduce the failure before the fix;
focused extension, async owner/capture and nested union tests validate the repair.
No Runtime Contract configuration or neoCLR-specific policy is involved.

### Workspace build-output discovery — 2026-09-27

Language-server automatic solution/project discovery and watched-file reloads now
exclude conventional `target` and `artifacts` output directories, like the existing
`bin`/`obj` exclusions. A generated solution must not replace the source workspace's
project group. Nested project.assets.json files under those output trees do not
trigger reload; the normal source project's obj/project.assets.json still does.
Explicit project/solution references retain their normal loading path.

Two focused generated-solution cases reproduce the old discovery error before
the fix. This reduces irrelevant editor scanning in mixed-language repositories;
there is no Runtime Contract configuration, semantic or emission change and no
neoCLR-specific policy. Old process stacks also showed JSON-RPC input processing;
the current SDK did not reproduce that CPU spin on closed stdin, so this fix does
not claim to resolve every older language-server CPU or memory report.


### Required interpolation member diagnostics (2026-09-27)

String interpolation resolves String.Concat against the selected reference library.
If no applicable overload exists, binding now reports the ordinary no-overload
error and emission fails; an error node may not silently erase the expression.
This is a general compiler correction, including reduced CLI reference profiles;
no neoCLR policy or Runtime Contract option is added. Successful member selection,
semantic expression type and emitted call contracts are unchanged. A runtime using
object interpolation must provide a compatible overload and its implementation.

Two modified System.Runtime-reference cases (leading text and interpolation-only)
failed before the fix and pass afterward. Eight focused interpolation/error-recovery
checks pass on .NET 11, including existing observable formatting and Unicode cases.
This is not execution evidence for .NET Framework or NanoFramework.


### Tuple metadata emission (2026-09-28)

Tuple syntax continues to use System.ValueTuple on ordinary .NET targets. Target
metadata emission retains the tuple projection's underlying constructed type for
nested generic arguments and field owners; labels remain source/attribute metadata.
Construction emits an ordinary instance constructor reference rather than applying
MakeGenericMethod to a temporary metadata proxy. Tuple types remain in the selected
metadata context when used in signatures. No new Runtime Contract option is needed.
The focused .NET 11 regression inspects nested ValueTuple metadata and executes the
result (42); this does not claim .NET Framework or NanoFramework execution.


### neoCLR tuple identity (experimental, 2026-09-28)

On the isolated neoCLR branch, TargetCoreAssemblyName = NeoCLR.CoreProbe selects
System.Tuple instead of System.ValueTuple in tuple binding and runtime type lookup.
The existing project property RavenTargetCoreAssemblyName supplies this selection;
there is no new general RuntimeTupleContract option. Only the value-type Tuple
family in NeoCLR.CoreProbe receives tuple special-type recognition. .NET's reference
System.Tuple remains an ordinary reference class. The semantic tuple projection
retains its element names, while emitted fields and constructors use the underlying
constructed target type. TupleElementNamesAttribute is supplied by the target reference.

The matching neoCLR reference/runtime currently supplies arities one through seven.
Empty parentheses retain the existing RuntimeUnitContract (System.Void); one-element
construction is nominal, since Raven rejects one-element tuple type syntax. Wider
flat tuples/Rest and the full .NET ValueTuple library API are not claimed. The native
consumer and metadata/layout checks live in neoCLR docs/experiments/tuples. Main's
90b996b1b fixes metadata tuple projection independently using .NET; it was integrated
here as ee3a23d15. The separate existing native void-pointer correction from
adaaa3db2 is retained with this branch's nominal-Void generic signature handling.

Validation: 41 focused target-branch compiler tests pass, as do nine neoCLR native
consumer cases and 70 importer layout/signature checks. The matching API snapshot
and focused tuple reference rendering pass; no full website build or SDK packaging
is part of this slice.

## Parameter pattern presentation metadata

Patterned parameters emit
`Raven.Runtime.CompilerServices.PatternParameterAttribute(int version, string pattern)`.
The attribute targets parameters and has read-only `Version` and `Pattern`
properties. The compiler embeds its definition when the target references do not
supply the constructor. This requires no new Runtime Contract option or runtime
pattern-matching service.

Version 1 stores the trivia-free Raven binding-pattern spelling, such as `(x, y)`
or `[head, ..tail]`, without the input type or a complete method signature. The
consumer reconstructs detached pattern syntax and combines it with the actual
CLI parameter type. Generic substitutions therefore update the displayed input
type while preserving binding names. The attribute supplies presentation data;
it does not change parameter count, overload identity, named-argument labels,
or invocation behavior. Extraction still belongs to the callee.

Unknown versions, malformed patterns, and duplicate pattern attributes are
ignored for presentation. The normal parameter/type display remains available.
Metadata parsing is bounded. Source comments and whitespace are not preserved.
The present coverage proves tuple, sequence/rest, discard, nested bindings, and
generic input types; nominal deconstruction also round-trips through this
representation. Explicit property patterns also round-trip and display on one
line. Semantic substitution inside typed pattern nodes remains part of the
broader redesign.

Validation: emitted metadata and separate-compilation import/display tests run
on .NET 11; editor signature-help tests run on .NET 10. This does not establish
execution on .NET Framework, NanoFramework, or neoCLR.

## Resolved contract ownership

`CliRuntimeContract.ResolveTypeOf` validates provider visibility, type kinds,
arity, assembly ownership, Current getter shape, and resolver signature. Binding
and emission consume the resulting semantic symbols through the existing internal
compilation entry point. The provider may be source-defined or imported; no host
reflection is needed to validate it. Resolved symbols are not cached in a shared
target singleton or reused across compilation snapshots.

Post-load unit validation uses the compilation's assembly-qualified metadata
lookup, retaining its existing precedence and cache. That helper remains internal;
no lookup/cache API is added to public consumers. Discovered-core identity is
passed to the runtime contract as a name. Emission-option identity comparison and
EmitOptions construction reside in `DotNetCompilationEmitter`. Target and backend
validation use `TargetDiagnostics` for the unchanged RAVT003 diagnostic identity.

This extraction preserves validation rules, emitted core identity, and failure
behavior. Reflection core handles and other .NET dependencies still exist in
shared compilation services, so it does not establish a replaceable target yet.

## Host assembly services

`DotNetCompilationTarget.HostRuntime` owns host assembly registration/loading,
trusted-platform discovery, assembly/path caches, runtime type lookup and host
emit-core discovery. Shared compilation delegates through internal entry points
that preserve setup ordering. This host execution service is distinct from the
semantic-data loader and runtime contract: a host implementation is not evidence
that a type is available in the selected target's reference universe. See
[metadata import ownership](metadata-import.md#host-assembly-service-ownership)
for cache lifetime and remaining reflection dependencies.

## Per-compilation semantic projection

The .NET target owns a single lazy ReflectionTypeLoader and injects it into its
semantic-data loader. Imported members and Compilation's reflection APIs use the
same compilation-bound projector. Allocation is independent of setup, and shared
metadata sessions never own projection caches. This keeps symbol identity local
to each snapshot without adding reflection methods to ISemanticDataLoader.
Reflection APIs and core handles still exist on Compilation; full target replacement
requires further separation.

## Loader dependency composition

The .NET target passes references and metadata import options explicitly to session
setup, then constructs its loader with the session, reflection projector and host
service. Host registration and discovery stay inside the .NET implementation;
Compilation no longer serves as a host-service registry for the loader. The shared
semantic loader interface remains reflection-free. Its .NET implementation still
uses CLI assembly symbols and a compilation-bound projector, so this is preparation
for target replacement, not a new selectable target or cross-compilation mode.

## Loader-owned metadata session reuse

The .NET semantic-data loader validates any previous metadata session offered by
its target. `Compilation` no longer fingerprints PE files or supplies a reuse
boolean. The session retains only a compilation-independent input snapshot and
metadata context; every compilation still creates its own loader and symbols.
This applies to both ordinary .NET and the experimental neoCLR CLI bridge.
A future native loader must own the revision and identity rules of its own inputs.

Reuse requires the same import options, resolved core selection, and ordered
supplied-file stamps (absolute path, existence, length, last-write time). Input
order matters because core discovery and duplicate-identity admission select the
first input. Reordering colliding references now opens a new context, preserving
the old compilation's symbols while exposing the new reference surface. Changes
between host-assisted and explicit-reference imports cannot reuse a session.

This retains the existing size/time revision policy, not content hashing or an
atomic snapshot during concurrent file writes. Host-assisted fallback paths keep
their existing process-wide registration lifetime and are not independently
revision-tracked. Prefer `CompilationOptions.DotNet` with a complete explicit
reference closure for isolated inputs. No emission ABI or neoCLR bridge encoding
changes in this extraction.

## Constructed type emission boundary

`ConstructedNamedTypeSymbol` supplies semantic definitions and type substitutions.
The .NET backend's `ConstructedTypeCodeGenResolver` constructs reflection types from
those facts, obtains source type builders, and maps generic method/async parameters
through the current CodeGenerator. It stores no reflection handles on the semantic
type and adds no reflection methods to the semantic interfaces.

`SubstitutedMemberCodeGenResolver` resolves constructors, methods and fields from
substituted semantic members. Reflection lookup, TypeBuilder mapping, fallback
order and caching remain backend operations. Substituted symbols expose semantic
original definitions and containing types; they no longer accept a CodeGenerator
or return reflection members. All reflection/codegen dependencies have been removed
from the constructed-type symbol file.

`ConstructedMethodCodeGenResolver` handles generic method reflection lookup,
signature matching, runtime argument projection and async/closure parameter mapping.
ConstructedMethodSymbol retains semantic substitution and exposes an internal lookup
for a parameter's existing substitution. The backend does not rebuild that map or
store reflection handles on the symbol; runtime caches stay on CodeGenerator.
ConstructedMethodSymbol no longer contains reflection or codegen dependencies.
Other symbol implementations and shared compiler adapters remain .NET-specific;
this does not make all symbols platform-neutral.
Repeated-emission coverage exercises imported generic containers and tuple fields
with source generic arguments, source field reads/writes, and both source and
imported generic method calls. It checks resulting values, generic arguments and
emitted assembly ownership.

## Backend member entry points

The per-emission RuntimeSymbolResolver exposes constructor, method and field
resolution. Field emitters, including async/closure storage paths, use that backend
entry point. The former field extension in the Symbols namespace is removed; tuple
field unwrapping stays inside FieldSymbolCodeGenResolver, which retains target
metadata proxy checks and source/imported/substituted field dispatch.

The resolver is a .NET backend service returning reflection objects, not a
platform-neutral target interface. PE metadata accessors remain available for
metadata normalization and documentation. Type resolution uses a shared policy-aware implementation, with signature
convenience helpers retained for internal backend callers. Repeated-emission coverage
includes named generic tuple field access alongside imported and source fields.

## Type-resolution usage and Unit policy

RuntimeSymbolResolver passes both RuntimeTypeUsage and treatUnitAsVoid to the same
recursive type resolver. Signature and method-body requests retain their distinct
generic-parameter mapping rules. CustomAttribute requests use host reflection types
needed by custom-attribute construction instead of the target-only metadata fast
path. Attribute emission and specialized method-body callers use this entry point;
the separate attribute/method-body helper methods have been removed.

Usage does not implicitly choose Unit erasure. With treatUnitAsVoid enabled, only
top-level Unit resolves to void; Unit nested inside arrays or generic types remains
a value type. With it disabled, method-body Unit resolution preserves the emitted
or runtime Unit type. Previously the backend facade ignored CustomAttribute usage
and forced void for every method-body Unit request. Focused tests cover both fixes
and nested Unit preservation. This remains an internal .NET backend policy, not a
new source-language feature or a cross-platform type conversion API.

## Semantic attribute usage validation

Attribute target and multiplicity diagnostics read AttributeUsageAttribute from
semantic AttributeData and walk the attribute type's semantic base chain. The
nearest declared usage supplies the complete contract: an omitted AllowMultiple
on that declaration defaults to false, even if a base declaration allows repeats.
Without a declared usage, the existing defaults remain all targets and no repeats.
A visited-type guard bounds traversal for erroneous inheritance cycles.

Imported named types now project their metadata attributes through the existing
PEAttributeDataFactory and cache the resulting semantic AttributeData per symbol,
including constructor and named arguments. Previously these types inherited the
empty default GetAttributes implementation, leaving the host fallback to supply
missing usage information. Attribute decoding retains the existing PE-member
best-effort policy: an unreadable attribute set returns empty.

Validation no longer resolves or loads host CLR types to obtain AttributeUsage.
Source-defined attributes and imported reference-only attributes therefore use the
same semantic path, including inherited usage. Regression coverage uses a CLI
reference assembly that the host rejects for execution and validates its derived
attribute's target restriction and multiplicity through explicit references.
System.AttributeUsageAttribute and AttributeTargets remain .NET contract names;
a future platform may require different mappings. This change removes host
execution from this validation decision, not all attribute-related .NET concepts.

Imported attribute projection also requires marker classification to avoid symbol
display: union detection compares attribute metadata names directly. Formatting a
type to classify it can reenter union detection through attributes on that type
and overflow the stack. A focused regression displays AttributeUsageAttribute
itself and verifies it is not classified as a union.

PE named-type union classification uses the loader's validated PEUnionSymbol shape,
not attribute projection. A marker alone does not establish a supported union;
classification must also avoid eagerly decoding attributes or initializing source
declarations. Incremental demand-driven binding tests cover this distinction.

## Removal of the shared CLR conversion adapter

The public `TypeSymbolExtensions.GetClrType(ITypeSymbol, Compilation)` extension
has been removed. It had no production callers after attribute usage validation
moved to semantic data. Its separate conversion algorithm mixed host lookup with
core metadata lookup, assumed core-only named types, and imposed an implicit
Unit-to-void policy. Keeping it would create a second type-resolution policy
outside the selected backend.

This is an intentional API break. Semantic consumers should use ITypeSymbol and
the semantic model directly. Internal .NET emission uses RuntimeSymbolResolver
with explicit RuntimeTypeUsage and Unit policy; it is not a public replacement
API. Its coverage includes constructed generic types and arrays, verifying that
signature types and their generic arguments remain in the target metadata
context while custom-attribute requests use host reflection types. Existing
Compilation reflection adapters remain and need further boundary work.

## Semantic common-type inference

TypeSymbolNormalization owns the common nominal type selection used by its
GetBestCommonType path. It no longer calls the .NET codegen type helper. The
algorithm reads base types, interfaces, aliases and literal underlying types from
semantic symbols without reflection handles or a code generator. A shared
non-object base is preferred; otherwise a shared interface is considered before
object fallback. Existing interface enumeration order is preserved, including
the existing first-match policy when several interfaces are shared.

This is an ownership change, not a new conversion or target policy. Nullable and
union normalization and binder-specific inference paths are unchanged. The
selected loader still supplies the semantic type hierarchy; a future loader
can supply that hierarchy without implementing .NET reflection conversion.

## Target-owned emission service

Each .NET compilation target composes an internal ICompilationEmitter. Its Emit
operation accepts caller-owned output/debug streams and emission options and
returns EmitResult containing only backend diagnostics. Compilation performs
setup, semantic checks, macro preparation and resolved-contract validation, then
combines its diagnostics with the backend result without assuming success. Macro-plugin compilations use their
own target's emitter and validate against their own resolved metadata context.

The shared Compilation emission path validates the emitting compilation's resolved
target contract even when semantic diagnostics were supplied by a caller. Normal
and macro-plugin emission use that same path. DotNetCompilationEmitter owns .NET
artifact-option validation and selects the output core identity from the metadata
core before constructing a fresh CodeGenerator or writing either stream. It no
longer depends on DotNetCompilationTarget. TargetDiagnostics preserves RAVT003 and
RAVT005 identity across target/backend validation. Setup, semantic and resolved
contract errors still prevent emission. A failure preserves earlier semantic warnings. The
service does not dispose caller streams or cache mutable code generators between
emissions. Unexpected implementation or I/O exceptions retain existing behavior;
this change does not catch all exceptions or promise transactional output.

This is an internal boundary, not an independently selectable codegen plugin.
EmitOptions still carries the existing .NET core-identity option, and Compilation
still composes DotNetCompilationTarget directly. A future target requires its own
loader, runtime/platform contract and emitter together; no cross-target emission
or public backend selection is enabled here.

## Explicit .NET host-service access

The internal Compilation.ResolveRuntimeType overloads have been removed. .NET
codegen and reflection projection now call the target-owned DotNetHostRuntime
service directly. Compilation retains a single internal DotNetHostRuntime
accessor that completes setup before exposing that service, preserving metadata
registration and same-thread setup reentrancy behavior. This is a transitional
.NET implementation entry point, not a platform-neutral semantic API.

A compilation returns the same service on repeated access; derived snapshots
receive their own service. Existing process-wide assembly/path caches remain
unchanged. Host type resolution does not make the host type available to semantic
binding or replace the target metadata core. Focused tests exercise initial
access before metadata lookup and both metadata-type and semantic-symbol mapping.

### Generated record Object equality annotations (2026-09-24)

Generated record Equals copies the parameter type from the selected Object.Equals
contract, including nullable reference metadata, instead of replacing it with an
unannotated Object. Body selection unwraps that annotation to identify the Object
overload. CLI method identity and equality behavior are unchanged: null and unrelated
objects compare false, and matching record components compare equal.

This is a general compiler correction requiring no Runtime Contract configuration.
Regression coverage checks emitted metadata, imported symbols and execution on
.NET 11. It does not widen the separately generated typed Equals parameter, add
nullable value types to other targets or imply .NET Framework/NanoFramework testing.


### Nullable value declaration policy (2026-09-24)

AllowNullableValueTypes defaults to true for .NET compatibility. Targets can opt out
through CompilationOptions.WithAllowNullableValueTypes(false), the project property
RavenAllowNullableValueTypes=false, or --no-nullable-value-types. An explicit
--nullable-value-types re-enables the policy for a compiler invocation. Project
loading/saving preserves the option, and the language-server project fingerprint
includes it so edits invalidate stale semantic state.

Binding reports RAV0407, "Value types can't be declared as nullable", for source
nullable value declarations, including primitive, enum, declared struct and
struct-constrained generic types, nested annotations and explicit Nullable<T>
declarations. Nullable references remain supported. Error bindings prevent emission;
valid metadata and default .NET behavior are unchanged. This is a source declaration
policy, not a ban on imported/inferred nullable values or a metadata rewrite.
Unconstrained generic parameters are not classified as known value types.

This general option does not select any runtime-specific defaults. A target that
lacks Nullable<T> can enable the restriction for an earlier actionable diagnostic;
it must still validate its runtime/importer surface independently. No .NET Framework
or NanoFramework execution is claimed.


### Typed record-class equality annotations (2026-09-24)

Generated typed record-class Equals now takes the nullable record reference. Nullable
record arguments and literal null select typed equality rather than falling back to
Object.Equals. The existing body returns false for null and compares matching record
components. Generated record-struct parameters stay non-nullable values. User-written
Equals methods retain their annotations and suppress duplicate synthesis.

The emitter also ignores top-level nullable reference annotations when matching
interface parameter/return slots, consistent with binding and CLI reference type
identity. It preserves nullable value wrappers. The mismatch previously caused a
TypeLoadException after changing the synthesized Equals signature: IEquatable's
implementation was no longer emitted with the required dispatch flags.

No Runtime Contract configuration changes. Tests cover overload selection, reflection
and reimported metadata, typed/interface invocation, explicit declarations and generic
record-class construction on .NET 11. Nested generic signature matching is not redesigned
here. The .NET comparison follows Microsoft's
[record reference](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/record)
and is separately checked on .NET 10 by the integration baseline. No .NET Framework
or NanoFramework execution is claimed.


### Record comparison operator annotations (2026-09-24)

Generated record-class `==` and `!=` accept nullable references on both sides,
matching their existing null and value-equality behavior and the .NET record
contract. Record-struct operands remain values. Explicit operators retain their
authored annotations and suppress synthesis, including mixed annotated operands.
Internal equality null guards use Object.ReferenceEquals; invoking overloaded
equality here can recurse or let custom operators change the null test.

No Runtime Contract option changes. Tests cover emitted and reimported metadata,
null/equal/different operands, explicit declarations and custom-operator isolation
on .NET 11. The integration baseline compares .NET 10. No .NET Framework or
NanoFramework execution is claimed. Nullable value support is unchanged.

## Native implementing-type Self (neoCLR experiment)

`CompilationOptions.RuntimeSelfTypeContract` selects a fieldless public marker by
assembly and metadata name. MSBuild projects set `RavenSelfAssemblyName` and
`RavenSelfType`, with `RavenTargetPlatform=NeoCLR`; neoCLR uses `NeoCLR.CoreProbe` and
`System.Runtime.CompilerServices.Self`. Both settings are required. This option is
exclusive to the neoCLR experiment and is disabled for ordinary CLR compilation.

Capital `Self` is resolved in interface member signatures to this transport marker;
in a concrete type it denotes that declaring type. Lowercase `self` retains its
instance-value meaning. Interfaces gain no hidden generic parameter. Constrained
member lookup substitutes Self with the constrained type parameter, including
static properties and operators; implementation checking substitutes the concrete
implementer. Arrays and constructed generic signatures substitute recursively.
Semantic member results expose the substituted signature. Emission retains the
original interface member and marker so the neoCLR importer can preserve native
Self dispatch. Inherited interfaces such as `ComparableTo<Self>` are concretized
on implementations. This metadata is transport, not directly executable CLR IL.

Calls to Self-dependent members through erased interface receivers are rejected.
Self-bearing interface methods with independent method type parameters are not
yet supported by this projection. The neoCLR application importer currently admits bounded numeric static generic
consumers and a bounded direct application Clone contract for class/struct
implementations. `Copy<T>(value: T) -> T where T: Cloneable => value.Clone()` binds
and emits a constrained call; the neoCLR importer preserves borrowed native Self
dispatch. General instance Self consumers and arbitrary constrained application
generics are not established by this compiler support. Native runtime validation
is authoritative for storage, conformance and dispatch restrictions.

`NativeSelfContractTests` covers nongeneric interface metadata, concrete Self,
constrained properties/operators and generic instance cloning, invalid implementations, erased calls and the
ordinary CLR opt-out. The neoCLR repository exercises the actual Number library
and records runtime/importer validation separately. No keyword token or TextMate
rule is added: Self remains a type identifier. Language services use normal binder
and semantic APIs; special completion suggestions for Self are not yet provided.


The subsequent neoCLR library migration replaces System.Clonable<T> with
nongeneric System.Clonable and Clone() -> Self, using these same settings and
emission rules. The target's class/struct cloning consumer now imports the actual
library contract. Update conformances and generic bounds to Clonable and rebuild
matching references. This is a neoCLR library/API change, with no additional
compiler option or ordinary CLR behavior change. Copy depth remains an explicit
implementation contract; Self guarantees only the result type relationship.

Known migration diagnostic boundary: an obsolete `Clonable<T>` generic bound can
reach the checked neoCLR importer, which rejects it as an unsupported bound.
Earlier generic-arity diagnostics are a deferred general compiler candidate;
validate independently before integrating outside this experiment. Missing
cloning bounds, wrong Self results and erased calls are compiler diagnostics.

Self inheritance remains an isolated neoCLR experiment, separate from the compiler
boundary/multi-target refactor. Self is anchored to the class declaring conformance:
Derived inheriting Base : Clonable retains Base-returning Clone, but does not
satisfy a native Self bound as Derived. Redeclaring Clonable requires a matching
Derived result. An explicit `func Clonable.Clone() -> Self` implementation can
coexist with the inherited Base method; virtual overrides keep the Base signature.
Explicit and inferred generic calls validate this rule under the configured Self
Runtime Contract, with no additional target option. Metadata retains the declared
return types and explicit interface mapping. These are bounded feature changes;
broader neoCLR target integration follows the separate multi-target refactor.

Validation for the inheritance follow-up: 12 NativeSelfContractTests and 17 nearby
constraint/declaration regressions pass. The neoCLR repository separately records
native runtime tests and a System.Clonable consumer covering inherited base results,
virtual overrides, explicit derived mappings and invalid derived bounds/results.
No new keyword, TextMate rule or language-service-specific state is introduced;
existing semantic diagnostics carry the rule. LSP execution was not rerun.

### Transitional neoCLR CLI compatibility (2026-09-30)

`Targets.NeoClrCliCompatibility` owns the existing experimental assembly-name
rules. `DotNetRuntimeContract` preserves legacy core-name selection for inhabited
function results and the tuple family; `NeoClrCliRuntimeContract` selects those
representations directly. The PE loader uses the compatibility helper to recognize value-type `System.Tuple`
imports; shared bound-node facts use it to classify terminal `System.Fault` calls.
This extraction preserves behavior and adds no public options.

Under the legacy .NET contract, the configured core name must exactly equal
`NeoCLR.CoreProbe` for unit-returning functions to use `Func<..., Unit>` and tuple
construction to use `System.Tuple`. Other core names retain `Action` and
`System.ValueTuple`. Explicit neoCLR selection chooses the inhabited result and
tuple representations and separately validates the profile configuration. Imported tuple aliases
require that exact assembly name and a value type. Terminal Fault recognition
still depends on the method's own assembly, namespace-member marker and signature,
independently of the configured core. Neither importing the assembly nor these
compatibility rules constitute explicit target selection or capability validation.

The explicit profile below adds target identity and preset defaults while retaining
these compatibility triggers. No native neoCLR backend or cross-compilation is introduced.


### Explicit platform option: .NET foundation (2026-09-30)

`CompilationOptions.TargetPlatform` and `WithTargetPlatform(...)` identify the
coherent loader/runtime-contract/emitter selection. `TargetPlatform.DotNet` was the initial supported value and remains the default
for constructors and the
`CompilationOptions.DotNet` preset. The latter still uses only supplied references;
selecting a platform does not locate a reference framework, set a core assembly,
or overwrite separately configured runtime contracts.

All immutable option copies preserve this value. Unsupported enum values produce
unsuppressible `RAVT005` before reference initialization; emission returns that
error without changing PE/PDB streams, including when callers supply diagnostics.
Incremental metadata/declaration reuse and semantic-state transfer reject platform
changes. Workspace option changes can recover after an invalid platform selection.

This is the .NET API foundation, not a completed multiple-platform implementation.
At that initial checkpoint there was no NeoCLR preset or project selector; the
subsequent slices below add them. The existing neoCLR CLI compatibility rules still apply within the
current pipeline; this option does not enforce a strict .NET capability matrix.
A supported neoCLR CLI profile, project configuration and caller migration must be
defined together before replacing those rules with explicit target enforcement.


### Project platform selection (2026-09-30)

Projects may select the supported pipeline with
`<RavenTargetPlatform>DotNet</RavenTargetPlatform>`. An absent or blank value keeps
the existing .NET default. Names are case-insensitive and surrounding whitespace
is ignored. The property is evaluated by MSBuild, including imports and property
expansion. Saving a project writes the canonical platform name and loading it
again preserves the selection. `TargetFramework`, metadata-core selection and
explicit references remain independent; this property does not install or discover
reference assemblies or replace existing core/contract settings.

Both `DotNet` and `NeoCLR` are accepted names. Numeric enum values, combined names
and unknown names are rejected during project evaluation with an
`InvalidDataException` naming `RavenTargetPlatform` and its value. The compiler
driver reports this as a project-loading error and exits unsuccessfully before
emission. This project-format validation is distinct from RAVT005 for unsupported
platform values supplied through the compiler API.

The external neoCLR props file currently also selects the separate Self experiment.
It must not be copied wholesale into a preset on main. The profile below defines the supported configuration independently of that
feature. Migration of existing runtime consumers remains separate work. Existing integration props
without RavenTargetPlatform retain their prior behavior.


### Experimental neoCLR CLI preset (2026-09-30)

Use `CompilationOptions.NeoCLR` or `<RavenTargetPlatform>NeoCLR</RavenTargetPlatform>`
to select `TargetPlatform.NeoCLR`. This names the existing CLI bridge, using the
same CLI loader/emitter implementation as the .NET pipeline. Supply matching
`NeoCLR.CoreProbe` reference artifacts explicitly; the preset downloads nothing
and does not select the compiler host's framework references. Projects also
suppress default .NET prelude imports. No reference artifact is bundled here.

| Contract or policy | Preset default |
| --- | --- |
| Metadata and emission core | `NeoCLR.CoreProbe`, explicit-only |
| Unit | `NeoCLR.CoreProbe:System.Void` |
| Iteration | `System.Collections.Iterable<T>` / `Iterator<T>`, `GetIterator`, `MoveNext`, `Current`; array shape `System.Array<T>` |
| Propagation | `System.Propagatable<T, E, R>` (three-parameter interface) |
| typeof | `System.Introspection.TypeInfo`, `System.Runtime.RuntimeContext` |
| Characters | Grapheme representation enabled; Unicode-scalar option disabled |
| Async | Heap state machines and cancellation propagation enabled; exception capture disabled |
| Source nullable values and array covariance | Disabled |
| Framework projections | None |

These are existing compiler settings, not new runtime guarantees. Explicit project
properties override individual defaults; partially specified contract properties
inherit the remaining preset fields. API `With...` methods replace the requested
option as usual. Core and unit changes that contradict the fixed CLI profile
produce RAVT003 before loading references or writing output. Simply changing a
.NET options object's TargetPlatform to NeoCLR does not apply preset defaults;
start from `CompilationOptions.NeoCLR`. Missing references still produce RAVT004.

The preset does not include Self or record-equatability/hash mappings. Their
runtime props exist outside this main-line configuration, and consumers depending
on them must retain the appropriate feature compiler/configuration. Full feature
availability checks, native metadata/codegen, matching runtime execution tests and
consumer migration are pending. Non-core feature/contract overrides are not yet
validated against a complete neoCLR capability matrix. Existing per-contract
symbol validation remains in effect.

Explicit NeoCLR selection now chooses the inhabited function-result and tuple
representations. Legacy core-name triggers and imported Fault/tuple recognition
remain for compatibility until controlled callers migrate; ordinary .NET defaults
are unchanged. Selecting DotNet does not yet prohibit all legacy neoCLR settings.


An exploratory preset run used the neoCLR `feature/function-types` development
bundle; it is not a supported-feature acceptance gate. Native Function types remain
deliberately deferred until the metadata layer and complete compiler support exist. Native Function support is not on
neoCLR main at e4f6fe41. Raven main's bridge support and that runtime feature branch
must not be conflated. See the [bridge inventory](https://github.com/marinasundstrom/raven/blob/main/docs/compiler/neoclr-cli-bridge.md) for the
branch-qualified result and eventual native metadata replacement direction.


### Function/structural-type branch isolation (2026-09-30)

The author clarified that native Function/structural-type work stays on feature
branches in both repositories. Raven's `codex/neoclr-structural-types` retains the
experimental inhabited unit-function transport, paired with neoCLR's existing
`feature/function-types`. Main keeps ordinary function syntax and nominal .NET
Func/Action behavior, including Action for unit-returning source functions.
Neither a NeoCLR preset nor the legacy core name enables structural Function
transport on main. This supersedes earlier recommendations to retain that
native-specific behavior on main. General target plumbing and other contracts
remain shared; native metadata/full compiler support are prerequisites for promotion.

Validation: 40 focused function, tuple and NeoCLR profile tests passed on the .NET
11 host; after simplifying the nominal-transport assertions, all five transport
cases passed again. This is compiler/CLI validation, not native runtime execution.

### Self target gate and nominal callback correction (2026-09-30)

Native Self requires both `TargetPlatform.NeoCLR` and an explicit
`RuntimeSelfTypeContract`. The .NET target reports RAVT003 before reference loading
or output writes when supplied that contract. Direct semantic queries also keep
ordinary user-defined `Self` types intact on .NET. The NeoCLR preset leaves the
marker opt-in; it does not promise complete native metadata/backend support.

Rebuilding neoCLR **main-based** runtime sources disproved the earlier assumption
that inhabited unit-function transport was structural-only: its nominal Func ABI
requires the same representation. Removing it makes callback types inaccessible
because that core exposes Func with an inhabited result, not Action. Retain this
bounded delegate transport policy on the shared line; structural Function identity,
assignability and introspection remain feature-branch work in both repositories.
This corrects the preceding branch-isolation note. No structural runtime code is
needed by Self integration.

Validation of the gate: 40 focused Self, target configuration, preset, delegate
transport and option-copy tests pass on the .NET 11 host. The Self tests use an
isolated synthetic CLI core; external runtime acceptance is recorded in neoCLR.

Projects can disable a preset typeof mapping by explicitly setting all three
`RavenTypeOfAssemblyName`, `RavenTypeOfInfoType`, and `RavenTypeOfContextType`
properties to empty values. Omitted properties retain preset defaults; partial
nonempty overrides still inherit the other fields. Runtime declaration builds use
this distinction when they define the contract types themselves.

The final focused gate passes 107 tests, including all 67 project-system cases
(the pre-change project baseline passed 66). The main-based native integration
rebuild exposed and motivated the explicit-empty typeof regression test.

Integration gate: `scripts/test-ci.sh` passes 315 compiler tests (.NET 11), 73
core and 256 language-server tests (.NET 10), with three existing LSP skips.

### Selected CLI runtime contracts (2026-09-30)

Each compilation selects an immutable .NET or neoCLR CLI runtime contract from its
platform option. The .NET implementation preserves ordinary representations and
the legacy probe-core transport triggers, and rejects native Self configuration.
The neoCLR implementation owns its profile check, inhabited delegate results,
tuple family, and Self availability (still requiring an explicit marker mapping).
Compilation's Self query delegates to that selected contract rather than checking
the platform enum itself. Selection does not load references or resolve symbols.

`CliRuntimeContract` shares the current special-type names, typeof handle protocol,
and CLI core/unit/marker validation. It is deliberately a transport implementation,
not the abstract contract for a future native loader/backend. Both concrete
contracts still use the same .NET metadata loader and emitter. The selection does
not introduce independent backend choices, new public options, or a capability
registry. No syntax, bridge encoding, configuration diagnostic or emission ABI
changes are intended. Native Self is not implicitly enabled by the neoCLR preset.

### Contract validation before backend dispatch (2026-09-30)

Resolved contract validity belongs to the shared emission pipeline; each backend
owns its artifact-specific option checks. Supplying cached semantic diagnostics
cannot establish or bypass the resolved runtime contract. Missing typeof providers
and invalid unit shapes reject before either caller-owned stream changes, retaining
supplied warnings and stream positions. Existing emission-option conflicts remain
backend errors. These internal ownership changes preserve public options, error
precedence and output ABI for .NET and the neoCLR CLI bridge. The guarantee covers
validation failures, not rollback after arbitrary I/O or code-generation failures.

### Provider-owned namespace-member discovery (2026-09-30)

Shared namespace-member lookup uses the internal `INamespaceMemberContainer`
capability instead of recognizing `PENamedTypeSymbol`. It identifies candidate
containers; the compiler still controls static-member promotion, duplicate removal,
merged namespaces, lookup precedence and namespace-member import options.
Providers can report this fact without supplying CLI attributes or binding
`GetAttributes`. No new public symbol API or loader registry is introduced.

The PE implementation preserves the existing exact/suffix name recognition for
`TopLevel` and `TopLevelAttribute`. Raw custom-attribute inspection stays within
that provider. Synthesized Raven namespace containers and source attribute-syntax
recognition keep their existing paths, avoiding recursive source-attribute binding.
An unused semantic attribute classifier was removed during extraction.

This capability does not grant terminal-call semantics. neoCLR Fault recognition
still requires its exact runtime marker, owner, assembly and signature checks in
the CLI compatibility policy. A custom namespace's TopLevel marker can permit
ordinary namespace lookup without making Fault terminate control flow. A future
native provider/backend must represent namespace ownership and terminal behavior
explicitly; the current container-based projection is not a universal native API.

### Provider-owned nested-type discovery (2026-09-30)

Recursive type traversal uses the optional internal `INestedTypeDiscovery`
capability rather than recognizing PE symbol classes. Providers return only nested
type candidates, avoiding ordinary member/signature materialization. Types without
that capability retain the existing `GetMembers` fallback. The PE implementation
keeps its lazy nested-type cache and reflection operations private.

Constructed symbols delegate discovery to the original definition's capability
when available. For PE types this preserves the existing nested declaration
identities; it does not substitute a closed owner's arguments into those discovery
candidates. Without a provider capability, constructed symbols retain their normal
member-substitution fallback. Discovery is not a replacement for semantic member
lookup on a closed generic type, and the public `GetTypeMembers` API is unchanged.

The same traversal now accepts in-memory/non-PE providers. No CLI bridge encoding,
source syntax, target feature rule or emitter behavior changes. Both current
targets continue using the PE implementation; native metadata can later implement
this capability without exposing reflection handles or eagerly loading methods.

### Type-level extension discovery ownership (2026-09-30)

Shared type queries consume the internal `IExtensionTypeInfo` capability for a
provider's receiver type and member-level extension presence. The PE provider
continues interpreting CLI extension attributes and markers; constructed symbols
substitute the receiver and forward member-level presence. Source declarations
retain their compiler-owned receiver facts. A receiver is a discovery hint, not
proof that every member is applicable to that receiver; binding still checks
individual members and generic constraints.

This is a type-level boundary only. Member-level extension decoding still has
PE-specific paths. It neither enables native neoCLR extension metadata nor makes
CLI marker encodings a requirement for future providers. Public semantic APIs,
CLI encodings and target feature availability are unchanged.

### Member extension receiver ownership (2026-09-30)

The core compiler must not depend on the PE symbol model. PE symbols are the
.NET target's implementation, currently reused by neoCLR's temporary CLI bridge.
Shared method/property receiver queries now ask `IExtensionReceiverResolver` for
provider-owned semantic facts. Method lookup finds the resolver on the original
declaration's containing type (or the current containing type) and passes the
actual member, including constructed views. The provider owns any mapping needed
to interpret that context; core lookup does not remap the returned receiver.

PE marker lookup, constructed-owner marker substitution and CLI ordinal-based
receiver parameter remapping live in `PENamedTypeSymbol.ExtensionReceivers`.
The general type-substitution algorithm remains shared. Explicit receiver
parameters, operator receivers, source extension rules and property accessor
precedence remain compiler-owned. This removes PE dependencies from these receiver
queries, not from all shared symbol queries; identity and fast signature lookup
still require further boundary work. No public API or extension encoding changes.

### Shallow method lookup identity ownership (2026-09-30)

`IMethodLookupIdentity` lets a symbol provider supply an opaque, provider-qualified
method declaration key without resolving parameter types. Shared lookup unwraps
declarations and adds method arity/type arguments; it does not know CLI module
version IDs, tokens or reflection's parameter-count fallback. The PE implementation
owns those details. Symbols without the capability retain signature-based lookup.

These internal keys serve shallow candidate deduplication, not symbol equality,
persistent metadata identity or a public cache API. Their string format is not a
contract. The PE fallback still uses containing type, name and parameter count
when reflection cannot supply a token, with the same pre-existing risk of merging
same-count overload candidates in that fallback case. This slice preserves that
behavior rather than imposing it on future providers. Fast signature queries and
other PE-dependent identity consumers remain separate work.

### Lazy parameter fact ownership (2026-09-30)

`IMethodParameterInfo` supplies parameter count, individual semantic parameter
types and optional/variadic usage facts. `MethodParameterQueries` uses this
internal capability for semantic-model count, required-count and type queries,
with the existing full-symbol path for symbols without the capability. A failed
provider query remains unavailable; it does not trigger full `Parameters` access.
Source and constructed symbols retain their existing signature/substitution path.

The PE adapter retains reflection-backed lazy loading, by-reference element-type
normalization and optional/default/ParamArray decoding. It also preserves the
existing unreadable-count fallback; the interface itself allows providers to
report count failure. These details are not requirements on native metadata.
PE-specific fast conversion scoring remains in the semantic model for a later
slice; this is a parameter-fact boundary, not complete signature independence.
No public cache API, syntax or emitted metadata changes.

### Available-state conversion shortcut ownership (2026-09-30)

`IParameterConversionClassifier` separates encoded-parameter conversion shortcuts
from shared candidate ranking. A provider returns a conversion category or
reports the shortcut unavailable. Shared `ParameterConversionQueries` retains
identity/numeric/object ranking and distinguishes a classified rejection from an
unavailable answer, which permits normal symbol-based conversion fallback.
`Compilation.ClassifyConversion` remains the general conversion authority.

PE owns the existing CLI special-type-name mapping, rank-one array name matching
and implicit numeric shortcut table. Semantic-model invocation scoring now uses
`IMethodParameterInfo` for lazy providers and the classifier when available;
extension receiver scoring uses the same classifier. It no longer selects these
paths by testing for `PEMethodSymbol`. Source/full-signature behavior and scoring
weights are unchanged. This is an internal available-state optimization contract,
not a new language conversion rule or a requirement that native providers use
CLI names. Other runtime-specific semantic paths remain future work.

### Provider overload-priority fallback (2026-09-30)

`IMethodOverloadPriority` supplies optional priority facts when ordinary semantic
attributes do not provide them. Shared overload resolution retains source syntax
handling, semantic attribute precedence, declaring-type/extension grouping and
priority comparison. It asks the method or its original definition for the
fallback without depending on PE symbols or reflection.

PE owns raw OverloadResolutionPriorityAttribute decoding and GetBaseDefinition
lookup, preserving existing precedence. Native providers can
supply their own priority facts without synthesizing a CLI attribute. This slice
does not redesign attribute-based priority policy or change override semantics;
other target-specific policies remain future boundary work.

The extraction exposed a metadata-only reflection bug: GetBaseDefinition was
called even for nonvirtual methods, so MetadataLoadContext threw before attribute
reading. PE now reads nonvirtual/new-slot declarations directly. Slot-reusing
overrides still report no fallback fact if base-definition reflection is
unsupported; determining that inherited fact needs a metadata-aware override
relationship and is not guessed here.

### Parameter default-kind ownership (2026-09-30)

`IParameterDefaultValueInfo` distinguishes a provider-synthesized type default
from an ordinary constant. Optional-argument binding and parameter display consume
the fact without inspecting PE symbols. `HasExplicitDefaultValue` still controls
whether the parameter has a default; the capability cannot make a required
parameter optional. Source default syntax and Option.None behavior are unchanged.

PE retains CLI optional/default attribute decoding and lazy default-value
construction. The capability does not prescribe CLI attributes or boxed default
representations for native providers. Existing constructed-parameter behavior is
unchanged; forwarding all provider facts through symbol wrappers is separate
work. No public symbol API or emitted encoding changes.

### Array semantic shape ownership (2026-09-30)

Shared `ArrayTypeSymbol` derives from `Symbol`, not `PESymbol`. Its base type can
provide `IArrayTypeProvider` to supply additional interfaces and member lookup for
the actual array rank/element type. Shared code retains interface deduplication,
inherited-interface closure and array-specific interface caching. Members retain
their provider-declared owners for dispatch. Assembly/module ownership remains
namespace-based; array documentation remains absent as before.

PE owns CLI vector collection interfaces and RuntimeIterationContract iterable/
array-shape projection, including validation and rejection of invalid explicit
shapes without host-interface fallback. Ordinary CLI array member lookup still
avoids loading vector interfaces unnecessarily. A base without the capability
provides only its declared members/interfaces; shared code no longer synthesizes
.NET collection interfaces merely because an assembly can resolve their names.
Native targets can define different rank-dependent shapes through the capability.
This does not provide native storage or emission support.

### Constructed parameter default-kind forwarding (2026-09-30)

Constructed method parameters and substituted containing-type parameters now
forward `IParameterDefaultValueInfo` from their underlying parameter, including
through nested wrappers. They continue substituting the parameter type normally;
the provider's default-kind fact is preserved rather than re-decoded or inferred
from the boxed value. Wrapping an ordinary literal default does not turn it into
a type default, and providers without the capability retain the previous behavior.

Previously, these wrappers copied HasExplicitDefaultValue/ExplicitDefaultValue
but dropped the type-default distinction. Binding a wrapped nonliteral struct
default could therefore report an invalid optional literal, and display could
lose `default`. The correction is shared across providers and does not change CLI
encoding or native target availability. Other provider capabilities and synthesized
parameter adapters require their own forwarding audit.

### Public operation facts for alternate emitters (2026-09-30)

Binary operations now publish resolved operator kind, lifting, checked semantics and
operator method; invocation Instance now publishes the bound receiver, with null for
static calls. These are shared compiler facts, with no new Runtime Contract option or
neoCLR-only semantic policy. Existing .NET and CLI-bridge emission remain unchanged.
Alternate emitters must reject unsupported flags/operators rather than infer behavior
from syntax. The independent neoCLR metadata library remains outside compiler symbols
and bound nodes; a consumer adapter owns the conversion into its builder objects.

### Independent metadata consumer probe (2026-09-30)

`tools/NeoClrMetadataProbe` is an explicit development executable referencing the
independent neoCLR metadata project. The frontend currently uses the normal .NET
runtime contract and existing .NET loader to bind an API-produced PE dependency. A
bounded operation adapter emits the application directly into native format 5 through
the metadata library, and the native runtime executes it. This is bootstrap evidence
for Int32 functions/calls; it is not a new TargetPlatform value or installed backend.

The supported source, command, diagnostics and exclusions are documented in the
[probe](../../../tools/NeoClrMetadataProbe/README.md). This staged integration keeps
compiler projections and lowering in Raven, format/model code in the library, and
modern .NET as the default. A native semantic loader and target composition remain next.

### Metadata probe reference imports (2026-09-30)

The opt-in `NeoClrMetadataProbe` imports read-only callable definitions through the
separate library's `ImportReference(definition, dependencyCoreLibrary)`. Its explicit
core assertion is the fixture's .NET core identity, not inferred native platform
compatibility. The emitter no longer receives the producer graph. Static Int32 calls
retain native format-5 identity/naming; ordinary .NET binding/emission is unchanged.
No native Runtime Contract/provider is registered. See the probe README and validation
report for result 42 and rejection checks; generic/structural imports remain excluded.

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

### Artifact backend selection

`EmitOptions.WithBackend(ICompilationEmissionBackend?)` selects an artifact producer
for `Compilation.Emit`. It does not change `CompilationOptions.TargetPlatform`, the
metadata importer, core identity or Runtime Contracts. The compiler validates these
contracts before calling the backend, including when semantic diagnostics were supplied
internally. `WithTargetCoreLibraryIdentity` preserves the backend; `WithBackend(null)`
restores ordinary target emission. The backend receives prepared source and returns
`EmitResult` with backend diagnostics only. Its public constructor normalizes a default
diagnostic array to empty. Implementations own capability checks and per-call builder
state, must leave caller streams open and must not recursively invoke Compilation.Emit.

The optional native implementation and its restrictions are documented in the
[neoCLR bridge](neoclr-cli-bridge.md#shared-emission-pipeline--2026-09-30).

### Shared method bodies

The internal linear-body model now serves eligible release .NET static Int32 methods
and the native adapter. Backend selection and Runtime Contracts remain distinct.
Backend adapters own method-handle and Console mapping; unsupported .NET bodies retain
the existing general generator, while unsupported native source produces diagnostics.
Debug/PDB emission stays on the established .NET path. See the
[scope and validation](neoclr-cli-bridge.md#shared-linear-body-lowering-and-backend-method-builders--2026-09-30).

The native codegen migration now shares compiler-lowered linear bodies rather than
source-operation rewriting. Runtime Contract and binding selection are unchanged;
see [the staged migration](architecture/native-target-codegen-migration.md).

The bounded callable reference table is scoped to one emission and delegates .NET
resolution to the existing target-aware resolver. It changes neither Runtime Contract
selection nor semantic reference binding; native imports retain explicit dependencies.

Shared source callable plans distinguish logical assembly ownership from CLI carrier
ownership. The existing .NET builder still chooses emitted names, attributes and core
types; native capability policy remains in its adapter. Target selection is unchanged.

Shared static type plans retain symbol ownership and metadata naming. .NET construction
still receives the existing flags and target-aware base resolution; native capability
validation remains adapter-owned. No Runtime Contract or semantic import change is implied.

Shared Int32 locals use MethodGenerator's target-aware type resolution on .NET; native
slots use the metadata writer's Int32 representation. Existing Runtime Contract and
loader selection are unchanged. General locals and address-taking remain outside the
bounded native backend.

Shared control-flow planning uses compiler label identities and backend label handles.
Native Boolean comparison results map to CLR stack conditions in the .NET adapter;
this adds no Boolean method signature or runtime-helper mapping. Debug/PDB fallback
and semantic Runtime Contract selection remain unchanged.


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

### Target-neutral metadata emission direction — 2026-10-01

The compiler-owned abstraction should support both .NET and neoCLR through typed
references/declarations and target adapters, with optional instruction/metadata
capabilities exposed explicitly. Neither Reflection.Emit handles nor native metadata
builders should become the general shared contract. Current native artifact selection
is still a backend override with hosted binding, not completed target composition.
The author's renewed CLI compatibility and later codegen-performance requirements
are recorded in the native-target migration plan; no configuration changes are made.

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

### Declaration-category admission — 2026-10-01

The internal backend profiles now govern logical assembly functions, static methods
and static types before shared builder use. Profiles are reused for body admission;
physical CLI carriers remain .NET policy and native assembly ownership is preserved.
No Runtime Contract option changes. See the matching declaration-category section
in neoclr-cli-bridge.md for implemented scope and remaining categories.


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


### Shared signed remainder — 2026-10-01

Int32/Int64 '%' now passes through the shared lowered-body planner, both capability
profiles and both instruction adapters. The independent writer supports Rem and the
Remainder helper using existing CLI/native rem encodings. No Runtime Contract option,
binder rule or metadata category is added. Ordinary results keep the dividend's sign;
zero divisors fault. Native minimum/-1 faults match the tested CLR; that CLR edge is
platform-sensitive and universal host equivalence is not asserted. The .NET Debug
fallback remains tested. Unsigned/floating arithmetic, exceptions and broader metadata
loading remain future work. CLI reference projection/#Neo limitations are unchanged.


### Integer bitwise emission — 2026-10-01

The shared lowered-body planner and both backend profiles now admit matching Int32/
Int64 AND, OR and XOR. Each adapter selects its existing instruction encoding; no
Runtime Contract configuration, binder rule or signature format changes. The separate
metadata library adds And/Or/Xor opcodes and BitwiseAnd/BitwiseOr/BitwiseXor helpers,
with typed stack validation. Negative values retain their fixed-width bit patterns.
At that integer-only checkpoint, Boolean/enum bitwise operations remained outside the producer; .NET's
general path retains its existing support. The CLI projection/#Neo bridge is unchanged.


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


The 2026-10-01 shared root/instance declaration slice adds internal backend capability
categories, not a new Runtime Contract option. Ordinary .NET emission shares primitive
nonvirtual instance plans; native source admission stays gated until complete class
bodies exist. See [the bridge record](neoclr-cli-bridge.md#shared-root-and-instance-declaration-contracts--2026-10-01).

The subsequent Order slice admits explicit root constructors and mutable primitive
auto-properties in the native backend. This is backend capability growth with no new
Runtime Contract option. See [Order execution](neoclr-cli-bridge.md#unchanged-order-source-executes--2026-10-01).

Root-class local admission is also backend-owned and explicitly enabled by the two
shared adapters. No Runtime Contract option is added. Nominal parameters/results and
generic locals remain future emission work.


Development checkpoint (2026-10-01): owned root-class signature admission is an internal
codegen capability enabled by the .NET and experimental neoCLR adapters, not a new Runtime
Contract configuration option. Default constructors and primitive field/property
initializers share the canonical compiler initialization plan. See
[the current integration scope](neoclr-cli-bridge.md#owned-nominal-callable-signatures--2026-10-01)
for signature ownership, reference-projection and initialization limits.


Owned nominal instance-field emission (2026-10-01) reuses the existing root-class
signature capability and source identities. It requires no Runtime Contract option.
The bounded neoCLR adapter supports mutable explicit fields and private storage with
owned class types; nominal property metadata, nullable and imported class contracts
remain separate work. See [the bridge contract](neoclr-cli-bridge.md#owned-nominal-field-storage--2026-10-01).


Owned nominal property emission (2026-10-01) also uses the existing root-class signature
capability with no new configuration. Auto/computed/explicit accessors preserve the
same owned identities as fields and methods. Refreshing a provisional auto-property
initializer is a shared binder correction and applies independently of target selection.
See [the property bridge contract](neoclr-cli-bridge.md#owned-nominal-property-emission--2026-10-01).


Explicit parameterless System.Object base initialization (2026-10-01) uses the same
bounded root-constructor contract as an implicit base call; no configuration changes.
Admission checks the bound constructor identity and empty source/bound arguments.
User-defined base initialization still requires a future native constructor contract.


Readonly instance storage (2026-10-01) requires the updated neoCLR runtime and metadata
library but no new Runtime Contract option. Private val storage and stored val properties
map IsReadOnly to CLI InitOnly and native field flags. Ordinary constructor writes use
the existing initialization plan. Managed field addresses outside the declaring
constructor are readonly; objects referenced by those fields can remain mutable.
See [the runtime compatibility boundary](neoclr-cli-bridge.md#readonly-instance-storage--2026-10-01).

Shared emission capabilities now explicitly admit vector types independently of root-class
signatures. Vector signatures, locals, storage and typed operations use backend maps;
no Runtime Contract configuration is needed. See [the bridge contract](neoclr-cli-bridge.md#shared-vector-emission--2026-10-01) for current limits.

Array for loops with an exact element local now use ordinary shared lowering before
emission. No target-specific iteration rewrite or Runtime Contract switch is introduced;
retained enumerator loops remain owned by general .NET codegen.

Indexed property accessors have an explicit shared emission capability. Signature and
body planning use ordinary callable contracts; receiver/index/value order is shared
across backends. No Runtime Contract option changes. See [indexer emission](neoclr-cli-bridge.md#shared-indexed-property-emission--2026-10-01).

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

### Explicit CLR source-void alias (development, 2026-10-03)

`MapClrVoidToUnit: true` opts a .NET bootstrap compilation into binding source CLR
`System.Void` type syntax as Raven's unit value. This applies to declarations and type
expressions, including generic arguments. Imported CLR no-result method returns retain
void. Unconfigured source binding and native unit profiles remain unchanged.

In this mode, AssemblyName/TypeName may identify a public, nongeneric, fieldless value
type in an ordinary referenced service assembly; no replacement core library is required.
CLR System.Void, reference/stateful types, missing selections and incompatible target/core
combinations reject with RAVT003. A configured target core, if present, must agree with the
unit assembly. Binary consumers must rebuild for the extended record constructor.

The .NET backend uses its intermediate unit carrier and projects it into the explicitly
resolved target assembly scope. Locals, stored/generic signatures, interface relationships
and MethodImpl references use the selected value type. Ordinary no-result returns stay
CLI void. This extends the existing unit projection rather than making CLR void an
inhabited CLR type. No runtime reflection layer or native metadata-format extension is added.

Focused C# tests execute an interface with out-unit storage, generic collections and
no-result calls and inspect the emitted scopes. Existing unit/core-retargeting and imported
interface tests cover unchanged profiles. Full custom CLR-array interface adaptation is
separate from this contract.


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

### Source-owned array declaration caches (2026-10-04)

A configured `RuntimeIterationContract.ArrayShapeTypeName` may belong to the
current source assembly. Reentrant metadata-name lookup during declarations must
not permanently cache its absence. Array interface projection stays provisional
until source declarations complete, using an internal provider readiness contract;
finished compilations retain normal caching. The default .NET array interfaces and
Reflection/Emit backend are unchanged. No metadata encoding or runtime change is
required.

The focused regression reads an imported vector's `Length` in an inferred static
initializer, then converts an imported vector to a source-owned interface in a
method. Provider tests also cover provisional empty interface sets. Direct
conversion inside an early static initializer remains a separate binding issue
(`RAV1504`); this fix does not promise declaration-order-independent initializer
conversions.

Native evidence: 57 unchanged System sources compile into one library; source-free
UTF-8/file consumers execute with exit 42 and expected file mutation, and unchanged
`application-order-collections` matches its expected output. Bootstrap ownership
and core/seed contracts remain explicit.

Validation: 65 focused array/provider/unit/external-signature tests and all seven
native semantic consumers pass on the integration branch.

The isolated fix was reproduced on main and validated with 22 iteration-contract
tests, then integrated as `340fdb759`. No experimental backend is required.


### Primitive lookup with overlapping reference declarations (2026-10-04)

Special-type lookup prefers `MetadataImportOptions.CoreAssemblyName` when supplied;
without an explicit selection, the existing System.Runtime preference remains. A
referenced library's same-named System.Boolean/Int32/etc. declaration must not replace
this canonical primitive identity merely because of metadata lookup ordering. This
selection affects semantic symbols for both .NET and NeoCLR; it adds no runtime mapping
or emitted instruction. A synthetic alternate CLI core alongside System.Runtime provides
an independent .NET regression. Unsupported/missing core configuration is still subject
to the existing target validation.


### Predefined floating unary operators (2026-10-04)

Single/Double unary `+` and `-` are predefined operations in binding, retaining the
operand's primitive result type. They do not require static operator declarations
in a selected reference core, and need no Runtime Contract switch. Floating `~`
remains invalid. Existing .NET emission is retained. This general correction is
independent of native metadata support; focused primitive lookup, numeric comparison
and integral conversion tests validate it separately from NeoCLR integration.


### Static abstract property conformance (2026-10-04)

Imported static abstract interface properties require an implementation during ordinary
binding, just like instance properties. Matching includes static/instance identity and
existing property/accessor signature checks. Missing, instance-only and wrong-return-type
implementations report RAV0330. This needs no target policy or Runtime Contract setting.
The defect reproduced independently on main with a .NET-emitted interface and a separate
consumer: three invalid variants previously produced no errors, while the valid static
property passed. The fix applies to .NET and native metadata symbols through the shared
semantic model; metadata writer validation remains a separate defensive boundary.


### Conversion queries during declaration binding (2026-10-04)

Source declaration binding can query conversions before base/interface relationships
are complete. Those provisional answers are no longer retained in the compilation's
conversion cache; completed declarations retain normal caching. Subsequent semantic
queries and initializer/assignment checks therefore use the completed relationships,
including inherited constructed generic interfaces. This is shared compiler behavior,
with no Runtime Contract option, new syntax, metadata representation or backend change.
Focused C# regression coverage warms a conversion during declaration binding and then
checks its completed answer; source-order and existing conversion/generic controls cover
ordinary .NET behavior. General declaration-order problems outside this conversion
cache are not claimed solved.

### Explicit native text providers (2026-10-04)

The NeoCLR host ownership manifest can assign System.String and System.Char to the
source-built library. Its nativePrimitives map feeds explicit MetadataImportOptions
provider selection; Char is a grapheme storage category, not a numeric primitive enum.
The retained seed excludes both declarations and keeps only required runtime services.
The existing iteration contract names the same source library for Sequence/Iterator and
array shape. Binding, emitted signatures and native linking therefore share text/collection
identities. Ordinary .NET defaults are unchanged. See the
[native text API configuration and validation](api/neoclr-emission.md#source-owned-char-and-string-2026-10-04).


Native bodyless service declarations are a bounded adapter contract, documented in
[the NeoCLR integration](neoclr-cli-bridge.md#source-owned-runtime-service-declarations-2026-10-05).
They consume the selected core's explicit MethodImpl marker and do not change ordinary
.NET extern/PInvoke semantics or the Runtime Contract defaults.

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

### Source Object semantic ownership (development, 2026-10-05)

`MetadataImportOptions` adds a four-argument constructor and a read-only
`UseSourceObjectRoot` property:

```csharp
new MetadataImportOptions(
    "NeoCLR.CoreProbe", primitiveAssemblies: null, sourcePrimitiveTypes: null,
    useSourceObjectRoot: true);
```

Use this only for a NeoCLR compilation defining its own System.Object. All existing
constructors default to false. The bootstrap still supplies other platform types;
Object ownership is separate from numeric/Char/String/handle member providers.
.NET rejects the option with RAVT003 and retains its ordinary special-type selection.

The compiler selects the source root after declaration shells exist across all trees
and before binding member signatures. `GetSpecialType(System_Object)`, named
`GetTypeByMetadataName("System.Object")`, keyword signatures, array elements, implicit
source bases and override contracts then use that same symbol. The root has no implicit
bootstrap base. During declaration construction the bootstrap remains provisional;
external semantic requests wait for the declaration phase, including concurrent cold
queries. Selection is established once per compilation, not copied from another snapshot.

The source root must be a public abstract nongeneric top-level class with no base or
instance fields. Missing or incompatible declarations produce RAVT003; missing roots
never fall back through the public selected-root lookup. Existing declaration diagnostics
still reject duplicate declarations and invalid inheritance. No language syntax or LSP
protocol changes are needed: this is a compiler semantic configuration. Editor project
configuration and native consumer root import remain later work.

The native backend now admits the baseless root and its three concrete virtual slots
through explicit ObjectRoot/ObjectRootSlot declaration capabilities. Signatures, local
base constructor calls and overrides use output-owned definitions. Bootstrap reference
validation compares its exact registered assembly identity, independently of source Object.
The Reflection.Emit adapter still rejects this option before publication; ordinary .NET
behavior remains unchanged. Generic reference owners under a source root currently reject
explicitly because the metadata builder's constructed local-base support is incomplete.
The ordinary driver/ownership manifest does not expose this option yet. Production System
and artifact-only native consumer root selection are still pending.

`NeoClrMetadataProbe --source-object-root <core.dll> [output.pe]` now emits native PE,
checks the root and rejects unsupported virtual/generic declarations and unregistered
bootstrap references without touching the caller's output stream. The checked-in runtime
fixture executes a Raven source-derived override under explicit host root selection.
This is compiler-API/native-runtime evidence, not the ordinary driver/VS Code release gate.

Validation: 15 source-root cases, 75 focused compiler cases in total (including existing
metadata/typeof, .NET inheritance, virtual members and constructor codegen), plus the
native probe against the NeoCLR bootstrap. Tests cover both file orders, early queries,
concurrent queries, incremental reuse boundaries, invalid roots, unselected .NET and
NeoCLR declarations, and unchanged stream bytes/position after rejected emission.
The native adapter builds against the separate metadata library. This target-specific
feature is not an independently useful .NET fix for backporting to main.

Source-root emission validation (2026-10-05): 47 focused compiler tests pass, including
the capability opt-in test and .NET inheritance/virtual/constructor controls. A stale
shared-plan assertion was corrected separately in `a16955e8c`; the override classifier
itself was unchanged. Matching metadata/runtime support is neoCLR `9d880af0`.

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


## Source-root metadata resolution — 2026-10-06

With explicit `UseSourceObjectRoot` / `--source-object-root`, the special Object
symbol belongs to a source assembly. ReflectionTypeLoader must therefore resolve
preferred assemblies and fallback metadata names through IAssemblySymbol; only the
PE adapter's final type-interning path may require PEAssemblySymbol. It previously
cast the source core to PEAssemblySymbol and crashed while completing imported base
types during the full NeoCLR System compilation.

Imported Object bases/signatures are also canonicalized to the explicitly selected
source root, so removing the cast does not leave two competing Object identities.

This changes semantic metadata resolution only. It introduces no new target option,
metadata category, primitive owner or emission behavior. Ordinary .NET PE-root
resolution is retained. The CLI primitive bootstrap is still required; this fix
removes the reflection-loader crash. The full-source audit then exposes a separate
union ToString synthesis failure while source Object members are incomplete. Declaration
ordering and wider source/core identity unification remain subsequent work.

Validation: 16 existing source-root baseline tests pass; the new regression fails
with the source root and passes with the PE root before the fix. All 41 focused
source-root, metadata-import and reflection-projection tests pass after the fix,
including an ordinary .NET control. The released Numbers/HTTP source compilation
controls still pass. This is an isolated integration-line fix: main does not yet
contain the source-root contract needed by its regression. Reassess the general
assembly-symbol resolution portion when integrating that contract; do not merge
the native backend solely to backport this fix.


### Source-root union member completion

Union ToString synthesis now uses the existing lazy source method-signature path
before selecting Object.ToString. The override refers to the actual source method,
including when the union precedes Object or shares its file; it does not borrow a
PE Object member or disable synthesis. No new public API, Runtime Contract option,
metadata encoding or emitter mapping is introduced. Ordinary .NET continues to
resolve its metadata-owned Object normally.

Validation: all 206 existing source-root/union semantic/generic tests pass with the
fix. Four new declaration-order cases reproduced the crash before the fix; the final
23-test source-root run passes and checks the synthesized override identities.
The full native System audit now returns diagnostics (exit 1) for both source-handle
and retained-bootstrap-handle layouts, with no output published. Source RuntimeTypeHandle
ownership still conflicts with the configured typeof contract; retained handle ownership
removes that configuration error and seven conversion errors. Missing runtime-service
signatures and other binding diagnostics remain. This is not full System compilation
or execution evidence. The regression depends on the integration-line source-root
contract; keep this slice isolated until that contract can be validated on main.

## Synchronous scope-exit disposal (2026-10-06)

`CompilationOptions.WithRuntimeDisposalContract(new RuntimeDisposalContract(
assemblyName, interfaceTypeName, UseExceptionHandling: false))` selects a synchronous
resource protocol and explicit cleanup on ordinary control-flow exits. The interface
must expose a public, nongeneric, parameterless instance `Dispose` returning
unit/void. The contract is preserved by option copies; changes invalidate incremental
semantic reuse. Binding and language services resolve the same protocol through
compiler APIs. A null contract retains ordinary .NET `IDisposable`/`IAsyncDisposable`
behavior and exception-safe finally regions. The alternative lowering is shared
compiler machinery, independent of neoCLR. `CompilationOptions.NeoCLR` selects
`NeoCLR.CoreProbe`'s `System.Disposable` with exception handling disabled. A bootstrap
ownership manifest may supply `Disposal` with `AssemblyName`, `InterfaceTypeName`, and
`UseExceptionHandling`; its interface must belong to a declared source-library owner.
Omitting that field preserves the selected profile's disposal contract.

The shared pass runs after propagation and structural control-flow lowering and before
optimization. Resources become active after successful initialization. Normal block
completion, explicit/implicit returns, Result error and Option None propagation, and
loop break/continue dispose the resources whose lifetimes end, in reverse acquisition
order. Return and value-block results are evaluated and saved before cleanup. A failed
later initializer disposes earlier resources only. Nested function bodies own separate
cleanup state. The output consists of ordinary bound locals, calls and branches, with
no generated exception regions or remaining use-disposal metadata.

This first slice diagnoses async and iterator use with RAVT006. It does not promise
cleanup on terminal faults, exception unwinding, process termination, or failed
disposal; suspension/cancellation cleanup and asynchronous disposal remain separate
work. Outward and backward goto exits also clean up; jumps that skip a use initializer
are rejected with RAVT007. Ordinary .NET goto restrictions are unchanged. No syntax,
operations API or TextMate grammar change is required. Automatic iterator disposal is
not added by this change.

Validation evidence is recorded with the focused tests and native consumer in [the
bridge notes](neoclr-cli-bridge.md#synchronous-use-cleanup-2026-10-06).


Integration on `codex/source-object-metadata-resolution` combines disposal commit
`13b52ca56` with source-root fixes `6b31997a9` and `d29179810`. This preserves the
explicit source-root selection while admitting synchronous cleanup. Async cleanup
under the no-exception policy remains deferred. The nullable metadata import gap
is independent and remains open.

The author selected `codex/source-object-metadata-resolution` as the continuing Raven
working branch on 2026-10-06; `codex/metadata-consumer` is superseded for new work.
Its existing clean worktree is preserved. Integration validation: 54 pre-merge focused
checks passed; 92 post-merge checks pass (use declarations, scope-exit execution,
source Object roots, target profiles and default .NET async resource lifetime).
The six native cleanup consumers verify and execute with exit 42 against neoCLR
`db5b74f4` plus metadata slice `1f956b37`; no native runtime code changed in the merge.
See `tools/NeoClrMetadataProbe/scope-exit-cleanup-integration-validation.json` for
runtime/core hashes. This does not qualify async cleanup on neoCLR.


## Nullable callable storage (2026-10-06)

Portable callable signature mapping now erases `AnnotatedUnderlyingType` wrappers
according to the existing semantic nullable ABI classification. This includes
unconstrained `T?`, whose physical signature is the original generic parameter, as
well as nullable references. Nullable value storage is not erased. The correction
is target-neutral and does not introduce nullable runtime checks. A generic .NET
identity method accepts null and preserves a non-null argument; all 17 focused
nullable emission/storage tests pass.

Main integration candidate: the portable `CallableSignature` implementation is not
present on Raven main at this revision. Keep this small fix isolated until its owning
shared layer is integrated; do not copy the experimental target refactoring onto main
solely to backport this correction.

### Native calendar constructor unions (2026-10-06)

Native import now accepts the constructor-union shape already used by the CLI importer:
UnionAttribute, public single-value constructors and a public instance Object Value
getter. Named-case metadata keeps its existing path; a marker alone is insufficient.
The metadata facade supplies declaration/signature facts. Raven projects alternatives,
nullability and generic substitutions into IUnionSymbol; emission uses those symbols.
Provider-interface-only unions remain outside this bounded native path.

The calendar gate compiles unchanged TimeZone, ZonedDateTime, LocalTimeMapping,
TimeZoneError and DateTime with explicit internal runtime adapters, then compiles a
consumer using only that artifact and the existing Numbers/core/seed catalog. It
executes DST gaps/overlaps, offsets, invalid-zone/range cases, and DateTime conversions
and type patterns. Prefer specific temporal types in user APIs; DateTime is optional
when accepting either local or zoned values. No new Runtime Contract option or .NET
behavior change is introduced, and no importer objects are reused during emission.

C# probe: `--constructor-union-symbols <Core.dll> <System.neox> <Numbers.dll>` checks
plain and open/constructed generic alternatives. The accompanying neoCLR integration
record is `docs/experiments/extended-cli-metadata/source-calendar-2026-10-06.md`.
This is a target-adapter fix; there is no shared .NET fix to backport to main.

### Native-width integer source providers (2026-10-06)

NeoCLR's metadata adapter now maps IntPtr/UIntPtr signatures into Raven nint/nuint
symbols and emits them as native-width primitives. The shared primitive identity
contract includes both categories; concrete metadata and Reflection.Emit handles stay
inside their adapters. Ordinary .NET numeric cast rules are unchanged.

`MetadataImportOptions` and bootstrap manifest `nativePrimitives` accept System.IntPtr
and System.UIntPtr as explicitly owned source/imported types, with the existing exact
owner validation and no fallback for a missing configured native provider. Their source
m_value fields map to runtime scalar storage, as for other primitive implementations.
No implicit provider selection or metadata projection is introduced.

The neoCLR source-native-integers gate builds both unchanged declarations into
NativeIntegers.dll and compiles an independent consumer. A metadata-API input library
supplies negative and maximum unsigned values because this gate does not introduce
new Raven native-integer casts. CompareTo exercises the real primitive receiver and
existing source widening calls; default-value receiver storage is fixed separately
in 24c2c4d40. The consumer verifies and exits 42. See neoCLR's matching
`docs/experiments/extended-cli-metadata/source-native-integers-2026-10-06.md`.
Console service integration remains a later gate. The shared primitive mapping is
part of the portable target contract, which is absent on main; no wholesale backend
backport is required by this change.


## Native terminal-function ownership (2026-10-07)

`RuntimeFailureContract(AssemblyName, NamespaceName = "System", FunctionName = "Fail")`
is an explicit host assertion that the selected namespace function never returns.
Configure it with `CompilationOptions.WithRuntimeFailureContract(contract)`; null leaves
existing behavior unchanged. The native bootstrap manifest accepts an optional `failure`
object with these fields. Its assembly must appear in the source-library catalog.

This contract is NeoCLR-only. The resolved function must be a public static non-generic
namespace function with one by-value string parameter and void/unit result. Source
namespace containers and native module functions qualify; arbitrary type members and
CLI projection declarations do not. Configuration errors, a missing owner, or an
incompatible signature reject before publication. Artifact identity/digest validation
continues through the normal host dependency catalog. A method name alone never opts
an ordinary .NET method into terminal behavior.

Source/native symbols expose an internal terminal-call fact derived from this contract
and their semantic signatures. Existing bound-flow and lowering consumers use that fact;
emission neither reopens metadata nor consumes introspection objects. This removes the
legacy core-assembly restriction for explicitly selected source/native owners while
retaining the old CLI check for existing users. The emitted signature remains CLI void;
no new metadata category is introduced. The existing impossible-return guard remains.

The native source-Fail gate exercises both local and imported let-else calls, successful
and terminal paths, and wrong-owner failure without output. Full-System binding errors
drop from 12 to four, all NativeAllocation. This is not full System emission or execution.
The matching runtime supplies its exact no-result Fail service; see neoCLR's
`docs/experiments/extended-cli-metadata/source-failure-flow-2026-10-07.md`.

### Imported Object semantic ownership (development, 2026-10-07)

`MetadataImportOptions.ObjectAssemblyName` and
`WithObjectAssemblyName(string? assemblyName)` select the native reference supplying
System.Object independently of the CLI primitive bootstrap. The method returns an
immutable copy; null clears the selection. Empty names and selecting an imported owner
while UseSourceObjectRoot is true throw ArgumentException. This contract is NeoCLR-only;
.NET configuration rejects it. Missing/incompatible providers never fall back to core.

The supplied native assembly must declare a public, abstract, nongeneric, top-level,
fieldless System.Object class with no base. The native symbol adapter validates those
facts and classifies only the selected declaration as System_Object. The selected root
is shared by `object`, `GetSpecialType(System_Object)`, System.Object metadata lookup,
source class bases, imported generic bases and bootstrap Object base/signature facts.
No runtime reflection is added. Existing native reference catalog checks establish exact
assembly/artifact identity; the option names a registered assembly, not a discovery path.

Use `rvnc neoclr --object-library Numbers --reference Numbers.dll --core-reference
Core.dll ...` for a consumer of the diagnostic source-owned aggregate. The flag rejects
unregistered names, combination with --source-object-root or --system-symbols, and
absence of a primitive core. Emission remains independently capability-checked and uses
semantic facts plus host artifact identities. Runtime loading separately selects that
same artifact with --object-root; compiler selection is not runtime authorization.

The existing --source-object-root mode remains for compiling the root library itself.
Bootstrap Core still owns remaining primitive transport declarations; output configuration
does not change the .NET compiler host. Project/LSP root-catalog propagation and production
assembly packaging are separate work; this slice establishes the ordinary compiler driver.

Validation: imported-root C# semantic probe; missing/CLI/non-root/unselected/.NET/conflicting
controls; 47 focused framework/source-root/ownership tests; unchanged orders source
compiled without library sources, then native verification and exact stdout/exit 0.
See neoCLR `docs/experiments/extended-cli-metadata/source-owned-orders-2026-10-07.md`.

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

## Hoisted generic sealed-case member owners

When a generic sealed hierarchy's nested case is emitted as a top-level CLI
type, its member references use the case definition's actual generic arity.
They must not append the lexical container's type arguments. Constructed member
resolution uses the same argument projection as type and constructor resolution,
including calls from synthesized record formatting. Ordinary CLI nested types
continue to include their enclosing generic arguments.

This shared backend correction does not change Runtime Contract configuration,
source symbols, public compiler APIs or the existing hoisted-case representation.
Debug/Release runtime regressions cover direct member calls and record formatting;
`sealed-interface-generic-case.rav` adds representative IL-verifier coverage without
the verifier's known static-abstract generic-math limitation. Validation on modern
.NET does not establish execution on native neoCLR or NanoFramework.

## Async unit payload returns

Return binding in async methods uses the selected task's result payload rather
than the task wrapper. `Task<unit>` and `ValueTask<unit>` accept both `return` and
`return ()`; a bare return supplies a bound unit value so awaitless value-task
construction and suspended state machines agree. Expression bodies returning
unit use that same payload context. Bare returns remain invalid for non-unit
payloads such as `Task<int>`. Non-generic Task/ValueTask rules are unchanged.

This is shared binding behavior and introduces no Runtime Contract option,
public API or CLI bridge representation. Focused modern .NET tests cover
awaitless and suspended Task/ValueTask results, arrow bodies and negative
non-unit returns; native runtime execution is separately qualified.

For .NET 11 runtime-async emission, a generic task's unit result is a stored
payload, not CLI void. Explicit and implicit returns leave the unit value on the
stack; exception-region exits preserve it in a return local. Ordinary unit
methods and non-generic task returns retain their void-like behavior. Runtime
await helpers that produce a value discard it when the await is used as a
statement, including a unit payload. Runtime tests cover Task/ValueTask, empty
and arrow bodies, suspension, configured awaits, and finally exits. The
`runtime-async-net11` framework-matrix sample exercises both unit task families.
This changes no Runtime Contract configuration or native neoCLR encoding.

### neoCLR interface names (2026-10-08)

Use descriptive interface names without the .NET `I` prefix. In particular the
neoCLR async compiler protocol is `AsyncStateMachine` and `TaskAwaiter`; Raven's
.NET target continues to use .NET's own identities. This is a naming consistency
choice, not a new dispatch or performance capability. Migrating old neoCLR
artifacts requires recompilation with a matching compiler/runtime bundle.

### Source identity after declaration lookup (2026-10-08)

Metadata-only fallback while source declarations are incomplete is provisional.
It must not populate canonical or namespace-scoped type lookup caches: later
queries and namespace imports must select a matching source declaration. This is
a general compiler correction with no Runtime Contract option or target-specific
policy. It fixes the neoCLR library bootstrap's same-assembly internal property
lookup without relaxing member accessibility or changing metadata-only queries.
