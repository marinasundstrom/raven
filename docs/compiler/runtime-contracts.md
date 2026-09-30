# Runtime Contracts

## Intersection address and byref restrictions (2026-09-28)

Address-of expression binding and byref invocation-argument binding now reject
operands whose semantic storage shape contains an intersection. They report
RAV0363 before creating a bound address, including for arrays containing a
compound type. Local and parameter operands are covered for `ref`, `out`, and
`in`; ordinary nominal storage and by-value argument handling are unchanged.

This closes two paths that bypassed shared storage validation. It does not add a
byref ABI for erased compound storage or enable source intersection annotations.
Return-type publication and overload-driven generic inference remain separate
enforcement work. No Runtime Contract setting or native neoCLR policy changes.

Validation on .NET 11: the 204-test baseline passed. All 17 new address/byref
cases and the expanded 221-test regression set passed, along with 10 separately
run byref runtime tests. Targeted compiler builds for net10.0/net11.0, the test
build, whitespace formatting, and diff checks succeeded. No execution on neoCLR,
.NET Framework, or NanoFramework was performed; earlier full-baseline failures
remain outside this slice.

## Semantic intersection storage restrictions (2026-09-28)

Shared binder storage validation now rejects semantic intersections recursively
inside arrays, nullable types, tuples, reference/address/pointer wrappers,
delegate signatures, generic arguments, and constructed containing types.
RAV0363 is reported before ordinary storage validation continues, and the result
is an error type. Type-parameter constraints and nominal members are not traversed.

Capture, await-crossing local, async-parameter, and iterator storage reporting
also reject these shapes. This is a compiler representation restriction, not
ref-like classification. Nominal projections retain normal CLI behavior.

The symbol API can still construct descriptive compound shapes, and internal
lowering tests can still supply bound locals directly. Source annotations remain
disabled; inference and direct byref-expression paths still need review before
allowing supported locals through storage validation. No public ABI, Runtime
Contract configuration, or native neoCLR policy changes.

Validation: the 115-test pre-change baseline passed. All 20 new storage/capture/
suspension cases and the expanded 220-test intersection/static/ref-like/scoped/
byref set passed on .NET 11. Targeted compiler builds for net10.0/net11.0, the test
build, whitespace formatting, and diff checks succeeded. No execution on neoCLR,
.NET Framework, or NanoFramework was performed, and earlier full-baseline failures
remain outside this slice.

## Intersection member-write lowering (2026-09-28)

Internal reference-local lowering now projects erased receivers for property
setters, field stores, and indexer access. Indexer assignments reuse that access
path without duplicating index or value evaluation. The existing declaring type
owns dispatch; no adapter, public ABI, or Runtime Contract setting is introduced.

The supplied-bound-input runtime tests now optionally run ILVerify as well as
executing the emitted assembly. Before the fix, all six new write/indexer cases
executed successfully but failed IL verification; the seven existing cases
passed verification. This distinction matters for CLI receiver typing.

Source annotations still report RAV0363. Capture, hoisting, byref, inferred
compound escapes, events, and other unsupported shapes still require a binding
gate before source locals can be enabled. Nominal projections remain distinct
from escaping compound metadata. No native neoCLR policy is changed.

Validation: the 158-test baseline passed. All 13 internal-local tests subsequently
passed runtime execution and ILVerify, and the expanded 164-test intersection/
indexer/property-assignment set passed on .NET 11. Targeted compiler builds for
net10.0/net11.0, the test build, whitespace formatting, and diff checks succeeded.
No neoCLR, .NET Framework, or NanoFramework execution was performed; the earlier
full-baseline failures remain outside this slice.

## Internal intersection local lowering (2026-09-28)

The compiler lowerer can erase already-bound reference intersection locals to
per-body `object` storage, preserving the original semantic symbols. Local
reassignment uses that storage; implicit projections, instance calls, and member
reads insert nominal receiver casts. This does not add a global CLR type mapping,
public ABI, Runtime Contract setting, or native neoCLR policy.

Source annotations still report RAV0363. The tests supply intersection-typed bound
locals directly, run the real lowerer, and emit/execute its result. Capture,
hoisting, byref escape, nullable/value-type storage, and remaining member-operation
support or diagnostics must be completed before source locals can be enabled.
See [internal local lowering](intersection-types.md#internal-local-lowering).

Validation: 104 intersection/control-flow tests passed before changes. All seven
new emitted-program tests and the expanded 114-test intersection/control-flow/
use/propagation set passed on .NET 11. Targeted compiler builds for net10.0 and
net11.0, the test build, whitespace formatting, and diff checks succeeded. This
does not establish source-local support, native neoCLR behavior, or execution on
.NET Framework or NanoFramework. The previously recorded full-baseline failures
remain outside this slice.

## Intersection local reference representation probe (2026-09-28)

An executable Raven-source probe explores `object` local storage with nominal
constituent casts. It covers reference identity, shared mutation, class virtual
dispatch, conflicting explicit interface implementations, and all-bounds
membership checks. See the [candidate lowering and remaining gates](intersection-types.md#standard-net-local-representation-probe).

The probe uses existing CLI contracts and requires no carrier or runtime hook.
It does not add a compiler lowering, enable intersection annotations, change
Runtime Contract configuration, or establish a public compound-type ABI.
neoCLR's future nominal/structural distinction remains independent of this
standard .NET experiment.

Validation: the five existing constraint-emission/reference-owner runtime tests
passed before adding the probe. All four new emitted-program tests and the
combined nine-test runtime set passed on .NET 11. The targeted test-project build,
whitespace formatting, and diff checks succeeded. These tests execute hand-written
lowering equivalents, not compound-typed locals. No neoCLR, .NET Framework, or
NanoFramework execution was performed.

## Intersection receiver ambiguity (2026-09-28)

Member-expression binding on an already-typed semantic intersection receiver
reports RAV0365 when multiple accessible non-method declarations remain. The
ambiguous bound expression preserves all candidates. Property reads and assignments
no longer choose a constituent by order; inaccessible declarations do not hide
accessible siblings. Nominal receiver behavior and method overload resolution
are unchanged.

This is compiler-layer groundwork tested using injected semantic locals, not
support for source intersection annotations, storage, or execution. The RAV0363
source gate, CLI constraint metadata, Runtime Contract configuration, and future
neoCLR structural-type contract are unchanged. No runtime ABI is introduced.

Validation on .NET 11: the 233-test intersection/member/property baseline passed.
All 10 new receiver tests and the expanded 271-test focused regression set passed.
The generator/build script, targeted test build, whitespace formatting, and diff
checks succeeded. This is not execution evidence for intersection values on .NET,
neoCLR, .NET Framework, or NanoFramework. Previously recorded full-baseline
union-import failures remain outside this slice.

## Intersection member candidates (2026-09-28)

The binder collects instance-member candidates from semantic intersection
constituents, including class and interface inheritance. Shared declarations are
deduplicated by symbol identity rather than merging equal signatures across
unrelated interfaces. Existing overload resolution retains ambiguity between
indistinguishable declarations; constituent order does not select an
implementation. Object-member fallback and explicit-interface visibility follow
the selected constituent views. Static compound lookup is empty.

This is a candidate-lookup foundation, not source receiver binding or dispatch.
Public `GetMembers` remains declared-member enumeration. Property/event ambiguity
diagnostics, indexing, completion, and source value/storage support remain later
work. Nominal lookup, generic constraint lowering, CLI metadata, and Runtime
Contract configuration are unchanged. No native neoCLR facility is introduced.

Validation on .NET 11: 87 intersection/interface/constraint checks and two nominal
symbol-query tests passed before changes. Targeted compiler and test builds
succeeded, and all 162 focused intersection, lookup, interface, constraint, and
overload tests passed afterward. New tests cover cold/warm diamond lookup,
order-independent ambiguity, argument-based overload selection, imported generic
interface inheritance, explicit-interface visibility, object fallback, and static
dispatch exclusion. Formatting and diff checks passed. This is compiler-layer
candidate and overload coverage, not compound-receiver runtime execution; the
previously recorded full-baseline union-import failures remain outside this slice.

## Intersection reference conversion classification (2026-09-28)

The compiler API classifies implicit membership and projection for intersections
of non-nullable named reference types. Every destination bound must be proven by
identity or a nominal reference relationship. An intersection source can supply
that proof through its constituents. Ordinary nullable reference wrappers use
existing lifting; nullable constituents and type-parameter entailment remain
deferred. See [intersection type APIs](intersection-types.md#reference-conversions)
for the supported boundary.

The classifier does not combine numeric, boxing, or user-defined conversions to
establish membership. Unsupported membership and runtime-checked narrowing return
no conversion. Source storage positions still report RAV0363; no emitter change,
native runtime contract, public ABI, or actual-value conversion execution is
introduced. This general compiler mechanism is separate from future neoCLR
structural-type metadata and dispatch.

Validation on .NET 11: the overload-resolution suite passed all 399 tests before
and after the change. All 118 focused intersection, conversion-classification,
and conversion-operator tests passed afterward. Coverage includes cold/warm
membership, inherited and variant interface projection, nullable wrappers,
rejected boxing/user conversions, and unchanged source-position diagnostics.
Targeted builds, whitespace formatting, and diff checks succeeded. This is
compiler classification coverage, not compound-value runtime execution; the
previously recorded full-baseline union-import failures remain outside this slice.

## Intersection constraint queries (2026-09-28)

`GetTypeInfo` and `GetSymbolInfo` now expose the normalized semantic type of a
whole intersection constraint, including grouped/nested conjunctions. Constraint
declarations retain ordinary nominal `ConstraintTypes` and CLI metadata. Function
constraint queries use the method's type-parameter scope. Missing constituents
do not yield a partial compound type.

The diagnostic path rechecks failed constraint resolution in the owning binder
when an earlier query or declaration binder already resolved the symbols. This
also preserves errors for ordinary comma-separated constraints. No diagnostic
state is added to public symbols or language services. Intersections in value
positions or inside generic, array, and nullable constraint wrappers remain
unsupported. There is no new runtime option or storage ABI.

Validation on .NET 11: the 52-test intersection/constraint baseline passed before
changes. Targeted compiler and test-project builds succeeded, followed by all 247
intersection, constraint, generic method/type, and accessibility tests. Coverage
includes cold/warm compound queries, method scope, missing bounds, and rejected
wrapper positions. Formatting and diff checks passed. The existing full-baseline
union-import failures remain outside this slice; no neoCLR execution is claimed.

## Semantic intersection symbols (2026-09-28)

The compiler API can construct normalized semantic intersections independently
of a runtime representation. See [intersection type APIs](intersection-types.md)
for normalization, structural identity, display, member enumeration, substitution,
and limits. Existing source constraint lowering remains unchanged. Source value
positions remain diagnosed; no Runtime Contract option or target ABI is added.

Validation on .NET 11: the generator/build script succeeded, and all 285 focused
intersection, equality/display, substitution, and generic method/type tests passed.
This validates compiler APIs, not native compound-type execution or a storage ABI.

## Intersection constraints (2026-09-28)

Top-level intersection constraints flatten into ordinary nominal constraint
entries. Parenthesized/nested conjunctions retain each constituent's syntax
reference for accessibility diagnostics. `ITypeParameterSymbol.ConstraintTypes`
exposes the flattened bounds; `IntersectionTypeSyntax` remains available for
source presentation. No first-class semantic intersection symbol is introduced
in this slice. Existing compiler member lookup and constraint checks consume the
same bounds for inline constraints and `where` clauses, including generic macro
declarations. Macro declaration resolution retains its existing skeleton-type path.

The accepted subset is interfaces plus at most one distinct class bound across
the full list. Class bounds cannot accompany `struct`; type-parameter operands,
value-type/union bounds, and multiple distinct class bounds are diagnosed with
RAV0364. This prevents those intersection forms from reaching emission as an
incomplete set of runtime requirements. Generic arguments and value/storage
positions still report RAV0363. No public storage ABI, disjunctive constraint,
native compound runtime type, or new Runtime Contract option is provided.

Emission uses existing CLI generic parameter constraints and existing duplicate
bound elimination. Other CLI consumers observe ordinary nominal constraints;
they do not need Raven-specific intersection metadata. Semantic queries for
members and individual bound names use the normal compiler API. A query for the
whole compound syntax is not a first-class intersection type query in this slice.

The storage-type validation path now checks constructed generic arguments,
including nested generic, array, nullable, and by-reference element types. This
also corrects an existing omission for comma-separated constraints in parameter
annotations. Constraint declarations are resolved separately, so recursive bounds
do not recursively invoke storage validation. Intersection-bound declaration
diagnostics are reported alongside constraint accessibility checks, rather than
being lost when an earlier lazy resolver's binder is replaced.

Validation on .NET 11: 276 combined syntax, semantic, accessibility, generic
storage, and metadata/runtime checks passed; after adding nested-storage and
recursive-bound regressions, all 42 intersection/storage checks passed. The
overload-resolution feature suite passed all 399 tests. Metadata tests execute
both constituent orders, inspect ordinary CLI bounds, compile separate consumers,
and confirm runtime rejection of an invalid generic argument. The generator/build
script and subsequent targeted builds succeeded. The pre-change full baseline
stopped on the two union-import failures recorded below; no full-green baseline,
.NET Framework, NanoFramework, or neoCLR execution is claimed.

## Intersection syntax foundation (2026-09-28)

The syntax API preserves `A & B` as `IntersectionTypeSyntax`, with a separated
constituent list and precedence above unions. Function returns retain recursive
type parsing; prefix by-reference and pointer types retain their existing operand
scope. This first slice does not expose a semantic intersection symbol or select
a carrier/native ABI. Unsupported semantic type positions report RAV0363.
No Runtime Contract option or emitted metadata convention is introduced.

Syntax visitors and rewriters are regenerated. The existing TextMate operator
rule and whitespace normalizer already cover `&`; semantic language-service
support remains staged with binding, through ordinary compiler APIs.

Validation: the generator/build script succeeds, and 215 focused syntax,
generic-type/method, and constraint/unsupported-position diagnostic tests pass
on .NET 11. Before compiler edits, the full baseline stopped on the two existing
`AttributedCustomUnionTests.TypedCaseCarrierLoadsAsUnionWithoutBoxedValue`
class/struct cases; the corresponding focused pre-change set passed 203 tests.
This is not evidence of runtime intersection support on any target.

The planned general model is a **runtime/platform contract** governing semantic
rules, available types, representations, supported features, compatible symbol
sources, and one or more code generators. See the
[selection and compatibility design](architecture/runtime-platform-contract-design.md).
The CLI-oriented options documented below are existing implementation mechanisms;
they do not require every future symbol source to use metadata or CLI assemblies.

The neoCLR bridge is a temporary transport, not the native platform specification.
See the [bridge behavior and replacement inventory](neoclr-cli-bridge.md) for current
encodings, limitations, semantic distinctions and branch-qualified exploratory evidence.

## CompilationOptions presets and planned configuration

The public configuration type remains `CompilationOptions`. The agreed direction
is TargetPlatform (coherent loader/codegen selection), LangVersion (Raven source
version), Contract (platform mappings), and Features (requested optional features).
See the [API direction](architecture/runtime-platform-contract-design.md#agreed-compilationoptions-api-direction)
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
[main integration plan](architecture/neoclr-main-readiness.md).

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

`RuntimeUnitContract(AssemblyName, TypeName)` selects a top-level, non-generic, empty value type in the
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
are not validated target capabilities. neoCLR also rejects use declarations against
its current Disposable contract. Do not silently claim these forms work.

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
must not be conflated. See the [bridge inventory](neoclr-cli-bridge.md) for the
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
