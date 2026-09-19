# Runtime Contracts

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

A Runtime Contract describes a compiler-facing requirement supplied by a target's
CLI metadata. A target profile selects contracts and reference assemblies; it does
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
