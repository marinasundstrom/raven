# Runtime Contracts

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

Native pointer signatures are independent of this contract. Raven's `*()` emits
CLI `void*`, including supplied metadata method references and source signatures.
Nested pointers preserve the same void element. A contract selecting
`System.ValueTuple`, for example, changes unit value storage but never native
void-pointer parameters or results. This is a general CLI interop rule; each
runtime integration must validate execution on its own target.

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

## Delegate bridges and no-result calls — 2026-09-19

When a method group with an inferred Unit result is assigned to a delegate explicitly
returning `System.Void`, its bridge consults the emitted method signature before
discarding a result. A CLI void call leaves no value to pop; emitting a pop caused
InvalidProgramException on ordinary .NET. Calls returning a real Unit value still
discard it when required. No Runtime Contract setting or neoCLR policy is involved.
A .NET 11 execution regression fails before the fix and checks the caller completes
afterward; all 19 focused delegate/unit checks pass.
.NET Framework and NanoFramework execution are not claimed by that check.

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
