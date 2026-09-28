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

## Attributed custom union metadata

Typed-case carriers marked with `System.Runtime.CompilerServices.UnionAttribute`
are recognized without target-specific configuration. The [CLI union contract](../lang/spec/dotnet-implementation.md)
defines the required constructors and typed accessors. This affects imported
symbols and documentation; it adds no Runtime Contract option, storage rewrite or
new extraction lowering. Independent .NET class/struct fixtures cover recognition,
negative shapes and RavenDoc case grouping.


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

### Nongeneric cases of generic unions

A generic union's companion may contain both generic payload cases and nongeneric
empty cases. Target-metadata emission preserves the actual CLI arity of each case: a
constructed symbol for an empty case does not make its metadata type generic. This
applies to ordinary separately compiled unions and does not require a target-specific
Runtime Contract setting. Focused imported-union tests cover both emission paths.


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
