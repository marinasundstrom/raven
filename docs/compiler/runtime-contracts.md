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
