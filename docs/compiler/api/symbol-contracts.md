# Symbol contract names

Raven's semantic symbol interfaces use names without an `I` prefix: `Symbol`,
`TypeSymbol`, `NamedTypeSymbol`, `MethodSymbol`, and the related contracts. They
remain interfaces. This is an intentional naming exception to the otherwise
Roslyn-like compiler API; .NET interfaces such as `IEquatable<T>` and external
Roslyn interfaces keep their names.

This migration is source- and binary-breaking. Recompile compiler consumers and
replace the old Raven interface names with their unprefixed names. It applies to
both target implementations, analyzers, language services, macro providers and
samples. .NET remains the default target. Equality, symbol identity, binding,
metadata representation and target capabilities are unchanged.

Concrete types in `Raven.CodeAnalysis.Symbols` have distinct implementation names:

| Previous concrete name | New concrete name |
| --- | --- |
| `Symbol` | `SymbolBase` |
| `AliasSymbol`, `LabelSymbol` | `AliasSymbolImplementation`, `LabelSymbolImplementation` |
| `ArrayTypeSymbol`, `PointerTypeSymbol`, `AddressTypeSymbol` | Corresponding name plus `Implementation` |
| `TupleTypeSymbol`, `IntersectionTypeSymbol`, `StructuralUnionTypeSymbol` | Corresponding name plus `Implementation` |
| `FunctionReturnTypeSymbol`, `ErrorSymbol`, `ErrorTypeSymbol` | Corresponding name plus `Implementation` |

Existing source/provider-specific names such as `SourceMethodSymbol` retain their
roles. The internal `ImportedAssemblySymbol`, `InstanceFieldLayoutSymbol` and
`UnionCompanionSymbol` contracts follow the same unprefixed convention.

Generated visitor method names are preserved, including `VisitIntersectionType`.
The bound-node generator recognizes `SymbolBase` and implementation suffixes;
interface names are no longer shortened by stripping their first letter.

This naming pass is separate from the Raven symbol implementation port. Existing
Raven sources reference the renamed bootstrap contracts until the corresponding
implementations are ported and validated. It does not establish native neoCLR
execution parity or complete the bootstrap compiler.


## Attribute constant values

`TypedConstant` is a readonly record struct. Its value consists of `Type`,
`Value`, and `Kind`; typed equality, object equality, equality operators and
hashing use those stored values. Array constants retain `ImmutableArray`
storage identity equality, rather than comparing their elements recursively.
A default array and an empty array remain distinct.

The constructor and compiler factories remain internal. The three stored
properties are init-only and support record copies, and deconstruction returns
`(Type, Value, Kind)`. `IsNull` and `Values` are computed views, not additional
record state. The existing `ToString()` result,
`Raven.CodeAnalysis.TypedConstant`, is preserved. `WithType` preserves the boxed
payload rather than allocating a new box for an array constant.

These compiler API semantics are shared across target policies. Validation of
the C# implementation and the isolated Raven translation runs on .NET; it does
not establish native neoCLR execution or a completed symbol implementation port.


## Generic interface implementation signatures

Method type parameters have distinct symbol identities in an interface and its
implementation. CLI interface mapping substitutes the contract's parameters by
ordinal before comparing return and parameter types, including constructed types
such as `Box<T>`. Parameter names need not match. Generic arity and static versus
instance shape must match; overloads with different arity are distinct. This
mapping does not change symbol identity or make unrelated type parameters equal.


## Macro binding values and attribute occurrences

`MacroParameterBinding` is a sealed record class. Its parameter, source role,
declaration ordinal and optional invocation argument ordinal determine equality
and hashing. Init-only properties support immutable copies, and deconstruction
returns those four values. Its internal constructor and original `ToString()`
output remain unchanged. This is a value mapping stored as a reference type.

`AttributeData` remains a sealed class with reference identity for bound attribute
occurrences. Its immutable argument arrays and syntax reference are retained
without copying. The Raven translation preserves this distinction instead of
making all immutable reference containers use the same equality model.


## Type uses and namespace ownership

By-reference and nullable wrappers describe uses of an existing type. They retain
its containing namespace for context, but they are not declarations in that
namespace. Creating either wrapper must not add entries to namespace `GetMembers`
or affect subsequent name lookup. This applies to source and imported namespaces
and is shared by the .NET and neoCLR semantic implementations. The regression
coverage exercises source and .NET metadata symbols; it does not establish native
neoCLR execution. These wrapper implementations remain C# bootstrap dependencies
until their Raven translations are integrated.


## Internal type identity facts

`SymbolEqualityComparer` consumes internal provider contracts for nullable,
structural tuple/function and target-selected Unit identity. Implementations expose
the same underlying type, tuple fields, callable signature and runtime representation
used previously; the comparer owns recursive equality and hashing. Constructed
named types reuse `IConstructedTypeSubstitutionInfo` for explicit type arguments.
These contracts do not expose binding services or caches, add public symbol
categories, or change equality rules. Their `I` prefix follows the existing internal
provider convention rather than the public semantic-symbol naming convention.

This separates equality from concrete symbol storage so both compiler source
implementations can share the same identity semantics. Runtime representation
selection stays target-owned, including Unit normalization. Focused coverage compares
independent providers with the existing implementations and retains nominal/generic,
nullable, native structural-function and tuple cases. These checks run on .NET;
neoCLR semantic tests are not proof of execution on a native runtime.

## Representation queries for display and identity

Symbol display and metadata-identity helpers query internal read-only type facts
instead of requiring specific symbol implementation classes. Existing nullable,
structural tuple/function and constructed-substitution contracts are reused.
`ILiteralTypeInfo` exposes a literal pseudo-type's underlying type,
`IByRefTypeInfo` exposes its referenced element, and
`ISynthesizedDelegateSignature` exposes synthesized callable parameters, ref kinds
and return type. These are internal provider contracts, not public symbol kinds
or new language rules. Implementations retain the existing display distinctions,
including synthesized-function parentheses and structural-function keyword policy.

This prepares the shared formatting algorithm for translation to Raven while
keeping concrete compiler storage independent. The corresponding Raven contracts
and formatting implementation must be integrated together when that dependency
slice is ported. Existing .NET display coverage and neoCLR structural-symbol
coverage validate the C# refactoring; native neoCLR execution is not claimed.

## Union content nullability

Union content normalization reads `INullableTypeIdentity` rather than requiring a
concrete nullable wrapper. It unwraps aliases around the wrapper and its underlying
type, preserves constructed `Nullable<T>` handling, and deduplicates non-null
pattern-domain members before adding the null alternative. These are shared symbol
semantics, independent of the target runtime representation. Independent-provider
tests run on .NET; they do not establish native neoCLR execution.

## Imported enum base types

CLI metadata import keeps `System.Enum` and `System.ValueType` as reference-type
classes. Concrete enum declarations retain value storage and `TypeKind.Enum`.
The fallback classification for types derived from `System.ValueType` must not
turn `System.Enum` itself into a struct; doing so selects the wrong receiver
representation for inherited instance calls such as `ToString` and `HasFlag`.
The shared importer follows this rule for .NET and neoCLR CLI references. Native
metadata import already uses the metadata view's explicit value-type flag.

### Nullable wrapper implementation

Nullable use-site symbols derive from the common symbol base rather than the
source-declaration symbol base. They have no declaration syntax or attributes of
their own and do not invoke source attribute binding. Their containing namespace
still supplies containing assembly/module, and constructing a wrapper never
registers another declaration. Nullable ABI projection and substitution preserve
the original annotation-versus-`System.Nullable<T>` distinction. This is shared
compiler infrastructure; it does not change target-selected runtime policy.

The Raven port now contains the concrete nullable wrapper, nullability transform
APIs, and `NullableMetadataFacts`. Return `MaybeNull`, input `AllowNull` and
`DisallowNull`, setter value-parameter attributes, and explicit ABI projection
remain interpreted identically to the C# bootstrap compiler.

### By-reference use-site wrappers

`RefTypeSymbol` uses the common symbol base without source-declaration binding.
Its element is required, but the element's containing symbol is optional. An
absent element throws `ArgumentNullException` for `elementType`; a detached valid
element remains valid. Namespace-owned assembly/module information is preserved,
and the wrapper does not register a source declaration.

The concrete wrapper implements `IEquatable<RefTypeSymbol>` and `==`/`!=` with
the same element-symbol equality as object equality. Its hash uses that same
symbol comparer, so independently constructed equal wrappers work in hash sets.
It remains a class rather than a record: it derives from the existing non-record
symbol base and retains its specialized symbol display and element equality.
Visitor acceptance dispatches directly to `DefaultVisit`, avoiding recursive
re-entry into `Visit`. These contracts are implemented in C# and Raven.

### Literal type wrappers

`LiteralTypeSymbol` stores the semantic underlying type and a possibly null
constant. Construction receives the assembly owner directly, retaining it as the
containing symbol, without depending on `Compilation` or source-declaration
binding. The expression-conversion helper supplies the same compilation assembly
as before. Literal display, underlying member forwarding and visitor behavior
are preserved.

The concrete class implements typed/object equality and `==`/`!=` using underlying
symbol equality plus constant equality; hashing uses the same components. It
retains class representation and the existing non-record symbol base. Constant
storage and its constructor parameter are explicitly nullable in both C# and
Raven, including emitted metadata. This does not change the public semantic type
of ordinary integer expressions or the rules for constant numeric conversion.

## Union discovery and diagnostic display

`UnionSymbolExtensions.Discovery` uses the public union/case symbol contracts and
classification flags. Constructed named types can resolve their union/case through
`ConstructedFrom`; an interface implementation alone does not override a false
classification flag. Diagnostic names use concrete case arguments when present,
otherwise union parameter names, and include namespaces to disambiguate equal
carrier names. These shared rules are ported to Raven without requiring source or
PE symbol implementations. Carrier construction and generic projection remain
separate helpers.

## Union case type-parameter projection

`UnionCaseTypeParameterMapping` is an internal optional provider contract for
case-declaration parameter mappings. It exposes the declaration's parameter list
and a nullable `TryGetProjectedUnionTypeParameter` result annotated
`NotNullWhen(true)`. `SourceUnionCaseTypeSymbol` supplies the existing immutable
map. Inference, conversion, semantic queries, and pattern binding consume this
contract instead of testing the concrete source symbol type.

`UnionFacts.Projection` is implemented in both C# and Raven. It first consults the
case's provider (or its original definition), then retries a rebound parameter
through its declaration ordinal. Imported cases retain name-based fallback.
Projection into supplied union arguments matches parameter identity first, then
name, checks bounds, and preserves the input parameter on failure. Consumers
that previously used only direct mapping retain their narrower fallback rules;
this extraction does not unify distinct projection algorithms or change target
representations. The rest of `UnionFacts` is not yet ported.

## Inference normalization port

The full internal `TypeSymbolNormalization` helper is maintained in C# and Raven.
Its common-type selection retains all operands, normalizes literal/address uses,
collapses compatible nullable and discriminated-union candidates, and selects a
common nominal base or most-specific common interface before object/error fallback.
It does not synthesize a structural union for unrelated branch types. Case argument
projection consumes `UnionCaseTypeParameterMapping`, with the existing name fallback
for imported declarations. Nullable successful outputs carry `NotNullWhen(true)`;
failed projection still returns null and the caller selects its established fallback.

### Nominal tuple projections

`TupleTypeSymbolImplementation` is a projection over an underlying named tuple
storage type. It derives from `SymbolBase`, not the PE-import base: it does not
load reflection metadata, and its documentation comment remains absent. Assembly
and module ownership continue to come from its containing namespace. Its
containing symbol may intentionally be null when a new tuple projection has no
original owner; both compiler sources annotate that explicitly.

`TupleStorageLayout` constructs and describes nested `ValueTuple` storage while
preserving logical element order, names and each element's `Rest` field path.
The Raven implementation shares the C# contracts for construction, projection,
malformed layouts, ownership and tuple visitor dispatch. These .NET storage
contracts do not change neoCLR's structural tuple representation or claim native
runtime validation.

### Generic substitution rules and construction

`TypeSubstitution` owns definition/argument inspection, tuple and composition
substitution, equivalent type-parameter matching and containing-type maps.
Reanchoring a nested definition constructs a new named type and is owned by
`ConstructedNamedTypeSymbol.ReanchorNested`; callers use that factory directly.
There is no forwarding dependency from the rule helpers back to the concrete
constructed-type implementation.

`TryGetEquivalentTypeParameterSubstitution` returns null on failure. Its output
is explicitly nullable and annotated `NotNullWhen(true)` in both compiler
sources. C# callers can use that annotation for flow analysis; Raven currently
requires explicit null checks/captures rather than consuming this annotation as
null-flow evidence. Substitution preserves unchanged object identity and retains
the existing nominal tuple when its replacement storage cannot be constructed.

### Callable substitution providers

`StructuralFunctionSubstitution` combines callable identity facts with a
substitution operation. Constructed named types and methods supply their type
mapping through this internal contract; the callable implementation owns
reconstruction of its signature and Invoke method. Their parameter projections
consume `IStructuralFunctionIdentity` rather than a concrete source-backed
callable class. The existing StructuralFunctionTypeSymbol implements the
contract without changing its constructor or reconstruction behavior.

FunctionReturnTypeSymbolImplementation also consumes callable identity. An
unresolved operand preserves the projection when unchanged or creates a new
projection when replaced; a resolved callable supplies its return type, with
void mapped to the projection's supplied unit type. These are shared compiler
rules for the neoCLR callable feature, not evidence of native execution.

### Constructed types and member projections

The Raven implementation now includes constructed named types and methods,
substituted fields, properties, events, methods, parameters and method type
parameters, alongside the previously ported named-type parameter wrapper.
These retain the C# substitution and ownership rules, including containing-type
maps, nested reanchoring, explicit interface projections and unchanged identity.

Member caching still consults declaration-provider state: stable declarations
reuse their projections, while mutable union and union-case declarations expose
updated members. Callable reconstruction uses StructuralFunctionSubstitution;
the source-backed callable implementation remains a later porting dependency.
These shared symbol rules introduce no new target policy.

### Source-method return attributes

SourceMethodAttributeFacts selects declaration-level return targets followed by
return-clause attributes for methods, functions and explicitly typed lambdas.
It resolves current syntax and binds through SourceDeclarationServices, leaving
Compilation/Binder access in the service implementation. Usage validation shares
one duplicate set across declarations; missing services, unresolved declarations,
macros and failed attribute bindings retain their existing skip behavior.
SourceMethodSymbol still owns the lazy return-attribute cache. The helper has
matching C# and Raven implementations and adds no target-specific rules.

### Source named-type declaration services

SourceTypeDeclarationServices extends the existing source declaration contract
with declaration completion, source special-type classification and configured
array-shape resolution. Compilation owns these operations and their existing
target/runtime configuration. SourceNamedTypeSymbol consumes the contract for
primary-constructor completion, provisional interface caching and array
projection, with no direct Compilation dependency. Missing services keep the
existing absent-projection behavior. Its explicit base-type constructor accepts
null, matching root and interface declarations and its nullable BaseType.

SourceMethodDeclarationIdentity exposes whether a source method is a signature
placeholder. Member containers use this fact to replace a placeholder with the
bound method without depending on the concrete SourceMethodSymbol class. Both
contracts have matching Raven declarations. Source named types now implement
these contracts in both codebases; source methods remain to be translated.

### Explicit property identity

Property duplicate detection includes the qualified metadata name. An ordinary
property and an explicit interface property with the same simple name remain
separate symbols, as do properties implementing different interfaces. Existing
partial-property merging and duplicate checks for the same qualified identity
remain in place. This is shared binding behavior, independent of runtime layout.

### Source named types in the maintained compiler implementations

SourceNamedTypeSymbol and its array-provider partial are translated in full.
Member insertion retains declaration identity, signature-placeholder replacement
and source/generated Main separation. SynthesizedEntryPointMethod identifies
the generated entry point without making member containers depend on its
concrete source-method subclass. It exposes the existing executable-code flag
and optional async implementation; it adds no entry-point semantics.

The entry-point name constant is shared through an EntryPointSignature.Name
partial in each language; the remaining signature logic is not yet ported.
Synthesized union helper names are also retained in Raven. Generated named-type
visitor partials remain owned by BoundNodeGenerator, selected in the port project.

The port preserves provisional and completed interface-cache behavior, generic
construction, nullable base types, record/deconstruction property lists, closed
hierarchy metadata and configured array-shape projections. Private setters use
Raven val properties with explicit private setters, and nullable cache getters
separate assignment from their result. No target selection or mapping changes.

### Async-iterator cancellation selection

AsyncIteratorCancellationUtilities is maintained in both compiler languages.
Canonical CancellationToken lookup uses AttributeValidationServices, allowing
source methods and synthesized iterators to share the helper without depending
on Compilation. Candidate order, the exactly-one selection rule, async-enumerable
eligibility and missing-attribute warnings retain their existing behavior.
Attribute recognition first inspects bound metadata, then the existing syntax
fallback used during declaration binding. Async enumerable recognition uses
the named definition and exact metadata namespace. Namespace traversal explicitly
annotates its nullable parent in both implementations. No cancellation runtime
or target policy changes are introduced.

### State-machine synthesis services

Async and iterator type synthesis consumes `StateMachineServices` for canonical
type lookup, error/array/pointer construction, heap allocation policy and async
exception capture. `Compilation` implements this internal contract and retains
ownership of the configured options and type factories. Both maintained languages
declare the contract and implement synthesized async and iterator types alongside
source methods and lambdas.

Separate C# compatibility partials retain the original Compilation constructors
and Compilation property for unported lowering/codegen consumers. That property
is valid only for Compilation-backed instances. Core synthesis uses Services and
is tested with an independent provider; new shared synthesis code must not use
the compatibility property. These are internal APIs, not new analyzer APIs.

### Explicit member lookup during forwarding

Unqualified lexical and implicit-instance lookup exclude explicit interface
implementations using the same identity predicate as qualified class-member
lookup. This prevents an explicit getter forwarding to a same-named ordinary
property from binding recursively to itself. GetMembers still exposes both
symbols; filtering belongs to lookup, not declaration storage. Interface-typed
lookup continues to expose the interface contract. These are shared .NET and
neoCLR binding semantics; native execution is not implied.

### Source-method and lambda port contracts

SourceMethodSymbol and SourceLambdaSymbol retain mutable binder-owned signature,
async/iterator state and closure/capture facts. SourceMethodDeclarationServices
keeps configured terminal-call policy in Compilation; source attribute creation
uses existing declaration services for canonical lookup. Source union/case tests
require both source-named identity and the union/case interface, preserving the
source-versus-imported distinction without depending on concrete union classes.

Local-function capture snapshots use explicit Volatile.Read/Write in both
implementations, preserving the acquire/release behavior of the previous private
C# volatile field. The dependency graph remains lazy so forward and recursive
calls observe bound callees. Cache results are returned explicitly after Raven
assignment statements. Private setters and intentionally absent references remain
annotated; ordinary method parameters retain their names and use mutable locals
when normalization requires reassignment.

ConstructedMembers, BuilderMembers and TypeParameterMapping are readonly record
structs in both sources. Their existing constructor signatures, stored properties
and validation remain intact; typed equality and operators make their value
semantics explicit. C# Compilation compatibility partials remain outside the Raven
state-machine implementations for unported lowering/codegen callers.


### Source union declarations in both compiler sources

SourceUnionSymbol, SourceUnionCaseTypeSymbol and
SynthesizedUnionCompanionTypeSymbol retain reference identity in the Raven port.
They preserve staged binder-owned members, explicit and inferred content
nullability, case parameter mutability and projected union type parameters.
Nongeneric unions own their metadata cases directly; generic unions cache one
nongeneric static companion. This representation describes the existing CLI
symbol model, not a permanent native neoCLR layout requirement.

The discriminator and primary payload references retain their existing staged
initialization contract: declaration binding installs them before consumers read
them. Their `null!` initializers do not introduce nullable public symbol APIs.
Named-type virtual extension points remain virtual in both implementations.
