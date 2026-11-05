# Semantic Analysis API

Symbol interfaces use unprefixed names such as `Symbol` and `TypeSymbol`; see the
[symbol contract migration](symbol-contracts.md) for compatibility details.

Semantic analysis is orchestrated by the `Compilation` and its per-tree
`SemanticModel`. Together they resolve symbols, infer types, track diagnostics,
and expose higher-level views such as operations.

---

## Building a compilation

Create a `Compilation` by supplying syntax trees (and optional references):

```raven
import Raven.CodeAnalysis.*
import Raven.CodeAnalysis.Syntax.*

let tree = SyntaxTree.ParseText(source)
let compilation = Compilation.Create("App", [tree])
```

Compilations are immutable—`AddSyntaxTrees`, `AddReferences`, and
`WithAssemblyName` return new instances that share existing state. Each
compilation manages a binder factory, declaration table, and synthesized symbols
for features such as delegates and the entry point.【F:src/Raven.CodeAnalysis/Compilation.cs†L1-L130】

## Accessing semantic models

`GetSemanticModel` materializes a `SemanticModel` for a specific tree. The
compilation caches models so repeated calls return the same instance. Models are
created lazily when diagnostics, symbol queries, type queries, operations, or
language-service features require them.【F:src/Raven.CodeAnalysis/Compilation.cs†L82-L123】

Each semantic model wires up the binder chain for the tree's root during
construction. Binders are the execution units that own derived semantic state for
their syntax and scope: parameters, locals, labels, pattern variables, bound
expressions, and binder-produced diagnostics. Public semantic queries may create
or reuse the responsible binder state for the requested node; diagnostic
collection uses the same binders but remains a separate reporting pipeline.

## Diagnostics

`SemanticModel.GetDiagnostics` consolidates binder diagnostics, deduplicates
entries, and caches the resulting immutable array. It triggers binding for the
current tree if diagnostics have not yet been collected.【F:src/Raven.CodeAnalysis/SemanticModel.cs†L23-L45】

At the tree level, `SyntaxTree.GetDiagnostics` provides parser diagnostics. A
compilation's `Emit` and `GetEntryPoint` flows also surface aggregated
information when generating IL or determining the program entry point.【F:src/Raven.CodeAnalysis/Syntax/SyntaxTree.cs†L46-L67】【F:src/Raven.CodeAnalysis/Compilation.cs†L124-L206】

## Symbol and type queries

Generic invocation queries infer a synchronous lambda's result from its body
when the delegate result is a still-unfixed method type parameter. The binder
preserves parameter hints without converting `match` arms prematurely to that
placeholder. Lexically declared type parameters remain authoritative targets,
even when a called method uses the same parameter name. `GetSymbolInfo` must
return the same constructed method and result type before or after diagnostic
collection. This uses binder-owned inference and introduces no public cache API,
metadata encoding, or target-specific rule.

Named function and method parameter signatures resolve an omitted annotation
from a nominal deconstruction pattern's type syntax. Explicit annotations take
precedence. The owning signature binder supplies the same `ParameterSymbol.Type`
for cold semantic queries and diagnostic binding; extracted designations remain
`LocalSymbol`s. This adds no public API or metadata encoding. Positional patterns
such as `(let id): SubmitOrder` use their annotation and can deconstruct a single
component. Editor consumers continue to use normal symbol/type queries; no new
TextMate tokens are needed.

Function-expression parameter destructuring is checked for total coverage after
the input type is resolved. Refutable patterns produce compiler error `RAV1618`,
including nested sequence requirements. Fixed-length array shapes and rest-only
sequence patterns are analyzed against their input types; sequence syntax is
not rejected as a category. Binding still supplies local symbols for editor
queries on an erroneous parameter, and cold symbol queries must not suppress
the diagnostic when diagnostics are subsequently requested. This validation
does not change the emitted input parameter list or add runtime mismatch checks.

`GetSymbolInfo` retrieves the symbol associated with a syntax node by binding the
node (or using cached results). For declarations, `GetDeclaredSymbol` resolves
keys from the declaration table and uses the symbol factory to return the
canonical symbol. `GetTypeInfo` has overloads for expressions and type syntax,
returning both the inferred and converted types when available.【F:src/Raven.CodeAnalysis/SemanticModel.cs†L47-L143】

These query APIs are non-reporting. They may bind narrowly to answer the
requested question, and the result may be reused by later queries, but they should
not publish compiler diagnostics or make analyzer results depend on diagnostic
collection order. If a query needs semantic meaning, prefer the public semantic
API that owns that meaning instead of reading binder caches directly.

Do not compare returned symbols by object reference. Lazy binding, diagnostic
binding, operation creation, metadata loading, and future incremental snapshots
can return different `Symbol` instances that represent the same declaration or
metadata member. Use `SymbolEqualityComparer.Default` for equality, hash sets,
dictionaries, distinctness, and comparisons with well-known symbols unless a
specific API documents otherwise.

Closure-aware tooling can query capture data directly from `SemanticModel`:

- `GetCapturedVariables(Symbol)` and `GetCapturedVariables(SyntaxNode)` return
  the symbols captured by local functions and lambdas.
- `IsCapturedVariable(Symbol)` reports whether a local/parameter is captured by
  any closure in the current syntax tree.

These APIs are used by the language server hover experience to annotate closure
information for function declarations and variable references.

These APIs cooperate with the binder hierarchy described in the semantic binding
architecture notes. Binders chain by scope so lookups naturally fall back to
parent contexts while preserving local information.【F:docs/compiler/architecture/semantic-binding.md†L1-L64】

### Macro identities

`MacroSymbol.Name` is the declared short name. `MacroSymbol.CanonicalName` includes
the complete containing namespace or logical module path: a macro `Identity` in
`Outer.Inner` reports `Outer.Inner.Identity`. A global macro reports `Identity`.
This shared semantic identity is independent of symbol display-format settings;
it uses the namespace's complete name rather than its short display text.
The same rule applies to .NET namespace declarations and neoCLR module declarations.

### Symbol facts helpers

The compiler exposes relationship helpers in `SemanticFacts` for advanced
analysis scenarios. `IsDerivedFrom` walks base types (including type-parameter
constraints) while `ImplementsInterface` traverses the interface closure (and
considers interface identity), and both accept optional
`SymbolEqualityComparer` instances so tooling can align with Roslyn's equality
semantics when desired. Array types mirror Roslyn's behavior by surfacing their
single-dimensional `IList<T>`/`IReadOnlyList<T>` contracts directly on the array
symbol, letting the helpers observe Roslyn-style constructed interfaces via the
array's element type.【F:src/Raven.CodeAnalysis/SemanticFacts.cs†L1-L120】【F:src/Raven.CodeAnalysis/Symbols/Constructed/ArrayTypeSymbol.cs†L1-L137】

## Operations

For syntax-agnostic analysis, call `SemanticModel.GetOperation`. The semantic
model caches realized operations and uses `OperationFactory` to translate bound
nodes into `IOperation` instances that mirror Roslyn's shape. The dedicated
[Operations API](operations.md) document covers the available kinds, caching
behavior, and current limitations.【F:src/Raven.CodeAnalysis/SemanticModel.Operations.cs†L1-L45】【F:docs/compiler/api/operations.md†L1-L55】


### Incomplete extension properties

Source extension property and indexer symbols retain their declared type while
accessors are being attached or when invalid source leaves them absent. Diagnostics
and duplicate-member checks may query these symbols during declaration recovery;
such queries must not throw merely because there is no getter or setter yet.
Once present, accessor types remain authoritative, including constructed generic
accessor substitutions. This is compiler-owned recovery state, with no additional
public cache API or language-service workaround. Storage declarations inside an
extension remain invalid; this change does not add extension backing fields.


### Source/import identity during declarations

Import binding resolves an indexed source type in the requested namespace before
metadata fallback when declaration completion is suppressed. It also retains
already-resolved source-owned symbols instead of canonicalizing them through a
metadata lookup that deliberately excludes incomplete source declarations. This
keeps member signatures bound to the source definition when a referenced assembly
contains a type with the same metadata name. Metadata-only candidates retain the
existing target/assembly selection rules. Regression coverage checks semantic and
emitted identity in both file orders, using a source `SyntaxKind` alongside the
retained compiler assembly, plus adjacent namespace and import cases.

### Delegate collectors during declaration completion

Top-level and nested delegate declarations retain their collector parameter flags
and normalized collection types in `Invoke` symbols. Repeated declaration
completion reuses the delegate's existing runtime members instead of registering
duplicate `Invoke` members. Public member queries and emitted signatures therefore
observe one owned delegate signature. `ParamsMetadataTests` covers both declaration
paths together with their exported array-collector metadata.

Collector validation examines the underlying collection type without discarding
the parameter's nullable annotation. Nullable arrays and collection interfaces
retain `IsVarParams`; normal-form null arguments remain null, while expanded and
omitted arguments construct collections. CLI array-collector metadata likewise
recognizes an array under a nullable annotation.

### Null-coalescing assignment

`??=` uses ordinary writable-location validation and assignment conversions,
then binds a guarded mutation with a null pattern. It preserves assignment's
`unit` type. The guard does not bind overloaded equality, and mutation-location
capture evaluates reference receivers and indices once. User RHS expressions
remain inside the guarded write, including property and indexer setters.
Conditional receiver assignments retain their outer receiver-null guard.

The emitter produces a `unit` value when a statement-only block is consumed as an
expression, including nested guarded mutation blocks. Runtime regressions cover
nullable reference/value locals, static fields, ref storage, arrays, properties,
indexers, conditional receivers, overloaded equality, and rejected storage/value
combinations. These are .NET execution checks, not native runtime qualification.

## Ref-like result lifetime analysis

Ref-safety analysis visits expression children before recording their result's
lifetime. This includes compiler-created receiver captures in range slicing.
Result blocks retain their final expression's origin; ref-like invocation results
can retain the receiver and unscoped arguments. Stack allocations, local references
and scoped values therefore remain restricted through slices and aliases.
Writes through `ref`/`out` storage distinguish ordinary value copies from
ref-like lifetime escapes. Reading an ordinary value from a scoped reference
(including an implicitly scoped `out` parameter) does not make that value scoped;
copying it to another destination does not report a scoped-escape diagnostic.
The analysis remains shared by .NET and neoCLR; runtime validation is reported
separately from target-independent diagnostic tests.
Non-ref-like call results do not inherit these lifetime restrictions. The runtime
contract selects slice members; shared flow analysis owns escape diagnostics.

Output-discard arguments (`out _`) have no input type during overload resolution.
They neither infer method type parameters nor favor a candidate's output type.
After selection, binding supplies a distinct hidden storage location of each
output parameter's element type. No source local named `_` is introduced.

Field declaration rebinding completes the initializer on the existing canonical
field symbol when its declared type is unchanged. Earlier references retain that
identity, while emission observes the fully bound initializer rather than a
provisional recovery expression from before all member signatures were available.
Unresolved initializers still contribute diagnostics and prevent successful emit.


### Local-function receiver captures

`GetCapturedVariables` includes the enclosing type symbol for a local function
that accesses the enclosing instance through `self` or an unqualified instance
member. Binding records an explicit receiver for those accesses so shared
capture analysis and target backends receive the same dependency. An explicitly
static function or a static containing method prevents enclosing-instance capture.

### Target-typed null suppression

A suppressed null literal uses the reference target supplied by its enclosing
return, initializer, assignment, or invocation. Cold `GetTypeInfo` queries bind
that context before using targetless argument pre-inference state; type and
operation queries must agree with diagnostics-first binding. Suppression changes
the reference's static type without adding a runtime null check. This is shared
language behavior; CLI execution tests validate the .NET implementation.

### Callable overload result comparison

Synchronous lambda overload ranking compares result conversions when candidate
callable input signatures agree. A provisional delegate attached during earlier
candidate binding does not act as the lambda's natural type for this comparison.
The shared path handles .NET delegates, expression-tree delegates, and neoCLR
structural callables; async-specific ranking remains separate.

Argument binding infers each generic overload's callback inputs from ordinary
arguments before binding the callback body, reusing each argument's natural
binding within the pre-inference pass. A type-qualified static extension call
has no implicit receiver argument. These choices belong to compiler binding, so
semantic-model queries and one-shot compilation use the same selected signature.

Same-signature class methods are ranked by their declaring-type inheritance
when argument conversions are otherwise tied: a derived declaration takes
precedence over its inherited counterpart, including different result types.
Inherited methods with different parameter signatures remain available.

Invocation result projection removes annotation-only nullable decoration from
known value types when the return contract uses the underlying value ABI. Method
symbols retain their declared annotation. Genuine nullable-value storage and
nullable reference returns remain nullable; this distinguishes unconstrained
generic `T?` annotations substituted with a struct from actual `Nullable<T>`.

Reference-conversion classification follows a type parameter's class constraints,
including chains through other type parameters, to the effective class and its
reference-convertible bases/interfaces. Constraint traversal is cycle-protected.
Unrelated class conversions are not introduced by these constraints.

Tuple expression binding infers element names from identifiers and member access,
then removes duplicate or reserved inferred names before creating tuple symbols.
GetTypeInfo and tuple member lookup expose those names on cold and warm queries;
explicitly annotated target tuples keep their declared names. The same binding
path creates .NET tuple projections and neoCLR structural tuple elements.

### Array-valued attribute constants

Source attributes expose array arguments as `TypedConstantKind.Array`, preserving
ordered `Values` for empty, short and larger arrays, including expanded `params`
arguments. Binding does not require the array length to match an internal builder
capacity. Cold symbol queries and diagnostic-first queries follow the same path;
metadata emission consumes the resulting constants. This is shared attribute
binding before target-specific metadata encoding.

### Explicit nullable reference conversions

`ClassifyConversion` recognizes ordinary reference casts from a nullable source
through its underlying reference relationship. Annotation narrowing to a
nonnullable reference is an explicit reference conversion, including an underlying
identity/upcast; it is not an implicit or identity conversion. `GetTypeInfo` and
conversion operations retain the written cast target. CLI emission already
projects reference annotations to their underlying type, preserving null and
object identity and throwing `InvalidCastException` for an incompatible runtime
object. Nullable value, unboxing and user-defined conversion rules are unchanged.

### Unrelated interface reference conversions

`ClassifyConversion` reports an explicit reference conversion between unrelated
interfaces. Property-pattern compatibility uses this same conversion fact, so a
value statically typed as one interface may be tested against another interface's
members. No implicit conversion is introduced. This shared classifier applies to
.NET and neoCLR; .NET runtime coverage does not establish native neoCLR execution.

### Local functions in default interface bodies

CLI emission recognizes block-bodied and expression-bodied local functions when
choosing interface method flags and emitting bodies. These internal helpers retain
their executable bodies and do not become abstract virtual interface contracts.
C# consumers need only implement the interface's declared abstract members. This
is a shared CLI-emitter correction; native neoCLR code generation is not validated
by the modern .NET runtime tests.

### Preliminary property override contracts

Source property accessor signatures retain declared abstract, virtual, override
and final modifiers before full property binding. Bodyless interface properties
also expose abstract accessors at this stage. These facts must be available during
reentrant binding; a later completion cannot undo an incorrect override diagnostic
already produced by a derived declaration.

Property override lookup admits the requested base member signature across partial
declarations before enumerating candidates, retaining constructed generic base
substitution. Cold `GetDeclaredSymbol` and diagnostics-first queries follow the
same rules. Modifier validation, override association and body binding remain owned
by the full member binder. This is shared language binding before target-specific
encoding; .NET remains the default. The corresponding Raven signature pass has
not yet been ported and must preserve this contract when translated.

### Init-only property assignment contexts

Property assignment binding checks the receiver and the innermost callable.
Constructors and init accessors may invoke accessible init setters on their
implicit receiver, self or base, including inherited generic properties. Initializer
blocks use the constructor context. Another instance, ordinary methods, lambdas
and local functions retain read-only diagnostics. Init setters participate in
ordinary assignment, compound updates and increments in the permitted context,
so computed setters retain observable behavior rather than being replaced by
backing-field writes. Imported CLI init setters follow the same binding rule.

This policy is shared before .NET/neoCLR lowering. The binder has not yet been
translated to Raven; its future translation must preserve this receiver-sensitive
contract. The pending ParseOptions translation retains its direct init-property
constructor assignments and benefits from the C# bootstrap correction.

Imported setters carrying the CLI `IsExternalInit` required return modifier expose
`MethodKind.InitOnly`. Ordinary setters expose `PropertySet`. Metadata import must
retain this distinction so assignment, compound updates, increments and pipeline
writes use the same construction/receiver checks as source init properties.

### Implicit metadata value construction

Parameterless metadata value construction binds to a canonical synthesized
constructor when no instance parameterless constructor was declared. Constructor
symbols, type information and object-creation operations retain normal construction
shape; lowering chooses value initialization. Constructed metadata member
projections retain reference identity across named and all-member queries, while
mutable declaration states continue through their existing refresh path.

### Common interface inference

Nominal inference includes an interface operand itself when testing shared
contracts, retains a unique most-specific common interface and preserves generic
arguments. Arrays participate through their semantic base type and implemented
interfaces, so `IEnumerable<T>? ?? T[]` retains `IEnumerable<T>` rather than
falling back to `object`. Every operand constrains inference; an array is never
silently removed from the comparison. Incomparable surviving interfaces use the
class/object fallback.
Reference `if` branches consult the same nominal inference when ordinary operand
conversion does not determine a more specific result. Existing valid interface
operand conversions retain their inferred type, including variance conversions.

Cold type/symbol queries and queries after diagnostics expose the same typed
contract. Runtime reference identity and coalescing short-circuit behavior are
preserved. The shared normalization and binder components have not yet been ported;
the pending Raven syntax-tree implementation benefits through its C# bootstrap.

### Same-named enum values and types

Member binding recognizes a simple receiver name whose value and type meanings
have the same enum type when accessing a declared enum constant. It binds a type
receiver without evaluating the value or requiring a property getter. Constant
parameter defaults use that same binder path. Member and receiver symbol queries
retain the enum field and enum type respectively, before or after diagnostics.
Qualified value receivers and differently typed shadows retain ordinary lookup.
This shared binder has not yet been ported; the C# bootstrap fix enables direct
Raven translation of ParseOptions without a source workaround.

### Variance-aware generic inference

Overload inference retains lower, upper and exact bounds per candidate instead
of destructively widening one substitution. Declared generic variance determines
the bound direction, including nested contravariance. Candidate fixing preserves
all nominal constraints. For the selected reference type, exact annotation bounds
take precedence over lower annotation bounds; upper-only annotations admit null
only when all matching upper bounds do. Method-group trials copy their evidence
and preserve already established ordinary-argument substitutions. Contextual
callback hints containing the inference variable itself supply no new bound.

The source and imported symbol contracts drive this shared algorithm; no target
name checks or metadata-specific comparer exceptions are used. The inference
component has not yet been translated, so the C# bootstrap correction enables
the unchanged Raven source.

Cold symbol queries inside callbacks resolve the enclosing executable context
before accepting a provisional overload failure. Rebind-root selection stops at
function and method declarations: an arrow-body query binds the body expression,
not a declaration that merely registers a signature. The existing re-entry guard
protects contextual binding. Repeated queries and diagnostics-first queries agree.

### Accessibility in method and field hiding diagnostics

Type-member binding checks accessibility from the declaring type when searching
base methods and fields for hiding candidates. Inaccessible members are skipped
while lookup continues through the base hierarchy. This uses shared symbol
accessibility, so both target frontends retain the same foundational rule. It
does not alter emitted members, runtime dispatch or explicit override validation.
The binder is not yet ported; generated Raven syntax-node caches benefit through
the C# bootstrap without renaming their private fields.

### Abstract properties have no backing storage

An abstract property is not classified as an auto-property and has no synthesized
backing field. This holds for explicit accessor lists and shorthand declarations,
before and after diagnostics. Accessor symbols remain abstract. CLI metadata
contains the abstract accessors without a storage field, and the uninitialized
property analyzer therefore has no stored value to check. The shared binder owns
this distinction for both targets; native storage/code-generation remains target
owned. This binder has not yet been translated, so the bootstrap fix enables the
unchanged generated Raven abstract syntax-node contracts.

### Explicit extension attributes retain lexical access

Methods marked with ExtensionAttribute in ordinary static classes use their
method/container as the accessibility context. They can invoke private helpers
in that container while receiver-private members remain inaccessible. Raven
extension declarations retain their existing assembly-level access context.
The distinction is based on declaration ownership, not the target name.

### Syntax reflection list membership

`SyntaxNodeReflectionExtensions.GetPropertyNameForListItem` recognizes generated
concrete `SyntaxList<TNode>` and `SeparatedSyntaxList<TNode>` properties through
their covariant node enumeration. Token lookup checks immediate child tokens and
separated-list separators. A `SyntaxTokenList` matches only when it contains the
requested token; an unrelated token returns no property name.

### Conditional nullability metadata

Imported `NotNullIfNotNull` attributes are available as metadata, but invocation
results retain their declared nullable types. This follows Raven's deliberate
absence of C#-style nullable flow state (see `docs/lang/feature-meaning.md`).
A known non-null result must be pattern-bound or explicitly unwrapped. The port
therefore unwraps the path helper results for known non-null inputs; this is an
adaptation to existing Raven semantics, not a missing flow-analysis fix.

### Grouped pattern syntax and operations

`ParenthesizedPatternSyntax` preserves grouping tokens and its inner `Pattern`.
It binds to the inner pattern without introducing a deconstruction operation.
Operation queries on the wrapper expose the inner pattern operation and retain
its children, including nested alternatives under `not`. An explicit singleton
positional pattern, `(pattern,)`, retains `PositionalPatternSyntax`; analyzer
fixes must preserve its distinguishing comma. Grouping applies to shared syntax
and binding, independently of runtime target.

### Out assignments in local initializers

The method-body out-parameter analysis visits local declarator initializers in
execution order. A normally completing initializer that forwards an `out`
parameter establishes assignment for subsequent statements and normal exits.
Calls on conditional short-circuit paths do not establish guaranteed assignment.
This is shared binder behavior for .NET and neoCLR; runtime regression validation
for this correction executes on modern .NET.

### Parser context last-token delegation

A parser context with a valid parent delegates LastToken even when the parent's
value is null. No consumed token is a valid initial state; only a missing parent
on the delegating base implementation raises the missing-context exception.
The C# bootstrap and Raven parser contexts use the same rule, keeping initial
terminator checks safe for EOF, closing braces and following statement keywords.
This is shared parser behavior, independent of target runtime policy.

### Conditional values at block-expression tails

A final non-semicolon-terminated if/if-let with an else in a BlockSyntax is
parsed in expression context, including its branch blocks. The parser restores
the initial checkpoint and diagnostics before parsing that final conditional
as an expression. Type queries therefore expose the conditional result rather
than a discarded statement value. Non-final conditionals and explicit semicolon
terminators preserve statement syntax. This is target-independent parsing;
compile-and-execute regressions currently exercise modern .NET.

### Constructor-constraint recovery

An incomplete `new(` constraint produces a constructor-constraint syntax node
with a missing closing-parenthesis token. Its character-expected diagnostic
identifies `)`, rather than an unrelated operator. Constraint parsing and
recovery are shared by targets; runtime-specific admissibility remains owned
by the target contracts.


### Abrupt match arms and contextual result types

A match arm that unconditionally transfers control (for example, a block whose
branches explicitly return) does not supply a match result. Binding retains its
control transfer and excludes it from result conversion, including a statement-form
match in the implicit-return position of a function. Ordinary value-producing arms
still receive and must satisfy their contextual result type. This is shared binder
behavior for .NET and neoCLR, not a target-specific conversion policy.

Property-pattern member paths use shared instance-member lookup, including base
classes and inherited interfaces. Source member signatures are completed through
the same binder-owned lookup path used for ordinary member access. This applies
to both source and metadata symbols and is independent of the runtime target.


### Out assignments in conditional tests

The method-body analyzer evaluates an `if` condition before analyzing its
branches. A direct `out` call, including one under unary negation or in the
always-evaluated left operand of a short-circuit expression, establishes
assignment for both branches and subsequent normal exits. A call confined to
the right operand of `&&` or `||` does not establish unconditional assignment.
The rule is shared by .NET and neoCLR binding; no runtime-specific encoding is
involved.


### Local function declarations and control flow

`AnalyzeControlFlow` does not treat a local function declaration as an executable
statement in its enclosing body. A declaration after a return or non-completing
loop neither restores reachability nor appears in `UnreachableStatements`.
Consequently it does not produce an enclosing unreachable-code warning. The
local function body retains its own control-flow analysis and diagnostics.
This is shared .NET/neoCLR compiler behavior; no target encoding changes.


### Explicit interface indexer symbols

Source explicit-interface indexers record `ExplicitInterfaceImplementations` on
the property symbol as well as each implemented accessor. An expression-bodied
indexer records its getter's interface method link just as an accessor-list
indexer does. Conformance checks and emitted interface dispatch use those links;
qualified metadata names alone do not establish implementation. This symbol
contract is shared across targets; runtime regression coverage executes on .NET.

Local function symbol identity includes declaration identity in addition to its
signature. Two same-named, same-signature functions in different methods are
distinct symbols, including when neither captures state. Equality and hashing
use the same declaration identity; emission caches must not merge their bodies
or capture frames.

The same definite-assignment rule applies to `while` conditions: the condition
runs before the first body execution and before normal condition-false exit.
An unconditional forwarded `out` call in the condition establishes assignment
in both places; a call reachable only through a short-circuit operand does not.
Assignments made only in the loop body are not assumed to occur.

Output declarations initialize their new local on the declaring call, including
readonly `let`/`val` bindings. Later writable by-reference uses retain ordinary
mutability checks. During overload resolution, an output declaration with no
known type is a binding-only placeholder, not an error or an input inference
source. The selected output parameter supplies the concrete local type before
conversion/lowering. Rebinding replaces the preliminary local in declaration
state and the identifier cache; placeholders must not reach emission.


Syntax-port stabilization preserves ordinary lexical lookup: nested types may
refer to accessible enclosing static members, nearest type members take priority,
and a local-function parameter shadows a later declaration in its enclosing
block. Lambda return inference also examines type parameters nested inside named,
array, nullable and reference return shapes, while retaining already-bound lexical
type parameters. These are shared binder contracts, not .NET-only syntax rules.


Ordinary call arguments may infer a method type parameter to the enclosing
method's same type parameter, including through constructed types, arrays, tuples
and params elements. Recursive generic calls therefore do not require redundant
explicit type arguments. Unfixed contextual callback placeholders remain excluded
from inference evidence; this distinction is owned by shared overload resolution.

### Escaped backing-field names

Binding, property-storage classification and semantic symbol projection recognize
only the unescaped `field` spelling as the backing-field keyword. Escaped names
resolve through ordinary scope lookup. Cold symbol queries and diagnostics-first
queries agree and do not introduce storage for an escaped local inside an accessor.
This source-level behavior is shared by .NET and neoCLR.

### Conversion result values

`Conversion` is a readonly value type with explicit generic value equality.
Equality, hashing and operators retain all conversion flags and compare method
and constructor symbols through `SymbolEqualityComparer.Default`. This custom
symbol-aware behavior is preserved rather than replaced by default member equality.
`default(Conversion)`, the parameterless constructor and `Conversion.None` represent
absence: `Exists`, `IsImplicit` and `IsExplicit` are all false. An existing explicit
conversion has `Exists == true` and `IsImplicit == false`. `WithAlias` preserves
absence and all other facts, and never removes an existing alias marker. These are
shared result-value contracts, independent of the selected target.

Conversion hashing handles optional symbols explicitly: a missing symbol contributes
zero; a present symbol contributes its semantic hash. This preserves the former
`HashCode.Add(value, comparer)` behavior while keeping nullable arguments out of
contravariant comparer conversions. Raven deliberately preserves the direction of
nullable reference variance: a non-null-consuming comparer is not a promise to
accept nullable values. This local implementation choice does not relax that rule.

### Cached conditional pattern scope

When a conditional expression is rebound with a contextual argument type, its
cached condition may already contain the declared pattern symbols. The binder
re-registers those same symbols for successful-branch lookup before binding nested
calls, then restores the prior scope before entering the else branch. Cached bound
nodes do not imply that temporary local registrations are still active. Cold
symbol queries, diagnostics-first queries and repeated type/symbol queries use the
same declared locals. Successful-branch identifier queries for ordinary `if`
expressions resolve through the existing pattern-declaration binding path, just
as `if let` expressions do; they must not return a temporary inferred local.
The correction is shared by both target configurations;
modern .NET runtime tests establish execution only for that target. No syntax or
public semantic-query API is added.

### Symbol result value contract

`SymbolInfo` is a readonly record struct. Typed equality, object equality and the
`==`/`!=` operators compare its symbol, candidate reason and candidate-array value.
Candidate arrays retain `ImmutableArray` storage equality; this change does not
introduce sequence comparison or change symbol equality. `default(SymbolInfo)`
retains a default candidate array, whereas `SymbolInfo.None` has an empty array.
Both display as `None`, but their stored values remain distinct. Existing internal
constructors, success classification and custom display text are preserved.
