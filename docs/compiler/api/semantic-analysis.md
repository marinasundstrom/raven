# Semantic Analysis API

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
precedence. The owning signature binder supplies the same `IParameterSymbol.Type`
for cold semantic queries and diagnostic binding; extracted designations remain
`ILocalSymbol`s. This adds no public API or metadata encoding. Positional patterns
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
can return different `ISymbol` instances that represent the same declaration or
metadata member. Use `SymbolEqualityComparer.Default` for equality, hash sets,
dictionaries, distinctness, and comparisons with well-known symbols unless a
specific API documents otherwise.

Closure-aware tooling can query capture data directly from `SemanticModel`:

- `GetCapturedVariables(ISymbol)` and `GetCapturedVariables(SyntaxNode)` return
  the symbols captured by local functions and lambdas.
- `IsCapturedVariable(ISymbol)` reports whether a local/parameter is captured by
  any closure in the current syntax tree.

These APIs are used by the language server hover experience to annotate closure
information for function declarations and variable references.

These APIs cooperate with the binder hierarchy described in the semantic binding
architecture notes. Binders chain by scope so lookups naturally fall back to
parent contexts while preserving local information.【F:docs/compiler/architecture/semantic-binding.md†L1-L64】

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
