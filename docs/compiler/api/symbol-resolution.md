# Symbol resolution

Symbol resolution in Raven happens in **two main scenarios**:

1. **Compiler-driven resolution (top-down)**
   During compilation, the binder walks the syntax tree **top-down** to build bound nodes and symbols.

   * Ensures declarations, references, and types are resolved consistently.
   * Produces diagnostics (e.g. *unknown symbol*, *ambiguous call*).
   * Runs when the compiler is preparing semantic models, generating IL, or collecting diagnostics.

2. **Tooling-driven resolution (on-demand)**
   Tools such as analyzers, IDE features, or user code using the `SemanticModel` can request resolution for a **specific syntax node**.

   * The semantic model locates or constructs the appropriate binder for that node.
   * Resolution may cascade into dependent nodes if needed, but is scoped to the query.
   * Examples:

     * IntelliSense: “what symbol is referenced at this cursor?”
     * Refactoring: “what type results from this expression?”

In practice, both modes use the same **binder infrastructure** under the hood.
The only difference is **triggering**:

* Compiler: **eager and hierarchical**
* Tooling: **lazy and local**

Both modes must converge on the same semantic answer. A tooling query may be the
first path to bind a node, and a later diagnostic pass may reuse that state, or
the diagnostic pass may bind first and a later query may ask for the same symbol.
The public query APIs are therefore non-reporting and compiler-owned: callers ask
for symbols and types, while diagnostic collection remains a separate reporting
operation.

Symbol identity is semantic, not object identity. Equivalent binding paths can
return different symbol instances for the same source declaration or metadata
member, especially as incremental snapshots start reusing declaration descriptors
and recreating snapshot-owned source symbols. Use `SymbolEqualityComparer.Default`
when comparing or storing `ISymbol` values.

---

## At a glance

* **GetDeclaredSymbol** → declarations → symbols you introduce
* **GetSymbolInfo** → expressions → symbols you reference
* **GetTypeInfo** → expressions → resolved types and conversions
* **GetBoundNode** → internal → full semantic information

---

## Public API

The `SemanticModel` provides several entry points for resolving symbols and types from syntax.
Most delegate into the binder infrastructure and ultimately resolve through the **bound tree**.

### `GetSymbolInfo(SyntaxNode node)`

Resolves the **referenced symbol** for an expression.

* `foo.Bar` → member symbol
* `Console.WriteLine` → best overload (or ambiguity)

Returns a `SymbolInfo` with:

* `Symbol`: resolved symbol (or `null`)
* `CandidateSymbols`: possible matches if ambiguous
* `Reason`: why no single symbol was resolved

Internally: calls **`GetBoundNode`** and extracts the referenced symbol.

---

### `GetDeclaredSymbol(SyntaxNode node)`

Resolves the **declared symbol** for a declaration.

* `class C { }` → `NamedTypeSymbol`
* `let x = …` → `LocalSymbol`

This directly queries the binder for the declaration symbol, bypassing full binding.

---

### `GetTypeInfo(ExpressionSyntax node)`

Resolves the **type and conversions** for an expression.

* `1 + 2` → type: `int`
* `1.0 + 2` → type: `double` (implicit conversion on second operand)

Internally:

* Calls **`GetBoundNode`**
* Extracts `Type` and `ConvertedType`

---

## Internal API

### `GetBoundNode(ExpressionSyntax node)`

Returns the **bound node** for the given syntax.
This is the shared entry point used by `GetSymbolInfo` and `GetTypeInfo`.

Bound nodes carry both symbol and type information and form the bridge between syntax and semantics.

Used internally for:

* Code generation
* Flow analysis
* Diagnostic collection

---

## Flow diagram

```
SyntaxNode ──► GetSymbolInfo ─┐
                              ├─► GetBoundNode ──► Binder
SyntaxNode ──► GetTypeInfo ───┘
SyntaxNode ──► GetDeclaredSymbol (direct to Binder)
```


## Nominal and ad-hoc union display

`ToDisplayString` preserves the distinction between named unions and structural
ad-hoc unions. In a type-use format, a nominal union remains `StreamError` or
`Optional<int>`. Declaration formats can request its `union struct`/`union class`
keyword and, with `IncludeUnionMemberTypes`, its own cases or member types.
That option does not recursively expand nominal types used as alternatives or
as generic arguments.

The standard `System.Union` carrier displays as `A | B`, including when the caller
requests declaration keywords or union members. It has no source-level nominal
declaration to decorate. This spelling applies to source-bound and imported
constructed carriers and remains compact inside generic signatures. This follows
the existing structural spelling used for Raven tuple/function types; .NET has
no direct ad-hoc union syntax. Explicit carrier syntax and ad-hoc syntax currently
share the same semantic carrier, so display cannot distinguish their origin.

This changes presentation only. Runtime Contract configuration, semantic type
identity, metadata encoding and emitted code are unchanged. Source and imported
nominal alternatives, generic nesting and declaration formats have focused
symbol-display coverage.

### Metadata type names

Synthesized top-level `Program` types follow the ordinary named-type contract:
`MetadataName` is `Program`, while `ContainingNamespace` owns the namespace.
For file-scoped code in `Samples`, metadata lookup uses `Samples.Program`, and
the emitted entry-point type has that same fully qualified name.

`Compilation.GetTypeByMetadataName` matches source declarations by metadata name,
including generic arity. `Box` selects only a nongeneric declaration; ``Box`1``
selects `Box<T>`. Nested names use `+` and apply the same rule
to each type segment. Missing nongeneric declarations do not fall back to generics.

Expression binding is a separate operation: a qualified generic union receiver
such as `Example.Maybe.None()` can name its open definition before the invocation
infers type arguments. That does not make `GetTypeByMetadataName("Example.Maybe")`
return the generic declaration.

Qualified case-pattern type expressions use the same open-definition lookup;
`if let Example.Maybe.None = value` must bind a case type rather than silently
retaining an error type. Metadata-name lookup remains exact.

### Consistent namespace-qualified metadata selection

A qualified source type such as `Example.Base` or `Example.Base<int>` must select
the same referenced definition as ordinary imported lookup and
`Compilation.GetTypeByMetadataName` for its full metadata name and arity. Merged
namespace enumeration order is not an assembly-selection policy. The qualified
binding fast path uses the existing canonical metadata lookup, retaining local
source declarations and the declaration-completion boundary used during early
binding. Reference ordering and cold versus warmed metadata queries must not
change the selected base type. No public cache API or target contract is added.

## Cold source imports and signature binding

A semantic query can discover an imported source type before the containing
function's declared symbol is requested. Deferred import setup must make source
type declarations available before resolving that scope; it must not complete
member signatures while the scope is unfinished. `GetSymbolInfo`, `GetTypeInfo`
and `GetDeclaredSymbol` return the same imported type regardless of whether a
symbol, declaration or diagnostics query runs first, and regardless of source-file
order. This remains compiler-owned lazy binding, with no language-service cache
bypass or requirement to request diagnostics first.
