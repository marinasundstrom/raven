# Raven Macro System

> ⚠️ 🧩 This proposal has been partly implemented

Current implementation status:

* `#[MacroName]` syntax is reserved for macro-style attributes.
* `#` only starts a macro attribute when it is immediately followed by `[`. Other `#...` forms continue to lex as directives such as `#pragma`.
* Macro-style attributes are kept out of the normal CLR attribute binding/emission pipeline.
* Initial .NET plugin contracts exist under `Raven.CodeAnalysis.Macros`.
* Project files can reference macro assemblies with `RavenMacro` items and the compiler now resolves attached macros against those plugin assemblies.
* Freestanding expression macros now use `#name(...)` syntax, resolve through the same plugin registry, and support the same typed parameter-object binding direction as attached macros.
* Unknown macros, duplicate exports, invalid targets, plugin load failures, plugin-thrown expansion failures, and macro-reported validation failures now produce compiler diagnostics.
* Attached macros are invoked through a generic semantic-model expansion path and expansion results are cached per compilation.
* `MacroExpansionResult` now models both additive members and optional declaration replacement.
* Generated-member and replacement integration into normal binding/codegen is not implemented yet.

## 1. Goals

Raven macros provide:

* Structured syntactic transformation
* Explicit and contained DSL boundaries
* Precise source diagnostics
* Minimal compiler intrusion
* Optional semantic validation
* Strong developer experience (completion, tooling, navigation)
* Full tooling compatibility

Macros are **compiler-integrated syntax transformers**, not textual preprocessors.

---

# 2. Core Principles

1. **SourceTree is the source of truth**
   All diagnostics are reported in terms of original source spans.

2. **Macros are explicit**
   DSL content exists only within explicit macro invocation syntax.

3. **Parsing is deterministic**
   The file parses fully without macro expansion.

4. **Expansion happens in a dedicated macro phase near binding**
   Macro resolution may use binding services, but expansion should not be an ad hoc callback spread through ordinary binder logic.

5. **Expansion is pure and cached**
   Macro expansion must be deterministic and side-effect free.

6. **Expansion lowers to ordinary Raven syntax**
   Binder and codegen operate only on standard Raven constructs.

7. **Macros are compiled code**
   Macros are implemented as compiled Raven or .NET code and executed at compile time.

---

# 3. Macro Syntax

## Attached Macros (current direction)

```raven
#[AddEquatable]
class User {
    val Name: string
}
```

Another motivating attached-macro shape is property notification:

```raven
class MyViewModel: INotifyPropertyChanged {
    #[Observable]
    var Title: string
}
```

Characteristics:

* Annotation-style syntax that feels familiar in a .NET-targeted language
* Distinct from normal CLR/custom attributes
* Follows the same placement rules as declaration attributes: it must appear directly on the target declaration with no intervening blank line
* Intended for plugin-backed expansion into ordinary Raven declarations
* Intended to support both additive member generation and declaration replacement
* Supports optional positional and named arguments using the normal attribute argument-list syntax

Example argument forms:

```raven
#[Observable]
#[Observable("TitleChanged")]
#[Observable(Name: "TitleChanged", Notify: true)]
```

The compiler parses and preserves these arguments generically. Their meaning is defined entirely by the macro plugin.

For attached declaration macros, the raw parsed arguments are exposed directly on `AttachedMacroContext.ArgumentList`, and a convenience parsed view is exposed on `AttachedMacroContext.Arguments`. Each parsed `MacroArgument` exposes a richer constant representation through `Constant`, plus the evaluated CLR value directly through `Value` as a convenience.

For macro-authored validation errors, the expansion result can also carry `MacroExpansionDiagnostic` entries. `AttachedMacroContext.CreateDiagnostic(...)` and `AttachedMacroContext.CreateArgumentDiagnostic(...)` provide the intended convenience path for reporting a custom message at either the macro site or a specific argument.

The next contract direction is typed parameter objects, exposed through generic macro definitions:

```csharp
public sealed class ObservableMacroParameters
{
    public bool Notify { get; init; } = true;
    public string? Name { get; init; }
}

public sealed class ObservableMacro : IAttachedDeclarationMacro<ObservableMacroParameters>
{
    ...
}
```

The initial binding slice for this direction supports:

* one public constructor for positional arguments
* public writable properties for named arguments
* constant conversion into common CLR primitive/reference types

This is intended to let the compiler eventually bind macro arguments the same way it binds normal attribute arguments:

* positional/named parameter validation before expansion
* constant conversion diagnostics before expansion
* editor completion and signature help based on the declared parameter object
* strongly typed access to parameters in the plugin

Current validation diagnostics reported by the macro itself are surfaced through a shared compiler-owned diagnostic ID, with the specific macro name and message carried in the formatted text rather than through plugin-defined descriptor IDs.

## Invocation Macros (future / Rust-style)

```raven
linq! {
    from user in db.Users
    where user.IsActive && user.Age >= 21
    select user.Name
}
```

Characteristics:

* Explicit `!` invocation marker
* Body captured as `TokenTree`
* Must appear in a valid syntactic slot
* Deferred until after attached macros are stable
* Not part of the current implementation slice

---

# 4. Macro Targets

Each macro declares a target category:

* `expr`
* `stmt`
* `member`

Expansion must match the declared category.

The parser enforces placement correctness.

---

# 5. Macro Implementations

## 5.1 Compiled Macro Assemblies

Macros are implemented as compiled code:

* Raven
* Any .NET language

They may reside in:

* A referenced macro assembly
* The same project (subject to compilation model)

Macros are discovered via a well-known contract (e.g. `IMacro` and/or `[RavenMacro]`).

---

## 5.2 Execution Model

When macro expansion runs:

1. Resolve macro implementation from referenced assemblies
2. Execute macro with structured Raven syntax
3. Receive:

   * Lowered Raven syntax
   * Source mapping
   * Macro diagnostics
4. Bind the lowered syntax in the invocation’s binder context

Macro output becomes part of the build as if written by the user.

### Current attached-macro composition semantics

The current implementation composes attached declaration macros by source order, but not by feeding one macro's rewritten declaration into the next macro.

For one annotated declaration:

1. each attached macro is resolved and expanded against the original declaration syntax
2. introduced members from all macros are integrated first, in macro source order
3. the declaration itself is kept or replaced, with the last `ReplacementDeclaration` taking effect
4. peer declarations from all macros are integrated last, in macro source order

For parent/child relationships, a parent declaration macro likewise sees the original parsed declaration shape for that parent. It should not assume that member-level macro replacements have already been substituted into `AttachedMacroContext.TargetDeclaration`.

This means the current model is intentionally closer to "independent transforms merged deterministically" than to a chained rewrite pipeline.

---

## 5.3 Determinism Requirement

Macro expansion must be deterministic with respect to:

* Invocation body tokens
* Compilation options
* Referenced assemblies
* Semantic context (if enabled)

Expansion results are cached per compilation.

Cache key includes:

* Invocation span
* Macro name
* Body token hash
* Macro assembly identity/version
* Compilation version

---

# 6. Macro Processing Model

Macros consist of three conceptual stages:

1. **Macro Resolution**
2. **Optional Semantic Validation**
3. **Expansion Building**

For the first implementation slice, the input model is ordinary Raven syntax rather than `TokenTree`.

---

# 7. Token Processing

Macros receive a `MacroContext` with:

### Token Cursor

* `Peek(offset)`
* `Read()`
* `TryRead(kind or text)`
* `Expect(kind or text)`

### Token Remapping (Optional)

Macros may use a local remapping view to interpret identifiers (e.g. `"where"`) as DSL keywords.

Remapping:

* Is contextual
* Does not mutate underlying tokens
* Does not affect the core language

---

# 8. Embedded Raven Parsing

`MacroContext` provides parser entrypoints:

* `ParseExpression(stopCondition)`
* `ParseStatement(stopCondition)`
* (optional) `ParseType`, `ParseMember`

These:

* Consume tokens from the macro cursor
* Use Raven’s real parser
* Preserve original token spans

This ensures precise source mapping for embedded Raven fragments.

---

# 9. Optional Semantic Pass

Macros may perform semantic validation before expansion.

Semantic context may provide:

* `GetOperation(node)`
* `GetTypeInfo(expression)`
* `GetSymbolInfo(node)`
* `LookupSymbols(position)`
* `ExpectedType` at invocation site

### Operations API

The semantic pass uses Raven’s Operations API as the primary semantic entrypoint.

### Constraint

The semantic pass must not require binding the macro’s generated expansion.

Allowed:

* Querying semantics of SourceTree nodes
* Querying semantics of embedded fragments parsed from macro input

Not allowed:

* Binding expansion output during semantic validation

This prevents semantic cycles.

---

# 10. Current Expansion Model

The current implementation keeps macro execution adjacent to semantic analysis, but not scattered through ad hoc binder callbacks.

Today the flow is:

1. Parse source
2. Resolve attached macro attributes against loaded `RavenMacro` assemblies
3. Validate macro target compatibility
4. Invoke the plugin with structured Raven syntax
5. Cache the resulting `MacroExpansionResult` on the semantic model
6. Surface plugin diagnostics through normal compiler diagnostics

In the current implementation slice, “plugin diagnostics” primarily means macro-owned expansion diagnostics lowered to the compiler’s shared macro-expansion diagnostic (`RAVM021`) with source-accurate locations.

This makes macro expansion available to tooling and inspection without yet committing Raven to generated-member substitution inside binding/codegen.

This is a substitution model.

SourceTree remains authoritative.

No merged tree is required.

## 10.1 Next Capability: Declaration Replacement

The next planned attached-macro capability is declaration replacement.

This is required for macros such as:

```raven
class MyViewModel: INotifyPropertyChanged {
    #[Observable]
    var Title: string
}
```

In that shape:

* `#[Observable]` must be able to replace or synthesize the annotated property implementation
* the surrounding type shape may already declare `INotifyPropertyChanged`

The important constraint is that Raven should stay generic:

* the compiler should not know what `Observable` means
* the macro plugin should decide whether a declaration is valid, redundant, or requires companion macros
* the compiler should only know how to apply generic replacement/additive expansion mechanics

So the macro system needs to evolve from:

* additive introduced members only

to:

* introduced members
* optional replacement for the annotated declaration
* diagnostics

That replacement should still lower to ordinary Raven syntax. The macro system gains a generic "replace this declaration with these ordinary declarations/accessors/members" capability, rather than any compiler-internal understanding of notification semantics.

---

# 11. Developer Experience and Tooling

## 11.1 Completion Inside Macro Bodies

The macro system supports completion within macro invocations.

Two levels are supported:

### DSL-Level Completion

Macros may optionally implement a language service interface:

```
GetCompletionItems(MacroCompletionContext context)
```

This enables:

* Clause keyword suggestions
* DSL structure suggestions
* Context-aware macro completions

### Embedded Raven Completion

When completion occurs inside an embedded Raven fragment:

1. The macro parses the fragment using `ParseExpression` or `ParseStatement`
2. A temporary binder overlay is created
3. Normal Raven completion is executed

This allows:

* Member completion (e.g. `user.`)
* Type-aware suggestions
* Full semantic completion inside macro bodies

---

## 11.2 Language Service Integration

Macros may optionally provide:

* Completion
* Classification
* Navigation
* Quick info

These integrate with Raven’s tooling pipeline without requiring macro expansion.

---

# 12. Macro Expansion Result

Each expansion produces:

```
MacroExpansionResult {
    IntroducedMembers
    ReplacementDeclaration?
    MappingTable
    Diagnostics
}
```

Expansion must be:

* Deterministic
* Pure
* Cached at Compilation level

---

# 13. Source Mapping

## Mapping Structure

Each expansion maintains:

```
ExpandedSpan → SourceSpan
```

Categories:

* `Exact`
* `Clause`
* `Invocation` (fallback)

## Diagnostic Remapping

When binding emits a diagnostic:

1. If location already points to original source span → keep.
2. Otherwise lookup mapping.
3. Rewrite to mapped source span.
4. If no entry → map to invocation span.

All user-facing diagnostics reference SourceTree coordinates.

---

# 14. Expanded View (Optional)

The compiler may expose:

* Full ExpandedTree
* Pretty-printed expansion
* Mapping information
* Diff view (source vs expansion)

ExpandedTree is not required for binding.

SourceTree remains canonical.

---

# 15. Stability Model

Stable:

* Syntax tree model
* Token model
* Macro host API
* Mapping infrastructure
* Operations API surface
* Diagnostic model
* Macro language service contracts

Internal / free to evolve:

* Binder internals
* Lowering passes
* Code generation
* Optimization pipeline

---

# 16. Architectural Positioning

Raven’s macro system combines:

* Rust-style explicit invocation syntax
* Roslyn-style structured syntax trees
* Nemerle-style compile-time transformation
* Compiler-as-a-service discipline
* First-class tooling integration

While preserving:

* Deterministic grammar
* Clear DSL boundaries
* Source-accurate diagnostics
* Controlled expansion

---

# 17. Summary

Raven macros are:

* Explicit
* Localized
* Deterministic
* Cached
* Source-mapped
* Tooling-aware
* Implemented as compiled code

Expansion occurs lazily during binding via substitution.

SourceTree remains the single source of truth for diagnostics and user-facing locations.
