# Macros

## Overview

Raven supports attached declaration macros and freestanding expression macros. A macro is a compiler-driven expansion that produces ordinary Raven syntax before normal semantic analysis continues.

Use a macro when code needs to generate or transform Raven declarations or
expressions—something an ordinary function cannot do. A macro expands to normal
Raven syntax before the compiler analyzes the program.

Macros are distinct from .NET attributes:

* `[Serializable]` is an attribute.
* `#[Observable]` is a macro.

Macros are compiler plugins. Macro resolution and expansion are owned by
`Compilation` and occur during binding; creating a `Workspace` is not required.
Analyzers and generators are instead workspace plugins whose discovery and
orchestration belong to a workspace or build host. A project system may resolve
a macro asset, but it passes that asset to the compiler and does not own the
macro's semantic execution.

An analyzer may optionally query compiler-provided retained structure for a
macro that explicitly supplies it. An `ExpressionSyntax` embedded in that
structure can trigger ordinary Raven expression analysis when an analyzer host
is present. If a macro does not retain structure, the query returns no
structure and analyzers must not infer one from raw tokens or expansion output.
Compiling and expanding a macro must not require a workspace or any analyzer to
be loaded.

Macros are resolved from compiler-plugin assemblies. Their meaning is defined
by the referenced macro implementation, not by the parser. The current SDK
uses an explicit `RavenMacro` project item as transitional plumbing; the
intended dependency model is provider-declared compiler-plugin metadata carried
through a normal project or package dependency.

`MacroKind` remains part of the common `IMacroDefinition` surface, but it is implied by the specialized macro interface:

* `IAttachedDeclarationMacro` implies `AttachedDeclaration`
* `IFreestandingExpressionMacro` implies `FreestandingExpression`
* `ITokenTreeExpressionMacro` implies `FreestandingExpression`

## Attached macro syntax

An attached macro uses a `#` directly followed by an attribute list:

```raven
#[Observable]
var Title: string
```

The `#` token is part of the macro syntax. It is not optional.

### Disambiguation with directives

`#` starts a macro attribute only when it is immediately followed by `[`.

Examples:

```raven
#[Observable]
var Title: string

#pragma warning disable RAV0103
```

`#pragma` and other directive forms remain directives. They do not parse as macros.

## Freestanding macro syntax

A freestanding expression macro uses `#name(...)` in expression position:

```raven
func Main() -> int => #answer()
```

The expression expands to an ordinary Raven expression before normal expression binding continues.

A token-tree expression macro uses a raw brace-delimited body:

```raven
func Main() -> string => #query {
    from user in users
    where user.IsActive
    select user.Name
}
```

The compiler recognizes and balances the invocation envelope, but does not run
the body through ordinary Raven tokenization. The body is preserved as authored
so a macro can implement a custom DSL lexer/parser without producing unrelated
Raven lexer diagnostics.

`TokenTreeMacroContext` exposes the raw body text, its authored
`BodySpan`, body-relative diagnostic helpers, and Raven expression or statement
parsing for the complete body or a selected body-relative span. This supports
both complete custom parsing and hybrid DSLs with embedded Raven fragments.

`ParseExpression()` and `ParseExpression(span)` return recovered Raven syntax
directly. The corresponding `ParseExpressionResult` overloads return a
`MacroSyntaxParseResult<ExpressionSyntax>` containing that syntax, immutable
native parser diagnostics, and `HasErrors`. These diagnostics retain locations
in the authored invocation tree and may be forwarded through
`FreestandingMacroExpansionResult.Diagnostics`.

`ParseStatement()` and `ParseStatement(span)` provide the equivalent
syntax-only API for one complete Raven statement. Their
`ParseStatementResult` counterparts return
`MacroSyntaxParseResult<StatementSyntax>` with native diagnostics mapped to the
authored body. Both expression and statement helpers reject trailing input.

Token-tree expression macros implement `ITokenTreeExpressionMacro`. A
token-tree-only macro must be invoked with braces; an argument-based macro must
be invoked with parentheses.

### Expression quotes

`#quote { expression }` is a compiler-owned token-tree macro. It captures one
complete Raven expression as syntax data and expands to ordinary, fully
qualified `SyntaxFactory` construction code. Tokens and trivia are preserved.
Parser diagnostics, trailing input, and incomplete recovery are rejected at
locations within the authored body.

Within an expression quote, `#(expression)` inserts the resulting
`ExpressionSyntax` into the quoted structure. The `#` and `(` are adjacent,
the hole contains exactly one complete ordinary Raven expression, and multiple
holes are permitted. Hole expressions are validated by Raven's parser at their
authored locations and type-checked through the ordinary generated expansion.
No splice-specific token kind is introduced.

The result is a runtime `ExpressionSyntax` value from
`Raven.CodeAnalysis`. The consuming project must currently carry a compatible
runtime reference to that assembly; the intrinsic itself does not require a
macro plugin reference. Statement, member, declaration, token, identifier,
list, and repetition quote/splice forms are not part of the current language.

The raw body is the source of truth. Any standard Raven token stream,
macro-local keyword overlay, custom lexer token stream, or custom DSL syntax
tree is derived from that body and remains scoped to the macro invocation.
Macro-local token kinds do not alter ordinary Raven lexing or `SyntaxKind`.

### Token streams

`TokenTreeMacroContext.CreateTokenStream()` returns the stream selected for the
resolved macro. Streams implement `IMacroTokenStream` and emit `SyntaxToken`
values with body-relative positions.

By default, Raven uses its normal lexer over the macro body. A macro can
implement `IMacroKeywordProvider` to reclassify selected identifier text with a
provider-owned `RawKind` and keyword or reserved-word metadata. The token keeps
its ordinary Raven `IdentifierToken` kind, so the overlay does not change
normal Raven grammar or lexing.

A macro with a genuinely different lexical grammar can implement
`IMacroTokenStreamProvider`. The compiler discovers that capability with the
macro definition and uses the returned custom stream instead of the default
Raven-backed stream. Fully custom tokens may use `SyntaxKind.None` plus their
provider-owned raw kind.

Equal raw-kind integers from different macro providers do not imply equal token
kinds. The provider owns their meaning.

The minimal direct-lowering pattern does not require a custom syntax tree. For
example, a macro can mark `unless` as a body-scoped keyword, consume it from the
standard stream, parse the remaining body-relative span as a Raven expression,
and return an ordinary logical-negation expression:

```raven
let shouldRetry = #guard {
    unless retryCount < 3
}
```

This pattern is the starting point for DSLs with multiple clauses and embedded
Raven fragments. Retained DSL structure is optional and can be added later when
editor tooling or more involved lowering requires it.

A macro may identify several fragment spans from the same stream. For example,
the sample `#choose` macro treats `test`, `then`, and `otherwise` as
macro-reserved clause words, parses the text between them as three Raven
expressions, and lowers them directly to an ordinary `if` expression. Clause
words are not added to Raven's global keyword set.

The initial LINQ-like sample supports:

```raven
let result = #query {
    from item in source
    where item.IsActive
    select item.Name
}
```

It lowers directly to `source.Where(item => item.IsActive).Select(item =>
item.Name)`. The `where` clause is optional. The authored range variable is
used as the generated lambda parameter; the macro does not introduce hidden
temporary names. This sample shape is not part of Raven's ordinary grammar and
does not add `from` or `select` to the global keyword set.

## Placement rules

Macro attributes follow the same placement rules as declaration attributes:

* A macro attribute may appear only directly before a declaration.
* No blank line may separate the macro attribute from the declaration it applies to.
* Multiple attribute lists may appear before the same declaration.
* Normal attributes and macro attributes may be mixed in the declaration prelude.
* Union case declarations are type declarations for attached macro target validation, so macros that target `MacroTarget.Type` may be applied to `case` declarations.

Example:

```raven
[Obsolete]
#[Observable]
public var Title: string
```

## Arguments

Attached macros may take arguments.

Both positional and named arguments are supported:

```raven
#[Observable]
#[Observable("TitleChanged")]
#[Observable(Name: "TitleChanged", Notify: true)]
```

The compiler parses and preserves these arguments generically. Their interpretation is defined by the macro implementation.

For attached declaration macros, plugins currently receive the raw parsed arguments through `AttachedMacroContext.ArgumentList` and a convenience parsed view through `AttachedMacroContext.Arguments`.

For freestanding expression macros, the equivalent APIs are `FreestandingMacroContext.ArgumentList` and `FreestandingMacroContext.Arguments`.

Each parsed `MacroArgument` exposes a richer constant representation through `Constant`, plus the evaluated CLR value directly through `Value` as a convenience.

For argument and usage validation inside the macro itself, plugins may also report macro-owned expansion diagnostics through `MacroExpansionResult.MacroDiagnostics` / `FreestandingMacroExpansionResult.MacroDiagnostics`. The helper methods `CreateDiagnostic(...)` and `CreateArgumentDiagnostic(...)` on both macro contexts create these diagnostics at either the macro site or a specific argument site.

This raw-argument model is transitional. The intended direction is typed macro parameter objects, so macro signatures can be validated and presented like normal attributes in completion and signature help. The public contract now includes `IMacroDefinition<TParameters>`, `IAttachedDeclarationMacro<TParameters>`, and `IFreestandingExpressionMacro<TParameters>` for that bound-parameter model.

Example direction:

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

The current typed-parameter binding slice supports:

* one public constructor for positional arguments
* public writable properties for named arguments
* constant conversion into common CLR primitive/reference types

The target experience is that macro arguments bind like attribute arguments:

* completion for named arguments
* signature help for supported shapes
* diagnostics for unknown names, missing required arguments, and invalid constant conversions
* typed parameter access in the macro implementation

Example macro-side validation:

```csharp
return new MacroExpansionResult
{
    MacroDiagnostics =
    [
        context.CreateArgumentDiagnostic(
            context.Arguments[0],
            "name cannot be empty",
            code: "VAL001")
    ]
};
```

## Expansion model

Macro expansion is not a preprocessor step. The source file is parsed normally first. After parsing, the compiler resolves macros from referenced macro assemblies and requests expansions using structured Raven syntax.

### Ordering and composition

When multiple attached macros apply to the same declaration, Raven runs them as a source-ordered pipeline over one declaration.

This has two consequences:

* Macros on the same declaration are visited in source order.
* `AttachedMacroContext.TargetDeclaration` always refers to the original authored declaration.
* `AttachedMacroContext.CurrentDeclaration` refers to the declaration shape immediately before the current macro runs.

When Raven integrates the results for one declaration, it uses this order:

1. introduced members from all attached macros, preserving macro source order
2. the effective declaration itself, where the last macro that returns `ReplacementDeclaration` wins
3. peer declarations from all attached macros, preserving macro source order

If a macro returns `ReplacementDeclaration`, that replacement becomes the `CurrentDeclaration` seen by later attached macros on the same declaration. If a macro only introduces members or peer declarations, `CurrentDeclaration` does not change.

For parent/child relationships, parent-declaration macros still see the original parsed shape of the parent declaration. A macro attached to a type should not assume that attached macros on its members have already rewritten the type syntax visible through `AttachedMacroContext.TargetDeclaration` or `AttachedMacroContext.CurrentDeclaration`.

The current attached-macro system supports these generic result shapes:

* compiler-owned macro expansion diagnostics with custom messages and precise locations
* raw compiler diagnostics for advanced scenarios
* introduced members
* replacement of the annotated declaration

Expansion must remain generic. The compiler does not hardcode macro-specific behaviors such as property notification or equality semantics.

Freestanding expression macros return a generic expression-expansion result shape:

* compiler-owned macro expansion diagnostics with custom messages and precise locations
* raw compiler diagnostics for advanced scenarios
* replacement expression

## Author guidelines

When designing attached macros:

* Prefer one replacement-owning macro per declaration. If multiple macros replace the same declaration, the last replacement wins.
* Use `TargetDeclaration` when you need the original authored syntax, and use `CurrentDeclaration` only when you intentionally want same-target pipeline behavior.
* Use introduced members for additive behavior and keep cross-macro coordination explicit rather than inferred from transformed syntax.
* When a parent declaration and its members both use macros, keep the parent macro resilient to the original member syntax shape.
* If two macros need to cooperate, define that cooperation through explicit arguments, naming conventions, or generated marker members instead of depending on expansion order side effects.

## Project references

The current SDK references macro implementations with `RavenMacro` items in the
project file.

Example:

```xml
<ItemGroup>
  <RavenMacro Include="../macros/ObservableMacros.rvnproj" />
</ItemGroup>
```

The compiler loads the referenced macro assembly, resolves exported macros by name, validates target compatibility, and reports failures as ordinary diagnostics.

`RavenMacro` is not the intended final consumer experience. A macro
project/package should declare that its output is a Raven compiler plugin. A
consumer then takes a normal project or package dependency, and the SDK
classifies and passes the compiler-plugin asset to the compilation
automatically. Raven should not scan and execute every ordinary runtime
reference, and source files should not need macro import directives. A future
assembly-level compiler-plugin marker may explicitly identify plugin types or
authorize discovery of `IRavenMacroPlugin` implementations within that marked
assembly.

The selected Raven compiler and SDK may also register a version-matched default
macro set automatically. Default macros require no source import or explicit
dependency and must be available in the Playground. `#quote` is the first such
macro; future defaults such as `#embedFile` may be compiler intrinsics or
SDK-bundled plugins without exposing that distinction at the invocation site.

The compiler API supports an explicit same-project macro source partition.
Trees supplied through `Compilation.AddMacroSyntaxTrees` are compiled as an
in-memory library and activated before consumer binding. Their diagnostics are
reported by the consumer compilation, their macros participate in completion,
and their implementation declarations are excluded from runtime emit.

The dedicated-file MVP can classify this partition automatically. A type that
implements the local plugin entry point is marked with an ordinary attribute:

```raven
import Raven.CodeAnalysis.Macros.*

[LocalMacroPlugin]
class ProjectMacros: IRavenMacroPlugin {
    // ...
}
```

`Compilation.AddSyntaxTreesWithLocalMacros`, Workspace compilation, and the SDK
move the complete file containing that marker into the compile-time partition.
The SDK form needs neither a `RavenMacro` item nor an explicit project reference
to the compiler contracts. `LocalMacroPluginAttribute` is not written with
`#[...]`: it classifies compiler-plugin implementation source rather than
invoking a macro.

The automatic rule is intentionally syntax-only and file-granular. Local macro
plugins and their supporting types must be kept in a dedicated source file;
consumer declarations in a marked file are not emitted into the runtime
assembly. The partition remains acyclic: macro source can reference metadata
and other macro plugins but cannot bind against consumer source declarations.

A mixed source file uses `[LocalMacro]` instead:

```raven
import Raven.CodeAnalysis.Macros.*

[LocalMacro]
class AnswerMacro: ITokenTreeExpressionMacro {
    // ...
}

let answer = #answer { }
```

`[LocalMacro]` classifies only the marked top-level type and everything nested
within it as compile-time-only. Every separate top-level plugin entry point,
macro definition, or support type needed by the local plugin must be marked.
The compiler creates same-length macro and consumer projections, retaining line
breaks and replacing declarations from the opposite partition with whitespace.
This preserves authored offsets for diagnostics while keeping macro
implementation types out of runtime emit.

The browser Playground supports this form in its single user buffer. Semantic
editor services inside the projected macro declarations are not yet complete;
the current implementation prioritizes compilation, expansion, diagnostics,
emit, and execution.

Across incremental compilations, an unchanged local macro partition may reuse
its emitted in-memory plugin artifact. Changes limited to consumer source do not
recompile that artifact. Changes to macro source, compilation or parse options,
metadata references, macro references, or assembly identity invalidate it and
therefore invalidate expansions that depend on the local registry. Every
snapshot still owns a fresh macro semantic compilation, and reused partition
diagnostics are associated with the current projected syntax trees.

Macro-reported validation failures currently surface through the shared compiler diagnostic `RAVM021`, with the macro name and custom message embedded in the diagnostic text. The diagnostic location may point either at the macro site or at a specific argument.

## Example

```raven
class MyViewModel: ObservableBase {
    #[Observable]
    var Title: string
}
```

In this example, `#[Observable]` is an attached property macro. The macro may replace the property declaration with ordinary Raven members such as backing storage and accessor bodies.
