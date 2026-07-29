# Macros

> [!NOTE]
> **Status: work in progress.** This page documents the macro behavior currently
> implemented by the compiler, not a stable compatibility contract. Macro
> syntax, the authoring and activation model, compiler APIs, and tooling
> integration may change as the design develops.

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
by the referenced macro implementation, not by the parser. A reusable Raven
macro project marks its assembly with `RavenCompilerPlugin`, preferably naming
each exported macro definition explicitly:

```raven
[assembly: RavenCompilerPlugin(typeof(QueryMacro))]
```

The same attribute and contracts can be used from a C# compiler-plugin
project.

Apply the marker once for each macro definition intentionally exported from the
assembly. The declared type must be a non-abstract macro implementation in the
marked assembly with a public parameterless constructor. A bare
`[assembly: RavenCompilerPlugin]` marker authorizes fallback discovery of
concrete `IMacroDefinition` implementations.
Explicit and bare markers cannot be mixed. The marker is an inter-assembly
export boundary, not part of declaring a same-project macro. Consumers use a
normal project reference.

`MacroKind` is compiler-owned classification metadata. Concrete macro classes
do not implement a `Kind` property. `MacroFacts.GetKind` derives it from the
single category-specific interface implemented by the definition:

* `IAttachedDeclarationMacro` implies `AttachedDeclaration`
* `IFreestandingExpressionMacro` implies `FreestandingExpression`
* `ITokenTreeExpressionMacro` implies `FreestandingExpression`

Target applicability belongs to `IAttachedDeclarationMacro`. Attached
definitions expose `Targets`; freestanding and token-tree definitions do not
declare a redundant `MacroTarget.None` property. Code that handles the common
`IMacroDefinition` surface can use `MacroFacts.GetTargets`, which returns
`MacroTarget.None` for non-attached definitions.

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
func Main() -> string => query! {
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

`FreestandingMacroExpansionResult.FromExpression(...)` creates an expression
result, optionally forwarding native parser diagnostics.
`FromDiagnostic(...)` and `FromDiagnostics(...)` create macro-authored,
native-parser, or combined diagnostic results without requiring property
initializers. `Empty` represents an explicit no-change result. Mutable
properties remain available for compatibility.

Attached declaration results use the corresponding
`MacroExpansionResult.FromReplacement(...)`, `FromIntroducedMembers(...)`,
`FromPeerDeclarations(...)`, `FromDiagnostic(...)`, and
`FromDiagnostics(...)` factories. Replacement overloads may include introduced
members and peer declarations. `MacroExpansionResult.Empty` represents no
declaration change.

Token-tree expression macros implement `ITokenTreeExpressionMacro`. They may
accept a typed argument list before the body by implementing
`ITokenTreeExpressionMacro<TParameters>`:

```raven
let result = query!(Dialect: "sql") {
    from user in users
    select user.Name
}
```

Token-tree macros retain a compatible hash spelling:

```raven
let result = #query(Dialect: "sql") {
    from user in users
    select user.Name
}
```

The two spellings have the same binding and expansion semantics. The syntax
tree represents them as `BangMacroExpressionSyntax` and
`HashMacroExpressionSyntax`, respectively, under the shared abstract
`FreestandingMacroExpressionSyntax` base. The bang spelling requires a
brace-delimited token-tree body. There must be no line break between the macro
name and `!`, or between `!` (or its argument list) and the opening brace; this
lookahead keeps ordinary postfix `!` expressions unambiguous.

The bang spelling is the preferred source form. A freestanding macro is a
parsed expression invocation that expands an owned region of syntax; it is not
a preprocessor directive. Directive-looking syntax remains appropriate for
lexical compilation controls such as `#if`, while `name! { ... }` preserves the
ordinary flow of expression code.

The compiler binds the parenthesized arguments into `context.Parameters` while
leaving the brace-delimited body unrestricted and available through the same
raw text and token-stream APIs. A non-generic token-tree macro rejects supplied
arguments. A token-tree macro must be invoked with braces; an argument-based
macro must be invoked with parentheses.

### Macro function declarations

Raven recognizes the initial function-oriented macro declaration syntax at
compilation-unit and namespace-member scope:

```raven
macro func Compile<TDelegate>(body: ExpressionSyntax) -> TDelegate
    where TDelegate: Delegate
{
    // Expansion implementation
}
```

`macro` is contextual. It remains an ordinary identifier unless it is followed
by `func` where a member declaration may begin. The syntax tree represents the
construct with a dedicated `MacroFunctionDeclarationSyntax`, retaining
attributes, modifiers, generic parameters, ordinary parameters, a return
clause, constraints, and either a block or expression body.

Declaring a macro function necessarily places its compile-time partition in
the `Raven.CodeAnalysis` programming model. The compiler supplies that assembly
reference when it builds local macros; source signatures and semantic tooling
therefore expose actual compiler API types rather than language-only facades.

This establishes syntax, the semantic declaration signature, and executable
lowering for same-compilation argument-style and attached macro functions.
`SemanticModel.GetDeclaredSymbol` returns an `IMacroFunctionSymbol` with
`SymbolKind.MacroFunction`, its call-site return type, parameters, generic
parameters, and constraints. A macro function is not an `IMethodSymbol`: it is
compile-time language structure rather than a CLR method. Its body is therefore
not emitted into the consumer program as an ordinary runtime function body.

An optional contextual `on` clause makes the macro attached and declares its
allowed target:

```raven
macro func Observe(enabled: bool) on property: Property {
    if enabled {
        replace Rewrite(property)
    }
    introduce CreateBackingField(property)
}
```

The named form `on property: Property` binds the current declaration to
`property`. The shorthand `on Property` binds it to `target`. The supported
target roles correspond to `MacroTarget`: `Type`, `Method`, `Property`,
`Field`, `Event`, `Parameter`, `Accessor`, and `Constructor`. A declaration
without `on` is an argument-style freestanding expression macro. Token-stream
and syntax-projection inputs use the same parameter syntax:

```raven
macro func AddOffset(offset: int, value: ExpressionSyntax) {
    expand ParseExpression(value.ToString() + " + " + offset.ToString())
}
```

`ExpressionSyntax` is the real Raven.CodeAnalysis syntax type. It remains a
caller-supplied argument, but the macro binder projects the authored argument
node instead of requiring a constant value. Syntax projections can be freely
combined with ordinary typed parameters and participate in the same positional
and named argument ordering. They cannot declare default values.

Raw token-stream input is expressed in the same way:

```raven
macro func Query(dialect: string, body: IMacroTokenStream) {
    let token = body.ReadToken()
    expand BuildQuery(dialect, token)
}
```

`IMacroTokenStream` is the real Raven.CodeAnalysis macro stream interface. In a
macro function signature it denotes the single raw `{ ... }` invocation body;
it is not an argument supplied inside the invocation's argument list. Other
parameters remain typed caller-supplied
values, so the example is invoked as `Query!("sql") { ... }`. A macro function
may declare at most one `IMacroTokenStream` parameter. It cannot have a default value
or be combined with an attached `on` target. `IParameterSymbol.MacroRole`
distinguishes caller-supplied `Value` parameters, caller-supplied
`ExpressionSyntax` projections, and the compiler-supplied `IMacroTokenStream`
parameter for semantic tooling. Runtime macro parameter descriptors expose the
same role through `MacroParameterDescriptor.Role`.

An advanced token-tree macro may instead declare a
`TokenTreeMacroContext` parameter. That parameter has the compiler-supplied
`Context` role and receives the complete provider context rather than a
caller-supplied argument. It is intended for macro libraries that need the
low-level diagnostic, token-tree, and expansion APIs; `Raven.Macros` currently
uses it to forward its Raven-authored declarations to the transitional
`StandardMacroExpansions` implementations.

Macro bodies are ordinary synchronous Raven blocks augmented by three
contextual contribution statements:

* `expand expression` sets the expression produced by a freestanding macro;
* `replace declaration` sets the replacement of an attached macro; and
* `introduce member-or-members` appends introduced members to an attached
  macro.

These statements do not return from the macro body. They update the
invocation's expansion result when execution reaches them, so they may coexist
with declarations, conditionals, loops, and other ordinary statements. A later
`expand` or `replace` supersedes the earlier value; repeated `introduce`
statements append in execution order. This permits one attached expansion to
both replace its target and introduce members. Using `replace` or `introduce`
in a freestanding macro, or `expand` in an attached macro, is a compile-time
error.

The compiler currently lowers an executable, non-generic compilation-unit
declaration into an isolated provider adapter and, when needed, a parameter
object implementing the existing typed macro contracts. `expand`, `replace`,
and `introduce` lower to operations on a compiler-provided result builder,
whose final value becomes `FreestandingMacroExpansionResult` or
`MacroExpansionResult`. These synthesized types are implementation details and
are not the semantic identity exposed to tools.

Macro functions are synchronous. The `async` modifier and `await` expressions
are not supported in a macro function. This describes the source expansion
contract; a compiler host or provider implementation may still use
asynchronous APIs internally without exposing asynchronous expansion semantics
to Raven code.

Generic macro invocation, executable namespace-member declarations,
namespace-qualified macro lookup, imported short names, and naming conventions
remain later layers.
Existing class-authored dynamic and strongly typed macros remain supported and
expose the underlying provider API directly.

### Expression quotes

`quote! { expression }` is a token-tree macro provided by the standard
`Raven.Macros` compiler-plugin assembly. Its `quote` alias enters scope through
`import Raven.Macros.*`; the canonical `Raven.Macros.Quote!` name is available
without that wildcard import. It captures one
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
`Raven.CodeAnalysis`. The Raven compiler and SDK project integration resolve
that assembly through the macro library's ordinary dependency closure. Runtime
dependencies are copied when the emitted application actually references them.
Hosts that use the `Compilation` API directly must provide the macro and
metadata references themselves.
Statement, member, declaration, token, identifier, list, and repetition
quote/splice forms are not part of the current language.

`quote!` is syntax quotation, analogous in shape to the
compiler-integrated
operation quotation that converts a target-typed lambda to
`Expression<TDelegate>`. The representations are different: expression trees
contain standardized .NET operations and no source syntax, while `quote!`
produces Raven syntax with its tokens and trivia. The resulting syntax object
can be traversed and rewritten into a new immutable tree using
`Raven.CodeAnalysis`; it is not semantically bound until inserted into a
compilation.

### Runtime expression compilation

`compile<TDelegate>! { expression }` constructs an `ExpressionSyntax` using
the same quotation and `#(expression)` hole rules as `quote!`, compiles that
syntax at runtime, and returns `TDelegate`:

```raven
let increment = compile<System.Func<int, int>>! {
    value => #(SyntaxFactory.IdentifierName("value")) + 1
}
```

Exactly one explicit type argument is required, and it must resolve to a
delegate type. The quoted expression must evaluate to that delegate type.
The explicit type is part of the ordinary generated generic call, so normal
Raven binding validates it; the current macro model does not separately model
strongly typed generic macro parameters.

The quote phase parses the authored body during macro expansion and rejects a
structurally invalid or incomplete Raven expression. At runtime, after hole
values have produced the final syntax tree, the compile phase parses the
generated wrapper, binds names and types, and emits it. No delegate is returned
unless both the structural and semantic checks succeed.

Runtime compilation references the platform assemblies, assemblies already
loaded by the process, and any additional metadata references passed to
`RavenCompiler.Compile`. Compilation errors throw
`RavenCompilationException`, whose `Diagnostics` property contains the Raven
diagnostics. Each successful compilation loads a generated assembly into the
default assembly load context. Callers should therefore cache delegates for
repeated use and must treat syntax derived from untrusted input as executable
code.

The macro expands into an ordinary call to
`Raven.CodeAnalysis.RavenCompiler.Compile<TDelegate>`. SDK builds resolve and
copy the macro library's compatible `Raven.CodeAnalysis` dependency and its
runtime closure when the emitted program references them. Direct `Compilation`
API hosts must provide the assembly references and arrange runtime deployment
themselves.

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
let shouldRetry = guard! {
    unless retryCount < 3
}
```

This pattern is the starting point for DSLs with multiple clauses and embedded
Raven fragments. Retained DSL structure is optional and can be added later when
editor tooling or more involved lowering requires it.

A macro may identify several fragment spans from the same stream. For example,
the sample `choose!` macro treats `test`, `then`, and `otherwise` as
macro-reserved clause words, parses the text between them as three Raven
expressions, and lowers them directly to an ordinary `if` expression. Clause
words are not added to Raven's global keyword set.

The initial LINQ-like sample supports:

```raven
let result = query! {
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

This raw-argument model remains available for unrestricted macro implementations. Typed macro parameter objects allow macro signatures to be validated and later presented like normal attributes in completion and signature help. The public contract includes `IMacroDefinition<TParameters>`, `IAttachedDeclarationMacro<TParameters>`, `IFreestandingExpressionMacro<TParameters>`, and `ITokenTreeExpressionMacro<TParameters>` for that bound-parameter model.

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
* typed arguments combined with an unrestricted token-tree body

Typed value parameters are limited to values representable by the compiler's
constant model and conversions explicitly supported by the macro binder. The
compiler does not execute caller expressions to obtain argument values and
does not inject arbitrary runtime objects into a macro. Arguments outside this
boundary are rejected with a diagnostic. Syntax-node and token-stream inputs
remain explicit, type-directed projections so the macro contract makes access
to authored code visible.

`MacroFacts.GetParametersType(...)` and `MacroFacts.GetParameters(...)` expose
the compiler-normalized parameter schema without requiring tooling to inspect
the macro implementation itself. Each `MacroParameterDescriptor` identifies
the CLR type, positional or named role, ordinal, required state, and optional
constructor default. `SemanticModel.GetMacroSignatureHelp(...)` resolves that
schema at an attached, argument-style, or token-tree invocation and identifies
the active parameter for compiler hosts and editor tooling.

Completion uses that schema inside typed attached, argument-style, and
token-tree macro argument lists. It offers unused writable properties with
their Raven-facing type and inserts the named-argument form, such as
`Optimize: `. Constructor parameters remain positional and participate in the
same signature help.

String-valued parameters may later preserve
`System.Diagnostics.CodeAnalysis.StringSyntaxAttribute` as optional tooling
metadata, matching ordinary Raven parameters. That attribute is not the
representation for a token-tree body. A body has tokens and authored source
spans rather than a string value, and future highlighting/completion support
will use a general compiler-owned syntax-content descriptor. For example,
`quote! { let x = "test" }` can identify its body as Raven syntax, while a DSL
macro can identify a standard or custom syntax without changing Raven's normal
lexer.

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

For an attached nominal-type macro, the effective replacement declaration also
supplies the base class and interface list used when Raven binds the type shape.
This allows a macro to introduce a required member and add its corresponding
interface contract in the same expansion.

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

A reusable Raven macro project marks its output as a compiler plugin:

```raven
import Raven.CodeAnalysis.Macros.*

[assembly: RavenCompilerPlugin(typeof(QueryMacro))]

class QueryMacro: ITokenTreeExpressionMacro {
    // ...
}
```

The consumer uses an ordinary project reference:

```xml
<ItemGroup>
  <ProjectReference Include="../macros/ObservableMacros.rvnproj" />
</ItemGroup>
```

The workspace recognizes the assembly-targeted marker in referenced Raven and
C# projects, builds the provider through the compiler-plugin path, and passes
the resulting macro reference to `Compilation`. The marked provider is not
added as a runtime project reference. A bare marker authorizes fallback
discovery of direct macro definitions.

Raven does not scan unmarked runtime references for plugins, and consumer
source needs no macro import directive. The former consumer-authored
`RavenMacro` project item is not supported; reusable providers use ordinary
marked project, assembly, or package references. During compilation setup,
portable references—including direct DLL and resolved package references—are
inspected for the assembly marker through metadata only. Marked references are
activated and join the same macro registry as explicitly supplied and
same-project macros. Unmarked assemblies are not loaded for macro execution or
searched for implementation types.

The active macro set belongs to an immutable compilation snapshot. A new
snapshot recomputes activation when local macro source or the reference set
changes. Existing portable-reference fingerprints prevent a changed assembly
at the same path from reusing stale metadata state. For a package with separate
assets, the `ref/<tfm>` assembly remains a consumer metadata reference while a
marked `lib/<tfm>` implementation is activated as a macro reference. The macro
load context resolves helper assemblies placed beside that implementation and
uses runtime assets from the selected NuGet target graph to resolve
dependencies supplied by transitive packages. These private dependency probes
are not consumer metadata references.

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

The dedicated-file MVP classifies this partition automatically from direct
macro declarations:

```raven
import Raven.CodeAnalysis.Macros.*

class QueryMacro: ITokenTreeExpressionMacro {
    // ...
}
```

`Compilation.AddSyntaxTreesWithLocalMacros`, Workspace compilation, and the SDK
recognize the macro interface in the declaration's base list and move the
declaration into the compile-time partition. A file containing only direct
macro declarations is therefore classified as a dedicated macro file. The SDK
form needs neither a `RavenMacro` item nor an explicit project reference to the
compiler contracts.

The object-oriented macro contracts are the authoritative MVP authoring and
execution model. Raven may later add dedicated declaration syntax that removes
macro class boilerplate and hides compile-time partition markers such
as `[LocalMacro]`, but that syntax is intentionally deferred until the
infrastructure and common macro cases are stable. Any such syntax must lower to
or interoperate with the same `IMacroDefinition`, context, token-stream,
diagnostic, and expansion-result
contracts; it must not create a parallel macro execution model.

After activation, local and referenced implementations are equivalent entries
in the macro registry. A future macro-specific symbol may expose this common
semantic identity, but macro execution does not currently require a dedicated
symbol kind.

Direct macro definitions are the only activation unit. They are first-class
manifest exports and are discovered automatically inside the local compile-time
partition. The category-specific macro interfaces and expansion contracts are
the authoritative implementation surface for the MVP.

The automatic rule is intentionally syntax-only and declaration-granular.
Supporting types can be moved with `[LocalMacro]`. The partition remains
acyclic: macro source can reference metadata
and other macro plugins but cannot bind against consumer source declarations.
If a reference in local macro code resolves only to a declaration in the
consumer partition, the compiler reports `RAVM003` at that reference. The
dependency must move into the local macro partition or a referenced assembly.

A mixed source file uses `[LocalMacro]` instead:

```raven
import Raven.CodeAnalysis.Macros.*

[LocalMacro]
class AnswerMacro: ITokenTreeExpressionMacro {
    // ...
}

let answer = answer! { }
```

`[LocalMacro]` classifies only the marked top-level type and everything nested
within it as compile-time-only. Every separate top-level macro definition or
support type needed by the local macro must be marked.
The compiler creates same-length macro and consumer projections, retaining line
breaks and replacing declarations from the opposite partition with whitespace.
This preserves authored offsets for diagnostics while keeping macro
implementation types out of runtime emit.

Compiler hosts may use `Compilation.GetSemanticModel(tree, position)`, and
Workspace callers may use `Document.GetSemanticModelAsync(position)`, to select
the semantic projection that owns an authored position. A position inside a
declaration marked `[LocalMacro]` returns the macro-partition semantic model;
other positions return the consumer model. Nodes for semantic queries must come
from the returned model's `SyntaxTree`. The projections preserve authored
positions, so the same position can be used to find the corresponding node.
Language-server hover, completion, definition, references, and rename use this
position-aware view inside local macro declarations. Their semantic answers
come from the macro partition rather than the masked consumer projection.
Reference locations and rename edits remain expressed against the authored
document because both projections preserve its positions.

When a Workspace analyzer host is present, document analysis traverses both
compiler-owned projections of a mixed local-macro document. Analyzer callbacks
receive the semantic model for the projection being traversed, so ordinary
Raven code in `[LocalMacro]` declarations participates in syntax, symbol, and
operation analysis alongside consumer code. This does not make analyzers part
of macro compilation or activation.

The browser Playground supports this form in its single user buffer. Analyzer
participation inside retained structured DSL regions is not yet complete; the
current implementation does not infer Raven fragments from raw macro tokens or
expansion output.

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
