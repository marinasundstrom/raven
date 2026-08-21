# Authoring Raven macros

Raven macros are procedural macros: compile-time programs that validate input
and produce ordinary Raven syntax. Start with `macro`. Move to provider
interfaces only when a macro needs capabilities the compact declaration syntax
does not yet project.

> [!NOTE]
> Macro authoring is experimental. Examples here describe the current
> implementation. Sections marked **Future** describe planned tooling.

## The 30-second model

A Raven macro receives typed values, authored syntax, or a lossless token body
at compile time. It returns ordinary Raven syntax, and the compiler then parses,
binds, diagnoses, emits, debugs, and serves editor features for that syntax in
the usual way.

A freestanding invocation always retains `!` as its visible extension marker.
A resolved alias may be colored like a contextual keyword, especially in a
declaration-shaped DSL, but it remains a library-provided macro name rather
than a reserved Raven keyword.

Keep four rules in mind:

1. Invoke a freestanding macro with `Name!(...)`, `Name! { ... }`, or both.
   `#` is reserved for directives and attached macro attributes.
2. Use `expand` once the freestanding result is ready. It sets the expansion and
   returns from that execution path.
3. Report expected input failures as diagnostics. Do not throw for malformed
   user input.
4. Expose spans and ordinary Raven fragments to editor tooling; keep the DSL's
   private parse tree private.

The delimiters are part of the contract, not interchangeable decoration.
`(...)` passes a fixed parameter list. `{...}` supplies a bounded content
region. The declaration carrier `Name! Decl(...) { ... }` combines a structured
declaration header with such a region. The proposed `Name![...]` family would
instead supply a variable number of homogeneous values or syntax nodes through
`MacroList<T>`; it is not implemented yet, and combinations with `{...}` remain
open design space.

Sections 1–4 form the shortest path from a small macro to a real DSL. Continue
with only the capability the DSL needs:

* section 5 for diagnostics and recovery;
* section 6 for highlighting, completion, hover, and navigation;
* section 7 for attached declaration transforms; or
* section 8 for distribution.

The advanced reference after the tutorial explains the lowered provider model,
debugging, build options, examples, and explicitly deferred work. Most macro
authors do not need to begin there.

## Choose the smallest useful shape

| Need | Start with |
| --- | --- |
| Typed compile-time values | ordinary `macro` parameters |
| An authored Raven expression | an `ExpressionSyntax` parameter |
| An unrestricted brace body | one `IMacroTokenStream` parameter |
| A declaration name, parameters, and body | `FreestandingMacroDeclarationSyntax` plus `IMacroTokenStream` |
| Body text, parsing, diagnostics, or file APIs | a `TokenTreeMacroContext` parameter |
| Replace or introduce declarations | one typed `on target: ...` parameter |
| Custom tokenization or fragment metadata | a class-authored provider interface |

The compact and class-authored forms are two projections of one model. They use
the same invocation syntax, registry, contexts, diagnostics, and results.

Raven does not currently define a declarative pattern-and-replacement macro
language. A macro library can parse such rules with the procedural APIs when a
domain needs them; the generated Raven syntax is then validated and bound in
the normal way. Prefer the direct typed or syntax-based contract until a
concrete rule language provides a clearer authoring experience.

This is also a complexity ladder. Typed-value and syntax-input macros are often
small. A full token DSL can require a grammar, recovery, diagnostics, source
mapping, highlighting, completion, hover, and navigation. Add those
responsibilities only when the DSL needs them; Raven provides integration
points, but the macro library remains responsible for its private language.

Procedural macros have two application positions. A **freestanding** macro
appears independently at any grammar position allowed by its declared syntax
result and is usually written with the function-like `Name!(...)` form. An
**attached** macro appears in an attribute-like position on an existing
declaration. Raw token bodies and injected contexts add capabilities to either
authoring model; they do not define additional macro kinds.

## Macros and source generators solve different problems

Use a macro when the programmer should opt into a transformation at a specific
source location. A freestanding macro replaces `Name!(...)` or `Name! { ... }`;
an attached macro transforms the declaration carrying it. The compiler retains
that relationship for diagnostics, source mapping, hover, navigation, and
debugging. A macro can inspect its explicit inputs, but it should not behave
like a hidden project-wide pass.

Use a source generator when a project-wide input should contribute separate
generated files—for example, a registry derived from all declarations in a
compilation. Generators run under workspace or build-host orchestration and add
syntax trees to the compilation. They do not replace an inline invocation and
do not own an authored macro-body span. They can also supply the implementation
half of an authored partial declaration: source establishes the shape, and a
generated partial declaration augments it through normal partial-type merging.

| Question | Macro | Source generator |
| --- | --- | --- |
| What triggers it? | An explicit invocation or attachment in source | A registered project generator |
| What does it produce? | Syntax replacing or augmenting that source site | Additional generated source files, often partial implementations |
| What input should it use? | Declared arguments, target syntax, or token body | The compilation and generator inputs |
| Who runs it? | The compiler during semantic expansion | The workspace or build host before the resulting compilation is consumed |
| How do tools relate output to source? | Through the macro invocation, fragment spans, and expansion mappings | Through generated-document identity and generator diagnostics |

See [Source generators](compiler/source-generators.md) for the standalone
generator guide and [Extending Raven projects](compiler/extending-projects.md)
for the broader analyzer and generator model.

## 1. Start with a local macro declaration

A macro in the same project is compiled in Raven's compile-time partition and
is not emitted as an ordinary runtime function:

```raven
import Raven.CodeAnalysis.Syntax.SyntaxFactory.*

macro Double(value: int) {
    let doubled = value * 2
    expand ParseExpression(doubled.ToString())
}

let answer = Double!(21)
```

`value` is a compile-time constant parameter. `expand` contributes the
`ExpressionSyntax` that replaces the invocation and returns from that macro
execution path. Normal Raven control flow can choose an expansion.

The omitted return annotation is the compact expression-macro default. Write
`-> ExpressionSyntax` when the category should be explicit. Other syntax return
types select other grammar positions: `StatementSyntax` selects statement
position, `ExpressionSyntax | StatementSyntax` permits either, and
category-untyped `SyntaxNode` permits every supported single-node position.
`SyntaxList<MemberDeclarationSyntax>` selects file, namespace, and type-member
positions and permits zero or more declarations.
The expanded node is then bound as ordinary Raven syntax, so its eventual value
type comes from normal semantic analysis rather than the macro annotation.

For the MVP, a raw-body invocation that occupies a whole statement selects
statement placement:

```raven
Log! { "saved" }
```

Parenthesizing the same invocation selects expression placement. If a macro
produces the wrong syntax category, Raven reports a diagnostic and discards the
node; it does not cast the node and risk corrupting later compiler or language-
server state.

Use typed parameters for configuration instead of recovering values from raw
text. The normalized parameter schema also drives binding, completion, and
signature help.

## 2. Receive authored Raven syntax

Use a syntax-role parameter when the macro needs the caller's expression rather
than its constant value:

```raven
import Raven.CodeAnalysis.Syntax.*
import Raven.CodeAnalysis.Syntax.SyntaxFactory.*

macro AddOffset(offset: int, expression: ExpressionSyntax) {
    let source = expression.ToString() + " + " + offset.ToString()
    expand ParseExpression(source)
}

let answer = AddOffset!(2, 40)
```

The compiler projects the second argument to `ExpressionSyntax`; it does not
execute that expression. For nontrivial construction, prefer immutable syntax
factories or `quote!` with syntax holes over long generated strings.

## 3. Add an unrestricted DSL body

One `IMacroTokenStream` parameter denotes the brace-delimited body. It is
compiler-supplied and does not appear in the argument list:

```raven
import Raven.CodeAnalysis.Macros.*
import Raven.CodeAnalysis.Syntax.SyntaxFactory.*

macro FirstTokenLength(offset: int, tokens: IMacroTokenStream) {
    let token = tokens.ReadToken()
    let length = token.Text.Length + offset
    expand ParseExpression(length.ToString())
}

let length = FirstTokenLength!(1) { raven }
```

A declaration-shaped macro can receive its complete header separately while
retaining the same token-body convention:

```raven
macro FunctionComponent(
    declaration: FreestandingMacroDeclarationSyntax,
    body: IMacroTokenStream,
    context: TokenTreeMacroContext
) -> MemberDeclarationSyntax {
    // component! Greeting(Name: string) { ... }
}
```

The declaration carrier preserves modifiers, the macro name, declared name,
parameter list, and body. This allows aliases such as `component` to read like
declaration keywords in `public component! Greeting(...) { ... }`, while the
body remains lossless input owned by the macro.

The `!` is intentional even in this declaration-like form. It lets the DSL
participate in Raven's declaration experience without suggesting that
`component` is permanently built into the language.

An alias does not become a lexical Raven keyword. It is resolved through the
macro registry using normal namespace and import rules. Once resolved, IDEs
contextually present the alias as a contextual keyword; the canonical macro
name retains the macro classification. The language server and Playground use
the same compiler-owned classification.

The standard stream uses Raven's lexer. A mostly Raven-shaped DSL should start
there and add body-scoped keyword overlays. Implement a custom token stream
only when the DSL has a genuinely different lexical grammar.

Compiler hosts can query the exact selected stream through
`SemanticModel.GetMacroTokens`. Each result includes the token's provider-owned
raw kind, stable kind name, text, body-relative and authored spans, plus an
optional lightweight classification. Standard Raven tokens receive their
`SyntaxKind` name. Implement `IMacroTokenKindProvider` only to name custom raw
kinds, and `IMacroTokenClassifier` only when DSL tokens need identifier,
literal, operator, punctuation, or comment presentation. Keyword overlays are
classified automatically. This metadata does not add global Raven token kinds.

The language server projects available keyword, identifier, literal, operator,
and comment categories to semantic tokens. It uses an already available
semantic model and falls back to ordinary syntax highlighting when semantic
work is cold or busy; highlighting never waits for macro tooling metadata.

## 4. Parse Raven fragments inside a DSL

Use `TokenTreeMacroContext` when a custom outer grammar contains ordinary Raven
expressions, statements, types, patterns, or declarations:

```raven
import Raven.CodeAnalysis.Macros.*

macro Guard(context: TokenTreeMacroContext) {
    let span = FindExpressionSpan(context.GetBodyText())
    let expression = context.ParseExpressionResult(span)
    context.ReportDiagnostics(expression)
    expand BuildGuardExpression(expression.Syntax)
}
```

For the common recursive-descent case, parse directly from the token stream's
current position:

```raven
let stream = context.CreateTokenStream()
let clauseKeyword = stream.ReadToken()
let expression = stream.ParseExpression()

context.ReportDiagnostics(expression)
let expressionSpan = expression.BodyRelativeSpan
```

`ParseExpression`, `ParseStatement`, `ParseType`, `ParsePattern`, and
`ParseMemberDeclaration` parse one Raven construct, advance the stream through
it, and return recovered syntax, diagnostics, and the body-relative span chosen
by Raven's parser. The same cursor still exposes `PeekToken`, `ReadToken`, and
`IsEndOfFile`, so the outer DSL can continue with its next clause.

Use the explicit `TextSpan` result overload when the outer DSL owns a delimiter
that is ambiguous in Raven grammar—for example, when a following DSL operator
could also continue a Raven expression. Cursor parsing is the convenient path,
not a reason to guess through an ambiguous language boundary.

The diagnostic-bearing parsers return recovered syntax and native parser
diagnostics mapped to the authored invocation:

| Category | Concise form | Diagnostic-bearing form |
| --- | --- | --- |
| Expression | `ParseExpression` | `ParseExpressionResult` |
| Statement | `ParseStatement` | `ParseStatementResult` |
| Type | `ParseType` | `ParseTypeResult` |
| Pattern | `ParsePattern` | `ParsePatternResult` |
| Compilation unit | `ParseCompilationUnit` | `ParseCompilationUnitResult` |
| Exactly one member | `ParseMemberDeclaration` | `ParseMemberDeclarationResult` |

Selected spans are relative to the macro body. The member parser diagnoses
empty input, multiple declarations, global statements, and compilation-unit
content rather than silently choosing a node.

Every `MacroSyntaxParseResult<TSyntax>` also exposes `BodyRelativeSpan`. For an
explicit-span parse this is the actual node span inside the selected region;
for a cursor parse it is likewise the recovered node's actual span. The stream
tracks the parser's consumed position separately so recovery tokens can still
advance the cursor safely without widening the node span reported to authors.

Parsed expressions can be inspected in the invocation's caller scope without
constructing a separate semantic model:

```csharp
var parsed = stream.ParseExpression();
var typeInfo = context.GetTypeInfo(parsed.Syntax);
var symbolInfo = context.GetSymbolInfo(parsed.Syntax);
```

These helpers see caller locals, parameters, members, and imports through the
compiler-owned semantic model used by expansion.

When a parsed fragment is transformed into generated syntax, map the result
back to the whole parsed source with the parse-result overload:

```csharp
var generated = BuildExpansion(parsed.Syntax);
generated = context.WithOrigin(generated, parsed);
```

For diagnostics and development, `MacroSyntax.GetStructure(syntax)` produces a
stable non-colorized tree view, while `MacroSyntax.GetFactoryForm(syntax)`
shows the equivalent immutable `SyntaxFactory` construction. These correspond
to the practical roles of Nim's `treeRepr` and `repr` without making either
representation part of expansion semantics.

Macro contexts accumulate diagnostics through the ordinary
`ReportDiagnostic` and `ReportDiagnostics` APIs. This deliberately avoids a
separate diagnostic statement in the language. `expand` supplies the final
expansion and returns from the current macro execution path; diagnostics
reported before it are retained. Reaching the end of the body also returns any
accumulated diagnostics and contributions.

### Generated binding names

When an expansion needs a temporary local or another generated binding, ask
the context for a collision-free name:

```raven
let temporary = context.CreateUniqueIdentifier("item")
```

`CreateUniqueName` is deterministic for one invocation. It avoids every
identifier authored in the invocation document and every name previously
allocated by that context. The hint is normalized into an identifier; it is a
readability aid rather than part of the uniqueness contract.

This helper prevents accidental textual capture for a binding that the macro
both declares and references. It does not choose definition-site or call-site
lookup for a constructed reference. Keep caller-authored references as
source-backed/spliced syntax until Raven's broader hygiene model supplies
explicit APIs for those lookup choices.

Use `CreateUniqueName` when an API needs the text itself and
`CreateUniqueIdentifier` when constructing an expression or reference node.
Both follow the same collision rules and intentionally make no stronger
hygiene claim.

### Validate syntax shapes without exceptions

When a transformation accepts a broad syntax node but requires a narrower
shape, use `RequireSyntax`:

```raven
if let expression: ExpressionSyntax =
    context.RequireSyntax<ExpressionSyntax>(node, "Expected an expression.") {
    // Transform expression.
}
```

A matching node is returned unchanged. A mismatch reports an error at the
authored node and returns `null`; detached generated syntax falls back to the
macro invocation. This keeps invalid input on the diagnostic path and allows
the macro to recover or end without destabilizing the compiler or language
server.

The nullable return is a bootstrap-era compiler API shape. Raven's eventual
authoring facade should project simple absence to `Option<TSyntax>`, binary
failure to `Result`, and genuinely multi-case outcomes to purpose-built unions.
Those union types remain consumable from C# while giving Raven authors normal
exhaustive matching.

## 5. Report precise diagnostics

Diagnostics accumulate on the context independently of the expansion. This
lets a compact declaration report every useful problem before it either
expands or reaches the end of its body:

```raven
import Raven.CodeAnalysis.Macros.*
import Raven.CodeAnalysis.Text.*

macro CheckedExpression(context: TokenTreeMacroContext) {
    let span = TextSpan(0, context.BodySpan.Length)
    let expression = context.ParseExpressionResult(span)
    context.ReportDiagnostics(expression)

    if expression.HasErrors == false {
        expand expression.Syntax
    }
}
```

Use the parser's native diagnostics for malformed embedded Raven. For a DSL
rule, create a diagnostic at a body-relative span with
`CreateBodyDiagnostic`, or use `CreateDiagnostic` when the whole invocation is
the right location, then pass it to `ReportDiagnostic`. Prefer diagnostics over
throwing for expected invalid input; an exception means the macro itself
failed.

The preferred `macro` declaration is shorthand over a method-shaped definition.
The same model can be authored as an ordinary Raven class when seeing or
controlling the underlying signature is useful:

```raven
import Raven.CodeAnalysis.Macros.*
import Raven.CodeAnalysis.Syntax.*

[assembly: RavenCompilerPlugin]

public class IdentityMacro<T> : IMacroDefinition {
    func Expand(
        value: T,
        syntax: ExpressionSyntax,
        context: FreestandingMacroContext
    ) -> ExpressionSyntax => syntax
}
```

`IMacroDefinition` does not declare `Expand`. It marks the nominal definition
for discovery; the class owns `T`, and the authored method is the complete
canonical signature. A `FooMacro` class defaults to the invocation name `Foo`,
although it may override `Name`. Caller inputs, syntax inputs, and injected
contexts may be freely interleaved in declaration order. Tooling projects only
caller-supplied parameters into `Foo!(...)`.

Raven lowers the class once to a direct erased entry point. Expansion does not
reflectively invoke the authored method. Generic arguments remain symbolic in
the canonical definition and execution snapshot rather than requiring a
loadable closed CLR type. `ExpressionSyntax<T>` is reserved as a future typed
syntax-input facade over this same model; the current API provides
`ExpressionSyntax`.

Advanced .NET providers can implement the erased `IMacroExecutor` transport
directly. Its `Expand` method receives one `MacroExecutionContext` and returns
a `MacroExecutionResult`. This is an execution ABI, not the canonical authoring
signature. The older category-specific provider interfaces remain as
compatibility adapters.

The compiler normalizes compatibility providers to `IMacroExecutor` when a
macro reference is registered. Symbol and optional editor-capability discovery
still use the authored provider, while expansion has one erased dispatch path.
New Raven provider packages should prefer `macro` declarations or ordinary
`IMacroDefinition` classes. Handwritten `IMacroExecutor` implementations are
for providers that need direct control of the erased transport; the
category-specific interfaces remain only for packages built against an older
Raven compiler API.

For compact declarations, return a syntax list when an invocation produces
declarations:

```raven
import Raven.CodeAnalysis.Macros.*
import Raven.CodeAnalysis.Syntax.*

macro Generate(context: TokenTreeMacroContext)
    -> SyntaxList<MemberDeclarationSyntax> {
    let unit = context.ParseCompilationUnit()
    expand unit.Members
}
```

The return annotation offers `Generate! { ... }` in file, namespace, and type
member positions. The compiler preserves source order, and an explicitly empty
list removes the invocation.

Class-authored providers declare the same applicability through
`IMacroDefinition.InvocationTargets` and return members with
`FreestandingMacroExpansionResult.FromMembers(...)`. Returning `Empty` leaves the
invocation in place as recoverable source. `FromNode(...)` may be used for
exactly one member. The compiler reports `RAVM022` if the result is an
expression or statement instead of a member, so a malformed provider cannot
force the expanded document into an invalid syntax category.

The same result form works at file and namespace scope. At those sites the
parser deliberately keeps `Name! { ... }` inside a global-statement carrier;
the semantic result decides whether it supplies a statement or declarations.
Return declarations that are legal in the containing scope—for example, a type
at namespace scope rather than a method declaration intended for a type body.
Generated declarations participate in normal lookup, binding, and emission.

## 6. Surface fragment spans for tooling

A DSL should identify the ordinary Raven fragments inside its token body. One
`fragment` contribution is enough to give the compiler and language server the
authored category and span:

```raven
import Raven.CodeAnalysis.Macros.*
import Raven.CodeAnalysis.Text.*

macro RavenExpression(context: TokenTreeMacroContext) {
    let span = TextSpan(0, context.BodySpan.Length)
    fragment context.CreateFragmentRegion(MacroFragmentKind.Expression, span)
    expand context.ParseExpression(span)
}
```

Only a syntax category and span cross the boundary. The HTML tree remains
private. The compiler maps body-relative regions to absolute authored spans.
Zero-width regions can say “an expression is expected here” in incomplete
input.

Contribute every recovered region that is still meaningful while the user is
typing. A private parser can return several expression spans and the macro can
emit one `fragment` statement for each. Use the class-authored
`IMacroFragmentProvider` only when tooling discovery must remain independent
from full expansion—for example, when expansion is expensive or deliberately
stops on malformed input. Both forms produce the same compiler-owned region
model.

When the DSL already knows an embedded expression's expected type, report it
without exposing the surrounding DSL structure:

```raven
let actionDefinition: INamedTypeSymbol = context.Compilation.GetTypeByMetadataName(
    "System.Action`1") else {
    return []
}
let actionType = actionDefinition.Construct(argumentType)
let callback = context.CreateExpressionFragmentRegion(callbackSpan, actionType)
```

`CreateExpressionFragmentRegion` target-types the recovered expression for
semantic tooling. This is particularly useful for inline lambdas: hover and
completion can see the parameter types implied by `Action<T>`, an expression
tree, or another delegate just as they would in ordinary Raven code. Use the
untyped `CreateFragmentRegion` overload when the DSL has no real contextual
type; do not manufacture one solely for display.

`SemanticModel.GetMacroFragmentRegions(invocation)` and the corresponding
`Compilation` API resolve this capability. Provider failures return no regions
instead of breaking unrelated semantic queries.

Editor integrations normally call `GetMacroInputSnapshot(invocation)` to obtain
both classified tokens and Raven-fragment regions from one immutable view. The
narrower token and fragment queries remain available when only one is needed.
`FindFragmentRegion(position)` returns the narrowest region at a cursor,
including an exact zero-width expected slot.

Ordinary Raven completion is automatically delegated into these regions. It
parses the reported category at its authored position and uses the invocation's
caller scope, so locals, parameters, fields, types, and member access behave as
they do outside the DSL. Macro authors do not implement a second completion
provider for embedded Raven syntax.

### Native fragment completion and DSL completion

There are two distinct completion layers in a mixed-language macro body:

1. **Native Raven completion** applies inside a reported fragment. The compiler
   owns parsing, binding, member lookup, replacement spans, and presentation for
   the embedded Raven code. A provider contributes the fragment category plus
   any introduced locals or target type.
2. **DSL completion** applies to the provider's private grammar. Examples are
   lifecycle clauses in an actor DSL, HTML tags and attributes in `markup!`,
   route templates, SQL tables and columns, or schema-derived names.

The first layer is implemented today through fragment regions. Keyword and
token providers also supply parsing and semantic classification for the outer
DSL, but they do not currently contribute custom completion items. Consequently,
`markup!` can provide normal Raven member completion inside `{ expression }`,
while HTML tag, attribute, component-parameter, and closing-tag suggestions
remain future work. Likewise, an actor block receives completion for `events.`
and `context.`, but clause-order suggestions such as `receive` after `started`
need a DSL completion capability.

The planned capability is an optional, compiler-owned macro completion provider,
not an LSP extension point. It should receive the invocation, a body-relative
cursor and replacement span, trigger information, cancellation, and the
provider's private input context. It should return editor-neutral items with a
label, insertion text or snippet, kind, detail/documentation, and ordering data.
The compiler should map spans, merge and deduplicate DSL items with native Raven
fragment items, and isolate provider failure just as it does for token and
fragment metadata. This keeps the language server a presenter and lets one macro
package behave consistently in VS Code, the Playground, and future editors.

A custom provider should be used only for knowledge Raven cannot derive. If a
suggestion denotes an ordinary Raven symbol—such as a component parameter—the
item should retain that symbol association so documentation and navigation stay
compiler-owned. A provider may reuse its private recovered parse internally,
but Raven should not require every DSL to expose a public syntax tree or build a
second semantic model.

Use `MacroFragmentKind.Block` when a reported region is a sequence of ordinary
Raven statements sharing one lexical scope. A DSL may report the entire body or
several independent block regions separated by its own structural keywords.
`TokenTreeMacroContext.ParseBlock(span)` parses that region with authored
positions; the parameterless overload parses the complete body. Hover,
completion, classifications, and inferred-type inlays use the same span-aware
block model. For declaration-shaped macros,
`CreateFragmentParameter(name, type, declarationSpan)` projects a typed header
parameter into the block as an `IParameterSymbol` and maps navigation back to
the parameter declaration:

```raven
let name = context.CreateFragmentParameter("Name", stringType, nameToken.Span)
let body = context.CreateFragmentRegion(
    MacroFragmentKind.Block,
    TextSpan(0, context.BodySpan.Length),
    [name])
```

Nested macros inherit the symbols visible at their authored position. A
`markup!` fragment inside a component block can therefore provide hover and
member completion for `Name` without either macro sharing a private syntax
tree.

For a query-like DSL, attach an introduced range variable only to the fragments
where it is visible:

```raven
let item = context.CreateSequenceElementLocal("item", sourceSpan)
let predicate = context.CreateFragmentRegion(
    MacroFragmentKind.Expression,
    predicateSpan,
    [item])
```

The compiler resolves the source expression in the invocation's caller scope
and infers the element type for arrays, strings, `IEnumerable<T>`, and
`IAsyncEnumerable<T>`. `MacroFragmentRegion.Locals` exposes the resulting name
and type. Completion treats these as immutable fragment-local values, with
fragment locals shadowing caller names. This is deliberately narrower than a
general custom symbol or scope API. A macro that already knows the type, such
as a schema-backed SQL macro, can instead call
`CreateFragmentLocal(name, type)`.

Fragment and token providers are optional editor capabilities. Treat malformed
or incomplete user input as data: return recovered regions and diagnostics
where possible, and reserve exceptions for provider defects. The compiler
isolates a failed optional provider to that request and returns no contributed
metadata; later requests and newer document snapshots remain independently
queryable. Providers must honor `context.CancellationToken` during potentially
long parsing or schema work. The language server consumes the immutable
compiler snapshot and must not cache semantic truth independently of its
document version.

## 7. Transform declarations

Mark one typed parameter with contextual `on` for an attached macro:

```raven
macro Observable(
    enabled: bool,
    on property: PropertyDeclarationSyntax
) {
    if enabled {
        replace Rewrite(property)
        introduce CreateBackingField(property)
    }
}
```

`replace` sets the current declaration replacement. `introduce` appends members
in execution order. The class-authored equivalent implements
`IMacroDefinition` and exposes one canonical `Expand` method; an injected
`AttachedMacroContext` exposes the original `TargetDeclaration` and composed
`CurrentDeclaration`.

A convenience macro should expand to the ordinary framework model rather than
create a parallel one. For example, the HTML/Blazor sample's `#[Parameter]`
adds Blazor's normal parameter attribute.

## 8. Package a reusable library

A reusable Raven macro project marks its assembly as a compiler plugin. A bare
marker exports the adapters generated for public compact declarations and may
appear in the same source file:

```raven
import Raven.CodeAnalysis.Macros.*
import Raven.CodeAnalysis.Syntax.*

[assembly: RavenCompilerPlugin]

[MacroAlias("twice")]
public macro Twice(expression: ExpressionSyntax) {
    expand expression
}
```

For a class-authored provider, name each intentionally exported provider type
with `[assembly: RavenCompilerPlugin(typeof(HtmlMacro))]` instead.
Declarations without `public` remain available within their own project but are
not discovered through a referenced bare-marker plugin assembly.

The consumer uses an ordinary project reference:

```xml
<ItemGroup>
  <ProjectReference Include="../macros/HtmlMacros.rvnproj" />
</ItemGroup>
```

The marked project is activated at compile time and is not added as an
application runtime reference. Keep a new DSL in its own sample and macro
project until its contract can be distributed. A future Playground preview
should consume that same package, not copy its parser.

## Advanced: projection to provider contracts

The compiler lowers `macro` declarations to adapters, but tools expose an
`IMacroDeclarationSymbol`, not the generated class.

| Source feature | Provider projection |
| --- | --- |
| ordinary parameter | typed parameter schema |
| `ExpressionSyntax` parameter | authored expression projection |
| `IMacroTokenStream` parameter | token-tree macro and token stream |
| `TokenTreeMacroContext` parameter | complete token-tree context |
| `FreestandingMacroContext` parameter | complete argument-style context |
| `AttachedMacroContext` parameter | complete attached context |
| `on target: BaseTypeDeclarationSyntax` / `on property: PropertyDeclarationSyntax` | compiler-supplied attached target |
| `expand` | final expansion and semantic return |
| reached `replace` | replacement declaration |
| reached `introduce` | ordered introduced members |
| reached `fragment` | ordinary Raven fragment metadata |
| reached `token` | token kind and classification metadata |

The two freestanding contexts expose a normalized carrier surface:
`Syntax` is the authored `SyntaxNode`, while `Name`, `ExclamationToken`,
`ArgumentList`, and `TokenTree` provide the shared `Name!` parts. This keeps a
macro independent of whether the parser used an expression carrier or a
type-member carrier unless the macro deliberately inspects `Syntax`.

`fragment` accepts a `MacroFragmentRegion` and is valid only for a token-tree
macro declaration. The generated adapter keeps reached regions on its expansion
result; `SemanticModel` uses them when the macro does not implement a dedicated
`IMacroFragmentProvider`. Section 6 shows the compact form. Implement the
provider directly only when tooling must remain independent from full
expansion, especially for heavily recovered or incomplete DSL input.

The same fragment declaration enables ordinary Raven hover as well as
completion. `SemanticModel.GetMacroFragmentSemanticInfo(invocation, position)`
resolves symbols and types in the invocation's caller scope, with the region's
`MacroFragmentLocal` values layered over that scope. The language server uses
that compiler result to render its normal Raven signature, containing-symbol,
and documentation presentation. Macro authors do not implement a hover
provider for ordinary Raven fragments. Go-to-definition uses the same result:
caller symbols navigate to their ordinary Raven declarations. A
DSL-introduced local can also supply its declaration token span:

Hovering the macro name itself presents the macro symbol, its invocation or
attachment details, and the command that reveals its expansion. Documentation
comments on compact `macro` declarations are projected onto that runtime macro
symbol automatically. A class-authored provider can offer the same experience
by implementing the optional `IMacroDefinition.Documentation` property;
`DocumentationFormat` defaults to Markdown. Keep DSL token hover semantic by
publishing fragments or token-symbol associations rather than folding a custom
hover protocol into the macro.

Fragments may contain another token-tree macro invocation. When the nested
macro reports its own fragment regions, semantic lookup descends recursively
and carries the lexical bindings visible at that invocation into the nested
fragment. For example, an HTML macro nested in a collection-comprehension
selector can resolve the comprehension item and caller members without either
macro exposing its private DSL structure. Macro resolution continues to use
the imports and namespace of the authored outer invocation.

```raven
let item = context.CreateSequenceElementLocal(
    rangeToken.ValueText,
    sourceExpressionSpan,
    rangeToken.Span)
```

The third argument is body-relative, like token-stream spans. The context maps
it to the authored invocation, and the local's semantic symbol carries that
source location. Omitting it remains valid when the DSL has no authored
declaration to navigate to.

The same token-tree function can publish stable token metadata while consuming
its stream:

```raven
let next = tokens.ReadToken()
token context.CreateTokenInfo(
    next,
    "ElementName",
    MacroTokenClassification.Identifier)
```

Reached `token` contributions form the complete contributed token snapshot in
source order. If none are reached, the compiler retains its normal token-stream
snapshot behavior. As with fragments, a macro class can use the dedicated
token kind and classification provider interfaces when metadata discovery must
remain independent from expansion.

An outer DSL token that denotes ordinary Raven code can also carry a symbol.
Source-authored macros pass it as the fourth `CreateTokenInfo` argument. A
class-authored provider can implement the narrow `IMacroTokenSymbolProvider`:

```raven
func GetTokenSymbol(context: TokenTreeMacroContext, token: SyntaxToken) -> ISymbol? {
    context.Compilation.GetTypeByMetadataName(token.ValueText)
}
```

This is useful for component tags, schema-backed table or column names, and
similar references. It enables normal hover and go-to-definition without a
custom hover format or public DSL tree. Return `null` for tokens that do not
denote Raven symbols.

The association is fundamentally between an authored DSL span and an ordinary
symbol; a token's span is the convenient unit supplied by the current token
metadata API. Resolution may depend on the token's private DSL context. For
example, an HTML
macro can use the token span to recognize `Name` as an attribute of
`<Greeting>`, resolve `Greeting` in the consumer compilation, and return its
ordinary `IPropertySymbol` for `Name`. A SQL macro can apply the same model to
resolve a column against the table or alias selected by its private parser.
Only the resulting symbol crosses the macro boundary; the compiler and editor
do not need the macro's syntax-tree representation.

An explicit DSL token-symbol association takes precedence over semantic
inference from a broader embedded Raven fragment containing the same position.

### Debugging executable fragments

`TokenTreeMacroContext.ParseExpression` and `ParseStatement` attach the
authored invocation origin to every executable syntax node they return. If a
macro splices that syntax into its expansion, the compiler uses the origin for
portable-PDB sequence points. A breakpoint inside the fragment therefore binds
to the `.rvn` source while generated expansion plumbing remains hidden during
stepping. Macro fragment regions and token-symbol associations remain the
editor-facing APIs for diagnostics, hover, navigation, and completion.

For generated syntax representing one authored DSL span, use `WithOrigin`:

```raven
let condition = SyntaxFactory.ParseExpression("enabled")
let mapped = context.WithOrigin(condition, enabledToken.Span)
```

String-based expansion builders can retain precise origins without exposing
their private DSL structure. Record the span where an authored expression was
inserted into the generated Raven text and map it back to the corresponding
body-relative span. The two spans must have equal lengths so nested syntax can
retain exact offsets:

```raven
let maps: ImmutableArray<MacroExpansionSourceMap> = [
    MacroExpansionSourceMap(generatedExpressionSpan, authoredExpressionSpan)
]
let expanded = ParseExpression(generatedText)
expand context.WithOrigins(expanded, maps)
```

Both APIs validate body-relative spans. Use mappings for executable Raven
fragments and intentionally associated operations, not every DSL token. Tags,
punctuation, and generated builder calls normally have no stepping point.

### Evaluated build options

MSBuild projects can provide immutable macro configuration through evaluated
`MacroOption` items:

```xml
<ItemGroup>
  <MacroOption Include="sample.theme" Value="dark" />
</ItemGroup>
```

The compiler projects these items into `SyntaxTree.Options.Features`, using the
item identity as the key and its `Value` metadata as the value. When duplicate
keys are evaluated, the last item wins. Because the normal project evaluator
owns this projection, command-line builds and project-backed language-server
snapshots observe the same values.

This channel is appropriate for small, deterministic build facts that affect a
macro expansion. For example, the HTML/Blazor sample maps a component source
file to the CSS scope selected by MSBuild. It is not a replacement for a DSL's
private parser, and macros should not use it to expose their internal trees or
to read mutable build outputs from `obj`.

## Working examples

The repository examples progress from compact syntax to full DSL handling:

* `samples/projects/macro-declarations` — typed, syntax, and token-stream inputs;
* `samples/projects/macro-dsl` — the minimal provider-class reference for one
  DSL keyword, one embedded Raven expression, native diagnostics, fragment
  tooling, and debugger source provenance;
* `samples/projects/macro-token-stream` — a custom lexer-backed stream;
* `samples/projects/macro-reactive` — attached replacement and introduction;
* `samples/projects/macro-freestanding` — LINQ-like query parsing, three
  embedded Raven expression regions, caller-scope completion, and an
  introduced sequence-element range variable;
* `samples/projects/macro-html-blazor` — the Blazor Component Macros showcase,
  with a private markup grammar, embedded Raven fragments, declaration-shaped
  function components, nested macro expansion, and Blazor lowering.

The sections above describe the supported macro forms and their current
restrictions. Use the working samples as the compatibility baseline.

## Tooling MVP and next slices

The current DSL-tooling MVP is intentionally span based. It supplies a cached
compiler-owned input snapshot, stable token kinds and classifications,
embedded Raven fragment regions, deterministic cursor lookup, and semantic
highlighting. A macro keeps its parser representation private.

Snapshots are immutable for one compilation and invocation. Absolute spans
refer to the authored document, while `BodyRelativeSpan` starts inside the
invocation braces. Repeated queries on the same semantic model reuse the
cached result; an edit creates a new compilation snapshot. Cancellation aborts
the query, while optional tooling metadata failures degrade safely.

Ordinary Raven completion works inside reported fragment spans without
requiring public custom syntax trees. Query-like macros can also bridge an
introduced sequence-element local into selected fragments. Broader custom
scope shapes remain future work and should be driven by a concrete DSL use
case.

Expression and raw-body statement placement, single-member and member-list
expansion, and declaration-shaped carriers are implemented. Type and pattern
invocation targets, the `[...]`/`MacroList<T>` input family, and typed syntax
wrappers remain future work.
