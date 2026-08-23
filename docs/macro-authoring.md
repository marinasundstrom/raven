# Authoring Raven macros

Raven macros are procedural macros: compile-time programs that validate input
and produce ordinary Raven syntax. Start with `macro`. Move to provider
interfaces only when a macro needs capabilities the compact declaration syntax
does not yet project.

The purpose of a macro is to make authored code simpler and more expressive by
giving a concise, meaningful form to behavior that expands into more complex
Raven code. Across call-like, expression-header, token-body, and
declaration-shaped forms, macros let a library build a domain-specific language
that integrates with Raven syntax rather than sitting beside it as an unrelated
string or external generator.

> [!NOTE]
> Macro authoring is experimental. Examples here describe the current
> implementation. Sections marked **Future** describe planned tooling.

The authoring aspiration is a native Raven boundary around an intentionally
open language region. A macro declaration and its invocation use Raven names,
signatures, lookup, diagnostics, and tooling conventions. Once a token-tree
invocation opens `{ ... }`, the macro may parse any language it needs. Fragment,
token, symbol, completion, and projection capabilities are explicit bridges
that integrate that private grammar with Raven or another editor language; the
body does not have to masquerade as ordinary Raven syntax.

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

1. Invoke a freestanding macro with its declared carrier: `Name!(...)`,
   `Name! expression`, `Name! { ... }`, or a declaration-shaped
   `Name! Decl<T>(...) ...`. `#` is reserved for directives and attached macro
   attributes.
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

When a syntax-role parameter may also be a compile-time constant, accept a
`FreestandingMacroContext` and inspect the corresponding `MacroArgument` in
`context.Arguments`. `HasValue` distinguishes evaluable constant syntax from
ordinary runtime expressions; `Constant`, `Value`, `Type`, and `ValueKind`
describe the evaluated value without requiring an internal evaluator. The
standard `sha256Digest!` macro uses this path while retaining the authored
expression for precise diagnostics.

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

The declaration carrier preserves modifiers and the macro name outside a
reusable `MacroDeclarationHeaderSyntax`. The header contains the declared name,
declared type parameters, parameter list, either a `BaseListSyntax` or
`ArrowTypeClauseSyntax`, standard `where` constraints, and an optional
`PermitsClauseSyntax`. Its token body is independently optional. This allows
aliases such as `component` to read like declaration keywords in forms such as:

```raven
public component<Blazor>! Greeting<T>(value: T)
    : ComponentBase, IRenderable<T>
    where T: Entity
{
    // lossless component DSL
}
```

Here `Blazor` is a macro type argument on the `GenericNameSyntax` before `!`;
`T` is a declared type parameter in the header after `Greeting`. Raven parses
the former for macro resolution and carries the latter to the expansion through
`MacroDeclarationHeaderSyntax`. Raven parses every standard header piece with
its ordinary syntax node and does not let the macro reinterpret a base type,
return type, constraint, or permitted type as private grammar.

### Declare optional capabilities with functions

A token-tree macro can declare optional compiler and editor capabilities after
its signature and before its expansion body. Each clause forwards the existing
provider interface member to an ordinary function:

```raven
macro Show(context: TokenTreeMacroContext) -> ExpressionSyntax
    keywords by ShowKeywords
    highlighting by ClassifyShowToken
    fragments by GetShowFragments
    symbols by GetShowTokenSymbol
    completion by GetShowCompletions
    projection by ProjectShowBody
{
    expand ExpandShow(context)
}

func ShowKeywords() -> ImmutableArray<MacroKeyword> { ... }
func ExpandShow(context: TokenTreeMacroContext) -> ExpressionSyntax { ... }
func GetShowFragments(context: TokenTreeMacroContext) -> ImmutableArray<MacroFragmentRegion> { ... }
```

Namespace functions beside the declaration are the simplest and preferred
organization. They remain implementation details of the macro assembly and do
not require a service class. When an implementation grows, a clause may name a
qualified static function such as `ShowServices.GetFragments`; the declaration
still acts only as the macro entry point and capability manifest. A support
class in the same project must be part of the compile-time macro partition;
mark it with `[LocalMacro]` (or implement a macro contract) rather than allowing
the compiler to pull an ordinary consumer class across that boundary
implicitly. A class in a referenced macro assembly needs no local marker.

The supported clauses project directly onto the current contracts:

| Clause | Generated provider contract |
| --- | --- |
| `keywords by` | `IMacroKeywordProvider` |
| `tokens by` | `IMacroTokenStreamProvider` |
| `tokenKinds by` | `IMacroTokenKindProvider` |
| `highlighting by` | `IMacroTokenClassifier` |
| `fragments by` | `IMacroFragmentProvider` |
| `symbols by` | `IMacroTokenSymbolProvider` |
| `completion by` | `IMacroCompletionProvider` |
| `projection by` | `IMacroEmbeddedLanguageProvider` |

The handler uses the signature of the corresponding interface member. Raven
generates the interface implementation and forwarding call; it does not create
a second service lifecycle or move the implementation into the macro body.
Only explicitly declared capabilities are projected. Duplicate clauses and
clauses on macros without a token-tree input are diagnosed.

Syntax tooling sees the same structure directly. `MacroDeclarationSyntax`
exposes its ordered `CapabilityClauses`, and each
`MacroCapabilityClauseSyntax` preserves the `CapabilityKeyword`, `ByKeyword`,
and handler `ExpressionSyntax`. An analyzer or formatter can therefore inspect
or rewrite capability declarations without parsing their source text. These are
syntax-tree API classes; macro implementations normally use the clauses above
and the corresponding provider contracts rather than constructing the nodes
directly.

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

When a macro must mask its own holes or delimiters before asking Raven to parse
the complete body, use `ParseProjectedExpressionResult(projectedBody)`. The
projected text must retain the authored body's exact length and line breaks, so
native parser diagnostics still point into the invocation. `quote!` uses this
to replace each `#(...)` hole with an equal-width identifier before parsing the
surrounding expression.

`RavenQuoterOptions.NodeSourceOverride` can then render selected parsed nodes
as caller-provided source instead of syntax-factory construction code. The
quoter preserves trivia around overridden nodes. Override text is emitted
verbatim, so validating and constructing it remains the macro author's
responsibility.

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

Use `MacroSyntax.StringLiteral(value)` when generated syntax must contain an
arbitrary string value. It creates a string-literal expression with Raven
escaping for quotes, slashes, line breaks, and control characters; macro code
does not need to assemble token text itself.

Freestanding and token-tree contexts can observe a source-relative text file
with `context.ReadFile(path)`. The result distinguishes `Success`, `Missing`,
and `Failed` and supplies the resolved path, content, or read error. Every read
is automatically recorded as an expansion input, including a missing file, so
the compiler invalidates the cached expansion when the file changes, is
deleted, or is later created. Macro implementations should not maintain file
timestamps or cache dependencies themselves.

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

### Implemented token-tree tooling services

Token-tree macros can compose the following optional compiler services. Each
contract contributes editor-neutral spans, tokens, symbols, or text; the
compiler validates, maps, routes, and failure-isolates that data, caching the
immutable snapshot or projection where applicable. Language servers and
editors present it. A macro should implement only the services its private
grammar needs.

| Contract | Macro contribution | Compiler and editor behavior | Markup use |
| --- | --- | --- | --- |
| `IMacroKeywordProvider` | Body-scoped DSL keywords over the standard Raven token stream | Contextual semantic classification without adding global Raven keywords | Not required by the HTML-shaped grammar |
| `IMacroTokenStreamProvider` | A custom lexer/token stream | `GetMacroTokens` exposes provider-owned tokens with authored and body-relative spans | Not required; Markup uses the standard token stream |
| `IMacroTokenKindProvider` | Stable names for custom raw token kinds | Tools can display provider kinds without extending `SyntaxKind` | Not required while Markup uses standard Raven token kinds |
| `IMacroTokenClassifier` | Lightweight classifications for body tokens | Semantic tokens distinguish identifiers, literals, punctuation, operators, and comments | Classifies markup identifiers, literals, and punctuation |
| `IMacroFragmentProvider` | Ordinary Raven expression, statement, type, pattern, member, or block regions, with optional locals and target types | Native parsing, diagnostics, binding, hover, completion, definition, classifications, and inlays run inside each region | Reports every `{ expression }`; callback expressions receive their real `Action` target type |
| `IMacroTokenSymbolProvider` | An ordinary Raven symbol associated with a DSL token | Standard symbol hover and go-to-definition work without a public DSL tree | Resolves Blazor component tags and component parameter names |
| `IMacroCompletionProvider` | DSL-owned completion items with body-relative replacement spans and optional symbols | Raven maps, orders, deduplicates, cancellation-checks, and presents the items | Completes Blazor component tags and `[Parameter]` properties, including incomplete markup |
| `IMacroEmbeddedLanguageProvider` | A language ID and position-preserving projected body | Hosts can reuse an existing language service; Raven validates equal length and line breaks and excludes reported Raven fragments from projection-owned requests | Projects the markup envelope as HTML while masking embedded Raven expressions; VS Code currently reuses HTML completion and hover |

### How cursor ownership is resolved

Mixed-language bodies do not have one provider that wins every editor request.
Raven resolves ownership according to the information needed by each feature:

1. Cursor lookup descends through reported Raven regions when they contain a
   nested token-tree macro invocation. The nested macro can then report its own
   fragments, tokens, completion, or embedded-language projection while
   inheriting the lexical scope at its authored position.
2. An explicit `IMacroTokenSymbolProvider` association wins symbol hover and
   definition for that DSL token, even when a broader Raven fragment contains
   the same position.
3. Otherwise, a reported Raven fragment owns native semantic requests inside
   its span. Raven supplies diagnostics, completion, hover, definition,
   classifications, and inlays, and an embedded-language projection is not
   offered at that position.
4. At positions owned by the outer DSL, `IMacroCompletionProvider` may add
   domain-specific items. An embedded-language host may also add results from
   the projected language. The VS Code bridge orders Raven's semantic items and
   hover content before projected HTML results and removes duplicate completion
   labels.
5. If no provider claims a position, the body retains neutral token
   presentation. Tools must not reinterpret arbitrary DSL text as Raven source
   or fall back to hover for the enclosing macro invocation.

This routing keeps the macro's structural parser authoritative without making
its private tree part of Raven's public syntax or semantic model.

### Choose completion or an embedded-language projection

| Need | Preferred service |
| --- | --- |
| Complete ordinary Raven inside the DSL | Report an `IMacroFragmentProvider` region; do not write a custom completion provider |
| Complete names known only to the DSL, a schema, or a framework model | Use `IMacroCompletionProvider` and retain an ordinary Raven symbol on an item when one exists |
| Give hover or definition to a DSL token that denotes a Raven type, member, or namespace | Use `IMacroTokenSymbolProvider`, not custom hover text |
| Reuse a mature language catalog and documentation set such as HTML | Use `IMacroEmbeddedLanguageProvider` with a position-preserving projection |
| Combine framework-specific semantics with a standard embedded language | Implement both completion and projection services; keep framework items compiler-owned and let the editor supplement them |
| Validate or lower the DSL | Keep using the macro's parser and `Expand`; an editor projection is never the structural authority |

### Strongly typed expression boundaries

Use `ExpressionSyntax<T>` when a macro must accept or promise an expression
with a particular Raven result type:

```raven
macro Render(
    model: ExpressionSyntax<ViewModel>
) -> ExpressionSyntax<RenderFragment> {
    // model.Syntax is the authored immutable expression node.
    // model.Type is its compiler-verified bound type.
    expand BuildRenderFragment(model.Syntax)
}

let fragment = Render!(LoadViewModel!())
```

The compiler checks the input before running `Render` and checks its ordinary
expanded expression after binding it. It does not evaluate `model`. Plain
`ExpressionSyntax` remains available when only the expression syntax category
matters. At an invocation, hover presents the promised Raven result type `T`,
not the macro-infrastructure facade `ExpressionSyntax<T>`. For an untyped
expression macro, hover instead reports the type inferred from its bound
expansion.

Class-authored providers keep returning an ordinary `ExpressionSyntax` or
`FreestandingMacroExpansionResult` and declare an output contract separately:

```raven
class MarkupMacro : IMacroDefinition {
    val ExpressionResultType: Type? => typeof(RenderFragment)

    func Expand(context: TokenTreeMacroContext) -> FreestandingMacroExpansionResult {
        // Parse the DSL and return an ordinary expression expansion.
    }
}
```

The checked-in Markup sample uses this contract because every successful
expansion is a `RenderFragment`. The standard Query macro is intentionally not
fixed to one result type yet: its precise result depends on the source operator
family and selector type, which requires a later generic inference contract.

### A composed Markup provider

The checked-in
[Markup provider](https://github.com/marinasundstrom/raven/blob/main/samples/projects/macro-html-blazor/macros/MarkupMacro.rvn)
demonstrates the complete composition. Its abbreviated class shape is:

```raven
class MarkupMacro :
    IMacroDefinition,
    IMacroFragmentProvider,
    IMacroTokenClassifier,
    IMacroTokenSymbolProvider,
    IMacroCompletionProvider,
    IMacroEmbeddedLanguageProvider {
    // Expand validates and lowers the private markup grammar.
    // GetFragmentRegions publishes embedded Raven expressions.
    // ClassifyToken presents markup tokens without new SyntaxKind values.
    // GetTokenSymbol associates component tags and parameters with symbols.
    // GetCompletions contributes Blazor component and parameter items.
    // GetEmbeddedLanguageProjection exposes the remaining envelope as HTML.
}
```

All six services use the same authored body and body-relative coordinate
system, but publish different editor-neutral views. Parser-backed services
recover the same private grammar; lightweight classification and incomplete
completion helpers preserve that grammar's coordinates during recovery.
`Expand` remains the only method that decides whether the markup is
structurally valid and produces the Blazor `RenderFragment`. Fragment regions
preserve `{ expression }` as Raven; the HTML projection replaces only the
expression text with spaces while retaining braces, length, and line breaks.
Component completion and symbols are derived from the consumer compilation,
while standard element and attribute knowledge remains in the editor's HTML
service.

`SemanticModel.GetMacroInputSnapshot` is the combined token-and-fragment query.
`GetMacroTokens`, `GetMacroFragmentRegions`,
`GetMacroFragmentSemanticInfo`, and
`GetMacroEmbeddedLanguageProjection` expose narrower compiler-owned views when
a host needs only one capability. Equivalent `Compilation` entry points are
available when the caller starts from a syntax tree. Optional-provider failure
is isolated to the corresponding tooling query, and all potentially expensive
providers receive the request cancellation token through their context.

`IMacroExpansionMetadataProvider` is different from the author-facing
contracts above. It is an adapter marker generated for compact `macro`
declarations whose reached `token` or `fragment` contributions are carried by
the expansion result; macro authors do not implement it directly.

### Authoring and testing checklist

Before treating a token-tree DSL as editor-ready:

- Use the same recovery rules and body-relative coordinate system for
  expansion, diagnostics, fragments, tokens, completion, and projections.
- Return meaningful partial tokens, regions, and completion targets while the
  user is typing incomplete input. Use zero-width regions for expected Raven
  syntax slots.
- Report malformed authored input with body-mapped diagnostics. Reserve
  exceptions for provider defects; optional tooling failures are isolated and
  contribute no result for that request.
- Honor `context.CancellationToken` during parsing, symbol lookup, schema work,
  and loops over large inputs. Providers can run on latency-sensitive editor
  requests and must be deterministic for one compilation snapshot.
- Keep body-relative spans within `context.BodySpan`. Preserve exact length and
  line-break positions in an embedded-language projection; mask excluded text
  with non-newline whitespace instead of deleting or reformatting it.
- Publish ordinary Raven symbols whenever DSL tokens or completion items denote
  real types or members. This keeps documentation and navigation consistent
  across compiler and editor features.
- Add focused acceptance coverage for malformed and incomplete input, authored
  span mapping, cancellation, provider failure isolation, ordinary Raven
  semantics inside fragments, DSL completion replacement spans, symbol hover
  and definition, nested macros, and projection exclusion inside Raven spans.

The projection contract is compiler-owned and host-neutral. Automatic reuse of
VS Code's embedded-language providers is a client capability: Raven's VS Code
extension currently bridges completion and hover for `html` projections.
Formatting, linked editing, and projected-language diagnostics are not yet
bridged, and other editors must consume the compiler projection API explicitly.

### Native fragment completion and DSL completion

There are two distinct completion layers in a mixed-language macro body:

1. **Native Raven completion** applies inside a reported fragment. The compiler
   owns parsing, binding, member lookup, replacement spans, and presentation for
   the embedded Raven code. A provider contributes the fragment category plus
   any introduced locals or target type.
2. **DSL completion** applies to the provider's private grammar. Examples are
   lifecycle clauses in an actor DSL, HTML tags and attributes in `markup!`,
   route templates, SQL tables and columns, or schema-derived names.

The first layer is implemented through fragment regions. The second uses the
optional `IMacroCompletionProvider` capability. Its items use body-relative
replacement spans; the compiler maps them back to the authored document,
deduplicates them, preserves ordinary symbol associations, and isolates a
provider failure to that completion request.

```raven
class RouteMacro : IMacroDefinition, IMacroCompletionProvider {
    func GetCompletions(
        context: TokenTreeMacroContext,
        bodyRelativePosition: int
    ) -> ImmutableArray<MacroCompletionItem> {
        // Recover the provider-owned grammar at the cursor.
        // Return editor-neutral items with body-relative replacement spans.
    }
}
```

`markup!` uses this capability for compiler-backed Blazor component tags and
component properties, including incomplete input. Ordinary HTML elements and
attributes are a separate editor-integration concern. The optional
`IMacroEmbeddedLanguageProvider` capability now lets a macro expose one
position-preserving virtual document without exposing its private parser tree:

```raven
class MarkupMacro : IMacroDefinition, IMacroEmbeddedLanguageProvider {
    func GetEmbeddedLanguageProjection(
        context: TokenTreeMacroContext
    ) -> MacroEmbeddedLanguageProjection? {
        let html = /* retain markup and mask embedded Raven */
        context.CreateEmbeddedLanguageProjection("html", html)
    }
}
```

The projected text must have the same length and line breaks as the authored
macro body. `SemanticModel.GetMacroEmbeddedLanguageProjection` returns the
normalized, cached projection with its authored body span; provider failures
remain isolated to the optional tooling query. The checked-in Markup macro
retains its HTML envelope and masks embedded Raven expression text. Raven's VS
Code extension mounts that projection as a virtual document, invokes VS Code's
HTML completion and hover providers, maps their ranges directly back by offset,
and merges their results after compiler-owned Raven tooling. This avoids
duplicating the HTML catalog and documentation in Raven. Other editor features
and hosts can consume the same compiler projection API without depending on VS
Code.
The Markup parser remains the structural authority for validation, expansion,
source mapping, and routing between HTML-owned positions, component semantics,
and embedded Raven regions.

Editor classification for a macro invocation is semantic rather than a
best-effort lexical overlay. On each document snapshot, Raven resolves the
macro's keyword, token, and fragment providers against that current compiler
snapshot before publishing semantic tokens. A concurrent completion or inlay
request may delay classification, but must not replace it with and cache a
syntax-only result that drops the DSL vocabulary.

This is a compiler-owned capability, not an LSP extension point. The current
contract receives the token-tree context and body-relative cursor and returns
editor-neutral `MacroCompletionItem` values. The context carries cancellation.
Future additions such as trigger metadata, richer item kinds, or ordering data
should extend this compiler boundary rather than introduce editor-specific
contracts. The language server remains a presenter, so one macro package can
behave consistently in VS Code, the Playground, and future editors.

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

An attached macro may also be validation-only. It can inspect typed syntax and
its containing declaration, report a diagnostic on an invalid path, and return
an empty expansion when there is no declaration transform to apply:

```raven
macro RequireString(
    message: ExpressionSyntax,
    on target: CaseDeclarationSyntax,
    context: AttachedMacroContext
) {
    let valid =
        if message is InterpolatedStringExpressionSyntax interpolated {
            true
        } else if message is LiteralExpressionSyntax literal {
            true
        } else {
            false
        }

    if !valid {
        expand MacroExpansionResult.FromDiagnostic(
            context.CreateDiagnostic(
                "RequireString expects a string expression.",
                syntax: message,
                code: "REQUIRESTRING001"))
    }

    expand MacroExpansionResult.Empty
}
```

`ErrorMessage` in `Raven.Macros` uses this pattern: it validates its expression
and containing union, while the separate `Error` macro owns the generated
members.

A convenience macro should expand to the ordinary framework model rather than
create a parallel one. For example, the HTML/Blazor sample's `#[Parameter]`
adds Blazor's normal parameter attribute. The standard `Error` macro is a
larger attached-transform example: its Raven implementation preserves the
authored union, adds `System.IError` to the typed base list when necessary, and
introduces only the missing `Message` and `Cause` properties.

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
| `keywords by Handler` | `IMacroKeywordProvider` forwarding member |
| `tokens by Handler` | `IMacroTokenStreamProvider` forwarding member |
| `tokenKinds by Handler` | `IMacroTokenKindProvider` forwarding member |
| `highlighting by Handler` | `IMacroTokenClassifier` forwarding member |
| `fragments by Handler` | `IMacroFragmentProvider` forwarding member |
| `symbols by Handler` | `IMacroTokenSymbolProvider` forwarding member |
| `completion by Handler` | `IMacroCompletionProvider` forwarding member |
| `projection by Handler` | `IMacroEmbeddedLanguageProvider` forwarding member |

The two freestanding contexts preserve the authored carrier through `Carrier`.
It is one of `ParenthesizedMacroCarrierSyntax`,
`ExpressionHeaderMacroCarrierSyntax`, `TokenTreeMacroCarrierSyntax`, or
`DeclarationMacroCarrierSyntax`. `Syntax` remains the complete authored node,
while `Name`, `ExclamationToken`, `ArgumentList`, `ExpressionArgument`, and
`TokenTree` are convenience projections. Compatibility projections are
nullable when that piece does not belong to the selected carrier.

Class-authored macros select a non-default source shape with
`IMacroDefinition.CarrierKinds`. For example, an expression-header macro can
accept both of these forms:

```raven
probe! value
probe! value {
    custom rules
}
```

Its definition publishes `MacroCarrierKinds.ExpressionHeader` and
`MacroBodyRequirement.Optional`, then declares one `ExpressionSyntax` input.
The compiler supplies the first form through `FreestandingMacroContext` and the
second through `TokenTreeMacroContext`; a shared `MacroContext` parameter works
for an entry point accepting both. `MacroBodyRequirement.None` forbids a body,
while `Required` requires one. Leaving both properties at `Default` preserves
the compatibility form inferred from the typed entry point.

The compact Raven `macro` declaration syntax still publishes its inferred
parenthesized or token-tree form. A source-level carrier clause for selecting
expression-header form is a later authoring slice; the normalized descriptor
and execution API no longer require another carrier-model redesign for it.

### Author the carrier shapes

A token-body macro can give an ordinary Raven block a statement-like outer
shape. For example, this abbreviated version of `timer` parses its complete
body and surrounds it with `Stopwatch` boilerplate:

```raven
import System.Collections.Immutable.*
import Raven.CodeAnalysis.Macros.*
import Raven.CodeAnalysis.Syntax.*
import Raven.CodeAnalysis.Text.*

macro Timer(context: TokenTreeMacroContext) -> StatementSyntax
    fragments by GetTimerFragments
{
    let bodyResult = context.ParseBlockResult()
    context.ReportDiagnostics(bodyResult)
    let stopwatch = context.CreateUniqueName("stopwatch")
    expand BuildTimedBlock(bodyResult.Syntax, stopwatch)
}

func GetTimerFragments(context: TokenTreeMacroContext) -> ImmutableArray<MacroFragmentRegion> {
    [context.CreateFragmentRegion(
        MacroFragmentKind.Block,
        TextSpan(0, context.BodySpan.Length))]
}
```

The application is statement-shaped source:

```raven
timer! {
    let index = LoadIndex()
    Rebuild(index)
    Save(index)
}
```

Curly braces delimit a losslessly captured token body; they do not require that
body to use Raven block grammar. The macro author decides whether to ask Raven
to parse it as a block, parse selected Raven fragments, or interpret it as a
completely custom DSL. That freedom comes with an authoring responsibility: the
chosen interpretation should match the expectations created by the macro's
surface syntax.

For `timer`, the block-like expectation is intentional. It asks Raven to parse
the complete body as `BlockStatementSyntax`. `BuildTimedBlock` places that
authored block inside a generated `try` and reports the elapsed duration from
`finally`, so the timer is stopped even when control leaves the body early.
`CreateUniqueName` prevents the generated stopwatch local from colliding with a
caller local. Its `IMacroFragmentProvider` also publishes the complete body as
a `MacroFragmentKind.Block`, preserving ordinary hover and related editor
features inside the braces. The `fragments by` clause generates that interface
implementation for the macro declaration, so this does not require a
class-shaped macro.

Parsing a carrier body as a Raven block gives the macro author Raven's normal
statement and lexical-scope building blocks; it does not by itself guarantee a
well-behaved expansion. The author remains responsible for preserving the
control-flow, evaluation, and scope behavior that the surface form leads users
to expect. A macro may deliberately generate unusual behavior, but it should
not make Raven-shaped syntax misleading accidentally. `timer` therefore keeps
the authored body as one nested block instead of flattening its statements into
the generated scope.

The standard `timer` macro in `Raven.Macros` also reports `TIMER002` when an
invocation is left in release code. That is macro policy rather than carrier
syntax: the macro still expands normally, but reports a warning at its
invocation when the compilation uses release optimization.

The expression-header shape is separate and is currently selected by a
class-authored macro:

```raven
public class ProbeMacro : IMacroDefinition {
    val Name: string => "probe"
    val CarrierKinds: MacroCarrierKinds => MacroCarrierKinds.ExpressionHeader
    val BodyRequirement: MacroBodyRequirement => MacroBodyRequirement.Optional

    func Expand(
        expression: ExpressionSyntax,
        context: MacroContext
    ) -> FreestandingMacroExpansionResult
        => FreestandingMacroExpansionResult.FromExpression(expression)
}
```

That contract admits both `probe! value` and `probe! value { ... }`. The first
form receives a `FreestandingMacroContext`; the second receives a
`TokenTreeMacroContext`. Their shared `MacroContext` base lets one entry point
accept both, while a type test exposes the optional token body when needed.

A compact macro can infer the declaration carrier from a
`FreestandingMacroDeclarationSyntax` input. The generic parameter on the macro
definition specializes the macro itself, while the type parameters found on
`declaration.Header` belong to the carried declaration:

```raven
import Raven.CodeAnalysis.Macros.*
import Raven.CodeAnalysis.Syntax.*

macro Component<TFramework>(
    declaration: FreestandingMacroDeclarationSyntax,
    body: IMacroTokenStream,
    context: TokenTreeMacroContext
) -> MemberDeclarationSyntax {
    let header = declaration.Header
    let declaredName = header.Identifier.ValueText
    let declaredTypeParameters = header.TypeParameterList
    let baseList = declaration.BaseList
    let constraints = declaration.ConstraintClauses

    // Interpret body and construct the resulting member.
    expand BuildComponent<TFramework>(
        declaredName,
        declaredTypeParameters,
        baseList,
        constraints,
        body,
        context
    )
}
```

For this application:

```raven
Component<Blazor>! Greeting<T>(value: T)
    : ComponentBase, IRenderable<T>
    where T: Entity
{
    render value
}
```

`Blazor` binds `TFramework` during macro resolution. `T` is never used to
construct the macro provider; it remains a `TypeParameterSyntax` in the carried
header and participates in `BuildComponent`'s expansion.

Omit the token-body parameter when the declaration shape is a bodyless marker:

```raven
macro Marker(
    declaration: FreestandingMacroDeclarationSyntax
) -> MemberDeclarationSyntax {
    expand BuildMarkerMember(declaration.Header)
}

Marker! GeneratedMember
```

Conversely, declaring `IMacroTokenStream` or `TokenTreeMacroContext` makes the
body part of the contract. The authoring signature therefore says both which
carrier pieces the implementation consumes and whether a token body is needed.

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

Outer DSLs can additionally contribute symbol-bearing completion items and
ordinary symbol associations for their own tokens. Position-preserving
embedded-language projections are implemented for host reuse; Raven's VS Code
extension currently delegates completion and hover to the projected language
service while reported Raven fragments retain cursor ownership. Formatting,
linked editing, and projected-language diagnostics remain later editor slices.

Expression and raw-body statement placement, expression-header syntax,
single-member and member-list expansion, and structured declaration headers
with generic parameters, base-list or return-type suffixes, constraints, and
permits clauses are implemented. Macro-defined declaration clauses,
compact-source carrier selection, type and pattern invocation targets, the
`[...]`/`MacroList<T>` input family, and typed syntax wrappers remain future
work.
