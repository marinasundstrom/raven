# Authoring Raven macros

Raven macros are compile-time programs that validate input and produce ordinary
Raven syntax. Start with `macro func`. Move to provider interfaces only when a
macro needs capabilities the compact declaration syntax does not yet project.

> [!NOTE]
> Macro authoring is experimental. Examples here describe the current
> implementation. Sections marked **Future** describe planned tooling.

## Choose the smallest useful shape

| Need | Start with |
| --- | --- |
| Typed compile-time values | ordinary `macro func` parameters |
| An authored Raven expression | an `ExpressionSyntax` parameter |
| An unrestricted brace body | one `IMacroTokenStream` parameter |
| Body text, parsing, diagnostics, or file APIs | a `TokenTreeMacroContext` parameter |
| Replace or introduce declarations | an attached `macro func ... on ...` |
| Custom tokenization or fragment metadata | a class-authored provider interface |

The compact and class-authored forms are two projections of one model. They use
the same invocation syntax, registry, contexts, diagnostics, and results.

## 1. Start with a local macro function

A macro in the same project is compiled in Raven's compile-time partition and
is not emitted as an ordinary runtime function:

```raven
import Raven.CodeAnalysis.Syntax.SyntaxFactory.*

macro func Double(value: int) {
    let doubled = value * 2
    expand ParseExpression(doubled.ToString())
}

let answer = Double!(21)
```

`value` is a compile-time constant parameter. `expand` contributes the
`ExpressionSyntax` that replaces the invocation. Normal Raven control flow can
choose an expansion; the last reached `expand` wins.

Use typed parameters for configuration instead of recovering values from raw
text. The normalized parameter schema also drives binding, completion, and
signature help.

## 2. Receive authored Raven syntax

Use a syntax-role parameter when the macro needs the caller's expression rather
than its constant value:

```raven
import Raven.CodeAnalysis.Syntax.*
import Raven.CodeAnalysis.Syntax.SyntaxFactory.*

macro func AddOffset(offset: int, expression: ExpressionSyntax) {
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

macro func FirstTokenLength(offset: int, tokens: IMacroTokenStream) {
    let token = tokens.ReadToken()
    let length = token.Text.Length + offset
    expand ParseExpression(length.ToString())
}

let length = FirstTokenLength!(1) { raven }
```

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

macro func Guard(context: TokenTreeMacroContext) {
    let span = FindExpressionSpan(context.GetBodyText())
    let expression = context.ParseExpressionResult(span)
    context.ReportDiagnostics(expression.Diagnostics)
    expand BuildGuardExpression(expression.Syntax)
}
```

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

Macro contexts accumulate diagnostics through the ordinary
`ReportDiagnostic` and `ReportDiagnostics` APIs. This deliberately avoids a
separate diagnostic statement in the language. `expand` supplies the final
expansion and returns from the current macro execution path; diagnostics
reported before it are retained. Reaching the end of the body also returns any
accumulated diagnostics and contributions.

## 5. Report precise diagnostics

The provider interface gives full control over diagnostics and results:

```raven
import Raven.CodeAnalysis.Macros.*

class GuardMacro : ITokenTreeExpressionMacro {
    val Name: string => "Guard"

    func Expand(context: TokenTreeMacroContext) -> FreestandingMacroExpansionResult {
        let span = FindExpressionSpan(context.GetBodyText())
        let expression = context.ParseExpressionResult(span)

        if expression.HasErrors {
            return FreestandingMacroExpansionResult.FromDiagnostics(
                expression.Diagnostics)
        }

        FreestandingMacroExpansionResult.FromExpression(
            BuildGuardExpression(expression.Syntax))
    }
}
```

Use native parser diagnostics for malformed embedded Raven. Use
`CreateBodyDiagnostic` or `CreateDiagnostic` for DSL rules. Prefer diagnostics
over throwing for expected invalid input; an exception means the provider
itself failed.

## 6. Surface fragment spans for tooling

A token-tree macro can implement `IMacroFragmentProvider` alongside expansion:

```raven
import System.Collections.Immutable.*
import Raven.CodeAnalysis.Macros.*

class HtmlMacro : ITokenTreeExpressionMacro, IMacroFragmentProvider {
    val Name: string => "Html"

    func GetFragmentRegions(
        context: TokenTreeMacroContext
    ) -> ImmutableArray<MacroFragmentRegion> {
        let parsed = HtmlDslParser(context.GetBodyText()).Parse()
        var regions: ImmutableArray<MacroFragmentRegion> = []

        for span in parsed.EmbeddedExpressionSpans {
            regions = regions.Add(context.CreateFragmentRegion(
                MacroFragmentKind.Expression,
                span))
        }

        regions
    }

    func Expand(context: TokenTreeMacroContext) -> FreestandingMacroExpansionResult {
        // Parse, validate, and lower to ordinary Raven syntax.
        FreestandingMacroExpansionResult.Empty
    }
}
```

Only a syntax category and span cross the boundary. The HTML tree remains
private. The compiler maps body-relative regions to absolute authored spans.
Zero-width regions can say “an expression is expected here” in incomplete
input.

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

Use an `on` clause for an attached macro:

```raven
macro func Observable(enabled: bool) on property: Property {
    if enabled {
        replace Rewrite(property)
        introduce CreateBackingField(property)
    }
}
```

`replace` sets the current declaration replacement. `introduce` appends members
in execution order. The class-authored equivalent is
`IAttachedDeclarationMacro`; its context exposes the original
`TargetDeclaration` and composed `CurrentDeclaration`.

A convenience macro should expand to the ordinary framework model rather than
create a parallel one. For example, the HTML/Blazor sample's `#[Parameter]`
adds Blazor's normal parameter attribute.

## 8. Package a reusable library

A reusable Raven macro project marks its assembly as a compiler plugin:

```raven
import Raven.CodeAnalysis.Macros.*

[assembly: RavenCompilerPlugin(typeof(HtmlMacro))]
```

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

## Projection from syntax to provider contracts

The compiler lowers `macro func` declarations to adapters, but tools expose an
`IMacroFunctionSymbol`, not the generated class.

| Source feature | Provider projection |
| --- | --- |
| ordinary parameter | typed parameter schema |
| `ExpressionSyntax` parameter | authored expression projection |
| `IMacroTokenStream` parameter | token-tree macro and token stream |
| `TokenTreeMacroContext` parameter | complete token-tree context |
| `FreestandingMacroContext` parameter | complete argument-style context |
| `AttachedMacroContext` parameter | complete attached context |
| `on Type` / `on Property` | attached target |
| `expand` | final expansion and semantic return |
| reached `replace` | replacement declaration |
| reached `introduce` | ordered introduced members |
| reached `fragment` | ordinary Raven fragment metadata |
| reached `token` | token kind and classification metadata |

Token-tree macro functions can publish editor regions through the same
execution-ordered contribution model as expansion:

```raven
macro func RavenExpression(context: TokenTreeMacroContext) {
    let span = TextSpan(0, context.BodySpan.Length)
    fragment context.CreateFragmentRegion(MacroFragmentKind.Expression, span)
    expand context.ParseExpression(span)
}
```

`fragment` accepts a `MacroFragmentRegion` and is valid only for a token-tree
macro function. The generated adapter keeps reached regions on its expansion
result; `SemanticModel` uses them when the macro does not implement a dedicated
`IMacroFragmentProvider`. Implement that provider directly when tooling must
remain independent from full expansion, especially for heavily recovered or
incomplete DSL input.

The same fragment declaration enables ordinary Raven hover as well as
completion. `SemanticModel.GetMacroFragmentSemanticInfo(invocation, position)`
resolves symbols and types in the invocation's caller scope, with the region's
`MacroFragmentLocal` values layered over that scope. The language server uses
that compiler result to render its normal Raven signature, containing-symbol,
and documentation presentation. Macro authors do not implement a hover
provider for ordinary Raven fragments. Go-to-definition uses the same result:
caller symbols navigate to their ordinary Raven declarations. A
DSL-introduced local can also supply its declaration token span:

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

* `samples/projects/macro-functions` — typed, syntax, and token-stream inputs;
* `samples/projects/macro-dsl` — the minimal provider-class reference for one
  DSL keyword, one embedded Raven expression, native diagnostics, fragment
  tooling, and debugger source provenance;
* `samples/projects/macro-token-stream` — a custom lexer-backed stream;
* `samples/projects/macro-reactive` — attached replacement and introduction;
* `samples/projects/macro-freestanding` — LINQ-like query parsing, three
  embedded Raven expression regions, caller-scope completion, and an
  introduced sequence-element range variable;
* `samples/projects/macro-html-blazor` — private HTML parsing, embedded Raven
  fragments, fragment metadata, component macros, and Blazor lowering.

See the [macro reference](lang/spec/macros.md) for current restrictions.

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

The predicted follow-on slices are maintained in dependency order under
“Predicted post-MVP DSL tooling slices” in
`docs/lang/proposals/macros/implementation-plan.md`. Ordinary Raven completion
now works inside reported fragment spans without requiring public custom syntax
trees. Query-like macros can also bridge an introduced sequence-element local
into selected fragments. Broader custom scope shapes should wait for another
concrete DSL use case.
