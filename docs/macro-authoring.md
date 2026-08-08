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

The compact syntax does not yet have a diagnostic contribution statement. Use
the provider contract when native diagnostics must be forwarded directly.

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
| `on Type` / `on Property` | attached target |
| reached `expand` | replacement expression |
| reached `replace` | replacement declaration |
| reached `introduce` | ordered introduced members |

`IMacroFragmentProvider` does not yet have a `macro func` projection. Future
syntax should contribute kind/span pairs through the same adapter result
boundary without requiring a public DSL tree. A contribution form parallel to
`expand`, `replace`, and `introduce` is plausible, but its spelling should wait
until completion and repeated-region cases establish the smallest surface.

## Working examples

The repository examples progress from compact syntax to full DSL handling:

* `samples/projects/macro-functions` — typed, syntax, and token-stream inputs;
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
