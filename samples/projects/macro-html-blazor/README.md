# HTML-to-Blazor macro prototype

For a progression from a minimal `macro func` to the provider and tooling
contracts used here, see the repository's
[macro authoring guide](../../../docs/macro-authoring.md).

This isolated experiment tests whether Raven's existing raw token-tree macros
are a practical foundation for embedded DSLs. It does not add HTML syntax to
the compiler or promote these macros into `Raven.Macros`.

The Raven-authored `Html!` macro parses a deliberately small HTML-shaped DSL
and lowers it directly to a Blazor `RenderFragment` implemented with
`RenderTreeBuilder`. The Raven-authored `#[Component]` attached macro derives a
class from `ComponentBase` and introduces `BuildRenderTree` by forwarding to
the authored `Render()` method. `#[Parameter]` performs a one-to-one expansion
to Blazor's ordinary `Microsoft.AspNetCore.Components.ParameterAttribute`.
Unit-returning functions omit `-> unit`; Raven infers `unit` when no meaningful
return value is produced.

Supported by the prototype:

- nested HTML elements;
- plain text with formatting whitespace collapsed;
- quoted attributes;
- `{ RavenExpression }` content parsed by Raven with authored-source diagnostics;
- `{ RavenExpression }` spans surfaced through the compiler's optional macro
  fragment-region API;
- HTML-body identifiers, literals, and punctuation surfaced through the
  compiler's classified token-stream API;
- event attributes such as `onClick={increment}`;
- self-closing component tags with Blazor parameters;
- deterministic, preorder render-tree sequence numbers; and
- body-relative diagnostics for malformed HTML envelopes.

Not supported by the macro:

- component child content, directives, loops, or conditionals;
- attribute splatting or Razor compatibility;
- tag-versus-attribute editor semantics or DSL-specific completion;
- multiple root elements.

`#[Parameter]` is convenience rather than a new parameter model. Components
can use Blazor's ordinary `[Parameter]` attribute directly when preferred.

Build and run:

```bash
dotnet run --project app/HtmlBlazorSample.rvnproj --property WarningLevel=0
```

The executable invokes each generated fragment against a `RenderTreeBuilder`
and reports its frame count. This proves macro expansion, Blazor binding, emit,
and runtime execution without requiring a web host.

Run the styled interactive browser demo:

```bash
dotnet run --project host/HtmlBlazorShowcase.csproj
```

The host is deliberately thin C#/Razor infrastructure. Its live Counter and
Greeting instances are the public component classes authored in Raven and
expanded by the sample macros. Clicking Counter exercises the generated
`EventCallback` through Blazor's interactive server renderer.

The showcase's two fixed source listings use static semantic spans and no
highlighting runtime. Any future editable or generated listing should consume
`src/Raven.VSCode/syntaxes/raven.tmLanguage.json` through the repository's
existing TextMate integration instead of adding another Raven tokenizer. The
TextMate/Oniguruma/Monaco pipeline remains outside this thin sample host.

## Editor-readiness fixture

`Html!` keeps every embedded Raven expression as a body-relative `TextSpan`
and delegates that span to `TokenTreeMacroContext.ParseExpressionResult`.
Malformed Raven therefore reports a native parser diagnostic at the authored
expression inside the HTML body. The HTML parser owns only the surrounding DSL
grammar.

The macro also implements `IMacroFragmentProvider` and reports those same spans
as `MacroFragmentKind.Expression`. `SemanticModel.GetMacroFragmentRegions`
therefore exposes body-relative and absolute authored spans without exposing
the HTML parser's representation. Zero additional HTML nodes enter Raven's
syntax or bound trees.

`IMacroTokenClassifier` supplies lightweight presentation categories over the
same standard token stream used by the macro infrastructure. Standard tokens
also expose stable Raven kind names. This prototype does not add HTML token
kinds to Raven's global `SyntaxKind`; a later custom HTML stream can introduce
provider-owned raw kinds and name them through `IMacroTokenKindProvider` if tag
and attribute names need distinct editor semantics.

This is the compiler-side routing primitive for future macro-aware editor
services. Semantic highlighting consumes the compiler snapshot today.
Ordinary Raven completion within the reported regions and macro-introduced
semantic scopes remain future work. The HTML parser's own tree, if it grows
one, remains private to the macro.

The sample now exercises the DSL-tooling MVP: immutable combined input
snapshots, token kinds and classifications, embedded expression spans,
deterministic cursor routing, failure isolation, and semantic highlighting.
Compiler acceptance coverage builds this checked-in `HtmlMacro.rvn` into an
in-memory plugin and verifies those contracts plus authored-source diagnostics,
preventing the sample and tooling API from drifting independently.
The dependency-ordered post-MVP slices are tracked in the
[macro implementation plan](../../../docs/lang/proposals/macros/implementation-plan.md#predicted-post-mvp-dsl-tooling-slices).

## Playground status

The repository's browser Playground can already compile Raven-authored local token-tree
macros in memory and its raw macro envelope accepts HTML punctuation. It does
not yet supply ASP.NET Core reference assemblies to user compilations, expose
expanded macro source, or mount dynamically emitted component types in the
host renderer. The recommended first Playground experiment is compile-only:
embed the minimum Blazor reference assemblies and show the expanded Raven
source before attempting live component rendering.

The proposed preview contract and staged extraction plan are described in
[PLAYGROUND.md](PLAYGROUND.md). The important constraint is that the sample
remains the source prototype; the Playground should consume its component and
macro contract rather than grow an independent HTML implementation. No
Playground integration should begin until those macros are distributed as a
separately consumable library/package.
