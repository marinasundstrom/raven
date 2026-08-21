# Blazor Component Macros

For a progression from a minimal `macro` to the provider and tooling
contracts used here, see the repository's
[macro authoring guide](../../../docs/macro-authoring.md).

This isolated experiment shows a DSL built by macros on top of Blazor. It is
another way to express ordinary Blazor components, not a competing UI runtime:
the macros remove authoring boilerplate and lower back to Blazor's existing
component, parameter, rendering, event, CSS, hosting, and interop infrastructure.
It does not add markup syntax to the compiler or promote these macros into
`Raven.Macros`.

The sample is therefore three demonstrations at once. At the application level,
it shows a compact, React-like way to author Blazor components in Raven. The
`Greeting` example now uses the declaration-shaped, function-style form:

```raven
import System.Console.*

component! Greeting(Name: string = "") {
    WriteLine("Rendering Greeting for ${Name}")

    markup! {
        <section class="greeting">
            <h1>Hello {Name}</h1>
        </section>
    }
}
```

`FunctionComponent` is the function-style declaration macro's canonical name;
`component` is its declaration-facing alias. This keeps it distinct from the
existing attached `Component` macro. The `component!` body is ordinary Raven
code rather than an HTML-only region: the `WriteLine` statement executes as
part of rendering before the body reaches its final expression.
`markup!` is a nested macro invocation that produces the final render fragment.
The component macro turns the declared name and typed parameters into a normal
Blazor `ComponentBase` class and a `Render()` method. The final expression in
the function-style body is promoted to the render result, so setup statements
can precede the nested markup invocation. Existing class-based
`#[Component]` examples remain alongside it while the declaration form is
experimental.

At the macro-composition level, it verifies a macro invocation nested inside
the token-tree input interpreted by another macro. `component!` first expands
the declaration-shaped carrier into ordinary Raven declarations. Its generated
`Render()` body retains the authored `markup!` invocation, which a subsequent
macro pass expands into a Blazor `RenderFragment`. This is the same composition
model advanced DSLs can use to delegate nested regions to focused macros.

At the macro-authoring level, it shows that a library can define its own
token-based markup DSL, lower that DSL to an existing framework, and
participate in Raven's diagnostics and editor tooling. `Markup!` is an ordinary
sample macro, not privileged compiler syntax. The broader name reflects that
the DSL composes components, embedded Raven expressions, control flow,
callbacks, and framework services into Blazor render trees rather than merely
parsing HTML.

Because a token-tree macro invocation is an expression, these templates can be
written inline in an ordinary `.rvn` code file. They are not restricted to a
separate template-file format. This is especially useful with Blazor: the macro
provides the concise embedded syntax, while the generated code still uses
Blazor's normal component, parameter, event, rendering, CSS, and interop models.

The Raven-authored `Markup!` macro parses a deliberately small HTML-shaped DSL
and lowers it directly to a Blazor `RenderFragment` implemented with
`RenderTreeBuilder`. The Raven-authored `#[Component]` attached macro derives a
class from `ComponentBase` and introduces `BuildRenderTree` by forwarding to
the authored `Render()` method. The declaration-shaped `component!` macro
generates the equivalent class from its declaration header and Raven body.
`#[Parameter]` performs a one-to-one expansion
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
- component tag identifiers resolved to their ordinary Raven component symbols
  for normal hover and go-to-definition;
- compiler-backed completion for incomplete Blazor component tags and their
  properties, retaining ordinary type/property symbols and authored replacement
  spans;
- imported component types from referenced Blazor projects, using the same
  namespace lookup and component frames as Raven-authored components;
- qualified component tags such as `<ExistingBlazorComponents.StatusBadge>`,
  with the terminal type name and parameters retaining ordinary symbol tooling;
- Raven component CSS isolation through explicit `RavenComponentCss` project
  items, Blazor's existing Static Web Assets pipeline, and build-provided scope
  attributes emitted by `Markup!`;
- event attributes such as `onClick={increment}`;
- self-closing component tags with Blazor parameters;
- component `EventCallback` and `EventCallback<T>` parameters accepting callback
  references or inline Raven lambdas, with the Blazor wrapper generated by the
  macro;
- scalar, `RenderFragment`, and sequences of fragment expressions as children;
- Raven `if` expressions for conditional content and attributes;
- canonical Raven `match value` expressions for exhaustive content and union
  case destructuring;
- Raven list comprehensions, including `if` filters, for repeated content;
- `key={expression}` mapped to Blazor's native component/element key;
- deterministic, preorder render-tree sequence numbers; and
- body-relative diagnostics for malformed HTML envelopes.

Not supported by the macro:

- component child content or macro-owned control-flow directives;
- attribute splatting or Razor compatibility;
- distinct tag-versus-attribute classifications, standard HTML catalog
  completion, or closing-tag completion;
- multiple root elements.

`#[Parameter]` is convenience rather than a new parameter model. Components
can use Blazor's ordinary `[Parameter]` attribute directly when preferred.

## Packaging direction

The component and markup implementations already live together in the separate
`ComponentMacros.Macros` class-library project and are consumed by the showcase
application through a project reference. The project carries the provisional
package identity `Raven.Blazor.Macros`, making the reusable boundary explicit
without publishing an unstable package yet. The intended next step is to move
that project out of the individual sample and validate it as an ordinary
downstream package reference.
Because the current HTML-shaped DSL lowers directly to `RenderFragment` and
understands Blazor components, events, parameters, and keys, a package identity
such as `Raven.Blazor.Macros` is more accurate than a backend-neutral HTML name.

`Markup` is the canonical macro name and `markup` is its function-style alias.
The former `Html!` spelling remains a compatibility alias while the surface is
experimental. A future non-Blazor backend could share a separately extracted
markup parser without conflating that parser with the Blazor-specific lowering.

## Sample layout

The application follows the same one-component-per-file convention expected
of a normal component project:

```text
app/src/
├── Program.rvn
├── Components/
│   ├── BlazorInteropShowcase.rvn
│   ├── Counter.rvn
│   ├── Counter.rvn.css
│   ├── Gallery.rvn
│   ├── Greeting.rvn
│   ├── MatchShowcase.rvn
│   ├── TodoItem.rvn
│   └── TodoList.rvn
└── Models/
    ├── BuildStage.rvn
    └── Todo.rvn

blazor/
├── ExistingBlazorComponents.csproj
├── StatusBadge.razor
└── StatusBadge.razor.css
```

`Program.rvn` contains only the executable render-tree verification. Component
and model declarations remain ordinary Raven source files discovered by the
project's existing `src/**/*.rvn` compile glob.

Build and run:

```bash
dotnet run --project app/ComponentMacros.rvnproj --property WarningLevel=0
```

The executable invokes each generated fragment against a `RenderTreeBuilder`
and verifies its frame shape. The Todo scenario renders a filtered
comprehension, changes the model, verifies that the list is re-evaluated, and
then includes completed items. This proves macro expansion, Blazor binding,
emit, keyed list rendering, and runtime execution without requiring a web host.
The match scenario uses the canonical prefix form over a closed union,
destructures each case payload directly into rendered text, advances the
component state, and verifies that the expression is re-evaluated.
The interop scenario imports a conventional Razor component from a referenced
.NET project, instantiates it in `Markup!`, and verifies the resulting native
Blazor component frame. `StatusBadge.razor.css` is processed by Blazor's normal
CSS-isolation pipeline; the host links its generated `.styles.css` bundle just
as an ordinary Blazor application does.

`Counter.rvn.css` demonstrates the Raven side of the same pipeline. The project
declares one stable scope through `RavenComponentCss`; the sample target
registers the stylesheet with Microsoft's `ScopedCssInput` processing and
projects that identical value as a source-file macro option. `Markup!` adds the
scope attribute to ordinary element frames, while the Static Web Assets SDK
rewrites, bundles, fingerprints, and publishes the CSS. No CSS parser or
bundler is implemented by Raven or the macro.

Control flow remains Raven code rather than becoming extra HTML-macro syntax:

```raven
{if showDetails {
    Markup! { <p>{details}</p> }
} else {
    Markup! { <p>No details</p> }
}}

{[for todo in todos if showCompleted || !todo.IsCompleted =>
    Markup! { <TodoItem key={todo.Id} Title={todo.Title} /> }]}

{match phase { 0 => "Design" 1 => "Compile" _ => "Ship" }}
```

The small `HtmlContent` adapter is Blazor-specific runtime support: it funnels scalar
values, fragments, and fragment sequences into `RenderTreeBuilder`. It does
not introduce a parallel component or state model.

Run the styled interactive browser demo:

```bash
dotnet run --project host/ComponentMacrosShowcase.csproj
```

The same showcase can run entirely in WebAssembly:

```bash
dotnet run --project wasm/ComponentMacrosShowcase.Wasm.csproj
```

The Server and WebAssembly hosts share the same `Home.razor`, stylesheet, and
Raven component library. They differ only in startup and render-mode plumbing.
The published WebAssembly host is the zero-install showcase linked from the
Raven documentation site. It is labeled experimental because the macro remains
a prototype rather than a committed framework surface. The thin C#/Razor host
is temporary; a future milestone is to author the entire Blazor application in
Raven.

To debug the templates, open this sample directory in VS Code and press F5.
The checked-in `.vscode/launch.json` builds the C# Blazor host as the startup
project, loads `ComponentMacros.pdb`, and opens the `http` launch profile.
Breakpoints bind inside ordinary Raven component methods, callbacks, inline
lambdas, and executable Raven expressions embedded in `Markup!`; generated
`RenderTreeBuilder` plumbing is skipped while stepping.

The host is deliberately thin C#/Razor infrastructure. Its live Counter,
Greeting, Gallery, Todo, Match, and interop showcases are public component
classes authored in Raven and expanded by the sample macros. The interop
showcase renders `StatusBadge`, an ordinary component authored in a `.razor`
file and resolved through `import ExistingBlazorComponents.*`. The Todo preview
uses a checkbox to update the parent model through an ordinary Blazor
`EventCallback`; the filtered comprehension then produces the next set of
keyed components.

The showcase's five fixed source listings use static semantic spans and no
highlighting runtime. Any future editable or generated listing should consume
`src/Raven.VSCode/syntaxes/raven.tmLanguage.json` through the repository's
existing TextMate integration instead of adding another Raven tokenizer. The
TextMate/Oniguruma/Monaco pipeline remains outside this thin sample host.

## Planned follow-up work

The CSS composition examples use normal Razor isolation for `StatusBadge` and
the same Static Web Assets machinery for Raven's `Counter`. The sample targets
are intentionally local until this integration can be distributed as a
library. A later JavaScript interop example should likewise use Blazor's
existing `IJSRuntime` and module model rather than introduce a mechanism owned
by the markup macro.

Debugger sequence-point parity is compiler-owned rather than implemented by
the markup macro. Ordinary and top-level functions, match expressions, user
locals, async methods, iterators, and mapped macro fragments now retain
non-overlapping spans on the correct emitted methods. Future debugger work can
therefore focus on advanced inspection and stepping behavior rather than
template-specific source mapping.

## Editor-readiness fixture

`Markup!` keeps every embedded Raven expression as a body-relative `TextSpan`
and delegates that span to `TokenTreeMacroContext.ParseExpressionResult`.
Malformed Raven therefore reports a native parser diagnostic at the authored
expression inside the HTML body. The markup parser owns only the surrounding DSL
grammar.

The macro also implements `IMacroFragmentProvider` and reports those same spans
as `MacroFragmentKind.Expression`. `SemanticModel.GetMacroFragmentRegions`
therefore exposes body-relative and absolute authored spans without exposing
the markup parser's representation. Zero additional HTML nodes enter Raven's
syntax or bound trees.

`IMacroTokenClassifier` supplies lightweight presentation categories over the
same standard token stream used by the macro infrastructure. Standard tokens
also expose stable Raven kind names. This prototype does not add HTML token
kinds to Raven's global `SyntaxKind`; a later custom HTML stream can introduce
provider-owned raw kinds and name them through `IMacroTokenKindProvider` if tag
and attribute names need distinct editor semantics.

`IMacroTokenSymbolProvider` resolves component tags and component attributes
against ordinary consumer symbols. `<Greeting Name="Raven" />` therefore gives
`Greeting` normal type hover and definition behavior, while `Name` presents
the `Greeting.Name` property. Qualified tags are also accepted; for
`<ExistingBlazorComponents.StatusBadge>`, the terminal `StatusBadge` token
publishes the resolved type symbol and its attributes resolve against that full
component name. The macro uses its private parse result to supply context, but
publishes only symbol metadata over each token span—not HTML nodes projected
into Raven's syntax tree.

The macro invocation hint remains available on the `Html` name. Inside its
braces, hover is reserved for explicit DSL token-symbol associations and
reported Raven fragments; unrelated HTML text does not fall back to the macro
invocation hint. Nested `Markup!` invocations inside comprehensions use the same
compiler-owned lookup path.

This is the compiler-side routing primitive for future macro-aware editor
services. Semantic highlighting consumes the compiler snapshot today.
Ordinary Raven completion, hover, and definition now route through reported
fragment spans, and component tags and attributes can publish ordinary symbol
targets. The markup parser's own tree, if it grows one, remains private to the
macro.

The macro also provides compiler-backed completion for Blazor component tags
and properties at incomplete markup positions. Standard HTML element and
attribute completion is intentionally separate. `MarkupMacro` now implements
`IMacroEmbeddedLanguageProvider` and projects the parser-owned markup envelope
as a position-preserving `html` document. Embedded Raven expression text is
masked while all offsets and line breaks remain unchanged. Raven's VS Code
extension mounts this projection in its existing HTML language service and maps
completion results directly back to the authored body. Standard HTML elements,
attributes, and closing tags therefore come from VS Code, while component tags
and parameters remain compiler-owned Raven completions. The Markup parser
remains responsible for structural validation, source coordinates, and the
embedded Raven exclusions.

The sample now exercises the DSL-tooling MVP: immutable combined input
snapshots, token kinds and classifications, embedded expression spans,
deterministic cursor routing, embedded-language projection, failure isolation,
and semantic highlighting.
Compiler acceptance coverage builds this checked-in `MarkupMacro.rvn` into an
in-memory plugin and verifies those contracts plus authored-source diagnostics,
preventing the sample and tooling API from drifting independently.
For component parameters whose resolved property type is Blazor's
`EventCallback` or `EventCallback<T>`, the macro target-types an expression as
`System.Action` or `System.Action<T>` and emits the matching
`EventCallback.Factory.Create` call. Callback references and inline lambdas
therefore use the same ordinary Raven expression-fragment contract:

```raven
<TodoItem Toggled={callback} />
<TodoItem Toggled={(id) => toggleTodo(id)} />
```

The remaining dependency-ordered post-MVP slices are tracked in the
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
