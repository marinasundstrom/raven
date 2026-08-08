# Playground preview proposal

## Product boundary

This sample is the prototype for a possible standalone Raven UI project. It is
the place to evolve the authoring surface, macro diagnostics, generated Blazor
shape, and interactive examples. Playground integration is a consumer of that
work, not a second implementation of the HTML DSL.

**Distribution is the gate for Playground work.** Do not add this prototype to
`Raven.Playground`, reference the sample projects from it, or copy the macros
into the Playground. Integration begins only after the component and HTML
macros are available as a separately consumable library/package with a stable
public contract and an ordinary downstream reference story.

The likely future extraction has three layers:

1. a Raven HTML macro package containing the HTML parser and lowering;
2. a Raven Blazor component macro package containing `#[Component]`; and
3. hosts such as the showcase and Playground that reference those packages.

Keeping those boundaries visible in the sample lets the experiment become a
separate repository or package without first untangling Playground UI code.

## Preview trigger

After a distributable library exists, the first Playground prototype should
use capability discovery instead of adding special syntax to Raven source:

- compile the program with Blazor reference assemblies available;
- inspect the expanded compilation for public, concrete classes derived from
  `Microsoft.AspNetCore.Components.ComponentBase`;
- make a Preview result tab available when at least one class is found;
- select the sole component automatically, or show a component picker when
  several are available; and
- prefer Preview after a successful compile when the program has components
  but no console entry point.

This is the trigger: a successfully emitted component-shaped artifact. It
works for the macro prototype without coupling the language to one IDE. A
future project manifest can name a root component when convention is no longer
enough.

## Sample initialization contract

Registered Playground samples should not rely on component discovery alone.
Each entry in the existing sample index should be able to declare a minimal
launch descriptor that tells the host which experience to initialize and what
artifact is its root. For example:

```json
{
  "id": "html-counter",
  "title": "HTML counter",
  "file": "html-counter.rvn",
  "category": "Macros",
  "launch": {
    "mode": "blazor-component",
    "component": "Samples.Counter"
  }
}
```

`mode` initially needs only the existing console behavior and
`blazor-component`. The fully qualified component name is stable compiler
output, so the host can select the Preview pane and mount that exact type after
a successful emit. Component parameters can be added to the descriptor when a
sample needs them; they are not required for the first preview slice.

This metadata belongs to the Playground sample registry, not to the HTML DSL or
the component macro. Ad hoc source without a descriptor can retain the
discovery behavior above: select the sole component automatically or ask the
user when several are available. That keeps registered demos deterministic
without introducing Playground-specific syntax into Raven programs.

## Why the existing Playground can host it

The Playground is already a Blazor WebAssembly application. Its compilation
pipeline emits an assembly image, and its runner already loads emitted Raven
assemblies into the browser process. The preview path can reuse that pipeline:

```text
Raven source
    -> macro expansion and compilation
    -> emitted in-memory assembly
    -> Assembly.Load(image)
    -> resolve discovered component Type
    -> ErrorBoundary + DynamicComponent
```

No separate server application or nested WebAssembly runtime is required for
the first version. The generated component and the Playground share the same
Blazor runtime identity, so parameters, render fragments, and event callbacks
can use the host renderer directly.

## Required Playground changes

1. Embed the `Microsoft.AspNetCore.App.Ref` component reference assemblies in
   addition to the existing `Microsoft.NETCore.App` references.
2. Make the prototype macro assembly available as a macro reference. After
   extraction, this becomes an ordinary package/reference choice.
3. Emit a library when a previewable component has no entry point; preserve
   the current console output mode for runnable programs.
4. Extend the compilation result with preview descriptors containing stable
   metadata type names and display names.
5. Load the emitted assembly once per successful generation and resolve the
   selected descriptor to a `Type`.
6. Add Output and Preview result tabs. Render the selected type through
   `DynamicComponent`, keyed by compilation generation so recompilation creates
   fresh component state.
7. Put the preview subtree inside an `ErrorBoundary` with a reset action so a
   component failure does not take down the editor.

## Prototype constraints

- Loading successive assemblies in browser WebAssembly is not unloadable; the
  first implementation should accept this session-lifetime cost and avoid
  recompiling on every keystroke.
- The preview shares the Playground document and CSS cascade. Start with a
  visibly bounded preview canvas; investigate stronger style isolation only
  after the rendering path proves useful.
- Executing a component has the same trust boundary as the existing Run action:
  user code runs locally in the browser. Preview must remain an explicit
  compile/run action, never an automatic response to typing.
- Expanded macro source and diagnostics should remain available beside Preview
  because they are central to evaluating the DSL, not merely debugging aids.

## Recommended order

The sample remains priority zero: make the component surface compelling and
the generated Blazor contract stable. Next, extract and validate a separately
distributable library/package. Only then add Playground support in two
increments:

1. **Package and consume** — build the macro libraries independently and prove
   that an external Raven application can restore/reference them without
   repository-specific project links.
2. **Compile and inspect** — Blazor references, package availability, expanded
   source, and discovered component descriptors.
3. **Mount and interact** — Preview tab, `DynamicComponent`, error containment,
   component selection, and state reset after recompilation.
