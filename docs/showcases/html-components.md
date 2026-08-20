# Build Blazor components with composable macros

This experimental sample is a DSL built by macros on top of Blazor. It offers
another way to express ordinary Blazor components while retaining Blazor's
component model, renderer, events, CSS isolation, hosting, and .NET interop.

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

## What the sample shows

- `component!` is a declaration-shaped macro that generates an ordinary Blazor
  `ComponentBase` class from its name, typed parameters, and Raven body.
- Ordinary Raven statements can run before the final render expression; this
  example logs through `System.Console` before evaluating `markup!`.
- `markup!` is an ordinary token-tree macro supplied by the sample library, not
  markup syntax built into the Raven compiler.
- The sample deliberately nests `markup!` inside input interpreted by
  `component!`, verifying that one macro can preserve and compose another macro
  invocation in its expansion.
- The macro reads the embedded template at compile time and expands it into a
  normal Blazor `RenderFragment`.
- Raven expressions such as `Name` remain visible inside the
  template and participate in compiler and editor tooling.
- The earlier `#[Component]` class form remains available beside the compact
  function-component form.
- The result still uses ordinary Blazor components, parameters, callbacks,
  rendering, CSS isolation, and .NET interop.

The prototype is intentionally labeled experimental. It demonstrates that a
library can add a compact domain-specific language without making that DSL a
privileged part of Raven.

## How macros fit

A Raven macro is a compile-time transformation that is explicitly invoked in
source. Here, `markup!` owns the HTML-shaped input and produces Raven syntax that
continues through normal binding and compilation. The application does not
parse templates or generate render trees at runtime.

Macros can also handle typed arguments, attach to declarations, embed files,
or implement other small domain-specific languages. They are most useful when
the transformation should be visible where it happens and invalid input should
produce a compiler diagnostic.

## Continue exploring

- [Try the live WebAssembly showcase](https://marinasundstrom.github.io/raven/experiments/html-macro/)
  without installing Raven.
- Read the
  [checked-in HTML and Blazor sample](https://github.com/marinasundstrom/raven/tree/main/samples/projects/macro-html-blazor)
  for the application, macro library, and host projects.
- Read [transform code with macros](../lang/features/macros.md) for the concise
  language feature overview.
- Continue to [authoring Raven macros](../macro-authoring.md) when you want to
  build a macro or an embedded DSL.
