# Build HTML components with a macro

This experimental sample shows a Raven macro turning an HTML-shaped template
into an ordinary Blazor component. The template stays beside the component's
state and behavior in one `.rvn` file.

```raven
#[Component]
public class Counter {
    var count = 0

    func increment() {
        count = count + 1
    }

    func Render() -> RenderFragment =>
        Html! {
            <button onClick={increment}>
                Count: {count}
            </button>
        }
}
```

## What the sample shows

- `Html!` is an ordinary token-tree macro supplied by the sample library, not
  HTML syntax built into the Raven compiler.
- The macro reads the embedded template at compile time and expands it into a
  normal Blazor `RenderFragment`.
- Raven expressions such as `increment` and `count` remain visible inside the
  template and participate in compiler and editor tooling.
- `#[Component]` is an attached macro that connects the authored class to
  Blazor's existing `ComponentBase` model.
- The result still uses ordinary Blazor components, parameters, callbacks,
  rendering, CSS isolation, and .NET interop.

The prototype is intentionally labeled experimental. It demonstrates that a
library can add a compact domain-specific language without making that DSL a
privileged part of Raven.

## How macros fit

A Raven macro is a compile-time transformation that is explicitly invoked in
source. Here, `Html!` owns the HTML-shaped input and produces Raven syntax that
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
