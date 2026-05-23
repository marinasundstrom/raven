## Proposal: Result-Builder Blocks (DSLs)

> Status: **Implemented and closed**.
>
> The implemented language behavior is now specified in
> [`docs/lang/spec/language-specification.md`](../../spec/language-specification.md#trailing-closure-calls).
> Further DSL work should be tracked in
> [`extensions.md`](extensions.md).

Raven supports Swift-like **trailing blocks** as invocation syntax. A trailing block supplies a final zero-argument closure argument to a function, method, delegate invocation, or constructor. Result-builder blocks build on that call form to enable declarative DSLs that combine expressions, control flow, and local bindings into a single structured value.

Result-builder blocks are inspired by Swift’s *result builders* and are designed to be **type-directed, extensible, and user-definable**.

---

## Motivation

Result-builder blocks allow users to write structured, declarative code using normal constructs (`if`, `for`, local variables) while deferring structural interpretation to a *result builder* and, when needed, component configuration to a receiver builder.

Typical use cases include:

* UI composition (SwiftUI-style DSLs)
* Declarative configuration
* Query builders
* Validation pipelines
* Structured code generation
* Declarative wrappers over existing object models (WPF, Xamarin.Forms, MAUI, Avalonia, etc.)

---

## Overview

A result-builder block is a trailing closure block that is **bound and lowered according to a result builder type**, rather than as a normal closure body, object initializer, or collection initializer.

The result builder type defines how expressions and control-flow constructs inside the block are combined into a final value. A combined `[Builder<TBuilder>, Receiver<TReceiver>]` parameter also has a receiver builder: an implicit receiver object whose members are in scope and which can produce the current component's sub-result from the built child content.

---

## Declaring a result builder

A result builder is declared by defining a type that exposes the appropriate static builder methods. The type itself does not require special syntax beyond being referenced by a `[Builder<T>]` attribute.

```raven
class ViewBuilder {
    static BuildBlock(nodes: ViewNode[]) -> ViewNode
    static BuildExpression(view: View) -> ViewNode
    static BuildEither(first: ViewNode) -> ViewNode
    static BuildEither(second: ViewNode) -> ViewNode
    static BuildArray(nodes: ViewNode[]) -> ViewNode
    static BuildFinalResult(component: ViewNode) -> View
}
```

Result-builder methods are discovered by name and signature during binding. Missing methods indicate unsupported language constructs.

---

## Using a result builder

A result builder is activated by annotating a function parameter or property with `[Builder<T>]`:

```raven
func View([Builder<ViewBuilder>] content: () -> ViewNode) -> View {
    ...
}
```

When a trailing `{ ... }` block is selected as the argument for a parameter annotated with `[Builder<T>]`, the block is treated as a **result-builder block** and bound using the specified result builder. Without that annotation, the same syntax is an ordinary zero-argument trailing closure.

Current implementation supports expression components, `return` components, local declarations, assignments, `if`/`else` when the result builder supplies `BuildOptional` or `BuildEither`, and `for` loops when the result builder supplies `BuildArray`.

The canonical `BuilderAttribute<T>` and shared DSL support types should live in `Raven.Core` once the feature graduates from prototype status. Samples may define a local `BuilderAttribute<T>` while bootstrapping the compiler feature, but framework adapters should ultimately depend on the Raven.Core definitions rather than redeclaring the attribute.

---

## Result-builder block semantics

A result-builder block:

* Introduces a normal lexical scope
* Allows local variable declarations
* Allows control-flow statements (`if`, `for`)
* Collects *component expressions* that are rewritten using the result builder

Only expressions that participate in the result builder are rewritten; other statements remain normal statements.

Result-builder blocks are never inferred implicitly; they are only used when a result builder is explicitly specified.

---

## Node construction within builder blocks

Builder blocks commonly use “node constructor calls” to declare components:

```raven
StackPanel(orientation: .Horizontal, spacing: 8.0) { ... }
Label("Hello")
```

### Arguments and defaults

Node APIs are ordinary Raven functions, methods, or constructors. Libraries
should expose framework configuration through normal parameters, usually with
default values so callers can omit the parameters they do not care about:

```raven
func StackPanel(
    orientation: Orientation = .Vertical,
    spacing: double = 0.0,
    [Builder<UiBuilder>] content: (() -> UiNode)? = null
) -> UiNode {
    ...
}

StackPanel {
    Label("Default")
}

StackPanel(spacing: 8.0) {
    Label("Spaced")
}

StackPanel(orientation: .Horizontal, spacing: 8.0) {
    Label("Horizontal")
}
```

The trailing block supplies the final function-typed parameter. If the final
parameter has a nullable function type and a default value of `null`, omitting
the block means no content was supplied; writing `{ }` means an empty content
closure was supplied and is still lowered normally.

### Trailing builder blocks

If an expression inside a builder block is immediately followed by `{ ... }`, the trailing block is first bound as the final closure argument of that call. For UI-style node construction APIs, that trailing closure convention is how children/content are supplied; adapter-specific child-slot interpretation happens after the builder has selected the callable target.

The same syntax can also supply an ordinary handler/action closure when the selected callable's final parameter is not annotated with `[Builder<T>]`:

```raven
Button("OK") {
    Submit()
}
```

In that shape the block is just the button's final `() -> ()` argument, similar to Swift and Kotlin APIs that place the primary action in a trailing closure. It is not interpreted as child content unless the selected parameter is explicitly builder-annotated.

---

## Framework reuse and zero-modification integration

Builder blocks can target existing frameworks **without modifying framework code**.

The compiler does not need built-in knowledge of WPF, Xamarin.Forms, MAUI, Avalonia, or any other UI stack. It only binds the Swift-like trailing block and lowers `[Builder<T>]` bodies into ordinary builder calls. Framework integration belongs in an **adapter layer**, responsible for:

* Resolving “node kinds” to framework types (if needed)
* Creating framework objects (factories / `Activator`)
* Designing wrapper APIs whose parameters map to framework properties and events
* Attaching children via framework-specific child-slot rules
* Reconciling updates incrementally across renders

This makes the first implementation path simple: mock the shape with a small Raven runtime model (`Window`, `StackPanel`, `Button`, `Label`, `Fragment`) while the compiler work lands. A real adapter can later replace those mock controls with descriptors over framework types without changing the language syntax.

### Reusing framework structure

Raven code may refer directly to framework types:

```raven
import Microsoft.Maui.Controls.*

StackPanel(orientation: .Horizontal) {
    Label("Hello")
}
```

This avoids compiler knowledge of every framework type, while still allowing
libraries to expose wrapper functions whose parameters map cleanly to framework
properties and events.

---

## Adapter descriptor model

For each framework type used in the DSL, the adapter builds and caches a descriptor:

* **Factory**: how to create an instance (`Activator` by default, overridable)
* **Parameters**: mapping of wrapper parameters → framework property/event setters
* **Events**: mapping of action parameters → subscription/unsubscription logic
* **Child slot**: how children are attached:

  * None
  * Single child (`Content`, WPF `ContentControl.Content`, Xamarin.Forms/Avalonia content controls)
  * Collection (`Children`, WPF `Panel.Children`, Xamarin.Forms/Avalonia layout children)
  * Specialized containers (e.g. WPF/Avalonia `Grid` row/column metadata)
* **Conversions**: value conversion rules (enums, thickness, color, etc.)

This descriptor is owned by the DSL runtime/adapter and does not require framework modifications.

### Adapter implementation outline

A practical adapter can be implemented in layers:

1. **Discovery**: build descriptors for framework types from reflection and optional hand-written overrides.
2. **Construction**: choose the constructor/factory from named arguments, then create the object.
3. **Configuration**: apply wrapper parameter values as properties, attached properties, bindable properties, routed events, or normal events.
4. **Children**: flatten builder fragments and attach children through the descriptor's child slot.
5. **Identity**: consume reserved metadata such as `key:` before property application.
6. **Reconciliation**: compare the new node description with the existing object graph and update in place when possible.

Framework-specific overrides are expected. For example, WPF needs attached properties such as `Grid.Row`, Xamarin.Forms/MAUI need `BindableProperty` and binding-context support, and Avalonia has styled/direct properties plus routed events. Those differences should live in adapter descriptors, not in compiler semantics.

---

## Identity and incremental updates

UI/stateful DSLs should define stable identity rules to avoid full rebuilds on every update.

Recommended matching order:

1. Explicit key (if provided by DSL surface)
2. Structural path + position in parent
3. Fallback replacement (dispose + recreate)

### Reserved DSL metadata

Certain parameters may be reserved for DSL metadata and are **not applied as framework properties**:

* `key:` stable identity for reconciliation

Example:

```raven
Label(item.Name, key: item.Id)
```

---

## Binding and lowering model

During binding, the compiler:

1. Determines whether a `{ ... }` block is a builder block based on the expected target and `[Builder<T>]`.
2. Binds the block into a `BoundDslBody` containing:

   * normal bound statements
   * component-producing expressions
   * associated builder descriptor
3. Lowers the DSL body into calls to builder static methods.

### Node construction lowering (conceptual)

Inside a builder block, a node construction expression:

```raven
StackPanel(orientation: .Horizontal, spacing: 8.0)
```

is lowered into a **node specification** form:

* kind: stack-panel wrapper over the framework layout control
* property assignments from parameters: `Orientation = Horizontal`, `Spacing = 8`
* metadata: optional `key`
* children: empty unless a trailing block is present

Trailing blocks attach children/content:

```raven
StackPanel(orientation: .Horizontal) {
    Label("A")
    Label("B")
}
```

becomes “node spec with children specs”.

---

## Runtime model for UI/stateful DSLs

Builder output should be treated as a **declarative description** that drives a runtime state machine over an object graph.

A typical lifecycle contract is:

* `CreateNode(type)` creates a framework node
* `SetProperty(node, name, value)` updates scalar state
* `AttachChild(parent, child, slot/index)` establishes hierarchy/ordering
* `ReconcileChildren(parent, children)` diffs and updates child collections
* `DisposeNode(node)` tears down removed nodes

This model allows DSL blocks to act as deterministic state transitions rather than one-shot object construction.

---

## Samples

### Sample 1: Basic MAUI layout using framework types

```raven
import Microsoft.Maui.Controls.*

func AppRoot([Builder<MauiBuilder>] content: () -> ViewNode) -> View {
    return MauiRuntime.Render(content)
}

val view =
AppRoot {
    StackPanel(orientation: .Vertical, spacing: 12.0) {
        Label("Hello from Raven")
        Button("Click me")
    }
}
```

Notes:

* Uses MAUI types directly (no wrapper types required).
* Wrapper parameters map to framework properties.
* Trailing block becomes children.

---

### Sample 2: Conditional content

```raven
import Microsoft.Maui.Controls.*

AppRoot {
    StackPanel {
        Label("Title")

        if isLoggedIn {
            Label("Welcome")
        } else {
            Button("Sign in")
        }
    }
}
```

Requires builder support for `BuildEither`.

---

### Sample 3: Looping with stable keys

```raven
import Microsoft.Maui.Controls.*

AppRoot {
    StackPanel {
        for item in items {
            Label(item.Name, key: item.Id)
        }
    }
}
```

Notes:

* `key:` is DSL metadata; used for reconciliation.
* Requires builder support for `BuildArray`.

---

### Sample 4: Events (zero-modification)

```raven
import Microsoft.Maui.Controls.*

AppRoot {
    Button("Save", onClick: () => Save())

    Button("Cancel") {
        Cancel()
    }
}
```

Adapter responsibilities:

* Map `onClick` to the framework button event
* Subscribe/unsubscribe incrementally on updates
* Store subscription state in node instance state bag
* Map an adapter-recognized final action closure to the framework's primary event when the framework surface exposes that convention

---

### Sample 5: Single-content containers (child slot rules)

```raven
import Microsoft.Maui.Controls.*

AppRoot {
    ScrollView {
        StackPanel {
            Label("Scrollable content")
        }
    }
}
```

Adapter child-slot rules (conceptual):

* `ScrollView` uses single child slot: `Content`
* `StackPanel` uses collection slot: `Children`

---

### Sample 6: Incremental update behavior (conceptual)

Render 1:

```raven
StackPanel {
    Label("A", key: 1)
    Label("B", key: 2)
}
```

Render 2:

```raven
StackPanel {
    Label("B", key: 2)
    Label("C", key: 3)
    Label("A", key: 1)
}
```

Reconciler operations:

* Move Key=2 to index 0
* Create Key=3 at index 1
* Move Key=1 to index 2
* Dispose removed nodes (none here)

---

## Diagnostics

The compiler/runtime produces targeted diagnostics when:

* A builder block uses an unsupported construct
* Required builder methods are missing
* Positional arguments are used in node construction inside builder blocks
* A named argument does not match a settable property or bindable event
* A component expression cannot be converted by the builder
* Children are provided for a node that has no child slot per adapter rules

---

## Closed follow-up

This proposal is closed with the core feature implemented:

* `expr { ... }` is trailing closure invocation syntax.
* `expr(args) { ... }` supplies one final zero-argument closure argument, while
  earlier optional parameters may be omitted through normal default-argument
  rules.
* `[Builder<T>]` activates builder-block binding for the selected closure parameter.
* Object initialization remains `Type with { ... }`.

Possible extensions, including computation-expression-style builders and richer adapter conventions, belong in [`extensions.md`](extensions.md).
