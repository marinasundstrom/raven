# Declarative components over .NET MAUI

> **Status:** exploration. This document records a direction that the Raven
> macro and editor infrastructure can exercise. It is not a Raven language
> feature, a supported MAUI application model, a package commitment, or a
> compatibility contract. The checked-in sample may change or be removed as
> the experiment teaches us more.

## Summary

Raven should explore a declarative component DSL that expands through macros
onto the existing .NET MAUI control model. A Raven-authored component should
remain an ordinary MAUI control with normal CLR properties, `BindableProperty`
descriptors, events, bindings, converters, child collections, and XAML
consumption.

The exploration has two related parts:

1. A compile-time macro layer provides XML-shaped construction, embedded Raven
   expressions, component declarations, diagnostics, and editor services.
2. A small optional runtime helper can schedule rendering and reconcile a
   declarative description with an existing native control subtree, preserving
   identity instead of rebuilding every control.

The runtime layer is justified only for behavior that MAUI does not already
provide. It must compose with MAUI binding and lifecycle facilities rather than
reimplement them.

The main value is the authored programming model: markup and Raven code can be
mixed in one typed render expression. A developer can use local values,
functions, conditionals, patterns, and collection comprehensions exactly where
they decide what the UI contains. The resulting flow should feel reminiscent
of React and Blazor components without copying their runtime object models or
placing either framework between the application and MAUI.

This also makes the experiment useful in two infrastructure dimensions. It
tests whether Raven macros can support a serious mixed-language DSL with
complete editor behavior, and whether Raven's expression-oriented control flow
can drive a retained native UI framework cleanly.

## Motivation

XAML provides a mature declarative surface for MAUI, including bindable
properties, bindings, resources, styles, templates, and converters. Raven can
add a different capability: declarative control markup and ordinary Raven code
can participate in the same composition. Expressions provide values and
children, control flow decides structure, functions handle behavior, and
collection comprehensions project data directly into controls.

```raven
public component! TodoList(
    Items: IReadOnlyList<TodoItem>,
    Select: TodoItem -> unit
) {
    render! {
        <VerticalStackLayout Spacing="8.0">
            {[for item in Items if item.IsVisible =>
                maui! {
                    <TodoRow
                        key={item.Id}
                        Item={item}
                        on:Selected={func () => Select(item)} />
                }]}
        </VerticalStackLayout>
    }
}
```

This is intended to be sleeker than splitting simple component behavior across
XAML, code-behind, converters, and framework plumbing. It is not intended to
make those native facilities inaccessible. A component can use render-time
Raven expressions where direct composition is clearer and opt into MAUI
bindings, converters, resources, styles, or templates where their retained
framework behavior is the better fit.

Expanding that source into a fresh MAUI tree is enough to demonstrate syntax
generation, but it is not a satisfactory update model. Replacing the subtree
can lose focus, selection, scroll position, animation state, platform-native
state, and state owned by nested components. Stable declarative components
therefore need a way to reconcile subsequent renders with existing controls.

MAUI data binding solves a different problem. It synchronizes values between a
source and a target bindable property; it does not reconcile arbitrary
conditional or list-shaped control trees. Rendering and binding should be
complementary mechanisms in this exploration.

## Design constraints

The experiment should preserve these boundaries:

1. **Native control ABI.** A generated component derives from a normal MAUI
   control such as `ContentView`. Its public inputs are CLR properties backed
   by public static `BindableProperty` fields, so C#, other .NET languages, and
   XAML can consume it normally.
2. **MAUI owns native behavior.** Layout, measurement, drawing, focus,
   accessibility, navigation, resources, styles, templates, animation,
   binding, and platform handlers remain MAUI responsibilities.
3. **Macros own the authored DSL.** `component!`, `render!`, and `maui!` remain
   explicit library macros. They do not add fixed Raven grammar or MAUI nodes
   to Raven's syntax or bound trees.
4. **The helper owns only reconciliation.** A runtime helper may schedule a
   component render, retain render metadata, and apply minimal mutations to
   native controls. It must not become a parallel layout or widget framework.
5. **Ordinary Raven remains visible.** Embedded expressions bind through the
   normal semantic model and retain Raven diagnostics, completion, hover,
   navigation, refactoring, and source mapping.
6. **No product promise.** Initial work remains sample-local and experimental.
   It should not enter `Raven.Sdk`, `Raven.Macros`, templates, or the supported
   workload documentation merely because a prototype works.

## Possible authored model

The exact spelling is intentionally unsettled. A useful direction separates
persistent component declarations from the expression that describes the
view:

```raven
public component! CounterView(InitialCount: int = 0) {
    var count = InitialCount

    func Increment() {
        count = count + 1
    }

    render! {
        <VerticalStackLayout Spacing="16.0">
            <Label Text={"Count: ${count}"} />
            <Button Text="Increment" on:Clicked={Increment} />
        </VerticalStackLayout>
    }
}
```

The component macro would lower component state to instance storage and the
render body to a generated render function. This differs from the current
prototype, which places ordinary statements and the final `maui!` expression
inside one generated `BuildContent` method. Persistent state, initialization,
methods, lifecycle, and rendering need an explicit authored separation before
automatic rerendering can be reliable.

The proposal does not yet choose whether persistent state uses ordinary member
syntax, a macro-local contextual form, or another explicit marker. That choice
must be evaluated against Raven parsing, source semantics, initialization
order, macro fragment categories, and editor recovery. It should not be
decided solely by how concise the counter sample becomes.

## Render-time values and native bindings

The DSL should distinguish a rendered value from a MAUI binding.

### Render-time values

Quoted attributes follow the XAML text-value convention and use the target
property's native type-conversion metadata:

```raven
<Label Text="Ready" FontSize="32.0" />
<VerticalStackLayout Spacing="16.0" />
```

A braced attribute is an ordinary Raven expression evaluated during render:

```raven
<Label Text={formatCount(count)} />
<VerticalStackLayout Padding={Thickness(24.0)} />
```

On a later render, the expression is evaluated again. The reconciler compares
the result with the previously rendered value and updates the native property
only when necessary. This is the component-style value flow; it is not
translated into a MAUI `Binding`.

### Native MAUI bindings

The DSL should also expose MAUI binding directly. One possible, non-final
spelling uses a `bind:` attribute prefix:

```raven
<Entry bind:Text="DisplayName" />
```

The quoted shorthand would construct a normal MAUI binding path and use the
target `BindableProperty`'s default binding mode. It must not guess that every
property uses the same mode.

An advanced form should accept an ordinary Raven expression whose value is a
native `BindingBase`:

```raven
let displayNameBinding: BindingBase = createDisplayNameBinding(converter)

render! {
    <Entry bind:Text={displayNameBinding} />
}
```

The macro resolves `Text` to `Entry.TextProperty` and lowers the attribute to
the normal `BindableObject.SetBinding` API. MAUI then owns path observation,
source and target updates, and binding diagnostics.

The experiment should surface all native binding modes rather than inventing
component-specific approximations:

- `Default`, using the mode declared by the target bindable property;
- `OneWay`;
- `TwoWay`;
- `OneTime`; and
- `OneWayToSource`.

Because the advanced value is a normal `BindingBase`, native binding features
remain available through the underlying MAUI object model, including source,
path, converter, converter parameter, string formatting, fallback values, and
target-null values where the selected binding type supports them.

Two-way binding is a required exploration case. A user edit to a native
control must update the source through MAUI's binding engine, and a subsequent
source notification must update the same retained control. The renderer must
not replace that binding with a rendered value or recreate the target control
during an unrelated component render.

The initial DSL need not parse XAML markup-extension strings such as
`"{Binding ...}"`. Those strings depend on XAML-specific parsing and service
contexts. The macro can respect the XAML conceptual model while using an
explicit `bind:` surface and ordinary `BindingBase` objects. Supporting a
compatible shorthand later requires a concrete interoperation benefit and
must still lower to MAUI's binding APIs.

## Bindable component inputs

Typed component inputs continue to generate both sides of the conventional
MAUI property pattern:

```text
public static readonly InitialCountProperty: BindableProperty
public var InitialCount: int
```

Changing an input through either the CLR wrapper or `SetValue` schedules the
component for rendering. XAML bindings therefore reach the same component
update path as direct .NET property assignment.

A component input can itself participate in two-way binding only when changes
originating inside the component are written through its bindable property.
For example, an editable `Value` component must update `ValueProperty`, not
only a disconnected private field. MAUI can then propagate that target change
back to the binding source according to the selected mode.

The proposal leaves the authored declaration of a component input's default
binding mode open. A future form might attach metadata to the input, but the
generated `BindableProperty.Create` call must ultimately carry a native
`BindingMode`, and the resulting CLR/metadata surface must remain conventional.

## Runtime reconciliation layer

The macro can generate a lightweight render description rather than directly
constructing an entire control tree on every update. The description might be
produced through calls resembling:

```raven
builder.OpenControl<VerticalStackLayout>(sequence: 0)
builder.SetValue(VerticalStackLayout.SpacingProperty, 16.0)
builder.OpenControl<Label>(sequence: 1)
builder.SetValue(Label.TextProperty, "Count: ${count}")
builder.CloseControl()
builder.CloseControl()
```

These calls are illustrative runtime output, not proposed user syntax. The
macro should resolve control types, properties, events, and target types at
compile time so the generated code does not depend on repeated string-based
reflection during rendering.

A render description needs only enough information to reconcile MAUI objects:

- control type, structural sequence, and optional explicit key;
- rendered property values;
- native `BindingBase` installations;
- event handlers;
- child/content relationships;
- component parameters;
- authored references such as `ref={name}`; and
- source provenance needed for diagnostics and debugging.

It is not a second layout tree. MAUI controls remain the live retained tree and
perform all platform behavior.

### Identity

Within one parent, an unkeyed child is matched by structural sequence and
control type. A list or conditional may provide an explicit key:

```raven
{[for item in Items =>
    maui! { <TodoRow key={item.Id} Item={item} /> }]}
```

A matching key and compatible type preserve the existing control. A changed
key or incompatible type replaces it. Key uniqueness is scoped to the
containing child region and duplicate keys produce an authored diagnostic.

This policy should preserve native focus, selection, scroll position,
animation state, handler state, and nested component state across ordinary
rerenders. The exact fallback behavior for unkeyed insertions and reorderings
must be documented by the experiment rather than inherited accidentally from
one collection implementation.

### Property and binding ownership

The reconciler owns only entries declared by the current render description.
It must:

- avoid setting an unchanged rendered value;
- remove or clear a previously rendered value when its attribute disappears;
- install a new native binding only when its binding description changes;
- preserve an unchanged native binding across unrelated renders;
- remove a binding when the corresponding `bind:` attribute disappears;
- avoid assigning a rendered value and a binding to the same target property;
- leave undeclared control properties and native state untouched; and
- avoid treating changes produced by MAUI, a handler, animation, or binding as
  permission to overwrite unrelated properties.

The DSL should diagnose duplicate ownership, for example supplying both
`Text={value}` and `bind:Text={binding}` on one element.

### Events and references

Event reconciliation must not accumulate subscriptions. Reusing a control
with the same event entry updates or preserves one effective subscription;
changing or removing the entry detaches the previous handler.

An event dispatched through generated component code invokes the authored
handler and requests a render after synchronous completion. An asynchronous
handler requests the render after its awaited completion. External timers,
subscriptions, and background callbacks require an explicit dispatcher-aware
render request, comparable to a component calling `StateHasChanged` through
its framework context.

`ref={name}` is committed after reconciliation so it refers to the retained or
new native control used by that render. The experiment must define when a
reference becomes unavailable after a conditional removal and prevent stale
references from silently targeting detached controls.

### Component boundaries

A nested Raven component is still a native control, but its owned content is a
reconciliation boundary. When a parent preserves the child component by type
and key, it updates the child's bindable inputs and lets that child's render
host schedule its own work. The parent does not reconcile inside the child's
private subtree.

Ordinary delegates and .NET events are sufficient for the first callback
model. A callback wrapper can request a render for its owning component after
invocation. A new `EventCallback`-like public abstraction should be introduced
only if ordinary delegates cannot preserve ownership, asynchronous completion,
or diagnostics cleanly.

## Scheduling and lifecycle

Each component can own a small render host without inheriting from a
Raven-specific base class. Conceptually, the generated `ContentView` stores a
private helper initialized with its render delegate.

The host should:

1. marshal work through the MAUI dispatcher;
2. coalesce repeated requests before a render begins;
3. prevent recursive rendering;
4. schedule one additional pass when rendering invalidates itself;
5. reconcile and commit the native tree atomically enough that authored refs
   and lifecycle callbacks observe a consistent result; and
6. detach events, bindings, and owned resources when nodes or the component are
   removed.

The experiment must map these operations onto MAUI's real loaded, unloaded,
handler, and dispatcher behavior. It should not create a second application
event loop or claim stronger lifecycle guarantees than MAUI provides.

## Packaging boundary

A possible experimental split is:

- `Raven.Maui.Macros`, containing compile-time macro implementations and
  editor metadata; and
- `Raven.Maui`, containing a small runtime render host, frame representation,
  reconciliation logic, and adapters for native MAUI content collections.

This split is descriptive, not a package commitment. Initially both parts can
remain under `samples/projects/macro-maui`. A runtime dependency does not make
the generated component non-native: its public base type and public property,
event, and XAML surface remain MAUI ABI. The dependency is an implementation
detail in the same sense as a helper used by any custom control.

The runtime becomes too broad if it starts defining replacements for MAUI
controls, layout, resources, styles, navigation, binding, templates, or
platform handlers. Crossing that line should stop the experiment and trigger a
design review.

## Editor requirements

This proposal is also a macro-infrastructure test. A credible prototype needs:

- XML-shaped syntax highlighting for the declarative envelope;
- ordinary Raven parsing and semantic coloring in every braced expression;
- completion and hover for MAUI control types;
- completion and navigation for CLR and bindable properties;
- `on:` completion restricted to native events;
- `bind:` completion restricted to bindable target properties;
- target typing of rendered expressions from the CLR property type;
- target typing of advanced binding expressions as `BindingBase`;
- typed `ref` locals and source navigation;
- diagnostics for duplicate keys, attributes, bindings, events, and refs;
- useful recovery while tags, attributes, expressions, comprehensions, and
  nested macro invocations are incomplete; and
- source mapping from generated render operations back to the authored tag,
  attribute, event, or expression.

The language server continues to consume compiler-owned macro token,
fragment, symbol, and expansion information. It must not parse or bind a
second private copy of the MAUI DSL.

## Investigation slices

The work should proceed as experiments, each small enough to discard:

1. **State and render boundary.** Establish an authored component shape that
   separates persistent state and members from one render description. Verify
   source mapping and editor fragments before adding runtime sophistication.
2. **Identity-preserving scalar update.** Re-render a label value while proving
   that the same native `Label` instance remains mounted.
3. **Conditional and keyed children.** Add, remove, and reorder controls while
   preserving keyed identities and detaching removed events.
4. **Nested components.** Preserve a child's private state while its parent
   rerenders and updates bindable inputs.
5. **Native one-way binding.** Install and preserve a MAUI binding through a
   `bind:` entry, including converter and formatting behavior supplied by
   `BindingBase`.
6. **Native two-way binding.** Edit a native control, observe the source update,
   update the source, and observe the same retained target control update.
   Repeat across an unrelated parent render.
7. **XAML boundary.** Instantiate the generated Raven control from XAML, bind a
   property to it, and verify that native property changes schedule rendering
   without a Raven-specific host application.
8. **Editor stress.** Exercise incomplete tags, binding entries,
   comprehensions, nested components, and rapid edits through repeated
   diagnostics, completion, hover, semantic-token, and expansion requests.

Tests should assert observable native behavior and object identity. They should
not make emitted instruction sequences or a particular private frame layout a
compatibility contract.

## Success and stop conditions

The exploration is promising if a small helper can preserve native control
identity, support conditional and keyed composition, coexist with MAUI's
one-way and two-way binding, and retain complete macro/editor behavior while
the generated public control remains conventional.

It should remain an experiment, or be stopped, if correctness requires a
parallel control hierarchy, replacement binding engine, replacement layout or
lifecycle model, broad reflection on every render, or editor logic outside the
compiler-owned macro APIs.

Only after the investigation slices produce stable evidence should Raven
consider whether this belongs in a separately versioned experimental library.
It should not be promoted merely because the counter sample is attractive.

## Open questions

- What authored syntax best separates persistent component members,
  initialization, and rendering without creating a hidden second Raven
  language inside `component!`?
- Should a rendered property use CLR equality, bindable-property semantics, or
  a property-specific comparer to decide whether an update is necessary?
- Can structural sequence identifiers remain stable through macro expansion
  and ordinary Raven control flow, or should every dynamic region receive an
  explicit generated scope?
- How should `key` behave when the key value changes on an existing component?
- How are asynchronous event exceptions surfaced through the MAUI application
  and Raven source location?
- Which MAUI lifecycle point should mount the initial native tree, especially
  for controls created from XAML whose bindable inputs may be assigned after
  construction?
- Can native binding descriptions be compared safely, or should the renderer
  preserve bindings by authored structural identity unless their frame changes?
- How should resources, styles, templates, and XAML markup extensions be
  surfaced without parsing or reproducing the XAML service-provider model?
- What is the minimum runtime API that generated code needs without exposing
  private reconciliation state as part of a component's public ABI?

## Related material

- [Complete macro system architecture](system-architecture.md)
- [Macro and DSL developer experience](developer-experience.md)
- [Macro ABI](abi.md)
- [.NET MAUI binding mode](https://learn.microsoft.com/dotnet/maui/fundamentals/data-binding/binding-mode)
- [.NET MAUI bindable properties](https://learn.microsoft.com/dotnet/maui/fundamentals/bindable-properties)
- [.NET MAUI binding converters](https://learn.microsoft.com/dotnet/maui/fundamentals/data-binding/converters)
- [`BindableObject.SetBinding`](https://learn.microsoft.com/dotnet/api/microsoft.maui.controls.bindableobject.setbinding)
