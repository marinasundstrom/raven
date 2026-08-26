# Declarative native MAUI components

Raven's experimental `component!` and `maui!` macros recreate a
component-oriented, declarative authoring style over the existing .NET MAUI
object model. They run at compile time and produce an ordinary public
`ContentView` subclass containing ordinary native controls. There is no Raven
renderer, virtual control tree, or Blazor runtime between the component and
MAUI.

```raven
public component! CounterView(InitialCount: int = 0) {
    var count = InitialCount

    maui! {
        <VerticalStackLayout Spacing="16.0">
            <Label ref={countLabel} Text={"Count: ${count}"} />
            <Button
                Text="Increment"
                on:Clicked={func (_, _) => {
                    count = count + 1
                    countLabel.Text = "Count: ${count}"
                }} />
            {[for caption in ["Declarative Raven", "Native MAUI"] =>
                Label { Text = caption }]}
        </VerticalStackLayout>
    }
}
```

The macros divide the work along the native MAUI boundary:

- `component!` generates the `ContentView` subclass. Each typed input becomes
  a public CLR property backed by a public static MAUI `BindableProperty`
  identifier and a native property-changed callback.
- `maui!` expands XML-shaped source into direct control construction, property
  assignments, native child-collection calls, and .NET event subscriptions.

The result is a normal MAUI control. Another .NET application can construct it
directly, and a conventional XAML page can consume the same Raven-authored
component:

```xml
<ContentPage
    xmlns="http://schemas.microsoft.com/dotnet/2021/maui"
    xmlns:raven="clr-namespace:MauiCounter;assembly=MauiCounter">
    <raven:CounterView InitialCount="2" />
</ContentPage>
```

## XAML conventions with Raven expressions

Quoted attributes retain XAML's convenient text-value convention. The macro
uses the target property's `TypeConverterAttribute`, or its ordinary .NET type
converter, to produce the property value:

```raven
<Label Text="test" FontSize="32.0" />
<VerticalStackLayout Spacing="16.0" />
```

Braces switch to an ordinary typed Raven expression and do not perform a text
conversion:

```raven
<Label Text={formatCount(count)} />
<VerticalStackLayout Padding={Thickness(24.0)} />
```

Expressions can also produce children. The collection comprehension in the
first example projects controls directly into MAUI's native child collection.
The same boundary allows conditionals, pattern matching, local functions, and
nested Raven-authored controls to participate in view construction without a
second UI object model.

`ref={name}` introduces a typed Raven local for a constructed control, while
`on:Clicked={handler}` subscribes its native .NET event.

## Editor and current runtime model

The `maui!` body is projected as XML for syntax highlighting. Braced regions
are registered as Raven expression fragments, and the macro supplies MAUI
control, property, and event completion, symbol resolution, and typed `ref`
locals. The declarative surface therefore retains normal Raven tooling inside
embedded code.

The prototype constructs controls eagerly and rebuilds component content when
an input bindable property changes. It does not yet provide keyed
reconciliation, collection-change tracking, bindings, resources, styles, data
templates, or hot reload. Those features should continue to use native MAUI
facilities where they fit; helpers should be introduced only where the native
ABI cannot express the desired component behavior cleanly.

See the
[checked-in MAUI component sample](https://github.com/marinasundstrom/raven/tree/main/samples/projects/macro-maui)
for the macro library, Raven-authored controls, headless ABI verification, and
the XAML host.
