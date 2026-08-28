# Declarative MAUI components

This experimental sample recreates a component-like declarative authoring
experience on top of the existing .NET MAUI APIs. Both macros run at compile
time. Their output is an ordinary MAUI control tree and an ordinary public
`ContentView` subclass; there is no Raven renderer, virtual control tree, or
runtime component framework between the application and MAUI.

The current implementation intentionally remains smaller than the possible
component model. The
[declarative MAUI component proposal](../../../docs/lang/proposals/macros/maui-declarative-components.md)
records an exploratory direction for persistent component state,
identity-preserving reconciliation, and native one-way and two-way MAUI
binding. It is a macro/compiler infrastructure test, not a product roadmap.

```raven
public component! CounterView(InitialCount: int = 0) {
    var count = InitialCount

    maui! {
        <VerticalStackLayout Spacing="16.0">
            <Label ref={countLabel} Text={"Count: ${count}"} />
            {IncrementButton {
                Increment = func () => {
                    count = count + 1
                    countLabel.Text = "Count: ${count}"
                }
            }}
            {[for caption in ["Declarative Raven", "Native MAUI"] =>
                Label { Text = caption }]}
        </VerticalStackLayout>
    }
}
```

The two macros have separate responsibilities:

- `component!` generates a public `ContentView` subclass. Typed inputs become
  public CLR properties backed by public static MAUI `BindableProperty`
  identifiers, including native property-changed callbacks.
- `maui!` expands XML-shaped source into direct control construction, CLR
  property assignments, native `Children.Add` calls, and .NET event
  subscriptions.

The list comprehension is ordinary Raven. Its resulting controls are projected
directly into the layout's native child collection. This is the important
difference from XAML: control flow, list comprehensions, pattern matching, local
functions, and other Raven expressions can participate in view construction
without introducing a second UI object model.

## Attribute conventions

Quoted attributes follow XAML's convenient value syntax:

```raven
<Label Text="test" FontSize="32.0" />
<VerticalStackLayout Spacing="16.0" />
```

The macro resolves the target CLR property and uses its
`TypeConverterAttribute`, or the property's normal .NET type converter, to
produce the target value. A braced value is an ordinary Raven expression and is
not converted from text:

```raven
<Label Text={formatCount(count)} />
<VerticalStackLayout Padding={Thickness(24.0)} />
```

`ref={name}` introduces a typed Raven local for the constructed control, and
`on:Clicked={handler}` subscribes the native MAUI event. Nested Raven-authored
components are normal controls and can be inserted with a Raven expression.

## Native MAUI interoperability

The generated component can be created by any .NET application and can also be
consumed from XAML:

```xml
<ContentPage
    xmlns="http://schemas.microsoft.com/dotnet/2021/maui"
    xmlns:raven="clr-namespace:MauiCounter;assembly=MauiCounter">
    <raven:CounterView InitialCount="2" />
</ContentPage>
```

The host project compiles this XAML against the Raven-generated assembly. That
keeps the public boundary at the MAUI control ABI: `ContentView`,
`BindableProperty`, CLR properties, events, and MAUI child collections.

## Editor experience

The `maui!` body is projected as XML while each braced region is registered as
a Raven expression fragment. The macro also provides MAUI control and
property/event completion, symbol resolution, and typed `ref` locals. Editors
therefore retain XML-shaped syntax highlighting without treating embedded Raven
code as XML text.

## Projects

```text
macro-maui/
├── macros/   Raven compiler plugins for `component!` and `maui!`
├── app/      Raven component library
├── verify/   headless executable testing the generated native tree
└── host/     MAUI application consuming the component from XAML
```

## Build and verify

This sample requires Raven 0.1.4 or later. When working in this repository, the
following commands build and use the repository compiler explicitly so local
compiler changes can be tested without changing the installed SDK:

```bash
dotnet build src/Raven.Compiler/Raven.Compiler.csproj --property WarningLevel=0

dotnet run \
  --project samples/projects/macro-maui/verify/MauiCounter.Verify.rvnproj \
  --property WarningLevel=0 \
  --property:RavenCompilerHost="$PWD/src/Raven.Compiler/bin/Debug/net11.0/rvnc.dll"
```

Build the Mac Catalyst XAML host with a matching Xcode installation:

```bash
dotnet build samples/projects/macro-maui/host/MauiCounter.Host.csproj \
  --framework net10.0-maccatalyst \
  --property:RavenCompilerHost="$PWD/src/Raven.Compiler/bin/Debug/net11.0/rvnc.dll"
```

For compile-only validation with an older installed Xcode, add
`--property:ValidateXcodeVersion=false --property:MtouchLink=SdkOnly`. Those
overrides are not publishing settings.

## Current scope

The prototype deliberately generates controls eagerly and rebuilds component
content when an input bindable property changes. It does not yet provide keyed
reconciliation, collection-change tracking, bindings, resources, styles, data
templates, or hot reload. Those features should continue to use native MAUI
facilities where they fit; helpers should be introduced only for behavior that
cannot be expressed cleanly through the existing MAUI ABI.
