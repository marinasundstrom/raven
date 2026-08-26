# MAUI view macro

This experimental sample composes native .NET MAUI controls from Raven. The
`maui!` macro owns an XML-shaped region and expands it into ordinary Raven
object construction, property assignment, child collection calls, and .NET
event subscriptions. It does not introduce a second UI runtime or place the
Blazor component model over MAUI.

```raven
public static class CounterView {
    static func Create(initialCount: int = 0) -> View {
        var count = initialCount
        let countLabel = Label {
            Text = "Count: ${count}"
            FontSize = 32.0
        }
        let increment = Command<object?>(func (_) => {
            count = count + 1
            countLabel.Text = "Count: ${count}"
        })

        return maui! {
            <VerticalStackLayout
                Padding={Thickness(24.0)}
                Spacing={16.0}>
                {countLabel}
                <Button Text="Increment" Command={increment} />
            </VerticalStackLayout>
        }
    }
}
```

The function creates a fresh `VerticalStackLayout` for each call. The label is
ordinary Raven-authored setup code embedded as a child, while the button is
constructed by the macro. Executing the native MAUI `Command` updates the
captured count and label directly.

## What the prototype supports

- a single root MAUI view;
- simple or fully qualified MAUI control type names;
- quoted strings for string properties;
- `{ RavenExpression }` property values and children;
- multiple children for MAUI layouts through `Children.Add`;
- one child for `ContentPage`, `ContentView`, `ScrollView`, and `Border`;
- `on:Event={handler}` for ordinary .NET event subscription; and
- XML projection plus Raven expression fragments for editor tooling.

The first slice intentionally does not implement bindings, converters, XAML
markup extensions, resources, styles, data templates, or hot reload. Property
names are CLR names, so the generated code remains close to the native MAUI
object model.

## Projects

```text
macro-maui/
├── macros/   Raven-authored `maui!` compiler plugin
├── app/      Raven `CounterView` library using native MAUI controls
├── verify/   headless Raven executable that exercises the generated tree
└── host/     Android and Mac Catalyst MAUI application shells
```

The verifier checks the initial label and button, executes the button command,
and confirms that the label changes from `Count: 2` to `Count: 3`.

## Build and verify

This sample depends on inherited .NET member lookup fixed after Raven 0.1.2.
Until 0.1.3 is available, build it from the repository root with the current
compiler host:

```bash
dotnet build src/Raven.Compiler/Raven.Compiler.csproj --property WarningLevel=0

dotnet run \
  --project samples/projects/macro-maui/verify/MauiCounter.Verify.rvnproj \
  --property WarningLevel=0 \
  --property:RavenCompilerHost="$PWD/src/Raven.Compiler/bin/Debug/net11.0/rvnc.dll"
```

With Raven 0.1.3 or later, the `RavenCompilerHost` override should not be
needed.

Build the Android host when an Android SDK is installed:

```bash
dotnet build samples/projects/macro-maui/host/MauiCounter.Host.csproj \
  --framework net10.0-android \
  --property:RavenCompilerHost="$PWD/src/Raven.Compiler/bin/Debug/net11.0/rvnc.dll"
```

Build the Mac Catalyst host with a matching Xcode installation:

```bash
dotnet build samples/projects/macro-maui/host/MauiCounter.Host.csproj \
  --framework net10.0-maccatalyst \
  --property:RavenCompilerHost="$PWD/src/Raven.Compiler/bin/Debug/net11.0/rvnc.dll"
```

For local compile validation when the installed Xcode is older than the MAUI
workload's recommended version, the host can be built with framework-only
linking:

```bash
dotnet build samples/projects/macro-maui/host/MauiCounter.Host.csproj \
  --framework net10.0-maccatalyst \
  --property:ValidateXcodeVersion=false \
  --property:MtouchLink=SdkOnly \
  --property:RavenCompilerHost="$PWD/src/Raven.Compiler/bin/Debug/net11.0/rvnc.dll"
```

That override is for local validation, not a publishing configuration.
