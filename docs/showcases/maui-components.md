# Compose native MAUI views with a macro

Raven's experimental `maui!` sample demonstrates functional-style UI
composition over the native .NET MAUI object model. XML-shaped source expands
at compile time into ordinary control construction, property assignments,
child collection calls, and event subscriptions.

```raven
func CreateCounter() -> View {
    let label = Label { Text = "Count: 0" }

    return maui! {
        <VerticalStackLayout Spacing={16.0}>
            {label}
            <Button Text="Increment" />
        </VerticalStackLayout>
    }
}
```

The macro is a library experiment rather than new compiler syntax. Raven
expressions remain ordinary typed fragments, each invocation returns a fresh
native `View`, and a conventional MAUI application hosts the result. No Blazor
runtime or parallel component object model is involved.

The initial scope covers controls, CLR properties, child composition, embedded
expressions, commands, and .NET events. Bindings, converters, resources,
styles, templates, and hot reload remain future work.

See the
[checked-in MAUI macro sample](https://github.com/marinasundstrom/raven/tree/main/samples/projects/macro-maui)
for the Raven macro library, `CounterView`, headless verification executable,
and Android/Mac Catalyst hosts.
