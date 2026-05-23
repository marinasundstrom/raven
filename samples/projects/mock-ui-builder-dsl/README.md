# Avalonia UI Builder DSL (.rvnproj)

This sample demonstrates builder-backed trailing blocks with receiver builders over a small Avalonia control set.

The important shape is `[Builder<UiBuilder>, Receiver<WindowBuilder>]`: `UiBuilder` is the result builder that lowers child `UiNode` expressions through `BuildExpression`, `BuildBlock`, `BuildOptional`, and `BuildArray`, while `WindowBuilder` is the receiver builder that exposes configuration members like `Title` inside the same block and produces the Avalonia `Window` in `BuildFinalResult`.

```raven
val window = Window {
    Title = "Tasks"

    StackPanel {
        Spacing = 8.0
        Label("Inbox")

        Button("Add") {
            Console.WriteLine("Clicked Add")
        }
    }
}
```

The sample intentionally stays headless: it builds an Avalonia `Window`, dumps the control tree, and wires button actions through Avalonia's `Click` event.

## Raven interop notes

While moving this sample to Avalonia, a few issues surfaced that are worth investigating separately:

- Direct Raven method signatures over Avalonia controls hit a `TypeLoadException` for `Avalonia.StyledElement`; the sample now keeps Avalonia controls behind `object`-backed wrapper types.
- Direct inherited member lookup/conversion on Avalonia controls was incomplete in a few places, such as `Button` to `Interactive` and inherited `RaiseEvent`.
- The Raven CLI currently copies managed NuGet assemblies for `--run`, but not all native runtime assets. Desktop Avalonia on macOS needed `libSkiaSharp.dylib` and `libAvaloniaNative.dylib`, which is why this sample uses `Avalonia.Headless`.
- Reflection over Avalonia styled properties did not produce useful sample dump values, so the Raven wrappers retain the configured display values while still creating real Avalonia controls.
- Some analyzer suggestions look suspicious in this sample, such as constructor parameters reported as unused when forwarded to `base(...)`, and an instance click handler reported as static even though it calls captured callback state.

## Build and run

From this folder:

```bash
dotnet run --project ../../../src/Raven.Compiler --framework net10.0 --property WarningLevel=0 -- MockUiBuilderDsl.rvnproj --run
```
