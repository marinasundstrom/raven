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

When run with `--run`, the sample starts Avalonia's desktop lifetime and shows a native window. The DSL builds the control tree first, then `RavenAvaloniaApp.OnFrameworkInitializationCompleted` installs it as the main window and delegates to `base.OnFrameworkInitializationCompleted()`.

## Raven interop notes

While moving this sample to Avalonia, a few issues surfaced that are worth investigating separately:

- Resolved: direct Raven method signatures over Avalonia controls previously hit a `TypeLoadException` for `Avalonia.StyledElement`. Raven now prefers NuGet `lib/` runtime assemblies when resolving emitted signatures from `ref/` metadata assemblies.
- Resolved: direct inherited member lookup/conversion on Avalonia controls was incomplete in a few places, such as `Button` to `Interactive` and inherited `RaiseEvent`. Raven now resolves package metadata base types through compilation-level metadata references when a module-local reference walk misses.
- Resolved: emitting calls to methods on constructed generic package types, such as `StackPanel.Children.Add(Control)`, previously failed runtime `MethodInfo` resolution. Raven now resolves those methods against the constructed declaring type.
- Resolved: the Raven CLI now copies native NuGet runtime assets for `--run` / `--publish`, so Desktop Avalonia can find native dependencies such as `libSkiaSharp.dylib` and `libAvaloniaNative.dylib`.
- Resolved: Raven now supports class-only `base` expressions, which lets the Avalonia app subclass call `base.OnFrameworkInitializationCompleted()`.
- Resolved: some analyzer suggestions were false positives. Constructor parameters forwarded to `base(...)` now count as used, and instance methods that invoke captured callable members are no longer suggested as static.
- Generic unused/private member analyzers do not currently see DSL-lowered builder entry points such as `BuildExpression` and `BuildFinalResult`. Those are intentionally not special-cased by builder method name.

## Build and run

From this folder:

```bash
dotnet run --project ../../../src/Raven.Compiler --framework net10.0 --property WarningLevel=0 -- MockUiBuilderDsl.rvnproj --run
```
