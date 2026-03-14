# Macro Reactive (`.rvnproj`)

This sample shows attached and freestanding macros working together in a Raven-authored macro plugin.

The sample shape is:

```raven
func Main() -> () {
    val viewModel = CounterViewModel()

    use subscription = #subscribe(viewModel.Count, (value) => {
        WriteLine(value)
    })

    viewModel.Count = 1
    viewModel.Count = 1
    viewModel.Count = 2
}

class CounterViewModel {
    #[Observable]
    var Count: int = 0
}
```

Current status:

- The macro plugin is written in Raven, not C#.
- `#[Observable]` introduces a companion `CountChanged` signal next to the property.
- The property replacement setter only pushes when the value actually changes.
- `#subscribe(...)` expands structurally from a property access like `viewModel.Count` to `viewModel.CountChanged.Subscribe(...)`.
- The sample is self-contained and does not depend on a base class or external reactive package.

Files:

- `app/MacroReactive.rvnproj`: Raven application using `#[Observable]` and `#subscribe(...)`
- `app/src/main.rvn`: executable sample plus a tiny `Signal<T>` runtime
- `macros/ReactiveMacros.rvnproj`: Raven macro plugin project
- `macros/main.rvn`: plugin implementation of `IRavenMacroPlugin`, `IAttachedDeclarationMacro`, and `IFreestandingExpressionMacro`

Build the macro plugin first:

```bash
dotnet run --framework net10.0 --project ../../../src/Raven.Compiler --property WarningLevel=0 -- macros/ReactiveMacros.rvnproj
```

Then analyze, build, or run the executable sample project:

```bash
dotnet run --framework net10.0 --project ../../../src/Raven.Compiler --property WarningLevel=0 -- app/MacroReactive.rvnproj --no-emit
```

```bash
dotnet run --framework net10.0 --project ../../../src/Raven.Compiler --property WarningLevel=0 -- app/MacroReactive.rvnproj --run
```

Expected output:

```text
1
2
```
