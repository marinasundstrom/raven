# Macro Observable (`.rvnproj`)

This sample shows a Raven-authored macro plugin that models the `ObservableBase` + `#[Observable]` direction.

The sample shape is:

```raven
func Main() -> unit {
    val viewModel = MyViewModel()
    viewModel.PropertyChanged += (sender: object?, args: PropertyChangedEventArgs) => {
        WriteLine(args.PropertyName ?? "")
    }
    viewModel.Title = "Hello from Raven"
    WriteLine(viewModel.Title)
}

class MyViewModel: ObservableBase {
    #[Observable]
    var Title: string = ""
}
```

Current status:

- The macro plugin is written in Raven, not C#.
- `#[Observable]` is resolved from a `RavenMacro` assembly reference.
- The plugin returns both an introduced backing field and a replacement property declaration through `MacroExpansionResult`.
- The expanded setter now calls `RaisePropertyChanged(...)` through the replacement declaration.

Files:

- `app/MacroObservable.rvnproj`: Raven application using `#[Observable]`
- `app/src/main.rvn`: `ObservableBase` plus `MyViewModel`
- `macros/ObservableMacros.rvnproj`: Raven macro plugin project
- `macros/main.rvn`: plugin implementation of `IRavenMacroPlugin` / `IAttachedDeclarationMacro`

Build the macro plugin first:

```bash
dotnet run --framework net10.0 --project ../../../src/Raven.Compiler --property WarningLevel=0 -- macros/ObservableMacros.rvnproj
```

Then analyze, build, or run the executable sample project:

```bash
dotnet run --framework net10.0 --project ../../../src/Raven.Compiler --property WarningLevel=0 -- app/MacroObservable.rvnproj --no-emit
```

```bash
dotnet run --framework net10.0 --project ../../../src/Raven.Compiler --property WarningLevel=0 -- app/MacroObservable.rvnproj
```

Expected output:

```text
Title
Hello from Raven
```
