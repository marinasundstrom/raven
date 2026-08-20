# Macro Observable (`.rvnproj`)

This sample shows a Raven-authored macro plugin that models the `ObservableBase` + `#[Observable]` direction.

The sample shape is:

```raven
func Main() -> unit {
    let viewModel = MyViewModel()
    viewModel.PropertyChanged += (sender: object?, args: PropertyChangedEventArgs) => {
        WriteLine(args.PropertyName ?? "")
    }
    viewModel.Title = "Hello from Raven"
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
- `#[Observable]` is resolved from an ordinary project reference to an
  assembly marked with `RavenCompilerPlugin`.
- The plugin transforms `context.CurrentDeclaration`, while `context.TargetDeclaration` remains available as the original authored syntax.
- The plugin builds its expansion with the syntax API instead of parsing a generated source string.
- The plugin returns both an introduced backing field and a replacement property declaration through `MacroExpansionResult`.
- The original property initializer is transferred onto the generated backing storage.
- The expanded setter guards against duplicate assignments before calling `RaisePropertyChanged(...)`.
- For this sample, `#[Observable]` only supports mutable storage properties (`var Name: T = ...`) and reports a macro diagnostic for accessor-bodied or expression-bodied properties.

Files:

- `app/MacroObservable.rvnproj`: Raven application using `#[Observable]`
- `app/src/Program.rvn`: `ObservableBase` plus `MyViewModel`
- `macros/ObservableMacros.rvnproj`: Raven macro plugin project
- `macros/ObservableMacro.rvn`: directly exported `IMacroDefinition` implementation

Build the macro plugin first:

```bash
dotnet build macros/ObservableMacros.rvnproj --property WarningLevel=0
```

Then analyze, build, or run the executable sample project:

```bash
dotnet run --framework net10.0 --project ../../../src/Raven.Compiler --property WarningLevel=0 -- app/MacroObservable.rvnproj --no-emit
```

```bash
dotnet build app/MacroObservable.rvnproj --property WarningLevel=0
```

Expected output:

```text
Title
Hello from Raven
```
