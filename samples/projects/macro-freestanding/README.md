# Macro Freestanding (`.rvnproj`)

This sample shows a Raven-authored freestanding expression macro plugin.

The sample shape is:

```raven
func Main() -> unit {
    val answer = #add(20, Right: 22)
    WriteLine(answer)
}
```

Current status:

- The macro plugin is written in Raven, not C#.
- `#add(...)` is resolved from a `RavenMacro` project reference.
- The plugin expands structurally with the syntax API instead of parsing a generated expression string.
- The expansion reuses the original argument expression syntax when it builds the final `left + right` expression.
- The sample uses a named argument to show the current freestanding macro argument shape.

Files:

- `app/MacroFreestanding.rvnproj`: Raven application using `#add(...)`
- `app/src/main.rvn`: executable sample
- `macros/FreestandingMacros.rvnproj`: Raven macro plugin project
- `macros/main.rvn`: plugin implementation of `IRavenMacroPlugin` / `IFreestandingExpressionMacro`

Build the macro plugin first:

```bash
dotnet run --framework net10.0 --project ../../../src/Raven.Compiler --property WarningLevel=0 -- macros/FreestandingMacros.rvnproj
```

Then analyze, build, or run the executable sample project:

```bash
dotnet run --framework net10.0 --project ../../../src/Raven.Compiler --property WarningLevel=0 -- app/MacroFreestanding.rvnproj --no-emit
```

```bash
dotnet run --framework net10.0 --project ../../../src/Raven.Compiler --property WarningLevel=0 -- app/MacroFreestanding.rvnproj --run
```

Expected output:

```text
42
```
