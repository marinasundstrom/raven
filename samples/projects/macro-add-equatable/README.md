# Macro AddEquatable (`.rvnproj`)

This sample shows the intended shape of an attached macro project for Raven.

Current status:

- `#[AddEquatable]` parses as a macro-style attribute.
- Macro-style attributes are intentionally excluded from normal CLR attribute binding/emission.
- The .NET plugin contract exists.
- Raven project files can reference macro assemblies with `RavenMacro`.
- The compiler resolves attached macros and invokes plugin expansion generically.
- Expansion results are currently available through the semantic model for tooling and inspection; generated members are not yet merged into binding/codegen.

Files:

- `MacroAddEquatable.rvnproj`: Raven project using `#[AddEquatable]`
- `src/main.rvn`: Raven source that uses the macro-style attribute
- `macros/AddEquatableMacros.csproj`: example .NET macro plugin project
- `macros/AddEquatableMacroPlugin.cs`: example plugin implementation that returns a generated member through `MacroExpansionResult`

The project file includes a `RavenMacro` item that points at the built plugin assembly.

Build the Raven source:

```bash
dotnet run --project ../../../src/Raven.Compiler --property WarningLevel=0 -- MacroAddEquatable.rvnproj
```

Build the example plugin assembly:

```bash
dotnet build macros/AddEquatableMacros.csproj /property:WarningLevel=0
```
