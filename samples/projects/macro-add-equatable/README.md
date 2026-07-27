# Macro AddEquatable (`.rvnproj`)

This sample shows the intended shape of an attached macro project for Raven.

Current status:

- `#[AddEquatable]` parses as a macro-style attribute.
- Macro-style attributes are intentionally excluded from normal CLR attribute binding/emission.
- The macro is implemented in C# through the same object-oriented plugin
  contracts available to Raven-authored macros.
- The provider declares
  `[assembly: RavenCompilerPlugin(typeof(AddEquatableMacroPlugin))]`.
- The Raven application consumes the provider through an ordinary
  `ProjectReference`; no consumer-authored `RavenMacro` item is needed.
- The compiler resolves attached macros and invokes plugin expansion generically.
- Generated members participate in ordinary binding and code generation.

Files:

- `MacroAddEquatable.rvnproj`: Raven project using `#[AddEquatable]`
- `src/main.rvn`: Raven source that uses the macro-style attribute
- `macros/AddEquatableMacros.csproj`: example .NET macro plugin project
- `macros/AddEquatableMacroPlugin.cs`: example plugin implementation that returns a generated member through `MacroExpansionResult`

Build and run the Raven application. Its normal project reference builds and
activates the marked C# provider:

```bash
dotnet run --project MacroAddEquatable.rvnproj --property WarningLevel=0
```

Expected output:

```text
Ada
```
