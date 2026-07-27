# Macro AddEquatable (`.rvnproj`)

This sample shows an attached macro that makes an annotated Raven class
implement `System.IEquatable<T>`.

Current status:

- `#[AddEquatable]` parses as a macro-style attribute.
- Macro-style attributes are intentionally excluded from normal CLR attribute binding/emission.
- The macro is implemented in Raven through the object-oriented compiler-plugin
  contracts.
- The provider declares
  `[assembly: RavenCompilerPlugin(typeof(AddEquatableMacro))]`, exporting the
  macro definition directly without a plugin container.
- The Raven application consumes the provider through an ordinary
  `ProjectReference`; no consumer-authored `RavenMacro` item is needed.
- The compiler resolves attached macros and invokes plugin expansion generically.
- The macro replaces the class base list with one that includes
  `IEquatable<User>` while preserving any authored base types.
- The macro introduces `Equals(other: User)`, which compares the sample's
  `Name` and `Age` properties.
- The replacement type shape and generated member participate in ordinary
  binding and code generation.

Files:

- `MacroAddEquatable.rvnproj`: Raven project using `#[AddEquatable]`
- `src/Program.rvn`: Raven source that uses the macro-style attribute
- `macros/AddEquatableMacros.rvnproj`: Raven macro plugin project
- `macros/AddEquatableMacro.rvn`: attached macro implementation that replaces the type shape and introduces `Equals`

Build and run the Raven application. Its normal project reference builds and
activates the marked Raven provider:

```bash
dotnet run --project MacroAddEquatable.rvnproj --property WarningLevel=0
```

Expected output:

```text
True
False
```
