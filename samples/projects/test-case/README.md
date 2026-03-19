# Pattern Showcase (.rvnproj)

This sample collects Raven pattern features in one project. It keeps the
existing tuple, nominal, `if`, and `for` pattern examples, and adds
collection-oriented cases such as spread elements, sequence patterns, and
dictionary iteration/deconstruction.

Project file:

- `HelloWorld.rvnproj`

Source file:

- `src/main.rvn`

## Build

From this folder:

```bash
dotnet run -f net10.0 --project ../../../src/Raven.Compiler --property WarningLevel=0 -- HelloWorld.rvnproj
```

## Run

```bash
dotnet bin/PatternShowcase.dll
```
