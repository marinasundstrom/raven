# Namespace-Level Functions and Constants (.rvnproj)

This sample demonstrates namespace-level functions and constants imported from another namespace, plus a `[TopLevel]` static helper whose members are promoted through the same namespace import.

Project file:

- `NamespaceMembers.rvnproj`

Source files:

- `src/Main.rvn`
- `src/Members.rvn`

## Build

From this folder:

```bash
dotnet run --project ../../../src/Raven.Compiler --property WarningLevel=0 -- NamespaceMembers.rvnproj
```

## Run

```bash
dotnet bin/Debug/net10.0/NamespaceMembers.dll
```
