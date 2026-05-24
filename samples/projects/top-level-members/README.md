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
dotnet build NamespaceMembers.rvnproj --property WarningLevel=0
```

## Run

```bash
dotnet bin/Debug/net10.0/NamespaceMembers.dll
```
