# Hello World (.ravenproj)

This sample is a simple Hello World program.

Project file:

- `HelloWorld.ravenproj`

Source file:

- `src/main.rav`

## Build

From this folder:

```bash
dotnet run -f net10.0 --project ../../../src/Raven.Compiler --property WarningLevel=0 -- HelloWorld.ravenproj
```

## Run

```bash
dotnet bin/HelloWorld.dll
```
