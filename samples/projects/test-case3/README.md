# Hello World (.rvnproj)

This sample is a simple Hello World program.

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
dotnet bin/HelloWorld.dll
```
