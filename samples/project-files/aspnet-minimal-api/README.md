# ASP.NET Core Minimal API (.ravenproj)

This sample shows a Raven project-file targeting an ASP.NET Core Minimal API endpoint.

Project file:

- `AspNetMinimalApi.ravenproj`
- Framework reference: `Microsoft.AspNetCore.App`

Source file:

- `src/main.rav`

## Build

From this folder:

```bash
dotnet run --project ../../../src/Raven.Compiler --property WarningLevel=0 -- AspNetMinimalApi.ravenproj
```

This emits output to `bin/` by default for project-file builds.

## Run

```bash
dotnet bin/AspNetMinimalApi.dll
```

Then browse to `http://localhost:5000/`.
