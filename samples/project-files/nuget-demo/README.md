# NuGet + .ravenproj sample

This sample uses a Raven project file (`NuGetDemo.ravenproj`) at the sample root, with source in `src/` and outputs in `bin/`.
It relies on implicit source inclusion, so all `*.rav` files under this folder are included automatically.

The project file contains a NuGet package reference:

- `Newtonsoft.Json` `13.0.3`

When you compile the project file, Raven resolves package assemblies from the global NuGet cache (`$NUGET_PACKAGES` or `~/.nuget/packages`).
If the package is missing, Raven triggers restore and then loads references from that cache.

## Compile

From this folder (`samples/project-files/nuget-demo`):

```bash
dotnet run --project ../../../src/Raven.Compiler --property WarningLevel=0 -- NuGetDemo.ravenproj
```

Optional run step:

```bash
dotnet bin/NuGetDemo.dll
```
