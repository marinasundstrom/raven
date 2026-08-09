# NuGet + .rvnproj sample

This sample uses a Raven project file (`NuGetDemo.rvnproj`) at the sample root, with source in `src/` and outputs in `bin/`.
It relies on the default Raven source glob, which automatically includes the preferred `*.rvn` files under this folder.

The project file contains a NuGet package reference:

- `Newtonsoft.Json` `13.0.3`

When you compile the project file, Raven resolves package assemblies from the global NuGet cache (`$NUGET_PACKAGES` or `~/.nuget/packages`).
If the package is missing, Raven triggers restore and then loads references from that cache.

## Compile

From this folder (`samples/projects/nuget-demo`):

```bash
dotnet build NuGetDemo.rvnproj --property WarningLevel=0
```

Optional run step:

```bash
dotnet bin/NuGetDemo.dll
```
