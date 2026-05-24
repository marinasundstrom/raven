# Raven From C# (MSBuild Integration)

This sample shows a C# SDK project referencing a Raven project through ordinary MSBuild `ProjectReference`.

Files:

- `src/raven/RavenGreeter.rvnproj` - Raven project producing `RavenGreeter.dll`
- `src/raven/main.rvn` - Raven code consumed from C#
- `src/csharp/Host.csproj` - C# host project with a `ProjectReference` to the Raven project
- `src/csharp/Program.cs` - Calls `Greeter.Message()` directly

## Run

From repository root:

```bash
dotnet run --project samples/projects/raven-msbuild-integration/src/csharp/Host.csproj --property WarningLevel=0
```

Expected output:

```text
Hello from Raven via MSBuild integration
```

## How it works

The repository `Directory.Build.props` wires `.rvnproj` files to `build/Raven.Language.targets`. During the C# build, MSBuild builds the referenced Raven project first and consumes its target path as a normal managed reference.

- `Raven.Language.targets` defines the Raven language targets needed by the .NET SDK.
- `CoreCompile` invokes `Raven.Compiler` for the `.rvnproj`.
- The emitted Raven assembly is copied through the SDK output pipeline and can be used by C# as a compile-time reference.

## Building Raven project separately

```bash
dotnet build samples/projects/raven-msbuild-integration/src/raven/RavenGreeter.rvnproj --property WarningLevel=0
```
