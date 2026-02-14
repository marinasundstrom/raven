# Raven From C# (Temporary MSBuild Integration)

This sample shows a temporary integration where a C# project imports a target file that compiles Raven code and places the produced assembly in the C# output.

Files:

- `src/raven/RavenGreeter.ravenproj` - Raven project producing `RavenGreeter.dll`
- `src/raven/main.rav` - Raven code consumed from C#
- `src/csharp/Host.csproj` - C# host project importing `build/Raven.MSBuild.targets`
- `src/csharp/Program.cs` - Loads `RavenGreeter.dll` and invokes `Greeter.Message()` via reflection

## Run

From repository root:

```bash
dotnet run --project samples/project-files/raven-msbuild-integration/src/csharp/Host.csproj --property WarningLevel=0
```

Expected output:

```text
Hello from Raven via MSBuild integration
```

## How it works

`build/Raven.MSBuild.targets` adds a `BuildRavenProject` target that runs before `ResolveReferences`:

1. Compiles the configured `.ravenproj` using `Raven.Compiler`
2. Writes output into `$(IntermediateOutputPath)raven\`
3. Injects the produced Raven DLL as a `<Reference>` for the C# compilation

This is intentionally a temporary bridge and keeps the Raven workspace/project model unchanged.

Current limitation:

- Direct compile-time consumption of Raven types from C# (`Greeter.Message()` as a typed call) is currently blocked by metadata compatibility (`CS0012` referencing `System.Private.CoreLib`).
- Runtime loading/invocation works and is what this temporary sample demonstrates.

## Building Raven project separately

`dotnet run --project ../../../../../src/Raven.Compiler --property WarningLevel=0 -- RavenGreeter.ravenproj`