# Raven Project System

Raven supports compiling either individual `.rav` files or a project file (`.ravenproj`).

You can scaffold a project in the current directory with:

```bash
dotnet run --project src/Raven.Compiler -- init
```

## Project file format

`*.ravenproj` is XML with a `<Project>` root.

Supported attributes on `<Project>`:

- `Name`: Logical project name.
- `TargetFramework`: Target framework moniker (for example `net9.0`).
- `Output`: Assembly name.
- `OutputKind`: `ConsoleApplication` or `DynamicallyLinkedLibrary`.
- `AllowUnsafe`: `true`/`false`.
- `AllowGlobalStatements`: `true`/`false` (`true` by default).
- `Configuration`: Build configuration name used for generated intermediate files (`Debug` default).

Supported child elements:

- `<Document Path="..."/>` or `<Document FilePath="..."/>`
- `<ProjectReference Path="..."/>`
- `<Reference Path="..."/>` (or `Include="..."`)
- `<PackageReference Include="Package.Id" Version="x.y.z"/>`
- `<FrameworkReference Include="Framework.Name"/>`

## `.editorconfig` diagnostic severity support

Raven reads `.editorconfig` files when compiling project and source files and applies
diagnostic severity overrides from:

- `dotnet_diagnostic.<ID>.severity`
- `dotnet_diagnostic.*.severity`
- `dotnet_analyzer_diagnostic.severity`

Supported severity values:

- `none`/`suppress` -> suppressed
- `silent`/`hidden` -> hidden
- `suggestion`/`info` -> info
- `warning`/`warn` -> warning
- `error` -> error
- `default` -> default severity

Example:

```ini
root = true

[*.rav]
dotnet_diagnostic.RAV9012.severity = none
dotnet_diagnostic.RAV9013.severity = none
dotnet_diagnostic.RAV9014.severity = none
```

## Implicit source inclusion

If no `<Document>` entries are present, Raven implicitly includes all Raven source files under the project directory recursively (`**/*.rav`).
Files under `bin/` and `obj/` are excluded from implicit inclusion.

This enables minimal project files like:

```xml
<Project Name="App" TargetFramework="net9.0" Output="App">
  <PackageReference Include="Newtonsoft.Json" Version="13.0.3" />
</Project>
```

## NuGet package references

When a `.ravenproj` includes `<PackageReference>`:

1. Raven first resolves package assemblies from the global NuGet cache:
   - `$NUGET_PACKAGES` when set
   - otherwise `~/.nuget/packages`
2. If required assets are missing, Raven runs `dotnet restore` for a temporary SDK project.
3. Raven reads resolved compile assets and adds those assemblies as metadata references.

When a `.ravenproj` includes `<FrameworkReference>`:

1. Raven restores a temporary SDK project that contains those framework references.
2. Raven resolves the corresponding framework reference packs from installed .NET SDK `packs/`.
3. Pack reference assemblies are added as metadata references for compilation.

## Build vs publish outputs

Raven now separates normal build output from publish-style output:

- Normal compile (`ravenc App.ravenproj`)
  - default output directory: `<project-dir>/bin`
  - emits apphost + `.dll` + `.runtimeconfig.json` for console apps
  - does **not** copy package/runtime dependency sets
- Publish (`ravenc App.ravenproj --publish`)
  - default output directory: `<project-dir>/bin/publish`
  - copies runtime dependencies (NuGet/framework/local assemblies) to output
  - emits runtime artifacts (`.runtimeconfig.json`, apphost)

`--run` uses the normal output directory (`bin` for `.ravenproj`) and stages runtime dependencies there as needed so the produced program can execute immediately.

Dependency copy details:

- Only `.dll` package dependencies are copied.
- If a compile reference comes from `ref/`, Raven prefers the runtime assembly under `lib/`.

## Generated intermediate sources

For project builds, Raven can generate intermediate Raven source files under:

- `<project-dir>/obj/<Configuration>/raven/generated/`

Current generated source:

- `<ProjectName>.TargetFrameworkAttribute.g.rav` containing:

```rav
import System.Runtime.Versioning.*

[assembly: TargetFramework(".NETCoreApp,Version=vX.Y")]
```

Generation rules:

- Emitted when `TargetFramework` is set on `.ravenproj`.
- Skipped if user source already declares assembly-level `TargetFrameworkAttribute`.

## CLI usage

Compile a project file:

```bash
dotnet run --project src/Raven.Compiler --property WarningLevel=0 -- path/to/App.ravenproj
```

Publish a project file:

```bash
dotnet run --project src/Raven.Compiler --property WarningLevel=0 -- path/to/App.ravenproj --publish
```

Use `-o` to override the output directory:

```bash
dotnet run --project src/Raven.Compiler --property WarningLevel=0 -- path/to/App.ravenproj -o path/to/out
```

Sample:

- `samples/project-files/nuget-demo/README.md`
- `samples/project-files/raven-msbuild-integration/README.md`
- `samples/project-files/runtime-async-net11/README.md`

## Runtime async for `net11.0`

If a `.ravenproj` sets `TargetFramework="net11.0"` (or newer), Raven enables runtime-async mode by default.

- Async methods emit with runtime async metadata.
- Await expressions emit `System.Runtime.CompilerServices.AsyncHelpers.Await(...)` calls when available.
- State-machine type synthesis is skipped.

When invoking the compiler through `dotnet run`, make sure the compiler host itself runs as `net11.0`:

```bash
dotnet run -f net11.0 --project src/Raven.Compiler --property WarningLevel=0 -- path/to/App.ravenproj --run
```

You can still override behavior explicitly:

- `--runtime-async` to force on.
- `--no-runtime-async` to force off.

## Temporary C# MSBuild bridge

For temporary integration from a C# SDK project, import:

- `build/Raven.MSBuild.targets`

and set:

- `RavenProjectFile` - path to the `.ravenproj` to compile

Example in a `.csproj`:

```xml
<PropertyGroup>
  <RavenProjectFile>..\raven\RavenGreeter.ravenproj</RavenProjectFile>
</PropertyGroup>
<Import Project="..\..\..\..\build\Raven.MSBuild.targets" />
```

Behavior:

- Before `ResolveReferences`, MSBuild runs Raven compilation for `RavenProjectFile`.
- Output goes to `$(IntermediateOutputPath)raven\` by default.
- The produced Raven assembly is added as a `<Reference>` for the C# build.

Current limitation (temporary bridge):

- Compile-time typed consumption of Raven-defined types from C# may fail with `CS0012` (`System.Private.CoreLib` reference mismatch).
- Runtime loading/invocation of the produced Raven assembly works.
- See `samples/project-files/raven-msbuild-integration/README.md` for the current reflection-based host example.

## Workspace and project-system services

`RavenWorkspace` now consumes project loading/saving through host services rather than hardcoding `.ravenproj` persistence logic in workspace APIs.

- `PersistenceService` delegates project open/save to `IProjectSystemService`.
- `RavenProjectSystemService` is the default implementation for `.ravenproj`.
- `RavenWorkspace.Create(..., projectSystemService: ...)` allows injecting an alternative implementation (for example, a future MSBuild-backed service) without changing workspace consumers.

## Scaffolding with `ravc init`

`ravc init` creates a starter layout in the current directory:

- `<ProjectName>.ravenproj`
- `src/main.rav`
- `bin/.gitkeep`

Options:

- `--name <project-name>`: set explicit project/assembly name.
- `--framework <tfm>`: set `TargetFramework` in the generated `.ravenproj`.
- `--type <app|classlib>`: set `OutputKind` (`app` default).
- `--force`: overwrite scaffold files when they already exist.
