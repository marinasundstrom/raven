# Raven Project System

Raven supports compiling either individual `.rvn` files (with legacy `.rav` compatibility) or a project file (`.rvnproj`).

You can scaffold a project in the current directory with:

```bash
rvn init
```

## Project file format

`*.rvnproj` is now a real MSBuild project file. The primary format matches SDK-style `.csproj` structure and relies on evaluated MSBuild properties/items rather than Raven-specific XML attributes.

Primary MSBuild properties Raven currently consumes:

- `TargetFramework`
- `TargetFrameworks` (first TFM is used for now)
- `AssemblyName`
- `OutputType` (`Exe` or `Library`)
- `AllowUnsafeBlocks` or `AllowUnsafe`
- `AllowGlobalStatements` or `RavenAllowGlobalStatements`
- `MembersPublicByDefault` or `RavenMembersPublicByDefault`
- `IntermediateOutputPath`
- `Configuration`

Primary MSBuild items Raven currently consumes:

- `<RavenCompile Include="..."/>`
- `<ProjectReference Include="..."/>`
- `<Reference Include="...">` with `HintPath`
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

[*.rvn]
dotnet_diagnostic.RAV9012.severity = none
dotnet_diagnostic.RAV9013.severity = none
dotnet_diagnostic.RAV9014.severity = none
```

## Raven source inclusion

Raven source files should be declared with `RavenCompile` items, just like C# uses `Compile` items.

Minimal example:

```xml
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <AssemblyName>App</AssemblyName>
    <OutputType>Exe</OutputType>
  </PropertyGroup>
  <ItemGroup>
    <RavenCompile Include="src/**/*.rvn" />
    <PackageReference Include="Newtonsoft.Json" Version="13.0.3" />
  </ItemGroup>
</Project>
```

Legacy `.ravenproj` files and legacy Raven-specific XML are still supported for compatibility, but they are no longer the primary project shape.

## NuGet package references

When a `.rvnproj` includes `<PackageReference>`:

1. Raven first resolves package assemblies from the global NuGet cache:
   - `$NUGET_PACKAGES` when set
   - otherwise `~/.nuget/packages`
2. If required assets are missing, Raven runs `dotnet restore` for a temporary SDK project.
3. Raven reads resolved compile assets and adds those assemblies as metadata references.

When a `.rvnproj` includes `<FrameworkReference>`:

1. Raven restores a temporary SDK project that contains those framework references.
2. Raven resolves the corresponding framework reference packs from installed .NET SDK `packs/`.
3. Pack reference assemblies are added as metadata references for compilation.

## Build vs publish outputs

Raven now separates normal build output from publish-style output:

- Normal compile (`rvn App.rvnproj`)
  - default output directory: `<project-dir>/bin/<Configuration>`
  - emits apphost + `.dll` + `.runtimeconfig.json` for console apps
  - does **not** copy package/runtime dependency sets
- Publish (`rvn App.rvnproj --publish`)
  - default output directory: `<project-dir>/bin/<Configuration>/publish`
  - copies runtime dependencies (NuGet/framework/local assemblies) to output
  - emits runtime artifacts (`.runtimeconfig.json`, apphost)

`--run` uses the normal output directory (`bin/<Configuration>` for `.rvnproj`) and stages runtime dependencies there as needed so the produced program can execute immediately.

Dependency copy details:

- Only `.dll` package dependencies are copied.
- If a compile reference comes from `ref/`, Raven prefers the runtime assembly under `lib/`.

## Generated intermediate sources

For project builds, Raven can generate intermediate Raven source files under:

- `<project-dir>/obj/<Configuration>/raven/generated/`

Current generated source:

- `<ProjectName>.TargetFrameworkAttribute.g.rvn` containing:

```rav
import System.Runtime.Versioning.*

[assembly: TargetFramework(".NETCoreApp,Version=vX.Y")]
```

Generation rules:

- Emitted when `TargetFramework` is set on `.rvnproj`.
- Skipped if user source already declares assembly-level `TargetFrameworkAttribute`.

## CLI usage

Compile a project file:

```bash
dotnet run --project src/Raven.Compiler --property WarningLevel=0 -- path/to/App.rvnproj
```

Publish a project file:

```bash
dotnet run --project src/Raven.Compiler --property WarningLevel=0 -- path/to/App.rvnproj --publish
```

Use `-o` to override the output directory:

```bash
dotnet run --project src/Raven.Compiler --property WarningLevel=0 -- path/to/App.rvnproj -o path/to/out
```

Sample:

- `samples/projects/nuget-demo/README.md`
- `samples/projects/raven-msbuild-integration/README.md`
- `samples/projects/runtime-async-net11/README.md`

## Runtime async for `net11.0`

If a `.rvnproj` sets `<TargetFramework>net11.0</TargetFramework>` (or newer), Raven enables runtime-async mode by default.

- Async methods emit with runtime async metadata.
- Await expressions emit `System.Runtime.CompilerServices.AsyncHelpers.Await(...)` calls when available.
- State-machine type synthesis is skipped.

When invoking the compiler through `dotnet run`, make sure the compiler host itself runs as `net11.0`:

```bash
dotnet run -f net11.0 --project src/Raven.Compiler --property WarningLevel=0 -- path/to/App.rvnproj --run
```

You can still override behavior explicitly:

- `--runtime-async` to force on.
- `--no-runtime-async` to force off.

## Temporary C# MSBuild bridge

For temporary integration from a C# SDK project, import:

- `build/Raven.MSBuild.targets`

and set:

- `RavenProjectFile` - path to the Raven project file to compile

Example in a `.csproj`:

```xml
<PropertyGroup>
  <RavenProjectFile>..\raven\RavenGreeter.rvnproj</RavenProjectFile>
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
- See `samples/projects/raven-msbuild-integration/README.md` for the current reflection-based host example.

## Workspace and project-system services

`RavenWorkspace` now consumes project loading/saving through host services rather than hardcoding project-file persistence logic in workspace APIs.

- `PersistenceService` delegates project open/save to `IProjectSystemService`.
- `RavenProjectSystemService` is the compatibility implementation for legacy `.ravenproj`.
- `MsBuildProjectSystemService` opens Raven projects authored as MSBuild-backed `.rvnproj` files.
- `CompositeProjectSystemService` lets the workspace route between legacy `.ravenproj` files and primary `.rvnproj` projects.
- `RavenWorkspace.Create(..., projectSystemService: ...)` still allows overriding the project-system implementation explicitly.

### MSBuild-backed Raven projects

The workspace can now load Raven projects from ordinary MSBuild project files when they declare Raven sources through `RavenCompile`.

Example:

```xml
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <OutputType>Library</OutputType>
  </PropertyGroup>

  <ItemGroup>
    <RavenCompile Include="src/**/*.rvn" />
    <ProjectReference Include="..\Lib\Lib.csproj" />
    <PackageReference Include="Newtonsoft.Json" Version="13.0.3" />
    <FrameworkReference Include="Microsoft.AspNetCore.App" />
  </ItemGroup>
</Project>
```

Current behavior:

- `RavenWorkspace.OpenProject(...)` can open that project through `MsBuildProjectSystemService`.
- `TargetFramework`, `AssemblyName`, `OutputType`, `AllowUnsafe` / `AllowUnsafeBlocks`, `AllowGlobalStatements`, and `MembersPublicByDefault` are mapped into Raven project state.
- `ProjectReference` paths are surfaced through the project-system abstraction so callers such as the language server can recurse without knowing the concrete project-file format.
- Referenced Raven MSBuild projects become workspace project references when they are loaded.
- Referenced non-Raven MSBuild projects are consumed as metadata references when their evaluated `TargetPath` already exists on disk.

Current behavior also includes save support for Raven-owned MSBuild state (`RavenCompile`, mapped Raven properties, and on-disk Raven source files) while preserving unrelated MSBuild items.

## Scaffolding with `rvn init`

`rvn init` creates a starter layout in the current directory:

- `<ProjectName>.rvnproj`
- `src/main.rvn`
- `bin/.gitkeep`

Options:

- `--name <project-name>`: set explicit project/assembly name.
- `--framework <tfm>`: set `TargetFramework` in the generated `.rvnproj`.
- `console|classlib`: select the scaffold type (`console` default).
- `--type <console|classlib>`: compatibility alias for selecting the scaffold type.
- `--force`: overwrite scaffold files when they already exist.
