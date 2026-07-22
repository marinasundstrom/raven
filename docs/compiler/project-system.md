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
- `FrameworkProjections` or `RavenFrameworkProjections` (`Standard` by default,
  or `None` for the ordinary .NET API surface)
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

Legacy `.ravenproj` files and legacy Raven-specific XML are deprecated. They remain loadable for compatibility, but new projects should use `.rvnproj` and the MSBuild-backed project shape.

## Generated prelude imports

Raven projects generate a `<ProjectName>.Prelude.g.rvn` source file by default.
It globally imports the common `System` namespaces plus `System.Result.*` and
`System.Option.*`.

Global imports are hoisted across the compilation, but they still use ordinary
import binding rules. Namespace imports are the most robust project-file import
shape because the namespace only has to exist after references and project
declarations are known. Type-scope imports such as `System.Result.*` and direct
nested-case imports such as `System.Result.Ok` require the imported type or
nested type to be available to the compilation. They are supported, but they are
less flexible than namespace imports and should normally be reserved for stable
library/prelude cases; user-defined union cases are usually clearer as qualified
or target-typed `.Case` references.

Set `GeneratePreludeImports` to `false` to disable the generated standard
imports:

```xml
<PropertyGroup>
  <GeneratePreludeImports>false</GeneratePreludeImports>
</PropertyGroup>
```

Projects can add prelude imports with `Import` items:

```xml
<ItemGroup>
  <Import Include="SuperheroApp.Models" />
  <Import Include="System.Console" Static="True" />
  <Import Include="System.DateTime" Alias="DT" />
</ItemGroup>
```

Non-aliased items generate global wildcard imports. `Static="True"` is intended
for type-scope imports such as `System.Console.*`. `Alias` generates a
project-wide alias in the prelude. If a source file repeats an import that is
already supplied by a global import, the compiler reports a hidden redundant
import diagnostic and editors can offer a remove-import fix.

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

Raven project builds use the standard .NET output layout:

- Normal build (`dotnet build App.rvnproj`)
  - default output directory: `<project-dir>/bin/<Configuration>`
  - emits apphost + `.dll` + `.runtimeconfig.json` for console apps
  - does **not** copy package/runtime dependency sets
- Publish (`dotnet publish App.rvnproj`)
  - default output directory: `<project-dir>/bin/<Configuration>/publish`
  - copies runtime dependencies (NuGet/framework/local assemblies) to output
  - emits runtime artifacts (`.runtimeconfig.json`, apphost)

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

Use `dotnet build` and `dotnet run --project` for normal application build and
run workflows.

Use `-o` with `rvnc` to override the output directory:

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

When invoking the compiler driver through `dotnet run`, make sure the compiler host itself runs as `net11.0`:

```bash
dotnet run -f net11.0 --project src/Raven.Compiler --property WarningLevel=0 -- path/to/App.rvnproj
```

When invoking a `net11.0` `.rvnproj` through `dotnet build` or
`dotnet run --project`, the selected .NET SDK must also support `net11.0`. Use a
project-local `global.json` to pin SDK 11 when a machine has multiple SDK bands
installed.

You can still override behavior explicitly:

- `--runtime-async` to force on.
- `--no-runtime-async` to force off.

## MSBuild build integration

`.rvnproj` files can build through the normal .NET SDK pipeline when MSBuild is
wired to Raven's language targets:

- `build/Raven.MSBuild.props` sets `.rvnproj` `LanguageTargets` to Raven's target file.
- `build/Raven.Language.targets` imports the common managed build targets and
  implements Raven's `CoreCompile`.
- The Raven compile writes the SDK intermediate assembly, copies it to the SDK
  reference-assembly slot when requested, and lets the normal SDK output pipeline
  copy files to `bin/<Configuration>/<TargetFramework>/`.
- MSBuild-resolved `ReferencePath` items are passed to `rvnc`; package restore
  and framework-reference resolution remain owned by the .NET SDK rather than
  the Raven compiler core.

Inside this repository, `Directory.Build.props` wires `.rvnproj` files
automatically, so sample projects build directly:

```bash
dotnet build samples/projects/hello-world/HelloWorld.rvnproj --property WarningLevel=0
```

For standalone projects before Raven is packaged as an SDK/NuGet build asset,
set `LanguageTargets` and, when needed, `RavenCompilerHost` explicitly:

```xml
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <LanguageTargets>/path/to/Raven/build/Raven.Language.targets</LanguageTargets>
    <RavenCompilerHost>/path/to/Raven/src/Raven.Compiler/bin/Debug/net10.0/rvnc.dll</RavenCompilerHost>
    <TargetFramework>net10.0</TargetFramework>
    <AssemblyName>RavenGreeter</AssemblyName>
    <OutputType>Library</OutputType>
  </PropertyGroup>

  <ItemGroup>
    <RavenCompile Include="src/**/*.rvn" />
  </ItemGroup>
</Project>
```

C# and other SDK projects can reference a Raven project with normal
`ProjectReference` once the referenced `.rvnproj` has Raven language targets:

```xml
<ItemGroup>
  <ProjectReference Include="..\raven\RavenGreeter.rvnproj" />
</ItemGroup>
```

The old `build/Raven.MSBuild.targets` bridge remains for compatibility with
host projects that set `RavenProjectFile`, but new projects should prefer
building the `.rvnproj` itself and using `ProjectReference`.

## Workspace and project-system services

`RavenWorkspace` now consumes project loading/saving through host services rather than hardcoding project-file persistence logic in workspace APIs.

- `PersistenceService` delegates project open/save to `IProjectSystemService`.
- `RavenProjectSystemService` is the deprecated compatibility implementation for legacy `.ravenproj`.
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
