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

Supported child elements:

- `<Document Path="..."/>` or `<Document FilePath="..."/>`
- `<ProjectReference Path="..."/>`
- `<Reference Path="..."/>` (or `Include="..."`)
- `<PackageReference Include="Package.Id" Version="x.y.z"/>`
- `<FrameworkReference Include="Framework.Name"/>`

## Implicit source inclusion

If no `<Document>` entries are present, Raven implicitly includes all Raven source files under the project directory recursively (`**/*.rav`).

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

## Output dependency copying

When compiling from a `.ravenproj`, Raven also copies NuGet package DLLs to the output directory so produced binaries can run without manual dependency copying.

- Only `.dll` package dependencies are copied.
- If a compile reference comes from `ref/`, Raven prefers the runtime assembly under `lib/`.

## CLI usage

Compile a project file:

```bash
dotnet run --project src/Raven.Compiler --property WarningLevel=0 -- path/to/App.ravenproj
```

Use `-o` to override the output directory:

```bash
dotnet run --project src/Raven.Compiler --property WarningLevel=0 -- path/to/App.ravenproj -o path/to/out
```

Sample:

- `samples/project-files/nuget-demo/README.md`

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
