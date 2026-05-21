# Analyzer Configuration

Analyzer configuration has two separate layers:

- project and compilation options decide which analyzers or analyzer modes run;
- diagnostic options decide the severity or suppression of diagnostics that are reported.

Keeping these separate matches Roslyn-style behavior and avoids surprising cases where a
severity entry silently enables an opt-in analyzer.

## Project Options

Project files can configure analyzer participation when a whole analyzer mode should be
selected. This is intended for coarse feature selection, not severity control.

Example `.ravenproj`:

```xml
<Project
  Name="App"
  TargetFramework="net10.0"
  OutputKind="DynamicallyLinkedLibrary"
  ReturnedValueHandlingMode="full" />
```

Example MSBuild-style `.rvnproj`:

```xml
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <RavenReturnedValueHandlingMode>full</RavenReturnedValueHandlingMode>
  </PropertyGroup>
</Project>
```

Boolean compatibility properties such as `EnableReturnedValueAnalyzer` and
`RavenEnableReturnedValueAnalyzer` map to the same mode selection. The only non-off
returned-value mode today is `full`.

## Diagnostic Severity

Severity is configured by diagnostic ID in `.editorconfig`:

```ini
[*.rvn]
dotnet_diagnostic.RAV9029.severity = warning
```

Accepted severity values follow the analyzer diagnostic options supported by Raven:

- `none` or `silent` for suppression/hidden reporting where supported;
- `suggestion` or `hidden`;
- `info`;
- `warning`;
- `error`;
- `default`.

The descriptor's `DefaultSeverity` is used when no option remaps the diagnostic.

## Language Server Updates

The language server watches `.editorconfig` files and reapplies diagnostic severity changes
to open projects without requiring a project reload. Project-file analyzer mode changes are
still project-system configuration and should be handled through normal workspace/project
reload paths.

## Source Suppression

Diagnostics can be suppressed in source with Raven pragma comments:

```rav
#pragma warning disable RAV9029
Compute()
#pragma warning restore RAV9029
```

See [Compiler diagnostics](../diagnostics.md) for the full pragma syntax.

## CLI Overrides

`Raven.Compiler` can override returned-value analyzer behavior for one invocation:

```bash
rvn main.rvn --returned-value-handling warning
rvn main.rvn --returned-value-handling error
rvn main.rvn --force-returned-value-handling
```

For `RAV9029`, non-off severity values enable `full` mode for that compiler invocation and
apply the requested severity override.
