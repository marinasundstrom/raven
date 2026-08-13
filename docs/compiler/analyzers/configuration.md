# Analyzer Configuration

Analyzer configuration has three separate layers:

- analyzer references decide which analyzer assemblies are available;
- project and compilation options decide which analyzers or analyzer modes run;
- diagnostic options decide the severity or suppression of diagnostics that are reported.

Keeping these separate matches Roslyn-style behavior. A configured severity
does activate a disabled-by-default rule after its external analyzer assembly
has been referenced.

## Project Options

Project files can configure analyzer participation when a whole analyzer mode should be
selected. This is intended for coarse feature selection, not severity control.

Example `.rvnproj`:

```xml
<Project Sdk="Raven.Sdk/VERSION">
  <PropertyGroup>
    <TargetFramework>net10.0</TargetFramework>
    <RavenReturnedValueHandlingMode>full</RavenReturnedValueHandlingMode>
    <RavenEnabledAnalyzers>category:typing;UnusedVariableAnalyzer</RavenEnabledAnalyzers>
    <RavenDisabledAnalyzers>DisposableObjectAnalyzer</RavenDisabledAnalyzers>
  </PropertyGroup>
</Project>
```

`RavenEnabledAnalyzers` opts into optional built-in analyzers by analyzer type name or fully
qualified analyzer type name. It can also enable the optional analyzers in one analyzer kind
with `category:typing`, `category:initialization`, `category:immutability`, `category:usage`,
`category:errorhandling`, or `category:design`. A kind
may contain both default and optional analyzers; the category token adds only its optional
members. Use `all` or `*` to enable every optional built-in analyzer.
`RavenDisabledAnalyzers` disables an analyzer and takes precedence if a name appears in both
sets. Values may be separated with `;`, `,`, or whitespace. The short unqualified type name
is preferred for project files. `UnusedVariableAnalyzer` is a compatibility group name that
controls both `UnusedLocalAnalyzer` and `UnusedParameterAnalyzer`.

These participation properties apply to compiler-hosted analyzers. For the
separate `Raven.Analyzers` package, the `PackageReference` makes the assembly
available. Its disabled-by-default rules are activated by assigning an
explicit non-`default` severity in `.editorconfig`.

Boolean compatibility properties such as `EnableReturnedValueAnalyzer` and
`RavenEnableReturnedValueAnalyzer` map to the same mode selection. The only non-off
returned-value mode today is `full`.

## Diagnostic Severity

Severity is configured by diagnostic ID in `.editorconfig`:

```ini
[*.rvn]
dotnet_diagnostic.RAV9034.severity = warning

# Activate an optional Raven.Analyzers convention.
dotnet_diagnostic.RAV9035.severity = info
```

Accepted severity values follow the analyzer diagnostic options supported by Raven:

- `none` or `silent` for suppression/hidden reporting where supported;
- `suggestion` or `hidden`;
- `info`;
- `warning`;
- `error`;
- `default`.

The descriptor's `DefaultSeverity` is used when no option remaps the diagnostic.
For a descriptor whose `IsEnabledByDefault` value is false, no diagnostic is
reported unless an explicit non-`default` severity enables it.

## Language Server Updates

The language server watches `.editorconfig` files and reapplies diagnostic severity changes
to open projects without requiring a project reload. Project-file analyzer mode changes are
still project-system configuration and should be handled through normal workspace/project
reload paths.

## Source Suppression

Diagnostics can be suppressed in source with Raven pragma comments:

```rav
#pragma warning disable RAV9034
Compute()
#pragma warning restore RAV9034
```

See [Compiler diagnostics](../diagnostics.md) for the full pragma syntax.

## CLI Overrides

`Raven.Compiler` can override returned-value analyzer behavior for one invocation:

```bash
rvnc main.rvn --returned-value-handling warning
rvnc main.rvn --returned-value-handling error
rvnc main.rvn --force-returned-value-handling
```

For `RAV9034`, non-off severity values enable `full` mode for that compiler invocation and
apply the requested severity override.
