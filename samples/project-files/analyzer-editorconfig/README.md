# Analyzer `.editorconfig` sample

This sample demonstrates using Raven in a "regular" style while disabling selected analyzer diagnostics via a project-local `.editorconfig`.

Disabled rules in this sample:

- `RAV9012` (`not-use-null`) so nullable declarations are allowed.
- `RAV9013` (`don't use throw`) so `throw` expressions/statements are allowed.
- `RAV9014` (`prefer Result/Option extensions`) so standard LINQ methods like `FirstOrDefault` are allowed.

Files:

- `.editorconfig`: rule severity overrides.
- `AnalyzerEditorConfig.ravenproj`: project file.
- `src/main.rav`: source that intentionally uses nullable values, `throw`, and regular LINQ.

## Build

From repository root:

```bash
dotnet run --project src/Raven.Compiler --property WarningLevel=0 -- samples/project-files/analyzer-editorconfig/AnalyzerEditorConfig.ravenproj -o samples/project-files/analyzer-editorconfig/bin
```
