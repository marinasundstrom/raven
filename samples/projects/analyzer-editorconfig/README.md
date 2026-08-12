# Analyzer `.editorconfig` sample

This sample demonstrates opting into policy analyzers in the project file, then
using Raven in a "regular" style by suppressing their diagnostics through a
project-local `.editorconfig`. Analyzer participation and diagnostic severity
remain independent.

Disabled rules in this sample:

- `RAV9012` (`not-use-null`) so nullable declarations are allowed.
- `RAV9013` (`don't use throw`) so `throw` expressions/statements are allowed.
- `RAV9014` (`prefer Result/Option extensions`) so standard LINQ methods like `FirstOrDefault` are allowed.

Files:

- `.editorconfig`: rule severity overrides.
- `AnalyzerEditorConfig.rvnproj`: opts into the three optional analyzers.
- `src/Program.rvn`: program that intentionally uses nullable values, `throw`, and regular LINQ.

## Build

From repository root:

```bash
dotnet build samples/projects/analyzer-editorconfig/AnalyzerEditorConfig.rvnproj --property WarningLevel=0
```
