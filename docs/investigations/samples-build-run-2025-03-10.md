# Samples build/run investigation (2025-03-10)

## Goal
Validate that building `Raven.Compiler` twice produces `ravc` and `Raven.Core.dll`, and confirm the core DLL can be copied into the samples output.

## Commands

```bash
dotnet build src/Raven.Compiler/Raven.Compiler.csproj --property WarningLevel=0
# repeated build per guidance

dotnet build src/Raven.Compiler/Raven.Compiler.csproj --property WarningLevel=0

mkdir -p samples/output
cp -f src/Raven.Core/bin/Debug/net9.0/Raven.Core.dll samples/output/
```

## Results

### Build pass 1

- The first `dotnet build` attempt failed with file-lock errors while generating runtime configuration files for `DiagnosticsGenerator` and with missing generated syntax types (`SyntaxKind`, `BinaryExpressionSyntax`, etc.) in `Raven.CodeAnalysis`.
- This indicates the build was interrupted partway through generation and compilation.

### Build pass 2

- The second `dotnet build` succeeded and produced:
  - `src/Raven.Compiler/bin/Debug/net9.0/ravc.dll`
  - `src/Raven.Core/bin/Debug/net9.0/Raven.Core.dll`

### DLL copy

- `Raven.Core.dll` was copied into `samples/output/` after the successful second build.

## Assessment

Building `Raven.Compiler` twice resolved the initial generator/file-lock issues and produced the expected `ravc` and `Raven.Core.dll`. The core runtime DLL is now present in `samples/output/` for subsequent sample compilation runs.
