# Samples build/run investigation (2025-03-11)

## Commands

```bash
cd samples
RAVEN_CORE=../src/Raven.Core/bin/Debug/net9.0/net9.0/Raven.Core.dll bash build.sh
OUTPUT_DIR=output bash run.sh
```

## Results

### Build

The build failed while compiling `Raven.CodeAnalysis`, with generated syntax types missing (e.g., `AttributeSyntax`, `EnumDeclarationSyntax`, `BlockStatementSyntax`, etc.). The failures occurred during the `Raven.Core` build, which calls `ravc` to emit core types. Because the core build failed, `Raven.Core.dll` was not produced and `ravc` was missing:

```
Warning: Raven.Core.dll not found; samples will be built without --raven-core
build.sh: line 87: /workspace/raven/src/Raven.Compiler/bin/Debug/net9.0/ravc: No such file or directory
```

As a result, all 78 sample compilations failed with `ravc` missing.

Summary from `build.sh`:

- Succeeded: 0
- Failed: 78
- `Raven.Core.dll` not generated
- `ravc` not present in `src/Raven.Compiler/bin/Debug/net9.0/`

### Run

`run.sh` found no output assemblies:

```
No .dll files found in '/workspace/raven/samples/output'.
```

## Assessment

The samples procedure failed because `Raven.Core` did not build, which in turn prevented `ravc` from being available for sample compilation. The immediate next step is to ensure code generation completes cleanly for `Raven.CodeAnalysis` (missing syntax types) before re-running `build.sh`.
