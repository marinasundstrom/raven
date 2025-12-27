# Samples build/run investigation (2025-03-09)

## Commands

```bash
cd samples
RAVEN_CORE=../src/Raven.Core/bin/Debug/net9.0/net9.0/Raven.Core.dll bash build.sh
OUTPUT_DIR=output bash run.sh
```

## Results

### Build

The build failed before sample compilation because the `Raven.Core` build invoked `BoundNodeGenerator` and hit a permissions error:

```
Unhandled exception: An error occurred trying to start process '/workspace/raven/tools/BoundNodeGenerator/bin/Debug/net9.0/BoundNodeGenerator' with working directory '/workspace/raven/src/Raven.CodeAnalysis'. Permission denied
```

This caused the `Raven.Core` build target to fail, so `ravc` was not produced (`/workspace/raven/src/Raven.Compiler/bin/Debug/net9.0/ravc` missing). As a result, every sample compile failed with `ravc: No such file or directory`.

Summary from `build.sh`:

- Succeeded: 0
- Failed: 78
- `Raven.Core.dll` was not produced, so the samples were built without `--raven-core`.
- `TestDep.dll` could not be copied because the output directory was not populated.

### Run

`run.sh` found no output assemblies:

```
No .dll files found in '/workspace/raven/samples/output'.
```

## Assessment

The sample procedure could not execute because the build failed early while running `BoundNodeGenerator`. The failure prevents `Raven.Core` from building and blocks `ravc` from being available for `build.sh`. The immediate next step is to resolve the `Permission denied` error for `tools/BoundNodeGenerator/bin/Debug/net9.0/BoundNodeGenerator` so the `Raven.Core` build can complete and `ravc` is emitted for sample compilation.
