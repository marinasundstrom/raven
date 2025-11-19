# Raven samples

Compile any sample from the repo root via:

```
dotnet run --project ../src/Raven.Compiler -- <file>.rav -o <file>.dll
```

Run the emitted assembly afterwards with `dotnet <file>.dll` (or use `run.sh`/`build.sh` for batch runs). Async demos now live under `samples/async/`, so invoke them with the sub-folder (for example, `dotnet run --project ../src/Raven.Compiler -- async/async-generic-compute.rav -o output/async-generic-compute.dll`).
