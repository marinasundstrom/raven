# Raven CLI samples

Compile any sample from the repo root via:

```
dotnet run -- samples/<file>.rav -o <file>.dll
```

Run the emitted assembly afterwards with `dotnet <file>.dll` (or use `run.sh`/`build.sh` for batch runs). Async demos now live under `samples/async/`, so invoke them with the sub-folder (for example, `dotnet run -- samples/async/async-generic-compute.rav -o async-generic-compute.dll`).
