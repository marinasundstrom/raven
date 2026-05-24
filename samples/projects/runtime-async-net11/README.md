# Runtime Async (.rvnproj, .NET 11)

This sample targets `.NET 11` and demonstrates runtime-async emission for:

- `Task`
- `Task<T>`
- `ValueTask<T>`
- `ConfigureAwait(false)` awaitables

Project file:

- `RuntimeAsyncNet11.rvnproj`
- Target framework: `net11.0`

Source file:

- `src/main.rvn`

## Build and run

This sample requires a .NET SDK that supports `net11.0`. The sample-local
`global.json` asks `dotnet` to select an installed .NET 11 SDK when you run from
this folder.

From this folder:

```bash
dotnet run --project RuntimeAsyncNet11.rvnproj --property WarningLevel=0
```

Runtime-async metadata emission is auto-enabled for `net11.0` targets. You can force behavior with:

```bash
# Force on
dotnet run -f net11.0 --project ../../../src/Raven.Compiler --property WarningLevel=0 -- RuntimeAsyncNet11.rvnproj --runtime-async --run

# Force off
dotnet run -f net11.0 --project ../../../src/Raven.Compiler --property WarningLevel=0 -- RuntimeAsyncNet11.rvnproj --no-runtime-async --run
```

## Inspect emitted IL

From this folder:

```bash
ilspycmd -il ./bin/Debug/net11.0/RuntimeAsyncNet11.dll | rg "AsyncHelpers::Await|HandleAsyncEntryPoint|GetAwaiter|GetResult|flag\\(2000\\)"
```

In runtime-async mode you should see `AsyncHelpers::Await...`, `AsyncHelpers::HandleAsyncEntryPoint(...)`, and `flag(2000)` on async methods.

## Our async impl vs runtime async

Raven async implementation today (classic lowering):

- Compiler synthesizes async state-machine types.
- Compiler rewrites await flow and emits awaiter calls directly.
- More compiler-side complexity when new await scenarios are introduced.
- Task-like coverage is not fully implemented across the classic lowering pipeline.

Runtime async on `.NET 11`:

- No compiler-synthesized async state-machine types for method bodies.
- Async methods are marked with async method-impl metadata (`flag(2000)`).
- Await sites emit `System.Runtime.CompilerServices.AsyncHelpers.Await(...)`.
- Runtime provides the async execution mechanics that classic lowering previously handled in compiler-generated state machines.
- This helps fill the task-like support gap by relying on runtime async helpers for supported awaitable shapes.

Current gaps/limits:

- Compiler host should run as `net11.0` to emit `AsyncHelpers.Await(...)`.
- Raven-specific `Result<..., ...>` entry-point wrappers still use compiler-emitted bridge logic to map success and error payloads to process results.
