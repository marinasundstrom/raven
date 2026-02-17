# Runtime Async (.ravenproj, .NET 11)

This sample targets `.NET 11` and demonstrates runtime-async emission for:

- `Task`
- `Task<T>`
- `ValueTask<T>`
- `ConfigureAwait(false)` awaitables

Project file:

- `RuntimeAsyncNet11.ravenproj`
- Target framework: `net11.0`

Source file:

- `src/main.rav`

## Build and run

From this folder:

```bash
dotnet run -f net11.0 --project ../../../src/Raven.Compiler --property WarningLevel=0 -- RuntimeAsyncNet11.ravenproj --run
```

Runtime-async metadata emission is auto-enabled for `net11.0` targets. You can force behavior with:

```bash
# Force on
dotnet run -f net11.0 --project ../../../src/Raven.Compiler --property WarningLevel=0 -- RuntimeAsyncNet11.ravenproj --runtime-async --run

# Force off
dotnet run -f net11.0 --project ../../../src/Raven.Compiler --property WarningLevel=0 -- RuntimeAsyncNet11.ravenproj --no-runtime-async --run
```

## Inspect emitted IL

From this folder:

```bash
ilspycmd -il ./bin/RuntimeAsyncNet11.dll | rg "AsyncHelpers::Await|GetAwaiter|GetResult|flag\\(2000\\)"
```

In runtime-async mode you should see `AsyncHelpers::Await...` and `flag(2000)` on async methods.

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
- Entry-point bridge still blocks with `GetAwaiter`/`GetResult` (synchronous wrapper behavior).
