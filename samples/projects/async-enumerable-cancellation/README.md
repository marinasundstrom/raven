# Async Enumerable Cancellation (.rvnproj)

This sample shows a standalone async iterator with cancellation in Raven.

It demonstrates:

- an async iterator returning `IAsyncEnumerable<int>`
- `[EnumeratorCancellation]` on the iterator's `CancellationToken` parameter
- consumer-side `await for`
- passing the cancellation token through the iterator call: `Count(cts.Token)`

Project file:

- `AsyncEnumerableCancellation.rvnproj`
- Target framework: `net10.0`

Source file:

- `src/main.rvn`

## Build and run

From this folder:

```bash
dotnet run -f net10.0 --project ../../../src/Raven.Compiler --property WarningLevel=0 -- AsyncEnumerableCancellation.rvnproj --run
```

Expected output is a short prefix of the sequence followed by `done`, for
example:

```text
1
2
3
4
done
```

`CancellationTokenSource.CancelAfter(...)` stops the iterator before it yields
the full sequence. The exact cutoff depends on timing.
