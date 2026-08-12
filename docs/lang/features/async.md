# Write asynchronous code

Raven uses .NET's task and async-stream types with `async`, `await`, and
`await for`.

## Await a task

Mark a function `async` and await the asynchronous operation:

```raven
import System.Net.Http.*
import System.Threading.Tasks.*

async func DownloadLength(url: string) -> Task<int> {
    use http = HttpClient()
    let text = await http.GetStringAsync(url)
    return text.Length
}
```

The return type remains a normal .NET `Task<T>`, so Raven functions can call
and be called by existing .NET libraries.

## Consume an async stream

Use `await for` with `IAsyncEnumerable<T>`:

```raven
await for reading in telemetry.Poll(cancellationToken) {
    Console.WriteLine(reading)
}
```

An async iterator can produce values with `yield` and await between them. This
works well for streaming endpoints, telemetry, and incremental data sources.

## Carry cancellation across boundaries

Accept a `CancellationToken` when the caller owns the operation lifetime and
pass it to the .NET APIs you await. Do not replace cancellation with a custom
boolean flag when the surrounding framework already uses tokens.

The [Web API guide](../../workloads/web-api.md) includes an async route handler
and a streaming handler. The [IoT monitor](../../workloads/iot-monitor.md)
consumes a cancellable telemetry stream.
