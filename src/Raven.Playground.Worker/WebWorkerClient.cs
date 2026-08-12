using Microsoft.JSInterop;

namespace Raven.Playground.Worker;

public sealed class WebWorkerClient(IJSObjectReference worker) : IAsyncDisposable
{
    private const int DefaultTimeoutMs = 60000;
    private static readonly string DefaultAssemblyName = typeof(WebWorkerClient).Assembly.GetName().Name!;

    public static async Task<WebWorkerClient> CreateAsync(
        IJSRuntime jsRuntime,
        int timeoutMs = DefaultTimeoutMs,
        string? assemblyName = null,
        CancellationToken cancellationToken = default)
    {
        try
        {
            await using var module = await jsRuntime.InvokeAsync<IJSObjectReference>(
                "import",
                cancellationToken,
                "./_content/Raven.Playground.Worker/dotnet-web-worker-client.js");

            var resolvedName = assemblyName ?? DefaultAssemblyName;
            var options = new { assemblyName = resolvedName };
            var workerReference = await module.InvokeAsync<IJSObjectReference>(
                "create",
                cancellationToken,
                timeoutMs,
                options);

            return new WebWorkerClient(workerReference);
        }
        catch (JSException exception)
        {
            throw new InvalidOperationException("Unable to create the Playground compiler worker.", exception);
        }
    }

    public async Task<TResult> InvokeAsync<TResult>(
        string method,
        object[] arguments,
        int timeoutMs = DefaultTimeoutMs,
        CancellationToken cancellationToken = default)
    {
        try
        {
            return await worker.InvokeAsync<TResult>(
                "invoke",
                cancellationToken,
                [method, arguments, timeoutMs]);
        }
        catch (JSException exception)
        {
            throw new InvalidOperationException("Unable to invoke the Playground compiler worker.", exception);
        }
    }

    public async ValueTask DisposeAsync()
    {
        try
        {
            await worker.InvokeVoidAsync("terminate");
        }
        catch (JSDisconnectedException)
        {
        }

        await worker.DisposeAsync();
    }
}
