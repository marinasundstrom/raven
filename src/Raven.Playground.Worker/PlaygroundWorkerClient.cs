using System.Text.Json;

using Microsoft.JSInterop;

using Raven.Playground.Services;

namespace Raven.Playground.Worker;

public sealed class PlaygroundWorkerClient(IJSRuntime jsRuntime) : IAsyncDisposable
{
    private const string WorkerMethods = "Raven.Playground.Worker.PlaygroundWorkerMethods";
    private readonly SemaphoreSlim _initializationGate = new(1, 1);
    private WebWorkerClient? _worker;

    public async Task WarmUpAsync(CancellationToken cancellationToken = default) =>
        _ = await GetWorkerAsync(cancellationToken);

    public async Task<IReadOnlyList<PlaygroundCompletionItem>> GetCompletionsAsync(
        string source,
        int position,
        CancellationToken cancellationToken = default)
    {
        var worker = await GetWorkerAsync(cancellationToken);
        var json = await worker.InvokeAsync<string>(
            $"{WorkerMethods}.GetCompletions",
            [source, position],
            cancellationToken: cancellationToken);

        return JsonSerializer.Deserialize<PlaygroundCompletionItem[]>(json) ?? [];
    }

    public async Task<PlaygroundHoverItem?> GetHoverAsync(
        string source,
        int position,
        CancellationToken cancellationToken = default)
    {
        var worker = await GetWorkerAsync(cancellationToken);
        var json = await worker.InvokeAsync<string>(
            $"{WorkerMethods}.GetHover",
            [source, position],
            cancellationToken: cancellationToken);

        return JsonSerializer.Deserialize<PlaygroundHoverItem?>(json);
    }

    public async Task<PlaygroundWorkerResult> CompileAsync(
        string source,
        bool run,
        CancellationToken cancellationToken = default)
    {
        var worker = await GetWorkerAsync(cancellationToken);
        var json = await worker.InvokeAsync<string>(
            $"{WorkerMethods}.Compile",
            [source, run],
            cancellationToken: cancellationToken);

        return JsonSerializer.Deserialize<PlaygroundWorkerResult>(json)
            ?? throw new InvalidOperationException("The Playground compiler worker returned no result.");
    }

    private async Task<WebWorkerClient> GetWorkerAsync(CancellationToken cancellationToken)
    {
        if (_worker is not null)
            return _worker;

        await _initializationGate.WaitAsync(cancellationToken);
        try
        {
            _worker ??= await WebWorkerClient.CreateAsync(jsRuntime, cancellationToken: cancellationToken);
            return _worker;
        }
        finally
        {
            _initializationGate.Release();
        }
    }

    public async ValueTask DisposeAsync()
    {
        if (_worker is not null)
            await _worker.DisposeAsync();

        _initializationGate.Dispose();
    }
}

public sealed record PlaygroundWorkerResult(
    bool Success,
    int AssemblyLength,
    IReadOnlyList<string> Diagnostics,
    int? ExitCode,
    string? Output);
