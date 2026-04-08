using System.Collections.Concurrent;
using System.Diagnostics;
using System.Linq;

using MediatR;

using Microsoft.Extensions.Logging;

using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using OmniSharp.Extensions.LanguageServer.Protocol.Server;

using Raven.CodeAnalysis.Text;

using SaveOptions = OmniSharp.Extensions.LanguageServer.Protocol.Server.Capabilities.SaveOptions;
using TextDocumentSelector = OmniSharp.Extensions.LanguageServer.Protocol.Models.TextDocumentSelector;
using TextDocumentSyncKind = OmniSharp.Extensions.LanguageServer.Protocol.Server.Capabilities.TextDocumentSyncKind;

namespace Raven.LanguageServer;

internal sealed class RavenTextDocumentSyncHandler : TextDocumentSyncHandlerBase
{
    private const int AnalysisWarmupDebounceMilliseconds = 75;
    private const int DiagnosticsDebounceMilliseconds = 250;
    private const double DidCloseLogThresholdMs = 50;

    private readonly DocumentStore _documents;
    private readonly ILanguageServerFacade _languageServer;
    private readonly ILogger<RavenTextDocumentSyncHandler> _logger;
    private readonly ConcurrentDictionary<DocumentUri, CancellationTokenSource> _pendingDiagnostics = new();
    private readonly ConcurrentDictionary<DocumentUri, SemaphoreSlim> _documentUpdateGates = new();
    private readonly ConcurrentDictionary<DocumentUri, int> _documentVersions = new();

    public RavenTextDocumentSyncHandler(DocumentStore documents, ILanguageServerFacade languageServer, ILogger<RavenTextDocumentSyncHandler> logger)
    {
        _documents = documents;
        _languageServer = languageServer;
        _logger = logger;
    }

    public override TextDocumentAttributes GetTextDocumentAttributes(DocumentUri uri)
        => new(uri, "raven");

    public override Task<Unit> Handle(DidOpenTextDocumentParams notification, CancellationToken cancellationToken)
        => HandleDidOpenAsync(notification, cancellationToken);

    private async Task<Unit> HandleDidOpenAsync(DidOpenTextDocumentParams notification, CancellationToken cancellationToken)
    {
        try
        {
            _documents.UpsertDocument(notification.TextDocument.Uri, notification.TextDocument.Text);
            if (notification.TextDocument.Version is { } openVersion)
                _documentVersions[notification.TextDocument.Uri] = openVersion;
            _logger.LogDebug(
                "DidOpen {Uri} (version={Version}, length={Length}).",
                notification.TextDocument.Uri,
                notification.TextDocument.Version,
                notification.TextDocument.Text?.Length ?? 0);

            return await ScheduleDiagnosticsPublishAsync(notification.TextDocument.Uri).ConfigureAwait(false);
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            return Unit.Value;
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "DidOpen handling failed for {Uri}.", notification.TextDocument.Uri);
            return Unit.Value;
        }
    }

    public override Task<Unit> Handle(DidChangeTextDocumentParams notification, CancellationToken cancellationToken)
        => HandleDidChangeAsync(notification, cancellationToken);

    private async Task<Unit> HandleDidChangeAsync(DidChangeTextDocumentParams notification, CancellationToken cancellationToken)
    {
        if (IsStaleChange(notification.TextDocument.Uri, notification.TextDocument.Version))
            return Unit.Value;

        var gate = _documentUpdateGates.GetOrAdd(notification.TextDocument.Uri, _ => new SemaphoreSlim(1, 1));
        await gate.WaitAsync(cancellationToken).ConfigureAwait(false);

        try
        {
            if (IsStaleChange(notification.TextDocument.Uri, notification.TextDocument.Version))
                return Unit.Value;

            var currentText = await GetCurrentDocumentTextAsync(notification.TextDocument.Uri, cancellationToken).ConfigureAwait(false);
            var updatedText = ApplyContentChanges(currentText, notification.ContentChanges);
            _documents.UpsertDocument(notification.TextDocument.Uri, updatedText);
            if (notification.TextDocument.Version is { } appliedVersion)
                _documentVersions[notification.TextDocument.Uri] = appliedVersion;
            _logger.LogInformation(
                "DidChange applied for {Uri} (version={Version}, changeCount={ChangeCount}, previousLength={PreviousLength}, updatedLength={UpdatedLength}).",
                notification.TextDocument.Uri,
                notification.TextDocument.Version?.ToString() ?? "<none>",
                notification.ContentChanges.Count(),
                currentText.Length,
                updatedText.Length);
            _logger.LogDebug(
                "DidChange {Uri} version={Version} changes={ChangeCount} currentLength={CurrentLength} updatedLength={UpdatedLength}.",
                notification.TextDocument.Uri,
                notification.TextDocument.Version,
                notification.ContentChanges.Count(),
                currentText.Length,
                updatedText.Length);
            return await ScheduleDiagnosticsPublishAsync(notification.TextDocument.Uri).ConfigureAwait(false);
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            return Unit.Value;
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "DidChange handling failed for {Uri}.", notification.TextDocument.Uri);
            return Unit.Value;
        }
        finally
        {
            gate.Release();
        }
    }

    private bool IsStaleChange(DocumentUri uri, int? incomingVersion)
    {
        if (incomingVersion is not { } version ||
            !_documentVersions.TryGetValue(uri, out var currentVersion) ||
            version >= currentVersion)
        {
            return false;
        }

        _logger.LogDebug(
            "DidChange ignored for {Uri}: stale version {IncomingVersion} < {CurrentVersion}.",
            uri,
            version,
            currentVersion);
        return true;
    }

    private async Task<string> GetCurrentDocumentTextAsync(DocumentUri uri, CancellationToken cancellationToken)
    {
        if (!_documents.TryGetDocument(uri, out var document))
            return string.Empty;

        var sourceText = await document.GetTextAsync(cancellationToken).ConfigureAwait(false);
        return sourceText?.ToString() ?? string.Empty;
    }

    private static string ApplyContentChanges(string currentText, IEnumerable<TextDocumentContentChangeEvent> contentChanges)
    {
        var text = currentText;

        foreach (var change in contentChanges)
        {
            if (change.Range is null)
            {
                text = change.Text ?? string.Empty;
                continue;
            }

            var source = SourceText.From(text);
            var start = PositionHelper.ToOffset(source, change.Range.Start);
            var end = PositionHelper.ToOffset(source, change.Range.End);

            start = Math.Clamp(start, 0, text.Length);
            end = Math.Clamp(end, start, text.Length);

            text = string.Concat(
                text.AsSpan(0, start),
                (change.Text ?? string.Empty).AsSpan(),
                text.AsSpan(end));
        }

        return text;
    }

    public override Task<Unit> Handle(DidCloseTextDocumentParams notification, CancellationToken cancellationToken)
        => HandleDidCloseAsync(notification, cancellationToken);

    private async Task<Unit> HandleDidCloseAsync(DidCloseTextDocumentParams notification, CancellationToken cancellationToken)
    {
        var stopwatch = Stopwatch.StartNew();

        try
        {
            _logger.LogInformation("DidClose started for {Uri}.", notification.TextDocument.Uri);
            CancelPendingDiagnostics(notification.TextDocument.Uri);
            _documentVersions.TryRemove(notification.TextDocument.Uri, out _);

            if (_documentUpdateGates.TryRemove(notification.TextDocument.Uri, out var gate))
                gate.Dispose();

            _languageServer.TextDocument.PublishDiagnostics(new PublishDiagnosticsParams
            {
                Uri = notification.TextDocument.Uri,
                Diagnostics = new Container<Diagnostic>()
            });

            _ = Task.Run(async () =>
            {
                try
                {
                    _documents.RemoveDocument(notification.TextDocument.Uri);
                    _logger.LogInformation("Deferred DidClose cleanup completed for {Uri}.", notification.TextDocument.Uri);
                }
                catch (Exception ex)
                {
                    _logger.LogDebug(ex, "Deferred DidClose cleanup failed for {Uri}.", notification.TextDocument.Uri);
                }
            }, CancellationToken.None);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "DidClose handling failed for {Uri}.", notification.TextDocument.Uri);
        }
        finally
        {
            stopwatch.Stop();
            if (stopwatch.Elapsed.TotalMilliseconds >= DidCloseLogThresholdMs)
            {
                _logger.LogInformation(
                    "DidClose completed for {Uri} in {ElapsedMs:F1}ms.",
                    notification.TextDocument.Uri,
                    stopwatch.Elapsed.TotalMilliseconds);
            }
            else
            {
                _logger.LogDebug(
                    "DidClose completed for {Uri} in {ElapsedMs:F1}ms.",
                    notification.TextDocument.Uri,
                    stopwatch.Elapsed.TotalMilliseconds);
            }
        }

        return Unit.Value;
    }

    public override async Task<Unit> Handle(DidSaveTextDocumentParams notification, CancellationToken cancellationToken)
    {
        _logger.LogDebug("DidSave {Uri}.", notification.TextDocument.Uri);
        return await ScheduleDiagnosticsPublishAsync(
            notification.TextDocument.Uri,
            warmupDelayMilliseconds: 0,
            diagnosticsDelayMilliseconds: DiagnosticsDebounceMilliseconds).ConfigureAwait(false);
    }

    protected override TextDocumentSyncRegistrationOptions CreateRegistrationOptions(TextSynchronizationCapability capability, ClientCapabilities clientCapabilities)
        => new()
        {
            DocumentSelector = TextDocumentSelector.ForLanguage("raven"),
            Change = TextDocumentSyncKind.Incremental,
            Save = new SaveOptions
            {
                IncludeText = true
            }
        };

    private Task<Unit> ScheduleDiagnosticsPublishAsync(
        DocumentUri uri,
        int warmupDelayMilliseconds = AnalysisWarmupDebounceMilliseconds,
        int diagnosticsDelayMilliseconds = DiagnosticsDebounceMilliseconds)
    {
        var source = new CancellationTokenSource();
        var token = source.Token;
        int? expectedVersion = _documentVersions.TryGetValue(uri, out var currentVersion)
            ? currentVersion
            : null;
        _logger.LogInformation(
            "Scheduling diagnostics for {Uri} (expectedVersion={ExpectedVersion}, warmupDelay={WarmupDelayMs}ms, diagnosticsDelay={DiagnosticsDelayMs}ms).",
            uri,
            expectedVersion?.ToString() ?? "<none>",
            warmupDelayMilliseconds,
            diagnosticsDelayMilliseconds);
        var current = _pendingDiagnostics.AddOrUpdate(
            uri,
            source,
            (_, previous) =>
            {
                try
                {
                    previous.Cancel();
                }
                catch (ObjectDisposedException)
                {
                }

                return source;
            });

        if (!ReferenceEquals(current, source))
        {
            source.Dispose();
            return Task.FromResult(Unit.Value);
        }

        _ = Task.Run(async () =>
        {
            try
            {
                if (warmupDelayMilliseconds > 0)
                    await Task.Delay(warmupDelayMilliseconds, token).ConfigureAwait(false);

                if (ShouldSkipVersion(uri, expectedVersion))
                    return;

                _logger.LogInformation(
                    "Starting analysis warmup for {Uri} (expectedVersion={ExpectedVersion}, warmupDelay={WarmupDelayMs}ms, diagnosticsDelay={DiagnosticsDelayMs}ms).",
                    uri,
                    expectedVersion?.ToString() ?? "<none>",
                    warmupDelayMilliseconds,
                    diagnosticsDelayMilliseconds);
                await _documents.WarmAnalysisAsync(
                    uri,
                    shouldSkipWork: () => token.IsCancellationRequested || ShouldSkipVersion(uri, expectedVersion),
                    token).ConfigureAwait(false);
                _logger.LogInformation(
                    "Completed analysis warmup for {Uri} (expectedVersion={ExpectedVersion}).",
                    uri,
                    expectedVersion?.ToString() ?? "<none>");

                var remainingDiagnosticsDelay = diagnosticsDelayMilliseconds - warmupDelayMilliseconds;
                if (remainingDiagnosticsDelay > 0)
                    await Task.Delay(remainingDiagnosticsDelay, token).ConfigureAwait(false);

                if (ShouldSkipVersion(uri, expectedVersion))
                    return;

                await PublishDiagnosticsAsync(uri, token, expectedVersion).ConfigureAwait(false);
            }
            catch (OperationCanceledException)
            {
                _logger.LogDebug("Debounced diagnostics publish canceled for {Uri}.", uri);
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Debounced diagnostics publish failed for {Uri}.", uri);
            }
            finally
            {
                if (_pendingDiagnostics.TryGetValue(uri, out var active) && ReferenceEquals(active, source))
                    _pendingDiagnostics.TryRemove(uri, out _);

                try
                {
                    source.Dispose();
                }
                catch (ObjectDisposedException)
                {
                }
            }
        }, CancellationToken.None);

        return Task.FromResult(Unit.Value);
    }

    private bool ShouldSkipVersion(DocumentUri uri, int? expectedVersion)
    {
        if (expectedVersion is not { } expected ||
            !_documentVersions.TryGetValue(uri, out var latestVersion))
        {
            return false;
        }

        if (latestVersion == expected)
            return false;

        _logger.LogDebug(
            "Skipping background analysis for {Uri}: computed for version {ExpectedVersion}, latest is {LatestVersion}.",
            uri,
            expected,
            latestVersion);
        return true;
    }

    private void CancelPendingDiagnostics(DocumentUri uri)
    {
        if (_pendingDiagnostics.TryRemove(uri, out var pending))
        {
            try
            {
                pending.Cancel();
            }
            catch (ObjectDisposedException)
            {
            }
        }
    }

    private async Task<Unit> PublishDiagnosticsAsync(DocumentUri uri, CancellationToken cancellationToken, int? expectedVersion = null)
    {
        try
        {
            if (ShouldSkipVersion(uri, expectedVersion))
                return Unit.Value;

            var diagnostics = await _documents.GetDiagnosticsAsync(
                uri,
                shouldSkipWork: () => cancellationToken.IsCancellationRequested || ShouldSkipVersion(uri, expectedVersion),
                cancellationToken).ConfigureAwait(false);

            if (expectedVersion is { } expected &&
                _documentVersions.TryGetValue(uri, out var latestVersion) &&
                latestVersion != expected)
            {
                _logger.LogDebug(
                    "Skipping diagnostics publish for {Uri}: computed for version {ExpectedVersion}, latest is {LatestVersion}.",
                    uri,
                    expected,
                    latestVersion);
                return Unit.Value;
            }

            _languageServer.TextDocument.PublishDiagnostics(new PublishDiagnosticsParams
            {
                Uri = uri,
                Diagnostics = new Container<Diagnostic>(diagnostics)
            });
            _logger.LogInformation(
                "Published diagnostics for {Uri} (expectedVersion={ExpectedVersion}, count={Count}).",
                uri,
                expectedVersion?.ToString() ?? "<none>",
                diagnostics.Count);
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            return Unit.Value;
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Publishing diagnostics failed for {Uri}.", uri);
        }

        return Unit.Value;
    }
}
