using System.Collections.Concurrent;
using System.Collections.Immutable;
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
    private const int FullDiagnosticsAfterSaveDelayMilliseconds = 350;
    private const int DiagnosticsRetryDelayMilliseconds = 150;
    private const double DidCloseLogThresholdMs = 50;

    private readonly DocumentStore _documents;
    private readonly HoverHandler _hoverHandler;
    private readonly ILanguageServerFacade _languageServer;
    private readonly ILogger<RavenTextDocumentSyncHandler> _logger;
    private readonly ConcurrentDictionary<DocumentUri, CancellationTokenSource> _pendingDiagnostics = new();
    private readonly ConcurrentDictionary<DocumentUri, SemaphoreSlim> _documentUpdateGates = new();
    private readonly ConcurrentDictionary<DocumentUri, long> _documentSessions = new();
    private readonly ConcurrentDictionary<DocumentUri, int> _documentVersions = new();
    private readonly ConcurrentDictionary<DocumentUri, int> _lastPublishedDiagnosticVersions = new();
    private readonly ConcurrentDictionary<DocumentUri, ImmutableArray<PublishedDiagnosticValue>> _lastPublishedDiagnostics = new();

    public RavenTextDocumentSyncHandler(DocumentStore documents, HoverHandler hoverHandler, ILanguageServerFacade languageServer, ILogger<RavenTextDocumentSyncHandler> logger)
    {
        _documents = documents;
        _hoverHandler = hoverHandler;
        _languageServer = languageServer;
        _logger = logger;
    }

    public override TextDocumentAttributes GetTextDocumentAttributes(DocumentUri uri)
        => new(uri, "raven");

    public override Task<Unit> Handle(DidOpenTextDocumentParams notification, CancellationToken cancellationToken)
        => HandleDidOpenAsync(notification, cancellationToken);

    private async Task<Unit> HandleDidOpenAsync(DidOpenTextDocumentParams notification, CancellationToken cancellationToken)
    {
        var stopwatch = Stopwatch.StartNew();

        try
        {
            _ = AdvanceDocumentSession(notification.TextDocument.Uri);
            _hoverHandler.InvalidateDocument(notification.TextDocument.Uri);
            _documents.UpsertDocument(notification.TextDocument.Uri, notification.TextDocument.Text);
            if (notification.TextDocument.Version is { } openVersion)
                _documentVersions[notification.TextDocument.Uri] = openVersion;
            LanguageServerPerformanceInstrumentation.RecordDocumentEdit(
                notification.TextDocument.Uri,
                notification.TextDocument.Version,
                "didOpen");
            _logger.LogDebug(
                "DidOpen {Uri} (version={Version}, length={Length}).",
                notification.TextDocument.Uri,
                notification.TextDocument.Version,
                notification.TextDocument.Text?.Length ?? 0);

            var result = await ScheduleDiagnosticsPublishAsync(notification.TextDocument.Uri).ConfigureAwait(false);
            stopwatch.Stop();
            LanguageServerPerformanceInstrumentation.RecordOperation(
                "didOpen",
                notification.TextDocument.Uri,
                notification.TextDocument.Version,
                stopwatch.Elapsed.TotalMilliseconds);
            return result;
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

        var totalStopwatch = Stopwatch.StartNew();
        var gateWaitStopwatch = Stopwatch.StartNew();
        double gateWaitMs = 0;
        double getCurrentTextMs = 0;
        double applyChangesMs = 0;
        double updateDocumentMs = 0;
        double scheduleMs = 0;
        var gate = _documentUpdateGates.GetOrAdd(notification.TextDocument.Uri, _ => new SemaphoreSlim(1, 1));
        await gate.WaitAsync(cancellationToken).ConfigureAwait(false);
        gateWaitMs = gateWaitStopwatch.Elapsed.TotalMilliseconds;

        try
        {
            if (IsStaleChange(notification.TextDocument.Uri, notification.TextDocument.Version))
                return Unit.Value;

            var stageStopwatch = Stopwatch.StartNew();
            var currentText = await GetCurrentDocumentTextAsync(notification.TextDocument.Uri, cancellationToken).ConfigureAwait(false);
            getCurrentTextMs = stageStopwatch.Elapsed.TotalMilliseconds;

            stageStopwatch.Restart();
            var updatedText = ApplyContentChanges(currentText, notification.ContentChanges);
            applyChangesMs = stageStopwatch.Elapsed.TotalMilliseconds;

            stageStopwatch.Restart();
            _hoverHandler.InvalidateDocument(notification.TextDocument.Uri);
            _documents.UpsertDocument(notification.TextDocument.Uri, updatedText);
            if (notification.TextDocument.Version is { } appliedVersion)
                _documentVersions[notification.TextDocument.Uri] = appliedVersion;
            updateDocumentMs = stageStopwatch.Elapsed.TotalMilliseconds;
            LanguageServerPerformanceInstrumentation.RecordDocumentEdit(
                notification.TextDocument.Uri,
                notification.TextDocument.Version,
                "didChange");
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
            var policy = GetEditDiagnosticsPolicy();
            stageStopwatch.Restart();
            var result = await ScheduleDiagnosticsPublishAsync(
                notification.TextDocument.Uri,
                includeWarmup: policy.IncludeWarmup,
                warmupDelayMilliseconds: AnalysisWarmupDebounceMilliseconds,
                initialDiagnosticsMode: policy.InitialMode,
                fullDiagnosticsDelayMilliseconds: policy.FullDiagnosticsDelayMilliseconds,
                diagnosticsDelayMilliseconds: policy.DiagnosticsDelayMilliseconds).ConfigureAwait(false);
            scheduleMs = stageStopwatch.Elapsed.TotalMilliseconds;
            return result;
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
            totalStopwatch.Stop();
            LanguageServerPerformanceInstrumentation.RecordOperation(
                "didChange",
                notification.TextDocument.Uri,
                notification.TextDocument.Version,
                totalStopwatch.Elapsed.TotalMilliseconds,
                stages:
                [
                    new LanguageServerPerformanceInstrumentation.StageTiming("gateWait", gateWaitMs),
                    new LanguageServerPerformanceInstrumentation.StageTiming("getCurrentText", getCurrentTextMs),
                    new LanguageServerPerformanceInstrumentation.StageTiming("applyChanges", applyChangesMs),
                    new LanguageServerPerformanceInstrumentation.StageTiming("updateDocument", updateDocumentMs),
                    new LanguageServerPerformanceInstrumentation.StageTiming("scheduleDiagnostics", scheduleMs)
                ]);
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

    private Task<Unit> HandleDidCloseAsync(DidCloseTextDocumentParams notification, CancellationToken cancellationToken)
    {
        var stopwatch = Stopwatch.StartNew();

        try
        {
            _logger.LogInformation("DidClose started for {Uri}.", notification.TextDocument.Uri);
            _ = AdvanceDocumentSession(notification.TextDocument.Uri);
            _hoverHandler.InvalidateDocument(notification.TextDocument.Uri);
            CancelPendingDiagnostics(notification.TextDocument.Uri);
            _documentVersions.TryRemove(notification.TextDocument.Uri, out _);
            _lastPublishedDiagnosticVersions.TryRemove(notification.TextDocument.Uri, out _);
            _lastPublishedDiagnostics.TryRemove(notification.TextDocument.Uri, out _);

            if (_documentUpdateGates.TryRemove(notification.TextDocument.Uri, out var gate))
                gate.Dispose();

            _languageServer.TextDocument.PublishDiagnostics(new PublishDiagnosticsParams
            {
                Uri = notification.TextDocument.Uri,
                Diagnostics = new Container<Diagnostic>()
            });

            _documents.RemoveDocument(notification.TextDocument.Uri);
            _logger.LogInformation("DidClose cleanup completed for {Uri}.", notification.TextDocument.Uri);
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

        return Task.FromResult(Unit.Value);
    }

    public override async Task<Unit> Handle(DidSaveTextDocumentParams notification, CancellationToken cancellationToken)
    {
        _logger.LogDebug("DidSave {Uri}.", notification.TextDocument.Uri);
        var policy = GetSaveDiagnosticsPolicy();
        return await ScheduleDiagnosticsPublishAsync(
            notification.TextDocument.Uri,
            includeWarmup: policy.IncludeWarmup,
            warmupDelayMilliseconds: 0,
            initialDiagnosticsMode: policy.InitialMode,
            fullDiagnosticsDelayMilliseconds: policy.FullDiagnosticsDelayMilliseconds,
            diagnosticsDelayMilliseconds: policy.DiagnosticsDelayMilliseconds).ConfigureAwait(false);
    }

    internal static SaveDiagnosticsPolicy GetSaveDiagnosticsPolicy()
        => new(
            IncludeWarmup: false,
            InitialMode: DocumentStore.DocumentDiagnosticsMode.SyntaxOnly,
            FullDiagnosticsDelayMilliseconds: FullDiagnosticsAfterSaveDelayMilliseconds,
            DiagnosticsDelayMilliseconds: 0);

    internal static SaveDiagnosticsPolicy GetEditDiagnosticsPolicy()
        => new(
            IncludeWarmup: true,
            InitialMode: DocumentStore.DocumentDiagnosticsMode.SyntaxOnly,
            FullDiagnosticsDelayMilliseconds: null,
            DiagnosticsDelayMilliseconds: DiagnosticsDebounceMilliseconds);

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
        bool includeWarmup = true,
        int warmupDelayMilliseconds = AnalysisWarmupDebounceMilliseconds,
        DocumentStore.DocumentDiagnosticsMode initialDiagnosticsMode = DocumentStore.DocumentDiagnosticsMode.Full,
        int? fullDiagnosticsDelayMilliseconds = null,
        int diagnosticsDelayMilliseconds = DiagnosticsDebounceMilliseconds)
    {
        var source = new CancellationTokenSource();
        var token = source.Token;
        var expectedSession = GetOrCreateDocumentSession(uri);
        int? expectedVersion = _documentVersions.TryGetValue(uri, out var currentVersion)
            ? currentVersion
            : null;
        if (ShouldClearStaleDiagnostics(expectedVersion, _lastPublishedDiagnosticVersions.TryGetValue(uri, out var lastPublishedVersion) ? lastPublishedVersion : null))
        {
            _languageServer.TextDocument.PublishDiagnostics(new PublishDiagnosticsParams
            {
                Uri = uri,
                Diagnostics = new Container<Diagnostic>()
            });
            _lastPublishedDiagnostics[uri] = ImmutableArray<PublishedDiagnosticValue>.Empty;
        }

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
                if (includeWarmup)
                {
                    if (warmupDelayMilliseconds > 0)
                        await Task.Delay(warmupDelayMilliseconds, token).ConfigureAwait(false);

                    if (ShouldSkipRequest(uri, expectedSession, expectedVersion))
                        return;

                    _logger.LogInformation(
                        "Starting analysis warmup for {Uri} (expectedVersion={ExpectedVersion}, warmupDelay={WarmupDelayMs}ms, diagnosticsDelay={DiagnosticsDelayMs}ms).",
                        uri,
                        expectedVersion?.ToString() ?? "<none>",
                        warmupDelayMilliseconds,
                        diagnosticsDelayMilliseconds);
                    await _documents.WarmAnalysisAsync(
                        uri,
                        shouldSkipWork: () => token.IsCancellationRequested || ShouldSkipRequest(uri, expectedSession, expectedVersion),
                        token).ConfigureAwait(false);
                    _logger.LogInformation(
                        "Completed analysis warmup for {Uri} (expectedVersion={ExpectedVersion}).",
                        uri,
                        expectedVersion?.ToString() ?? "<none>");

                    var remainingDiagnosticsDelay = diagnosticsDelayMilliseconds - warmupDelayMilliseconds;
                    if (remainingDiagnosticsDelay > 0)
                        await Task.Delay(remainingDiagnosticsDelay, token).ConfigureAwait(false);
                }
                else if (diagnosticsDelayMilliseconds > 0)
                {
                    await Task.Delay(diagnosticsDelayMilliseconds, token).ConfigureAwait(false);
                }

                if (ShouldSkipRequest(uri, expectedSession, expectedVersion))
                    return;

                await PublishDiagnosticsAsync(uri, token, expectedSession, expectedVersion, initialDiagnosticsMode).ConfigureAwait(false);

                if (fullDiagnosticsDelayMilliseconds is { } followUpDelay &&
                    initialDiagnosticsMode != DocumentStore.DocumentDiagnosticsMode.Full)
                {
                    await Task.Delay(followUpDelay, token).ConfigureAwait(false);

                    if (ShouldSkipRequest(uri, expectedSession, expectedVersion))
                        return;

                    await PublishDiagnosticsAsync(uri, token, expectedSession, expectedVersion, DocumentStore.DocumentDiagnosticsMode.Full).ConfigureAwait(false);
                }
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

    private long AdvanceDocumentSession(DocumentUri uri)
        => _documentSessions.AddOrUpdate(uri, 1, static (_, current) => current + 1);

    private long GetOrCreateDocumentSession(DocumentUri uri)
        => _documentSessions.GetOrAdd(uri, 1);

    private bool ShouldSkipRequest(DocumentUri uri, long expectedSession, int? expectedVersion)
    {
        var latestSession = _documentSessions.TryGetValue(uri, out var currentSession)
            ? currentSession
            : (long?)null;
        var latestVersion = _documentVersions.TryGetValue(uri, out var currentVersion)
            ? currentVersion
            : (int?)null;

        if (!ShouldSkipDiagnosticRequest(expectedSession, latestSession, expectedVersion, latestVersion))
        {
            return false;
        }

        if (latestSession != expectedSession)
        {
            _logger.LogDebug(
                "Skipping background analysis for {Uri}: computed for session {ExpectedSession}, latest is {LatestSession}.",
                uri,
                expectedSession,
                latestSession?.ToString() ?? "<none>");
            return true;
        }

        _logger.LogDebug(
            "Skipping background analysis for {Uri}: computed for version {ExpectedVersion}, latest is {LatestVersion}.",
            uri,
            expectedVersion?.ToString() ?? "<none>",
            latestVersion?.ToString() ?? "<none>");
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

    private async Task<Unit> PublishDiagnosticsAsync(
        DocumentUri uri,
        CancellationToken cancellationToken,
        long expectedSession,
        int? expectedVersion = null,
        DocumentStore.DocumentDiagnosticsMode mode = DocumentStore.DocumentDiagnosticsMode.Full)
    {
        var stopwatch = Stopwatch.StartNew();
        int diagnosticsCount = 0;
        var outcome = PublishDiagnosticsOutcome.Published;

        try
        {
            if (ShouldSkipRequest(uri, expectedSession, expectedVersion))
            {
                outcome = PublishDiagnosticsOutcome.SkippedVersionMismatch;
                return Unit.Value;
            }

            var result = await _documents.TryGetDiagnosticsAsync(
                uri,
                mode,
                shouldSkipWork: () => cancellationToken.IsCancellationRequested || ShouldSkipRequest(uri, expectedSession, expectedVersion),
                cancellationToken).ConfigureAwait(false);
            if (result.WasSkipped)
            {
                outcome = PublishDiagnosticsOutcome.SkippedRequeued;
                RequeueDiagnosticsPublish(uri, expectedSession, expectedVersion, mode, DiagnosticsRetryDelayMilliseconds);
                return Unit.Value;
            }

            var diagnostics = result.Diagnostics;
            var diagnosticValues = CreatePublishedDiagnosticValues(diagnostics);

            if (ShouldSkipRequest(uri, expectedSession, expectedVersion))
            {
                outcome = PublishDiagnosticsOutcome.SkippedVersionMismatch;
                _logger.LogInformation(
                    "Skipped diagnostics publish for {Uri}: request is stale (expectedSession={ExpectedSession}, expectedVersion={ExpectedVersion}).",
                    uri,
                    expectedSession,
                    expectedVersion?.ToString() ?? "<none>");
                return Unit.Value;
            }

            if (_lastPublishedDiagnostics.TryGetValue(uri, out var lastPublishedDiagnostics) &&
                lastPublishedDiagnostics.SequenceEqual(diagnosticValues))
            {
                diagnosticsCount = diagnostics.Count;
                if (expectedVersion is { } stableVersion)
                    _lastPublishedDiagnosticVersions[uri] = stableVersion;

                var diagnosticSummary = SummarizeDiagnosticsForLog(diagnostics);
                outcome = PublishDiagnosticsOutcome.SkippedUnchanged;

                _logger.LogInformation(
                    "Skipped diagnostics publish for {Uri}: diagnostic set unchanged (expectedVersion={ExpectedVersion}, count={Count}, summary={Summary}).",
                    uri,
                    expectedVersion?.ToString() ?? "<none>",
                    diagnostics.Count,
                    diagnosticSummary);
                return Unit.Value;
            }

            _languageServer.TextDocument.PublishDiagnostics(new PublishDiagnosticsParams
            {
                Uri = uri,
                Diagnostics = new Container<Diagnostic>(diagnostics)
            });
            diagnosticsCount = diagnostics.Count;
            _lastPublishedDiagnostics[uri] = diagnosticValues;
            if (expectedVersion is { } publishedVersion)
                _lastPublishedDiagnosticVersions[uri] = publishedVersion;
            var publishedDiagnosticSummary = SummarizeDiagnosticsForLog(diagnostics);
            _logger.LogInformation(
                "Published diagnostics for {Uri} (expectedVersion={ExpectedVersion}, count={Count}, summary={Summary}).",
                uri,
                expectedVersion?.ToString() ?? "<none>",
                diagnostics.Count,
                publishedDiagnosticSummary);
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            return Unit.Value;
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Publishing diagnostics failed for {Uri}.", uri);
        }
        finally
        {
            stopwatch.Stop();
            LanguageServerPerformanceInstrumentation.RecordOperation(
                GetPublishDiagnosticsOperationName(mode, outcome),
                uri,
                expectedVersion,
                stopwatch.Elapsed.TotalMilliseconds,
                resultCount: diagnosticsCount);
        }

        return Unit.Value;
    }

    private void RequeueDiagnosticsPublish(
        DocumentUri uri,
        long expectedSession,
        int? expectedVersion,
        DocumentStore.DocumentDiagnosticsMode mode,
        int delayMilliseconds)
    {
        _ = Task.Run(async () =>
        {
            try
            {
                await Task.Delay(delayMilliseconds).ConfigureAwait(false);
                if (ShouldSkipRequest(uri, expectedSession, expectedVersion))
                    return;

                using var retryCancellation = new CancellationTokenSource();
                await PublishDiagnosticsAsync(uri, retryCancellation.Token, expectedSession, expectedVersion, mode).ConfigureAwait(false);
            }
            catch (OperationCanceledException)
            {
            }
            catch (Exception ex)
            {
                _logger.LogDebug(ex, "Deferred diagnostics retry failed for {Uri}.", uri);
            }
        }, CancellationToken.None);
    }

    internal static bool ShouldClearStaleDiagnostics(int? expectedVersion, int? lastPublishedVersion)
        => expectedVersion is { } expected &&
           lastPublishedVersion is { } published &&
           published != expected;

    internal static bool ShouldSkipDiagnosticRequest(
        long expectedSession,
        long? latestSession,
        int? expectedVersion,
        int? latestVersion)
    {
        if (latestSession is not { } currentSession || currentSession != expectedSession)
            return true;

        return expectedVersion is { } expected &&
               latestVersion is { } currentVersion &&
               currentVersion != expected;
    }

    internal static ImmutableArray<PublishedDiagnosticValue> CreatePublishedDiagnosticValues(IReadOnlyList<Diagnostic> diagnostics)
        => diagnostics
            .Select(static diagnostic => PublishedDiagnosticValue.From(diagnostic))
            .OrderBy(static diagnostic => diagnostic)
            .ToImmutableArray();

    internal static string SummarizeDiagnosticsForLog(IReadOnlyList<Diagnostic> diagnostics)
    {
        if (diagnostics.Count == 0)
            return "none";

        return string.Join(
            ", ",
            diagnostics
                .GroupBy(static diagnostic => GetDiagnosticCodeForLog(diagnostic))
                .OrderByDescending(static group => group.Count())
                .ThenBy(static group => group.Key, StringComparer.Ordinal)
                .Select(static group => $"{group.Key}x{group.Count()}"));
    }

    private static string GetDiagnosticCodeForLog(Diagnostic diagnostic)
    {
        var code = diagnostic.Code?.String;
        return string.IsNullOrWhiteSpace(code) ? "<no-code>" : code;
    }

    internal static string GetPublishDiagnosticsOperationName(
        DocumentStore.DocumentDiagnosticsMode mode,
        PublishDiagnosticsOutcome outcome)
        => (mode, outcome) switch
        {
            (DocumentStore.DocumentDiagnosticsMode.SyntaxOnly, PublishDiagnosticsOutcome.Published) => "publishSyntaxDiagnostics",
            (DocumentStore.DocumentDiagnosticsMode.SyntaxOnly, PublishDiagnosticsOutcome.SkippedRequeued) => "publishSyntaxDiagnosticsSkipped",
            (DocumentStore.DocumentDiagnosticsMode.SyntaxOnly, PublishDiagnosticsOutcome.SkippedUnchanged) => "publishSyntaxDiagnosticsUnchanged",
            (DocumentStore.DocumentDiagnosticsMode.SyntaxOnly, PublishDiagnosticsOutcome.SkippedVersionMismatch) => "publishSyntaxDiagnosticsVersionMismatch",
            (_, PublishDiagnosticsOutcome.Published) => "publishDiagnostics",
            (_, PublishDiagnosticsOutcome.SkippedRequeued) => "publishDiagnosticsSkipped",
            (_, PublishDiagnosticsOutcome.SkippedUnchanged) => "publishDiagnosticsUnchanged",
            _ => "publishDiagnosticsVersionMismatch"
        };

    internal readonly record struct SaveDiagnosticsPolicy(
        bool IncludeWarmup,
        DocumentStore.DocumentDiagnosticsMode InitialMode,
        int? FullDiagnosticsDelayMilliseconds,
        int DiagnosticsDelayMilliseconds);

    internal enum PublishDiagnosticsOutcome
    {
        Published,
        SkippedRequeued,
        SkippedUnchanged,
        SkippedVersionMismatch
    }

    internal readonly record struct PublishedDiagnosticValue(
        int StartLine,
        int StartCharacter,
        int EndLine,
        int EndCharacter,
        string Message,
        string? Code,
        DiagnosticSeverity? Severity,
        string? Source,
        string TagsKey) : IComparable<PublishedDiagnosticValue>
    {
        public static PublishedDiagnosticValue From(Diagnostic diagnostic)
            => new(
                diagnostic.Range.Start.Line,
                diagnostic.Range.Start.Character,
                diagnostic.Range.End.Line,
                diagnostic.Range.End.Character,
                diagnostic.Message ?? string.Empty,
                diagnostic.Code?.String,
                diagnostic.Severity,
                diagnostic.Source,
                diagnostic.Tags is null || !diagnostic.Tags.Any()
                    ? string.Empty
                    : string.Join("|", diagnostic.Tags.OrderBy(static tag => (int)tag).Select(static tag => ((int)tag).ToString())));

        public int CompareTo(PublishedDiagnosticValue other)
        {
            var comparison = StartLine.CompareTo(other.StartLine);
            if (comparison != 0)
                return comparison;

            comparison = StartCharacter.CompareTo(other.StartCharacter);
            if (comparison != 0)
                return comparison;

            comparison = EndLine.CompareTo(other.EndLine);
            if (comparison != 0)
                return comparison;

            comparison = EndCharacter.CompareTo(other.EndCharacter);
            if (comparison != 0)
                return comparison;

            comparison = string.Compare(Code, other.Code, StringComparison.Ordinal);
            if (comparison != 0)
                return comparison;

            comparison = Severity.GetValueOrDefault().CompareTo(other.Severity.GetValueOrDefault());
            if (comparison != 0)
                return comparison;

            comparison = string.Compare(Source, other.Source, StringComparison.Ordinal);
            if (comparison != 0)
                return comparison;

            comparison = string.Compare(Message, other.Message, StringComparison.Ordinal);
            if (comparison != 0)
                return comparison;

            return string.Compare(TagsKey, other.TagsKey, StringComparison.Ordinal);
        }
    }
}
