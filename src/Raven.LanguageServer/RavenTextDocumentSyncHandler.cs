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
using OmniSharp.Extensions.LanguageServer.Protocol.Workspace;

using Raven.CodeAnalysis.Text;

using SaveOptions = OmniSharp.Extensions.LanguageServer.Protocol.Server.Capabilities.SaveOptions;
using TextDocumentSelector = OmniSharp.Extensions.LanguageServer.Protocol.Models.TextDocumentSelector;
using TextDocumentSyncKind = OmniSharp.Extensions.LanguageServer.Protocol.Server.Capabilities.TextDocumentSyncKind;

namespace Raven.LanguageServer;

internal sealed class RavenTextDocumentSyncHandler : TextDocumentSyncHandlerBase
{
    private const int DiagnosticsDebounceMilliseconds = 900;
    private const int DocumentCompilerDiagnosticsAfterEditDelayMilliseconds = 1_000;
    private const int DocumentCompilerDiagnosticsAfterOpenDelayMilliseconds = 1_000;
    internal const int RelatedDocumentCompilerDiagnosticsAfterEditDelayMilliseconds = DocumentCompilerDiagnosticsAfterEditDelayMilliseconds;
    internal const int RelatedDocumentCompilerDiagnosticsAfterOpenDelayMilliseconds = 10_000;
    private const int DocumentAnalyzerDiagnosticsAfterEditDelayMilliseconds = 2_000;
    private const int DocumentAnalyzerDiagnosticsAfterOpenDelayMilliseconds = 4_000;
    private const int ActiveDiagnosticsRetryDelayMilliseconds = 250;
    private const int DiagnosticsRetryDelayMilliseconds = 2_500;
    internal const int DocumentCommitDebounceMilliseconds = 600;
    private const double DidCloseLogThresholdMs = 50;

    private readonly DocumentStore _documents;
    private readonly ILanguageServerFacade _languageServer;
    private readonly ILogger<RavenTextDocumentSyncHandler> _logger;
    private readonly ConcurrentDictionary<DocumentUri, PendingDiagnosticsRequest> _pendingDiagnostics = new();
    private readonly ConcurrentDictionary<DocumentUri, SemaphoreSlim> _documentUpdateGates = new();
    private readonly ConcurrentDictionary<DocumentUri, long> _documentSessions = new();
    private readonly ConcurrentDictionary<DocumentUri, int> _documentVersions = new();
    private readonly ConcurrentDictionary<DocumentUri, int> _lastPublishedDiagnosticVersions = new();
    private readonly ConcurrentDictionary<DocumentUri, ImmutableArray<PublishedDiagnosticValue>> _lastPublishedDiagnostics = new();
    private readonly ConcurrentDictionary<DocumentUri, VersionedDiagnostics> _lastPublishedAnalyzerDiagnostics = new();
    private readonly ConcurrentDictionary<CompletedDiagnosticsKey, byte> _completedDiagnostics = new();
    private readonly ConcurrentDictionary<CompletedDiagnosticsKey, byte> _activeDiagnostics = new();
    private readonly ConcurrentDictionary<PendingDiagnosticsRetryKey, byte> _pendingDiagnosticsRetries = new();
    private readonly ConcurrentDictionary<DocumentUri, CancellationTokenSource> _pendingDocumentCommits = new();
    private long _nextDiagnosticsScheduleId;

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
        var stopwatch = Stopwatch.StartNew();

        try
        {
            _ = AdvanceDocumentSession(notification.TextDocument.Uri);
            CancelPendingDocumentCommit(notification.TextDocument.Uri);
            _documents.DiscardPendingDocumentChange(notification.TextDocument.Uri);
            var upsertResult = await _documents.UpsertDocumentWithResultAsync(
                notification.TextDocument.Uri,
                SourceText.From(notification.TextDocument.Text ?? string.Empty)).ConfigureAwait(false);
            if (upsertResult.TextChanged)
                ClearCompletedDiagnostics(notification.TextDocument.Uri);

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

            var policy = GetOpenDiagnosticsPolicy();
            var result = await ScheduleDiagnosticsPublishAsync(
                notification.TextDocument.Uri,
                reason: "didOpen",
                includeWarmup: policy.IncludeWarmup,
                warmupDelayMilliseconds: policy.WarmupDelayMilliseconds,
                initialDiagnosticsMode: policy.InitialMode,
                followUpDiagnosticsDelayMilliseconds: policy.FollowUpDiagnosticsDelayMilliseconds,
                followUpDiagnosticsMode: policy.FollowUpMode,
                analyzerFollowUpDiagnosticsDelayMilliseconds: policy.AnalyzerFollowUpDiagnosticsDelayMilliseconds,
                analyzerFollowUpDiagnosticsMode: policy.AnalyzerFollowUpMode,
                diagnosticsDelayMilliseconds: policy.DiagnosticsDelayMilliseconds).ConfigureAwait(false);
            if (upsertResult.ProjectChanged)
            {
                ScheduleRelatedOpenDocumentCompilerDiagnostics(
                    notification.TextDocument.Uri,
                    RelatedDocumentCompilerDiagnosticsAfterOpenDelayMilliseconds,
                    replacePendingDiagnostics: false,
                    reason: "relatedProjectOpen");
            }

            stopwatch.Stop();
            LanguageServerPerformanceInstrumentation.RecordOperation(
                "didOpen",
                notification.TextDocument.Uri,
                notification.TextDocument.Version,
                stopwatch.Elapsed.TotalMilliseconds,
                detail: $"{notification.TextDocument.Uri} version={notification.TextDocument.Version}");
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

    private void ScheduleRelatedOpenDocumentCompilerDiagnostics(
        DocumentUri changedUri,
        int diagnosticsDelayMilliseconds,
        bool replacePendingDiagnostics,
        string reason)
    {
        var scheduledRelatedDiagnostics = false;
        foreach (var uri in _documents.GetOpenDocumentUrisInSameProject(changedUri, excludeSelf: true))
        {
            if (!_documentVersions.ContainsKey(uri))
                continue;

            scheduledRelatedDiagnostics = true;
            ClearCompletedDiagnostics(uri);
            _logger.LogInformation(
                "Scheduling related diagnostics for {Uri} because {ChangedUri} changed (reason={Reason}, delay={DelayMs}ms, replacePending={ReplacePending}).",
                uri,
                changedUri,
                reason,
                diagnosticsDelayMilliseconds,
                replacePendingDiagnostics);
            _ = ScheduleDiagnosticsPublishAsync(
                uri,
                reason: reason,
                includeWarmup: false,
                initialDiagnosticsMode: DocumentStore.DiagnosticLane.DocumentCompiler,
                diagnosticsDelayMilliseconds: diagnosticsDelayMilliseconds,
                replacePendingDiagnostics: replacePendingDiagnostics);
        }

        if (scheduledRelatedDiagnostics)
            RequestInlayHintRefresh(changedUri, reason);
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
            var contentChanges = notification.ContentChanges.ToArray();
            var updatedText = ApplyContentChanges(currentText, contentChanges);
            var shouldScheduleStructuralDiagnostics = ShouldScheduleStructuralDiagnosticsAfterEdit(currentText, contentChanges);
            applyChangesMs = stageStopwatch.Elapsed.TotalMilliseconds;

            stageStopwatch.Restart();
            var textChanged = !currentText.ContentEquals(updatedText);
            if (textChanged)
            {
                _documents.QueuePendingDocumentChange(
                    notification.TextDocument.Uri,
                    updatedText,
                    deferMacroConsumerRefresh: true);
                ClearCompletedDiagnostics(notification.TextDocument.Uri);
                CancelPendingDiagnostics(notification.TextDocument.Uri);
            }

            if (notification.TextDocument.Version is { } appliedVersion)
                _documentVersions[notification.TextDocument.Uri] = appliedVersion;
            updateDocumentMs = stageStopwatch.Elapsed.TotalMilliseconds;
            LanguageServerPerformanceInstrumentation.RecordDocumentEdit(
                notification.TextDocument.Uri,
                notification.TextDocument.Version,
                "didChange");
            _logger.LogInformation(
                "DidChange queued for {Uri} (version={Version}, changeCount={ChangeCount}, previousLength={PreviousLength}, updatedLength={UpdatedLength}, textChanged={TextChanged}).",
                notification.TextDocument.Uri,
                notification.TextDocument.Version?.ToString() ?? "<none>",
                contentChanges.Length,
                currentText.Length,
                updatedText.Length,
                textChanged);
            _logger.LogDebug(
                "DidChange {Uri} version={Version} changes={ChangeCount} currentLength={CurrentLength} updatedLength={UpdatedLength}.",
                notification.TextDocument.Uri,
                notification.TextDocument.Version,
                contentChanges.Length,
                currentText.Length,
                updatedText.Length);
            stageStopwatch.Restart();
            if (textChanged)
            {
                ScheduleDebouncedDocumentCommit(
                    notification.TextDocument.Uri,
                    notification.TextDocument.Version,
                    shouldScheduleStructuralDiagnostics);
            }
            scheduleMs = stageStopwatch.Elapsed.TotalMilliseconds;
            return Unit.Value;
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
                detail: $"{notification.TextDocument.Uri} version={notification.TextDocument.Version}",
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

    private void ScheduleDebouncedDocumentCommit(
        DocumentUri uri,
        int? expectedVersion,
        bool shouldScheduleStructuralDiagnostics)
    {
        var source = new CancellationTokenSource();
        var token = source.Token;
        var expectedSession = GetOrCreateDocumentSession(uri);
        var current = _pendingDocumentCommits.AddOrUpdate(
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
            return;
        }

        _ = Task.Run(async () =>
        {
            try
            {
                await Task.Delay(DocumentCommitDebounceMilliseconds, token).ConfigureAwait(false);
                if (ShouldSkipRequest(uri, expectedSession, expectedVersion))
                    return;

                await CommitPendingDocumentChangeAndScheduleDiagnosticsAsync(
                    uri,
                    expectedSession,
                    expectedVersion,
                    shouldScheduleStructuralDiagnostics,
                    token).ConfigureAwait(false);
            }
            catch (OperationCanceledException)
            {
                _logger.LogDebug("Debounced document commit canceled for {Uri}.", uri);
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Debounced document commit failed for {Uri}.", uri);
            }
            finally
            {
                if (_pendingDocumentCommits.TryGetValue(uri, out var active) && ReferenceEquals(active, source))
                    _pendingDocumentCommits.TryRemove(uri, out _);

                try
                {
                    source.Dispose();
                }
                catch (ObjectDisposedException)
                {
                }
            }
        }, CancellationToken.None);
    }

    private async Task CommitPendingDocumentChangeAndScheduleDiagnosticsAsync(
        DocumentUri uri,
        long expectedSession,
        int? expectedVersion,
        bool shouldScheduleStructuralDiagnostics,
        CancellationToken cancellationToken)
    {
        var stopwatch = Stopwatch.StartNew();
        WorkspaceManager.DocumentUpsertResult? upsertResult = null;

        try
        {
            upsertResult = await _documents.FlushPendingDocumentChangeAsync(uri, cancellationToken).ConfigureAwait(false);
            if (upsertResult is { TextChanged: true })
                ClearCompletedDiagnostics(uri);

            if (ShouldSkipRequest(uri, expectedSession, expectedVersion))
                return;

            if (upsertResult is null or { ProjectChanged: true })
            {
                ScheduleRelatedOpenDocumentCompilerDiagnostics(
                    uri,
                    RelatedDocumentCompilerDiagnosticsAfterEditDelayMilliseconds,
                    replacePendingDiagnostics: true,
                    reason: "relatedProjectEdit");
            }

            var policy = GetEditDiagnosticsPolicy();
            await ScheduleDiagnosticsPublishAsync(
                uri,
                reason: "didChangeCommit",
                includeWarmup: policy.IncludeWarmup,
                warmupDelayMilliseconds: policy.WarmupDelayMilliseconds,
                initialDiagnosticsMode: policy.InitialMode,
                followUpDiagnosticsDelayMilliseconds: shouldScheduleStructuralDiagnostics
                    ? DocumentCompilerDiagnosticsAfterEditDelayMilliseconds
                    : policy.FollowUpDiagnosticsDelayMilliseconds,
                followUpDiagnosticsMode: policy.FollowUpMode,
                analyzerFollowUpDiagnosticsDelayMilliseconds: policy.AnalyzerFollowUpDiagnosticsDelayMilliseconds,
                analyzerFollowUpDiagnosticsMode: policy.AnalyzerFollowUpMode,
                diagnosticsDelayMilliseconds: policy.DiagnosticsDelayMilliseconds).ConfigureAwait(false);
        }
        finally
        {
            stopwatch.Stop();
            LanguageServerPerformanceInstrumentation.RecordOperation(
                "commitPendingDocumentChange",
                uri,
                expectedVersion,
                stopwatch.Elapsed.TotalMilliseconds,
                detail: $"{uri} version={expectedVersion} textChanged={upsertResult?.TextChanged.ToString() ?? "<flushed>"} projectChanged={upsertResult?.ProjectChanged.ToString() ?? "<unknown>"}");
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

    private async Task<SourceText> GetCurrentDocumentTextAsync(DocumentUri uri, CancellationToken cancellationToken)
    {
        if (_documents.TryGetPendingDocumentText(uri, out var pendingText))
            return pendingText;

        if (!_documents.TryGetDocument(uri, out var document))
            return SourceText.From(string.Empty);

        var sourceText = await document.GetTextAsync(cancellationToken).ConfigureAwait(false);
        return sourceText ?? SourceText.From(string.Empty);
    }

    private static SourceText ApplyContentChanges(SourceText currentText, IEnumerable<TextDocumentContentChangeEvent> contentChanges)
    {
        var text = currentText;

        foreach (var change in contentChanges)
        {
            if (change.Range is null)
            {
                text = text.Replace(new TextSpan(0, text.Length), change.Text ?? string.Empty);
                continue;
            }

            var start = PositionHelper.ToOffset(text, change.Range.Start);
            var end = PositionHelper.ToOffset(text, change.Range.End);

            start = Math.Clamp(start, 0, text.Length);
            end = Math.Clamp(end, start, text.Length);

            text = text.Replace(new TextSpan(start, end - start), change.Text ?? string.Empty);
        }

        return text;
    }

    internal static bool ShouldScheduleStructuralDiagnosticsAfterEdit(
        SourceText previousText,
        IReadOnlyList<TextDocumentContentChangeEvent> contentChanges)
    {
        var previousTextString = previousText.ToString();

        foreach (var change in contentChanges)
        {
            if (ContainsImportSensitiveLine(change.Text))
                return true;

            if (change.Range is null)
                return ContainsImportSensitiveLine(previousTextString);

            var start = PositionHelper.ToOffset(previousText, change.Range.Start);
            var end = PositionHelper.ToOffset(previousText, change.Range.End);
            start = Math.Clamp(start, 0, previousText.Length);
            end = Math.Clamp(end, start, previousText.Length);

            var lineStart = start;
            while (lineStart > 0 && previousTextString[lineStart - 1] is not '\r' and not '\n')
                lineStart--;

            var lineEnd = end;
            while (lineEnd < previousTextString.Length && previousTextString[lineEnd] is not '\r' and not '\n')
                lineEnd++;

            if (ContainsImportSensitiveLine(previousTextString[lineStart..lineEnd]))
                return true;
        }

        return false;
    }

    private static bool ContainsImportSensitiveLine(string? text)
    {
        if (string.IsNullOrWhiteSpace(text))
            return false;

        using var reader = new StringReader(text);
        string? line;
        while ((line = reader.ReadLine()) is not null)
        {
            var trimmed = line.TrimStart();
            if (trimmed.StartsWith("import ", StringComparison.Ordinal) ||
                trimmed.StartsWith("alias ", StringComparison.Ordinal) ||
                trimmed.StartsWith("global", StringComparison.Ordinal))
            {
                return true;
            }
        }

        return false;
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
            CancelPendingDocumentCommit(notification.TextDocument.Uri);
            CancelPendingDiagnostics(notification.TextDocument.Uri);
            _documentVersions.TryRemove(notification.TextDocument.Uri, out _);
            _lastPublishedDiagnosticVersions.TryRemove(notification.TextDocument.Uri, out _);
            _lastPublishedDiagnostics.TryRemove(notification.TextDocument.Uri, out _);
            _lastPublishedAnalyzerDiagnostics.TryRemove(notification.TextDocument.Uri, out _);
            ClearCompletedDiagnostics(notification.TextDocument.Uri);

            if (_documentUpdateGates.TryRemove(notification.TextDocument.Uri, out var gate))
                gate.Dispose();

            PublishDiagnosticsToClient(notification.TextDocument.Uri, [], version: null);

            _documents.DiscardPendingDocumentChange(notification.TextDocument.Uri);
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
        CancelPendingDocumentCommit(notification.TextDocument.Uri);
        var upsertResult = await _documents.FlushPendingDocumentChangeAsync(
            notification.TextDocument.Uri,
            cancellationToken).ConfigureAwait(false);
        if (upsertResult is { TextChanged: true })
            ClearCompletedDiagnostics(notification.TextDocument.Uri);

        var policy = GetSaveDiagnosticsPolicy();
        return await ScheduleDiagnosticsPublishAsync(
            notification.TextDocument.Uri,
            reason: "didSave",
            includeWarmup: policy.IncludeWarmup,
            warmupDelayMilliseconds: 0,
            initialDiagnosticsMode: policy.InitialMode,
            followUpDiagnosticsDelayMilliseconds: policy.FollowUpDiagnosticsDelayMilliseconds,
            followUpDiagnosticsMode: policy.FollowUpMode,
            analyzerFollowUpDiagnosticsDelayMilliseconds: policy.AnalyzerFollowUpDiagnosticsDelayMilliseconds,
            analyzerFollowUpDiagnosticsMode: policy.AnalyzerFollowUpMode,
            diagnosticsDelayMilliseconds: policy.DiagnosticsDelayMilliseconds,
            replacePendingDiagnostics: true).ConfigureAwait(false);
    }

    internal static SaveDiagnosticsPolicy GetSaveDiagnosticsPolicy()
        => new(
            IncludeWarmup: false,
            WarmupDelayMilliseconds: 0,
            InitialMode: DocumentStore.DiagnosticLane.Syntax,
            FollowUpDiagnosticsDelayMilliseconds: DocumentCompilerDiagnosticsAfterEditDelayMilliseconds,
            FollowUpMode: DocumentStore.DiagnosticLane.DocumentCompiler,
            AnalyzerFollowUpDiagnosticsDelayMilliseconds: DocumentAnalyzerDiagnosticsAfterEditDelayMilliseconds,
            AnalyzerFollowUpMode: DocumentStore.DiagnosticLane.DocumentWithAnalyzers,
            DiagnosticsDelayMilliseconds: 0);

    internal static SaveDiagnosticsPolicy GetOpenDiagnosticsPolicy()
        => new(
            IncludeWarmup: false,
            WarmupDelayMilliseconds: 0,
            InitialMode: DocumentStore.DiagnosticLane.Syntax,
            FollowUpDiagnosticsDelayMilliseconds: DocumentCompilerDiagnosticsAfterOpenDelayMilliseconds,
            FollowUpMode: DocumentStore.DiagnosticLane.DocumentCompiler,
            AnalyzerFollowUpDiagnosticsDelayMilliseconds: DocumentAnalyzerDiagnosticsAfterOpenDelayMilliseconds,
            AnalyzerFollowUpMode: DocumentStore.DiagnosticLane.DocumentWithAnalyzers,
            DiagnosticsDelayMilliseconds: 0);

    internal static SaveDiagnosticsPolicy GetEditDiagnosticsPolicy()
        => new(
            IncludeWarmup: false,
            WarmupDelayMilliseconds: 0,
            InitialMode: DocumentStore.DiagnosticLane.Syntax,
            FollowUpDiagnosticsDelayMilliseconds: DiagnosticsDebounceMilliseconds,
            FollowUpMode: DocumentStore.DiagnosticLane.DocumentCompiler,
            AnalyzerFollowUpDiagnosticsDelayMilliseconds: DocumentAnalyzerDiagnosticsAfterEditDelayMilliseconds,
            AnalyzerFollowUpMode: DocumentStore.DiagnosticLane.DocumentWithAnalyzers,
            DiagnosticsDelayMilliseconds: 0);

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
        string reason,
        bool includeWarmup = true,
        int warmupDelayMilliseconds = 0,
        DocumentStore.DiagnosticLane initialDiagnosticsMode = DocumentStore.DiagnosticLane.ProjectWithAnalyzers,
        int? followUpDiagnosticsDelayMilliseconds = null,
        DocumentStore.DiagnosticLane? followUpDiagnosticsMode = null,
        int? analyzerFollowUpDiagnosticsDelayMilliseconds = null,
        DocumentStore.DiagnosticLane? analyzerFollowUpDiagnosticsMode = null,
        int diagnosticsDelayMilliseconds = DiagnosticsDebounceMilliseconds,
        bool replacePendingDiagnostics = true)
    {
        var source = new CancellationTokenSource();
        var token = source.Token;
        var expectedSession = GetOrCreateDocumentSession(uri);
        int? expectedVersion = _documentVersions.TryGetValue(uri, out var currentVersion)
            ? currentVersion
            : null;
        var scheduleId = Interlocked.Increment(ref _nextDiagnosticsScheduleId);
        var request = new PendingDiagnosticsRequest(
            scheduleId,
            source,
            expectedSession,
            expectedVersion,
            reason);

        _logger.LogInformation(
            "Diagnostics schedule #{ScheduleId} queued for {Uri} (reason={Reason}, expectedSession={ExpectedSession}, expectedVersion={ExpectedVersion}, initialLane={InitialLane}, followUpLane={FollowUpLane}, analyzerLane={AnalyzerLane}, warmupDelay={WarmupDelayMs}ms, diagnosticsDelay={DiagnosticsDelayMs}ms, followUpDelay={FollowUpDelayMs}, analyzerDelay={AnalyzerDelayMs}, replacePending={ReplacePending}).",
            scheduleId,
            uri,
            reason,
            expectedSession,
            expectedVersion?.ToString() ?? "<none>",
            initialDiagnosticsMode,
            followUpDiagnosticsMode?.ToString() ?? "<none>",
            analyzerFollowUpDiagnosticsMode?.ToString() ?? "<none>",
            warmupDelayMilliseconds,
            diagnosticsDelayMilliseconds,
            followUpDiagnosticsDelayMilliseconds?.ToString() ?? "<none>",
            analyzerFollowUpDiagnosticsDelayMilliseconds?.ToString() ?? "<none>",
            replacePendingDiagnostics);
        RecordDiagnosticsScheduleEvent(
            "diagnosticsScheduleQueued",
            uri,
            scheduleId,
            expectedSession,
            expectedVersion,
            reason,
            $"initialLane={initialDiagnosticsMode} followUpLane={followUpDiagnosticsMode?.ToString() ?? "<none>"} analyzerLane={analyzerFollowUpDiagnosticsMode?.ToString() ?? "<none>"} diagnosticsDelay={diagnosticsDelayMilliseconds} followUpDelay={followUpDiagnosticsDelayMilliseconds?.ToString() ?? "<none>"} analyzerDelay={analyzerFollowUpDiagnosticsDelayMilliseconds?.ToString() ?? "<none>"} replacePending={replacePendingDiagnostics}");
        PendingDiagnosticsRequest current;
        if (replacePendingDiagnostics)
        {
            while (true)
            {
                if (!_pendingDiagnostics.TryGetValue(uri, out var previous))
                {
                    if (_pendingDiagnostics.TryAdd(uri, request))
                    {
                        current = request;
                        break;
                    }

                    continue;
                }

                if (!_pendingDiagnostics.TryUpdate(uri, request, previous))
                    continue;

                _logger.LogInformation(
                    "Diagnostics schedule #{ScheduleId} replaced pending schedule #{PreviousScheduleId} for {Uri} (reason={Reason}, previousReason={PreviousReason}, expectedSession={ExpectedSession}, expectedVersion={ExpectedVersion}, previousSession={PreviousSession}, previousVersion={PreviousVersion}).",
                    scheduleId,
                    previous.Id,
                    uri,
                    reason,
                    previous.Reason,
                    expectedSession,
                    expectedVersion?.ToString() ?? "<none>",
                    previous.ExpectedSession,
                    previous.ExpectedVersion?.ToString() ?? "<none>");
                RecordDiagnosticsScheduleEvent(
                    "diagnosticsScheduleReplaced",
                    uri,
                    scheduleId,
                    expectedSession,
                    expectedVersion,
                    reason,
                    $"previousSchedule={previous.Id} previousReason={previous.Reason} previousSession={previous.ExpectedSession} previousVersion={previous.ExpectedVersion?.ToString() ?? "<none>"}");

                try
                {
                    previous.Cancellation.Cancel();
                }
                catch (ObjectDisposedException)
                {
                }

                current = request;
                break;
            }
        }
        else if (!_pendingDiagnostics.TryAdd(uri, request))
        {
            source.Dispose();
            if (_pendingDiagnostics.TryGetValue(uri, out var existing))
            {
                _logger.LogInformation(
                    "Diagnostics schedule #{ScheduleId} skipped for {Uri}: schedule #{ExistingScheduleId} is already pending (reason={Reason}, existingReason={ExistingReason}, expectedSession={ExpectedSession}, expectedVersion={ExpectedVersion}, existingSession={ExistingSession}, existingVersion={ExistingVersion}).",
                    scheduleId,
                    uri,
                    existing.Id,
                    reason,
                    existing.Reason,
                    expectedSession,
                    expectedVersion?.ToString() ?? "<none>",
                    existing.ExpectedSession,
                    existing.ExpectedVersion?.ToString() ?? "<none>");
                RecordDiagnosticsScheduleEvent(
                    "diagnosticsScheduleSkipped",
                    uri,
                    scheduleId,
                    expectedSession,
                    expectedVersion,
                    reason,
                    $"existingSchedule={existing.Id} existingReason={existing.Reason} existingSession={existing.ExpectedSession} existingVersion={existing.ExpectedVersion?.ToString() ?? "<none>"}");
            }
            else
            {
                _logger.LogInformation(
                    "Diagnostics schedule #{ScheduleId} skipped for {Uri}: another schedule is already pending (reason={Reason}, expectedSession={ExpectedSession}, expectedVersion={ExpectedVersion}).",
                    scheduleId,
                    uri,
                    reason,
                    expectedSession,
                    expectedVersion?.ToString() ?? "<none>");
                RecordDiagnosticsScheduleEvent(
                    "diagnosticsScheduleSkipped",
                    uri,
                    scheduleId,
                    expectedSession,
                    expectedVersion,
                    reason,
                    "existingSchedule=<unknown>");
            }

            return Task.FromResult(Unit.Value);
        }
        else
        {
            current = request;
        }

        if (!ReferenceEquals(current, request))
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
                    {
                        _logger.LogInformation(
                            "Diagnostics schedule #{ScheduleId} skipped before warmup for {Uri}: stale request (reason={Reason}, expectedSession={ExpectedSession}, expectedVersion={ExpectedVersion}).",
                            scheduleId,
                            uri,
                            reason,
                            expectedSession,
                            expectedVersion?.ToString() ?? "<none>");
                        RecordDiagnosticsScheduleEvent(
                            "diagnosticsScheduleStale",
                            uri,
                            scheduleId,
                            expectedSession,
                            expectedVersion,
                            reason,
                            "stage=beforeWarmup");
                        return;
                    }

                    _logger.LogInformation(
                        "Diagnostics schedule #{ScheduleId} starting analysis warmup for {Uri} (reason={Reason}, expectedVersion={ExpectedVersion}, warmupDelay={WarmupDelayMs}ms, diagnosticsDelay={DiagnosticsDelayMs}ms).",
                        scheduleId,
                        uri,
                        reason,
                        expectedVersion?.ToString() ?? "<none>",
                        warmupDelayMilliseconds,
                        diagnosticsDelayMilliseconds);
                    await _documents.WarmAnalysisAsync(
                        uri,
                        shouldSkipWork: () => token.IsCancellationRequested || ShouldSkipRequest(uri, expectedSession, expectedVersion),
                        token).ConfigureAwait(false);
                    _logger.LogInformation(
                        "Diagnostics schedule #{ScheduleId} completed analysis warmup for {Uri} (reason={Reason}, expectedVersion={ExpectedVersion}).",
                        scheduleId,
                        uri,
                        reason,
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
                {
                    _logger.LogInformation(
                        "Diagnostics schedule #{ScheduleId} skipped before initial publish for {Uri}: stale request (reason={Reason}, expectedSession={ExpectedSession}, expectedVersion={ExpectedVersion}, lane={Lane}).",
                        scheduleId,
                        uri,
                        reason,
                        expectedSession,
                        expectedVersion?.ToString() ?? "<none>",
                        initialDiagnosticsMode);
                    RecordDiagnosticsScheduleEvent(
                        "diagnosticsScheduleStale",
                        uri,
                        scheduleId,
                        expectedSession,
                        expectedVersion,
                        reason,
                        $"stage=beforeInitialPublish lane={initialDiagnosticsMode}");
                    return;
                }

                await PublishDiagnosticsAsync(uri, token, expectedSession, expectedVersion, initialDiagnosticsMode, scheduleId).ConfigureAwait(false);

                var followUpMode = followUpDiagnosticsMode ?? DocumentStore.DiagnosticLane.ProjectWithAnalyzers;
                if (followUpDiagnosticsDelayMilliseconds is { } followUpDelay &&
                    initialDiagnosticsMode != followUpMode)
                {
                    await Task.Delay(followUpDelay, token).ConfigureAwait(false);

                    if (ShouldSkipRequest(uri, expectedSession, expectedVersion))
                    {
                        _logger.LogInformation(
                            "Diagnostics schedule #{ScheduleId} skipped before follow-up publish for {Uri}: stale request (reason={Reason}, expectedSession={ExpectedSession}, expectedVersion={ExpectedVersion}, lane={Lane}).",
                            scheduleId,
                            uri,
                            reason,
                            expectedSession,
                            expectedVersion?.ToString() ?? "<none>",
                            followUpMode);
                        RecordDiagnosticsScheduleEvent(
                            "diagnosticsScheduleStale",
                            uri,
                            scheduleId,
                            expectedSession,
                            expectedVersion,
                            reason,
                            $"stage=beforeFollowUpPublish lane={followUpMode}");
                        return;
                    }

                    await PublishDiagnosticsAsync(uri, token, expectedSession, expectedVersion, followUpMode, scheduleId).ConfigureAwait(false);
                }

                var analyzerFollowUpMode = analyzerFollowUpDiagnosticsMode ?? followUpMode;
                if (analyzerFollowUpDiagnosticsDelayMilliseconds is { } analyzerFollowUpDelay &&
                    analyzerFollowUpMode != initialDiagnosticsMode &&
                    analyzerFollowUpMode != followUpMode)
                {
                    await Task.Delay(analyzerFollowUpDelay, token).ConfigureAwait(false);

                    if (ShouldSkipRequest(uri, expectedSession, expectedVersion))
                    {
                        _logger.LogInformation(
                            "Diagnostics schedule #{ScheduleId} skipped before analyzer publish for {Uri}: stale request (reason={Reason}, expectedSession={ExpectedSession}, expectedVersion={ExpectedVersion}, lane={Lane}).",
                            scheduleId,
                            uri,
                            reason,
                            expectedSession,
                            expectedVersion?.ToString() ?? "<none>",
                            analyzerFollowUpMode);
                        RecordDiagnosticsScheduleEvent(
                            "diagnosticsScheduleStale",
                            uri,
                            scheduleId,
                            expectedSession,
                            expectedVersion,
                            reason,
                            $"stage=beforeAnalyzerPublish lane={analyzerFollowUpMode}");
                        return;
                    }

                    await PublishDiagnosticsAsync(uri, token, expectedSession, expectedVersion, analyzerFollowUpMode, scheduleId).ConfigureAwait(false);
                }
            }
            catch (OperationCanceledException)
            {
                _logger.LogInformation(
                    "Diagnostics schedule #{ScheduleId} canceled for {Uri} (reason={Reason}, expectedSession={ExpectedSession}, expectedVersion={ExpectedVersion}).",
                    scheduleId,
                    uri,
                    reason,
                    expectedSession,
                    expectedVersion?.ToString() ?? "<none>");
                RecordDiagnosticsScheduleEvent(
                    "diagnosticsScheduleCanceled",
                    uri,
                    scheduleId,
                    expectedSession,
                    expectedVersion,
                    reason);
            }
            catch (Exception ex)
            {
                _logger.LogError(
                    ex,
                    "Diagnostics schedule #{ScheduleId} failed for {Uri} (reason={Reason}, expectedSession={ExpectedSession}, expectedVersion={ExpectedVersion}).",
                    scheduleId,
                    uri,
                    reason,
                    expectedSession,
                    expectedVersion?.ToString() ?? "<none>");
            }
            finally
            {
                if (_pendingDiagnostics.TryGetValue(uri, out var active) && ReferenceEquals(active, request))
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
                pending.Cancellation.Cancel();
            }
            catch (ObjectDisposedException)
            {
            }

            _logger.LogInformation(
                "Diagnostics schedule #{ScheduleId} canceled for {Uri}: pending diagnostics were cleared (reason={Reason}, expectedSession={ExpectedSession}, expectedVersion={ExpectedVersion}).",
                pending.Id,
                uri,
                pending.Reason,
                pending.ExpectedSession,
                pending.ExpectedVersion?.ToString() ?? "<none>");
            RecordDiagnosticsScheduleEvent(
                "diagnosticsScheduleCanceled",
                uri,
                pending.Id,
                pending.ExpectedSession,
                pending.ExpectedVersion,
                pending.Reason,
                "source=clearPending");
        }
    }

    private void CancelPendingDocumentCommit(DocumentUri uri)
    {
        if (_pendingDocumentCommits.TryRemove(uri, out var pending))
        {
            try
            {
                pending.Cancel();
            }
            catch (ObjectDisposedException)
            {
            }

            try
            {
                pending.Dispose();
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
        DocumentStore.DiagnosticLane lane = DocumentStore.DiagnosticLane.ProjectWithAnalyzers,
        long? scheduleId = null)
    {
        var stopwatch = Stopwatch.StartNew();
        int diagnosticsCount = 0;
        var outcome = PublishDiagnosticsOutcome.Published;
        var publishStarted = false;

        try
        {
            if (ShouldSkipRequest(uri, expectedSession, expectedVersion))
            {
                outcome = PublishDiagnosticsOutcome.SkippedVersionMismatch;
                _logger.LogInformation(
                    "Diagnostics publish skipped for {Uri}: request is stale (schedule={ScheduleId}, expectedSession={ExpectedSession}, expectedVersion={ExpectedVersion}, lane={Lane}).",
                    uri,
                    FormatOptionalId(scheduleId),
                    expectedSession,
                    expectedVersion?.ToString() ?? "<none>",
                    lane);
                return Unit.Value;
            }

            if (IsDiagnosticsPublishAlreadyCompleted(uri, expectedVersion, lane))
            {
                outcome = PublishDiagnosticsOutcome.SkippedAlreadyCompleted;
                _logger.LogInformation(
                    "Diagnostics publish skipped for {Uri}: already completed (schedule={ScheduleId}, expectedSession={ExpectedSession}, expectedVersion={ExpectedVersion}, lane={Lane}).",
                    uri,
                    FormatOptionalId(scheduleId),
                    expectedSession,
                    expectedVersion?.ToString() ?? "<none>",
                    lane);
                return Unit.Value;
            }

            if (!TryStartDiagnosticsPublish(uri, expectedVersion, lane))
            {
                outcome = PublishDiagnosticsOutcome.SkippedRequeued;
                _logger.LogInformation(
                    "Diagnostics publish requeued for {Uri}: same version and lane are already active (schedule={ScheduleId}, expectedSession={ExpectedSession}, expectedVersion={ExpectedVersion}, lane={Lane}, retryDelay={RetryDelayMs}ms).",
                    uri,
                    FormatOptionalId(scheduleId),
                    expectedSession,
                    expectedVersion?.ToString() ?? "<none>",
                    lane,
                    ActiveDiagnosticsRetryDelayMilliseconds);
                RequeueDiagnosticsPublish(
                    uri,
                    expectedSession,
                    expectedVersion,
                    lane,
                    ActiveDiagnosticsRetryDelayMilliseconds,
                    ignoreCompleted: true,
                    originScheduleId: scheduleId);
                return Unit.Value;
            }

            publishStarted = true;
            _logger.LogInformation(
                "Diagnostics publish started for {Uri} (schedule={ScheduleId}, expectedSession={ExpectedSession}, expectedVersion={ExpectedVersion}, lane={Lane}).",
                uri,
                FormatOptionalId(scheduleId),
                expectedSession,
                expectedVersion?.ToString() ?? "<none>",
                lane);

            var result = await _documents.TryGetDiagnosticsAsync(
                uri,
                lane,
                shouldSkipWork: () => cancellationToken.IsCancellationRequested || ShouldSkipRequest(uri, expectedSession, expectedVersion),
                cancellationToken).ConfigureAwait(false);
            if (result.WasSkipped)
            {
                outcome = PublishDiagnosticsOutcome.SkippedRequeued;
                _logger.LogInformation(
                    "Diagnostics publish requeued for {Uri}: diagnostics computation was skipped (schedule={ScheduleId}, expectedSession={ExpectedSession}, expectedVersion={ExpectedVersion}, lane={Lane}, retryDelay={RetryDelayMs}ms).",
                    uri,
                    FormatOptionalId(scheduleId),
                    expectedSession,
                    expectedVersion?.ToString() ?? "<none>",
                    lane,
                    DiagnosticsRetryDelayMilliseconds);
                RequeueDiagnosticsPublish(
                    uri,
                    expectedSession,
                    expectedVersion,
                    lane,
                    DiagnosticsRetryDelayMilliseconds,
                    originScheduleId: scheduleId);
                return Unit.Value;
            }

            var diagnostics = result.Diagnostics;
            var snapshotKey = result.SnapshotKey;
            var lastPublishedVersion = _lastPublishedDiagnosticVersions.TryGetValue(uri, out var publishedVersion)
                ? publishedVersion
                : (int?)null;
            var diagnosticsToPublish = MergePartialDiagnosticsForPublish(
                lane,
                diagnostics,
                GetCarryForwardAnalyzerDiagnostics(uri, expectedVersion, snapshotKey));
            var diagnosticValues = CreatePublishedDiagnosticValues(diagnosticsToPublish);

            if (ShouldSkipRequest(uri, expectedSession, expectedVersion))
            {
                outcome = PublishDiagnosticsOutcome.SkippedVersionMismatch;
                _logger.LogInformation(
                    "Skipped diagnostics publish for {Uri}: request is stale (schedule={ScheduleId}, expectedSession={ExpectedSession}, expectedVersion={ExpectedVersion}, lane={Lane}, computedCount={Count}, summary={Summary}).",
                    uri,
                    FormatOptionalId(scheduleId),
                    expectedSession,
                    expectedVersion?.ToString() ?? "<none>",
                    lane,
                    diagnosticsToPublish.Length,
                    SummarizeDiagnosticsForLog(diagnosticsToPublish));
                return Unit.Value;
            }

            var hasLastPublishedDiagnostics = _lastPublishedDiagnostics.TryGetValue(uri, out var lastPublishedDiagnostics);
            if (!ShouldPublishDiagnostics(
                    hasLastPublishedDiagnostics,
                    lastPublishedDiagnostics,
                    lastPublishedVersion,
                    diagnosticValues,
                    expectedVersion))
            {
                diagnosticsCount = diagnosticsToPublish.Length;
                if (expectedVersion is { } stableVersion)
                    _lastPublishedDiagnosticVersions[uri] = stableVersion;

                UpdatePublishedAnalyzerDiagnostics(uri, lane, expectedVersion, snapshotKey, diagnosticsToPublish);
                MarkDiagnosticsPublishCompleted(uri, expectedVersion, lane);
                var diagnosticSummary = SummarizeDiagnosticsForLog(diagnosticsToPublish);
                outcome = PublishDiagnosticsOutcome.SkippedUnchanged;

                _logger.LogInformation(
                    "Skipped diagnostics publish for {Uri}: diagnostic set unchanged (schedule={ScheduleId}, expectedVersion={ExpectedVersion}, lane={Lane}, count={Count}, summary={Summary}).",
                    uri,
                    FormatOptionalId(scheduleId),
                    expectedVersion?.ToString() ?? "<none>",
                    lane,
                    diagnosticsToPublish.Length,
                    diagnosticSummary);
                return Unit.Value;
            }

            PublishDiagnosticsToClient(uri, diagnosticsToPublish, expectedVersion);
            diagnosticsCount = diagnosticsToPublish.Length;
            _lastPublishedDiagnostics[uri] = diagnosticValues;
            UpdatePublishedAnalyzerDiagnostics(uri, lane, expectedVersion, snapshotKey, diagnosticsToPublish);
            if (expectedVersion is { } completedVersion)
                _lastPublishedDiagnosticVersions[uri] = completedVersion;
            MarkDiagnosticsPublishCompleted(uri, expectedVersion, lane);
            var publishedDiagnosticSummary = SummarizeDiagnosticsForLog(diagnosticsToPublish);
            _logger.LogInformation(
                "Published diagnostics for {Uri} (schedule={ScheduleId}, expectedVersion={ExpectedVersion}, lane={Lane}, count={Count}, summary={Summary}).",
                uri,
                FormatOptionalId(scheduleId),
                expectedVersion?.ToString() ?? "<none>",
                lane,
                diagnosticsToPublish.Length,
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
            if (publishStarted)
                CompleteDiagnosticsPublish(uri, expectedVersion, lane);

            stopwatch.Stop();
            LanguageServerPerformanceInstrumentation.RecordOperation(
                GetPublishDiagnosticsOperationName(lane, outcome),
                uri,
                expectedVersion,
                stopwatch.Elapsed.TotalMilliseconds,
                resultCount: diagnosticsCount,
                detail: $"{uri} schedule={FormatOptionalId(scheduleId)} version={expectedVersion} lane={lane} outcome={outcome}");
        }

        return Unit.Value;
    }

    private void UpdatePublishedAnalyzerDiagnostics(
        DocumentUri uri,
        DocumentStore.DiagnosticLane lane,
        int? version,
        DocumentStore.DiagnosticSnapshotKey? snapshotKey,
        ImmutableArray<Diagnostic> diagnostics)
    {
        if (lane is not DocumentStore.DiagnosticLane.DocumentWithAnalyzers and
            not DocumentStore.DiagnosticLane.ProjectWithAnalyzers)
        {
            return;
        }

        var analyzerDiagnostics = diagnostics
            .Where(CanCarryForwardAnalyzerDiagnostic)
            .ToImmutableArray();
        if (analyzerDiagnostics.IsDefaultOrEmpty)
        {
            _lastPublishedAnalyzerDiagnostics.TryRemove(uri, out _);
            return;
        }

        _lastPublishedAnalyzerDiagnostics[uri] = new VersionedDiagnostics(version, snapshotKey, analyzerDiagnostics);
    }

    private ImmutableArray<Diagnostic> GetCarryForwardAnalyzerDiagnostics(
        DocumentUri uri,
        int? version,
        DocumentStore.DiagnosticSnapshotKey? snapshotKey)
    {
        return _lastPublishedAnalyzerDiagnostics.TryGetValue(uri, out var previous)
            ? GetCarryForwardAnalyzerDiagnosticsForPresentation(version, snapshotKey, previous)
            : [];
    }

    private void RequeueDiagnosticsPublish(
        DocumentUri uri,
        long expectedSession,
        int? expectedVersion,
        DocumentStore.DiagnosticLane lane,
        int delayMilliseconds,
        bool ignoreCompleted = false,
        long? originScheduleId = null)
    {
        if (expectedVersion is { } stableVersion)
        {
            var retryKey = new PendingDiagnosticsRetryKey(uri, expectedSession, stableVersion, lane);
            if (!_pendingDiagnosticsRetries.TryAdd(retryKey, 0))
            {
                _logger.LogDebug(
                    "Skipped diagnostics retry scheduling for {Uri}: retry already pending (expectedSession={ExpectedSession}, expectedVersion={ExpectedVersion}, lane={Lane}).",
                    uri,
                    expectedSession,
                    stableVersion,
                    lane);
                return;
            }

            _logger.LogInformation(
                "Diagnostics retry scheduled for {Uri} (originSchedule={ScheduleId}, expectedSession={ExpectedSession}, expectedVersion={ExpectedVersion}, lane={Lane}, delay={DelayMs}ms, ignoreCompleted={IgnoreCompleted}).",
                uri,
                FormatOptionalId(originScheduleId),
                expectedSession,
                stableVersion,
                lane,
                delayMilliseconds,
                ignoreCompleted);
            RecordDiagnosticsRetryEvent(
                "diagnosticsRetryScheduled",
                uri,
                originScheduleId,
                expectedSession,
                stableVersion,
                lane,
                $"delay={delayMilliseconds} ignoreCompleted={ignoreCompleted}");

            _ = Task.Run(async () =>
            {
                var retryKeyRemovedBeforePublish = false;
                try
                {
                    await Task.Delay(delayMilliseconds).ConfigureAwait(false);
                    if (ShouldSkipRequest(uri, expectedSession, expectedVersion) ||
                        (!ignoreCompleted && IsDiagnosticsPublishAlreadyCompleted(uri, expectedVersion, lane)))
                    {
                        _logger.LogInformation(
                            "Diagnostics retry skipped for {Uri} (originSchedule={ScheduleId}, expectedSession={ExpectedSession}, expectedVersion={ExpectedVersion}, lane={Lane}, ignoreCompleted={IgnoreCompleted}).",
                            uri,
                            FormatOptionalId(originScheduleId),
                            expectedSession,
                            expectedVersion?.ToString() ?? "<none>",
                            lane,
                            ignoreCompleted);
                        RecordDiagnosticsRetryEvent(
                            "diagnosticsRetrySkipped",
                            uri,
                            originScheduleId,
                            expectedSession,
                            expectedVersion,
                            lane,
                            $"ignoreCompleted={ignoreCompleted}");
                        return;
                    }

                    if (ignoreCompleted)
                        ClearCompletedDiagnostics(uri, expectedVersion, lane);

                    _pendingDiagnosticsRetries.TryRemove(retryKey, out _);
                    retryKeyRemovedBeforePublish = true;

                    using var retryCancellation = new CancellationTokenSource();
                    await PublishDiagnosticsAsync(uri, retryCancellation.Token, expectedSession, expectedVersion, lane, originScheduleId).ConfigureAwait(false);
                }
                catch (OperationCanceledException)
                {
                }
                catch (Exception ex)
                {
                    _logger.LogDebug(ex, "Deferred diagnostics retry failed for {Uri}.", uri);
                }
                finally
                {
                    if (!retryKeyRemovedBeforePublish)
                        _pendingDiagnosticsRetries.TryRemove(retryKey, out _);
                }
            }, CancellationToken.None);
            return;
        }

        _logger.LogInformation(
            "Diagnostics retry scheduled for {Uri} (originSchedule={ScheduleId}, expectedSession={ExpectedSession}, expectedVersion={ExpectedVersion}, lane={Lane}, delay={DelayMs}ms, ignoreCompleted={IgnoreCompleted}).",
            uri,
            FormatOptionalId(originScheduleId),
            expectedSession,
            expectedVersion?.ToString() ?? "<none>",
            lane,
            delayMilliseconds,
            ignoreCompleted);
        RecordDiagnosticsRetryEvent(
            "diagnosticsRetryScheduled",
            uri,
            originScheduleId,
            expectedSession,
            expectedVersion,
            lane,
            $"delay={delayMilliseconds} ignoreCompleted={ignoreCompleted}");

        _ = Task.Run(async () =>
        {
            try
            {
                await Task.Delay(delayMilliseconds).ConfigureAwait(false);
                if (ShouldSkipRequest(uri, expectedSession, expectedVersion) ||
                    (!ignoreCompleted && IsDiagnosticsPublishAlreadyCompleted(uri, expectedVersion, lane)))
                {
                    _logger.LogInformation(
                        "Diagnostics retry skipped for {Uri} (originSchedule={ScheduleId}, expectedSession={ExpectedSession}, expectedVersion={ExpectedVersion}, lane={Lane}, ignoreCompleted={IgnoreCompleted}).",
                        uri,
                        FormatOptionalId(originScheduleId),
                        expectedSession,
                        expectedVersion?.ToString() ?? "<none>",
                        lane,
                        ignoreCompleted);
                    RecordDiagnosticsRetryEvent(
                        "diagnosticsRetrySkipped",
                        uri,
                        originScheduleId,
                        expectedSession,
                        expectedVersion,
                        lane,
                        $"ignoreCompleted={ignoreCompleted}");
                    return;
                }

                if (ignoreCompleted)
                    ClearCompletedDiagnostics(uri, expectedVersion, lane);

                using var retryCancellation = new CancellationTokenSource();
                await PublishDiagnosticsAsync(uri, retryCancellation.Token, expectedSession, expectedVersion, lane, originScheduleId).ConfigureAwait(false);
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

    private void PublishDiagnosticsToClient(
        DocumentUri uri,
        IReadOnlyList<Diagnostic> diagnostics,
        int? version)
    {
        _languageServer.TextDocument.PublishDiagnostics(new PublishDiagnosticsParams
        {
            Uri = uri,
            Version = version,
            Diagnostics = new Container<Diagnostic>(diagnostics)
        });
    }

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

    private void RequestInlayHintRefresh(DocumentUri changedUri, string reason)
    {
        if (_languageServer is null)
        {
            RecordInlayHintRefreshEvent(
                "inlayHintRefreshSkipped",
                changedUri,
                reason,
                advertisedRefreshSupport: false,
                "languageServer=<null>");
            return;
        }

        var advertisedRefreshSupport = HasAdvertisedInlayHintRefreshSupport(_languageServer.ClientSettings?.Capabilities);
        RecordInlayHintRefreshEvent(
            "inlayHintRefreshRequested",
            changedUri,
            reason,
            advertisedRefreshSupport,
            "stage=beforeSend");
        try
        {
            _languageServer.Workspace.SendInlayHintRefresh(new InlayHintRefreshParams());
            RecordInlayHintRefreshEvent(
                "inlayHintRefreshSent",
                changedUri,
                reason,
                advertisedRefreshSupport);
            _logger.LogInformation(
                "Requested inlay hint refresh because {ChangedUri} changed (reason={Reason}, advertisedRefreshSupport={RefreshSupport}).",
                changedUri,
                reason,
                advertisedRefreshSupport);
        }
        catch (Exception ex)
        {
            RecordInlayHintRefreshEvent(
                "inlayHintRefreshFailed",
                changedUri,
                reason,
                advertisedRefreshSupport,
                $"exception={ex.GetType().Name}");
            _logger.LogInformation(
                ex,
                "Failed to request inlay hint refresh because {ChangedUri} changed (reason={Reason}, advertisedRefreshSupport={RefreshSupport}).",
                changedUri,
                reason,
                advertisedRefreshSupport);
        }
    }

    internal static bool HasAdvertisedInlayHintRefreshSupport(ClientCapabilities? capabilities)
    {
        if (capabilities?.Workspace is not { } workspace ||
            !workspace.InlayHint.IsSupported)
        {
            return false;
        }

        return workspace.InlayHint.Value.RefreshSupport;
    }

    internal static ImmutableArray<Diagnostic> MergePartialDiagnosticsForPublish(
        DocumentStore.DiagnosticLane lane,
        IReadOnlyList<Diagnostic> diagnostics,
        ImmutableArray<Diagnostic> lastPublishedDiagnostics)
    {
        if (lane is DocumentStore.DiagnosticLane.DocumentWithAnalyzers or DocumentStore.DiagnosticLane.ProjectWithAnalyzers ||
            lastPublishedDiagnostics.IsDefaultOrEmpty)
        {
            return diagnostics.ToImmutableArray();
        }

        var builder = ImmutableArray.CreateBuilder<Diagnostic>(diagnostics.Count + lastPublishedDiagnostics.Length);
        builder.AddRange(diagnostics);
        var existing = CreatePublishedDiagnosticValues(builder);
        foreach (var previous in lastPublishedDiagnostics)
        {
            if (!CanCarryForwardAnalyzerDiagnostic(previous))
                continue;

            var value = PublishedDiagnosticValue.From(previous);
            if (existing.Contains(value))
                continue;

            builder.Add(previous);
        }

        return builder
            .OrderBy(static diagnostic => PublishedDiagnosticValue.From(diagnostic))
            .ToImmutableArray();
    }

    private static bool CanCarryForwardAnalyzerDiagnostic(Diagnostic diagnostic)
    {
        if (!string.Equals(diagnostic.Source, "raven", StringComparison.OrdinalIgnoreCase))
            return true;

        return diagnostic.Code?.String is { } code &&
               code.StartsWith("RAV9", StringComparison.Ordinal);
    }

    internal static ImmutableArray<Diagnostic> GetCarryForwardAnalyzerDiagnosticsForPresentation(
        int? version,
        DocumentStore.DiagnosticSnapshotKey? snapshotKey,
        VersionedDiagnostics previous)
    {
        if (snapshotKey is not null && previous.SnapshotKey is not null)
            return previous.Diagnostics;

        if (previous.SnapshotKey is not null)
            return [];

        return previous.Version == version ? previous.Diagnostics : [];
    }

    internal static ImmutableArray<PublishedDiagnosticValue> CreatePublishedDiagnosticValues(IReadOnlyList<Diagnostic> diagnostics)
        => diagnostics
            .Select(static diagnostic => PublishedDiagnosticValue.From(diagnostic))
            .OrderBy(static diagnostic => diagnostic)
            .ToImmutableArray();

    private static void RecordDiagnosticsScheduleEvent(
        string operation,
        DocumentUri uri,
        long scheduleId,
        long expectedSession,
        int? expectedVersion,
        string reason,
        string? extraDetail = null)
    {
        var detail = $"{uri} schedule={scheduleId} reason={reason} expectedSession={expectedSession} version={expectedVersion?.ToString() ?? "<none>"}";
        if (!string.IsNullOrWhiteSpace(extraDetail))
            detail = $"{detail} {extraDetail}";

        LanguageServerPerformanceInstrumentation.RecordOperation(
            operation,
            uri,
            expectedVersion,
            0,
            detail: detail);
    }

    private static void RecordDiagnosticsRetryEvent(
        string operation,
        DocumentUri uri,
        long? originScheduleId,
        long expectedSession,
        int? expectedVersion,
        DocumentStore.DiagnosticLane lane,
        string? extraDetail = null)
    {
        var detail = $"{uri} originSchedule={FormatOptionalId(originScheduleId)} expectedSession={expectedSession} version={expectedVersion?.ToString() ?? "<none>"} lane={lane}";
        if (!string.IsNullOrWhiteSpace(extraDetail))
            detail = $"{detail} {extraDetail}";

        LanguageServerPerformanceInstrumentation.RecordOperation(
            operation,
            uri,
            expectedVersion,
            0,
            detail: detail);
    }

    private static void RecordInlayHintRefreshEvent(
        string operation,
        DocumentUri uri,
        string reason,
        bool advertisedRefreshSupport,
        string? extraDetail = null)
    {
        var detail = $"{uri} reason={reason} advertisedRefreshSupport={advertisedRefreshSupport}";
        if (!string.IsNullOrWhiteSpace(extraDetail))
            detail = $"{detail} {extraDetail}";

        LanguageServerPerformanceInstrumentation.RecordOperation(
            operation,
            uri,
            version: null,
            0,
            detail: detail);
    }

    internal static bool ShouldPublishDiagnostics(
        bool hasLastPublishedDiagnostics,
        ImmutableArray<PublishedDiagnosticValue> lastPublishedDiagnostics,
        int? lastPublishedVersion,
        ImmutableArray<PublishedDiagnosticValue> currentDiagnostics,
        int? currentVersion)
    {
        if (!hasLastPublishedDiagnostics ||
            !lastPublishedDiagnostics.SequenceEqual(currentDiagnostics))
        {
            return true;
        }

        return currentVersion is { } stableVersion &&
               lastPublishedVersion != stableVersion;
    }

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

    private static string FormatOptionalId(long? id)
        => id?.ToString() ?? "<none>";

    internal static string GetPublishDiagnosticsOperationName(
        DocumentStore.DiagnosticLane lane,
        PublishDiagnosticsOutcome outcome)
        => (lane, outcome) switch
        {
            (DocumentStore.DiagnosticLane.Syntax, PublishDiagnosticsOutcome.Published) => "publishSyntaxDiagnostics",
            (DocumentStore.DiagnosticLane.Syntax, PublishDiagnosticsOutcome.SkippedRequeued) => "publishSyntaxDiagnosticsSkipped",
            (DocumentStore.DiagnosticLane.Syntax, PublishDiagnosticsOutcome.SkippedUnchanged) => "publishSyntaxDiagnosticsUnchanged",
            (DocumentStore.DiagnosticLane.Syntax, PublishDiagnosticsOutcome.SkippedVersionMismatch) => "publishSyntaxDiagnosticsVersionMismatch",
            (DocumentStore.DiagnosticLane.Syntax, PublishDiagnosticsOutcome.SkippedAlreadyCompleted) => "publishSyntaxDiagnosticsAlreadyCompleted",
            (DocumentStore.DiagnosticLane.Syntax, PublishDiagnosticsOutcome.SkippedAlreadyPending) => "publishSyntaxDiagnosticsAlreadyPending",
            (DocumentStore.DiagnosticLane.DocumentCompiler, PublishDiagnosticsOutcome.Published) => "publishDocumentCompilerDiagnostics",
            (DocumentStore.DiagnosticLane.DocumentCompiler, PublishDiagnosticsOutcome.SkippedRequeued) => "publishDocumentCompilerDiagnosticsSkipped",
            (DocumentStore.DiagnosticLane.DocumentCompiler, PublishDiagnosticsOutcome.SkippedUnchanged) => "publishDocumentCompilerDiagnosticsUnchanged",
            (DocumentStore.DiagnosticLane.DocumentCompiler, PublishDiagnosticsOutcome.SkippedVersionMismatch) => "publishDocumentCompilerDiagnosticsVersionMismatch",
            (DocumentStore.DiagnosticLane.DocumentCompiler, PublishDiagnosticsOutcome.SkippedAlreadyCompleted) => "publishDocumentCompilerDiagnosticsAlreadyCompleted",
            (DocumentStore.DiagnosticLane.DocumentCompiler, PublishDiagnosticsOutcome.SkippedAlreadyPending) => "publishDocumentCompilerDiagnosticsAlreadyPending",
            (DocumentStore.DiagnosticLane.ProjectCompiler, PublishDiagnosticsOutcome.Published) => "publishProjectCompilerDiagnostics",
            (DocumentStore.DiagnosticLane.ProjectCompiler, PublishDiagnosticsOutcome.SkippedRequeued) => "publishProjectCompilerDiagnosticsSkipped",
            (DocumentStore.DiagnosticLane.ProjectCompiler, PublishDiagnosticsOutcome.SkippedUnchanged) => "publishProjectCompilerDiagnosticsUnchanged",
            (DocumentStore.DiagnosticLane.ProjectCompiler, PublishDiagnosticsOutcome.SkippedVersionMismatch) => "publishProjectCompilerDiagnosticsVersionMismatch",
            (DocumentStore.DiagnosticLane.ProjectCompiler, PublishDiagnosticsOutcome.SkippedAlreadyCompleted) => "publishProjectCompilerDiagnosticsAlreadyCompleted",
            (DocumentStore.DiagnosticLane.ProjectCompiler, PublishDiagnosticsOutcome.SkippedAlreadyPending) => "publishProjectCompilerDiagnosticsAlreadyPending",
            (DocumentStore.DiagnosticLane.DocumentWithAnalyzers, PublishDiagnosticsOutcome.Published) => "publishDocumentWithAnalyzersDiagnostics",
            (DocumentStore.DiagnosticLane.DocumentWithAnalyzers, PublishDiagnosticsOutcome.SkippedRequeued) => "publishDocumentWithAnalyzersDiagnosticsSkipped",
            (DocumentStore.DiagnosticLane.DocumentWithAnalyzers, PublishDiagnosticsOutcome.SkippedUnchanged) => "publishDocumentWithAnalyzersDiagnosticsUnchanged",
            (DocumentStore.DiagnosticLane.DocumentWithAnalyzers, PublishDiagnosticsOutcome.SkippedVersionMismatch) => "publishDocumentWithAnalyzersDiagnosticsVersionMismatch",
            (DocumentStore.DiagnosticLane.DocumentWithAnalyzers, PublishDiagnosticsOutcome.SkippedAlreadyCompleted) => "publishDocumentWithAnalyzersDiagnosticsAlreadyCompleted",
            (DocumentStore.DiagnosticLane.DocumentWithAnalyzers, PublishDiagnosticsOutcome.SkippedAlreadyPending) => "publishDocumentWithAnalyzersDiagnosticsAlreadyPending",
            (DocumentStore.DiagnosticLane.ProjectWithAnalyzers, PublishDiagnosticsOutcome.Published) => "publishProjectWithAnalyzersDiagnostics",
            (DocumentStore.DiagnosticLane.ProjectWithAnalyzers, PublishDiagnosticsOutcome.SkippedRequeued) => "publishProjectWithAnalyzersDiagnosticsSkipped",
            (DocumentStore.DiagnosticLane.ProjectWithAnalyzers, PublishDiagnosticsOutcome.SkippedUnchanged) => "publishProjectWithAnalyzersDiagnosticsUnchanged",
            (DocumentStore.DiagnosticLane.ProjectWithAnalyzers, PublishDiagnosticsOutcome.SkippedAlreadyCompleted) => "publishProjectWithAnalyzersDiagnosticsAlreadyCompleted",
            (DocumentStore.DiagnosticLane.ProjectWithAnalyzers, PublishDiagnosticsOutcome.SkippedAlreadyPending) => "publishProjectWithAnalyzersDiagnosticsAlreadyPending",
            _ => "publishProjectWithAnalyzersDiagnosticsVersionMismatch"
        };

    internal readonly record struct SaveDiagnosticsPolicy(
        bool IncludeWarmup,
        int WarmupDelayMilliseconds,
        DocumentStore.DiagnosticLane InitialMode,
        int? FollowUpDiagnosticsDelayMilliseconds,
        DocumentStore.DiagnosticLane? FollowUpMode,
        int? AnalyzerFollowUpDiagnosticsDelayMilliseconds,
        DocumentStore.DiagnosticLane? AnalyzerFollowUpMode,
        int DiagnosticsDelayMilliseconds);

    internal enum PublishDiagnosticsOutcome
    {
        Published,
        SkippedRequeued,
        SkippedUnchanged,
        SkippedVersionMismatch,
        SkippedAlreadyCompleted,
        SkippedAlreadyPending
    }

    private bool IsDiagnosticsPublishAlreadyCompleted(
        DocumentUri uri,
        int? version,
        DocumentStore.DiagnosticLane lane)
        => version is { } stableVersion &&
           _completedDiagnostics.ContainsKey(new CompletedDiagnosticsKey(uri, stableVersion, lane));

    private void MarkDiagnosticsPublishCompleted(
        DocumentUri uri,
        int? version,
        DocumentStore.DiagnosticLane lane)
    {
        if (version is { } stableVersion)
            _completedDiagnostics[new CompletedDiagnosticsKey(uri, stableVersion, lane)] = 0;
    }

    private bool TryStartDiagnosticsPublish(
        DocumentUri uri,
        int? version,
        DocumentStore.DiagnosticLane lane)
        => version is not { } stableVersion ||
           _activeDiagnostics.TryAdd(new CompletedDiagnosticsKey(uri, stableVersion, lane), 0);

    private void CompleteDiagnosticsPublish(
        DocumentUri uri,
        int? version,
        DocumentStore.DiagnosticLane lane)
    {
        if (version is { } stableVersion)
            _activeDiagnostics.TryRemove(new CompletedDiagnosticsKey(uri, stableVersion, lane), out _);
    }

    private void ClearCompletedDiagnostics(DocumentUri uri)
    {
        foreach (var key in _completedDiagnostics.Keys)
        {
            if (key.Uri == uri)
                _completedDiagnostics.TryRemove(key, out _);
        }

        foreach (var key in _activeDiagnostics.Keys)
        {
            if (key.Uri == uri)
                _activeDiagnostics.TryRemove(key, out _);
        }

        foreach (var key in _pendingDiagnosticsRetries.Keys)
        {
            if (key.Uri == uri)
                _pendingDiagnosticsRetries.TryRemove(key, out _);
        }
    }

    private void ClearCompletedDiagnostics(
        DocumentUri uri,
        int? version,
        DocumentStore.DiagnosticLane lane)
    {
        if (version is { } stableVersion)
            _completedDiagnostics.TryRemove(new CompletedDiagnosticsKey(uri, stableVersion, lane), out _);
    }

    private readonly record struct CompletedDiagnosticsKey(
        DocumentUri Uri,
        int Version,
        DocumentStore.DiagnosticLane Lane);

    private readonly record struct PendingDiagnosticsRetryKey(
        DocumentUri Uri,
        long Session,
        int Version,
        DocumentStore.DiagnosticLane Lane);

    internal sealed record PendingDiagnosticsRequest(
        long Id,
        CancellationTokenSource Cancellation,
        long ExpectedSession,
        int? ExpectedVersion,
        string Reason);

    internal readonly record struct VersionedDiagnostics(
        int? Version,
        DocumentStore.DiagnosticSnapshotKey? SnapshotKey,
        ImmutableArray<Diagnostic> Diagnostics);

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
