using System.Collections.Concurrent;
using System.Collections.Immutable;

using Microsoft.Extensions.Logging;

using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Text;

using Diagnostic = OmniSharp.Extensions.LanguageServer.Protocol.Models.Diagnostic;
using LspRange = OmniSharp.Extensions.LanguageServer.Protocol.Models.Range;

namespace Raven.LanguageServer;

internal sealed class LanguageServerDispatcher
{
    private const int MaxRecentEvents = 512;
    private const int MaxCachedInlayHintEntries = 256;

    private readonly DocumentStore _documents;
    private readonly ILogger<LanguageServerDispatcher> _logger;
    private readonly ConcurrentDictionary<DocumentUri, int> _lastPublishedDiagnosticVersions = new();
    private readonly ConcurrentDictionary<DocumentUri, ImmutableArray<RavenTextDocumentSyncHandler.PublishedDiagnosticValue>> _lastPublishedDiagnostics = new();
    private readonly ConcurrentDictionary<DocumentUri, ImmutableArray<Diagnostic>> _lastPublishedDiagnosticObjects = new();
    private readonly ConcurrentDictionary<DocumentUri, SourceText> _lastPublishedDiagnosticSourceTexts = new();
    private readonly ConcurrentDictionary<DocumentUri, DocumentStore.DiagnosticLane> _lastPublishedDiagnosticLanes = new();
    private readonly ConcurrentDictionary<DocumentUri, RavenTextDocumentSyncHandler.VersionedDiagnostics> _lastPublishedAnalyzerDiagnostics = new();
    private readonly ConcurrentDictionary<InlayHintDocumentCacheKey, ImmutableArray<InlayHint>> _inlayHintCache = new();
    private readonly ConcurrentDictionary<string, InlayHintDocumentCacheEntry> _latestInlayHintDocumentCache = new();
    private readonly ConcurrentQueue<LanguageServerDispatcherEvent> _recentEvents = new();
    private long _eventSequence;
    private long _workspaceEpoch;

    public LanguageServerDispatcher(DocumentStore documents, ILogger<LanguageServerDispatcher> logger)
    {
        _documents = documents;
        _logger = logger;
    }

    public long CurrentEpoch => Volatile.Read(ref _workspaceEpoch);

    public long CurrentSequence => Volatile.Read(ref _eventSequence);

    public ImmutableArray<LanguageServerDispatcherEvent> GetRecentEvents()
        => _recentEvents.ToImmutableArray();

    public void ClearPresentationState(DocumentUri uri, string reason)
    {
        _lastPublishedDiagnosticVersions.TryRemove(uri, out _);
        _lastPublishedDiagnostics.TryRemove(uri, out _);
        _lastPublishedDiagnosticObjects.TryRemove(uri, out _);
        _lastPublishedDiagnosticSourceTexts.TryRemove(uri, out _);
        _lastPublishedDiagnosticLanes.TryRemove(uri, out _);
        _lastPublishedAnalyzerDiagnostics.TryRemove(uri, out _);
        _latestInlayHintDocumentCache.TryRemove(uri.ToString(), out _);
        RecordWorkItemEvent(
            "PresentationStateCleared",
            uri,
            reason,
            extraDetail: "diagnostics=true inlays=true");
    }

    public long RecordWorkspaceEvent(
        string eventName,
        DocumentUri uri,
        string reason,
        int? editorVersion = null,
        long? documentSession = null)
    {
        var epoch = Interlocked.Increment(ref _workspaceEpoch);
        TryCaptureSnapshot(uri, documentSession ?? 0, epoch, out var snapshot);
        RecordEvent(
            "dispatcherWorkspaceEvent",
            uri,
            editorVersion,
            eventName,
            reason,
            snapshot,
            extraDetail: null);
        return epoch;
    }

    public void RecordWorkItemEvent(
        string eventName,
        DocumentUri uri,
        string reason,
        WorkspaceSnapshotKey? snapshot = null,
        int? editorVersion = null,
        string? extraDetail = null)
        => RecordEvent(
            "dispatcherWorkItemEvent",
            uri,
            editorVersion,
            eventName,
            reason,
            snapshot,
            extraDetail);

    public bool TryCaptureSnapshot(
        DocumentUri uri,
        long documentSession,
        out WorkspaceSnapshotKey snapshot)
        => TryCaptureSnapshot(uri, documentSession, CurrentEpoch, out snapshot);

    public bool TryCaptureSnapshot(
        DocumentUri uri,
        long documentSession,
        long workspaceEpoch,
        out WorkspaceSnapshotKey snapshot)
    {
        if (!_documents.TryGetDocument(uri, out var document) || document is null)
        {
            snapshot = default;
            return false;
        }

        snapshot = CreateSnapshot(uri, document, documentSession, workspaceEpoch);
        return true;
    }

    public WorkspaceSnapshotKey CreateSnapshot(
        DocumentUri uri,
        Document document,
        long documentSession)
        => CreateSnapshot(uri, document, documentSession, CurrentEpoch);

    public WorkspaceSnapshotKey CreateSnapshot(
        DocumentUri uri,
        Document document,
        long documentSession,
        long workspaceEpoch)
        => new(
            uri,
            document.Project.Id,
            document.Version,
            document.Project.Version,
            documentSession,
            workspaceEpoch);

    public bool IsCurrent(WorkspaceSnapshotKey snapshot)
    {
        if (!_documents.TryGetDocument(snapshot.Uri, out var currentDocument) ||
            currentDocument is null)
        {
            return false;
        }

        return IsCurrent(snapshot, currentDocument);
    }

    public static bool IsCurrent(WorkspaceSnapshotKey snapshot, Document currentDocument)
        => snapshot.ProjectId == currentDocument.Project.Id &&
           snapshot.DocumentVersion == currentDocument.Version &&
           snapshot.ProjectVersion == currentDocument.Project.Version;

    public bool IsCurrent(DocumentStore.DiagnosticSnapshotKey snapshot)
    {
        var uri = DocumentUri.From(snapshot.Uri);
        if (!_documents.TryGetDocument(uri, out var currentDocument) ||
            currentDocument is null)
        {
            return false;
        }

        return snapshot.ProjectId == currentDocument.Project.Id &&
               snapshot.DocumentVersion == currentDocument.Version &&
               snapshot.ProjectVersion == currentDocument.Project.Version;
    }

    public bool ShouldAcceptInlayResult(
        WorkspaceSnapshotKey snapshot,
        string reason,
        out WorkspaceSnapshotKey? currentSnapshot)
    {
        if (IsCurrent(snapshot))
        {
            currentSnapshot = snapshot;
            RecordWorkItemEvent(
                "InlayResultAccepted",
                snapshot.Uri,
                reason,
                snapshot,
                extraDetail: "outcome=Accepted");
            return true;
        }

        currentSnapshot = TryCaptureSnapshot(
            snapshot.Uri,
            snapshot.DocumentSession,
            out var latest)
            ? latest
            : null;
        RecordWorkItemEvent(
            "InlayResultDiscarded",
            snapshot.Uri,
            reason,
            snapshot,
            extraDetail: FormatCurrentSnapshotDetail("outcome=DiscardedStale", currentSnapshot));
        return false;
    }

    public bool ShouldAcceptDiagnosticsResult(
        DocumentUri uri,
        DocumentStore.DiagnosticSnapshotKey? snapshot,
        DocumentStore.DiagnosticLane lane,
        string reason,
        int? editorVersion)
    {
        if (snapshot is null)
            return true;

        if (IsCurrent(snapshot.Value))
        {
            RecordDiagnosticsDecision(
                "DiagnosticsResultAccepted",
                uri,
                snapshot.Value,
                lane,
                reason,
                editorVersion,
                "outcome=Accepted");
            return true;
        }

        var currentSnapshot = TryCaptureSnapshot(
            uri,
            documentSession: 0,
            out var latest)
            ? latest
            : (WorkspaceSnapshotKey?)null;
        RecordDiagnosticsDecision(
            "DiagnosticsResultDiscarded",
            uri,
            snapshot.Value,
            lane,
            reason,
            editorVersion,
            FormatCurrentSnapshotDetail("outcome=DiscardedStale", currentSnapshot));
        return false;
    }

    public DiagnosticPresentationResult AcceptDiagnosticsForPublish(
        DocumentUri uri,
        DocumentStore.DiagnosticLane lane,
        IReadOnlyList<Diagnostic> diagnostics,
        int? editorVersion,
        DocumentStore.DiagnosticSnapshotKey? snapshotKey,
        SourceText? sourceText = null)
    {
        var carriedAnalyzerDiagnostics = _lastPublishedAnalyzerDiagnostics.TryGetValue(uri, out var previousAnalyzers)
            ? RavenTextDocumentSyncHandler.GetCarryForwardAnalyzerDiagnosticsForPresentation(
                editorVersion,
                snapshotKey,
                previousAnalyzers)
            : [];
        var diagnosticsToPublish = RavenTextDocumentSyncHandler.MergePartialDiagnosticsForPublish(
            lane,
            diagnostics,
            carriedAnalyzerDiagnostics);
        diagnosticsToPublish = MergeStickySyntaxDiagnosticsForPublish(
            uri,
            lane,
            sourceText,
            diagnosticsToPublish,
            out var forceStickyPublish,
            out var carriedStickyDiagnostics);
        var diagnosticValues = RavenTextDocumentSyncHandler.CreatePublishedDiagnosticValues(diagnosticsToPublish);
        var lastPublishedVersion = _lastPublishedDiagnosticVersions.TryGetValue(uri, out var publishedVersion)
            ? publishedVersion
            : (int?)null;
        var hasLastPublishedDiagnostics = _lastPublishedDiagnostics.TryGetValue(uri, out var lastPublishedDiagnostics);
        var shouldPublish = forceStickyPublish ||
            RavenTextDocumentSyncHandler.ShouldPublishDiagnostics(
                hasLastPublishedDiagnostics,
                lastPublishedDiagnostics,
                lastPublishedVersion,
                diagnosticValues,
                editorVersion);

        if (!shouldPublish)
        {
            if (editorVersion is { } stableVersion)
                _lastPublishedDiagnosticVersions[uri] = stableVersion;

            UpdatePublishedAnalyzerDiagnostics(uri, lane, editorVersion, snapshotKey, diagnosticsToPublish);
            RecordDiagnosticsPresentationDecision(
                "DiagnosticsPresentationUnchanged",
                uri,
                lane,
                editorVersion,
                snapshotKey,
                diagnosticsToPublish,
                "outcome=Unchanged");
            return new DiagnosticPresentationResult(diagnosticsToPublish, ShouldPublish: false);
        }

        _lastPublishedDiagnostics[uri] = diagnosticValues;
        _lastPublishedDiagnosticObjects[uri] = diagnosticsToPublish;
        if (sourceText is not null)
            _lastPublishedDiagnosticSourceTexts[uri] = sourceText;

        _lastPublishedDiagnosticLanes[uri] = GetPublishedDiagnosticsLane(
            uri,
            lane,
            carriedStickyDiagnostics);
        if (editorVersion is { } completedVersion)
            _lastPublishedDiagnosticVersions[uri] = completedVersion;

        UpdatePublishedAnalyzerDiagnostics(uri, lane, editorVersion, snapshotKey, diagnosticsToPublish);
        RecordDiagnosticsPresentationDecision(
            "DiagnosticsPresentationChanged",
            uri,
            lane,
            editorVersion,
            snapshotKey,
            diagnosticsToPublish,
            "outcome=Changed");
        return new DiagnosticPresentationResult(diagnosticsToPublish, ShouldPublish: true);
    }

    private ImmutableArray<Diagnostic> MergeStickySyntaxDiagnosticsForPublish(
        DocumentUri uri,
        DocumentStore.DiagnosticLane lane,
        SourceText? sourceText,
        ImmutableArray<Diagnostic> diagnostics,
        out bool forcePublish,
        out bool carriedPreviousDiagnostics)
    {
        forcePublish = false;
        carriedPreviousDiagnostics = false;
        if (lane != DocumentStore.DiagnosticLane.Syntax ||
            sourceText is null ||
            !_lastPublishedDiagnosticLanes.TryGetValue(uri, out var previousLane) ||
            previousLane == DocumentStore.DiagnosticLane.Syntax ||
            !_lastPublishedDiagnosticObjects.TryGetValue(uri, out var previousDiagnostics) ||
            previousDiagnostics.IsDefaultOrEmpty ||
            !_lastPublishedDiagnosticSourceTexts.TryGetValue(uri, out var previousSourceText))
        {
            return diagnostics;
        }

        var existing = RavenTextDocumentSyncHandler.CreatePublishedDiagnosticValues(diagnostics).ToHashSet();
        var builder = diagnostics.ToBuilder();
        var carriedAnyPreviousDiagnostic = false;

        foreach (var previous in previousDiagnostics)
        {
            if (!TryTranslateDiagnostic(previousSourceText, sourceText, previous, out var translated))
                continue;

            carriedAnyPreviousDiagnostic = true;
            carriedPreviousDiagnostics = true;
            if (existing.Add(RavenTextDocumentSyncHandler.PublishedDiagnosticValue.From(translated)))
                builder.Add(translated);
        }

        forcePublish = carriedAnyPreviousDiagnostic &&
            !previousSourceText.ContentEquals(sourceText);

        return builder
            .OrderBy(static diagnostic => RavenTextDocumentSyncHandler.PublishedDiagnosticValue.From(diagnostic))
            .ToImmutableArray();
    }

    private DocumentStore.DiagnosticLane GetPublishedDiagnosticsLane(
        DocumentUri uri,
        DocumentStore.DiagnosticLane lane,
        bool carriedStickyDiagnostics)
    {
        if (lane != DocumentStore.DiagnosticLane.Syntax ||
            !carriedStickyDiagnostics ||
            !_lastPublishedDiagnosticLanes.TryGetValue(uri, out var previousLane) ||
            previousLane == DocumentStore.DiagnosticLane.Syntax)
        {
            return lane;
        }

        return previousLane;
    }

    public bool TryGetCachedInlayHints(InlayHintParams request, SourceText? currentSourceText, out InlayHint[] hints)
    {
        var uri = request.TextDocument.Uri;
        hints = [];

        if (!_documents.TryGetDocument(uri, out var document) ||
            document is null)
        {
            RecordWorkItemEvent(
                "InlayPresentationCacheMiss",
                uri,
                "cacheLookup",
                extraDetail: "outcome=NoDocument");
            return false;
        }

        var hasLatestCache = _latestInlayHintDocumentCache.TryGetValue(uri.ToString(), out var latestCache);
        var editorTextMatchesLatestCache = currentSourceText is null ||
            hasLatestCache &&
            latestCache.SourceText.ContentEquals(currentSourceText);

        if (editorTextMatchesLatestCache &&
            _inlayHintCache.TryGetValue(CreateInlayCacheKey(document), out var cachedHints))
        {
            hints = cachedHints
                .Where(hint => IsInRange(hint.Position, request.Range))
                .ToArray();
            if (hints.Length > 0)
            {
                RecordInlayPresentationDecision(
                    "InlayPresentationCacheHit",
                    uri,
                    document,
                    request.Range,
                    hints.Length,
                    "outcome=ExactSnapshot");
                return true;
            }
        }

        if (currentSourceText is not null &&
            editorTextMatchesLatestCache &&
            hasLatestCache &&
            latestCache.DocumentVersion != document.Version &&
            !latestCache.Hints.IsDefaultOrEmpty)
        {
            hints = latestCache.Hints
                .Where(hint => IsInRange(hint.Position, request.Range))
                .ToArray();
            if (hints.Length > 0)
            {
                RecordInlayPresentationDecision(
                    "InlayPresentationCacheHit",
                    uri,
                    document,
                    request.Range,
                    hints.Length,
                    "outcome=LatestSourceText");
                return true;
            }
        }

        if (currentSourceText is null ||
            !hasLatestCache ||
            latestCache.Hints.IsDefaultOrEmpty ||
            latestCache.SourceText.ContentEquals(currentSourceText))
        {
            RecordInlayPresentationDecision(
                "InlayPresentationCacheMiss",
                uri,
                document,
                request.Range,
                0,
                "outcome=NoReusableSnapshot");
            return false;
        }

        var hasStickyHints = TryCreateStickyHints(
            latestCache.SourceText,
            currentSourceText,
            request.Range,
            latestCache.Hints,
            out hints);

        RecordInlayPresentationDecision(
            hasStickyHints ? "InlayPresentationStickyHit" : "InlayPresentationCacheMiss",
            uri,
            document,
            request.Range,
            hints.Length,
            hasStickyHints
                ? "outcome=TranslatedSnapshot"
                : "outcome=TranslationFailed");
        return hasStickyHints;
    }

    public void CacheInlayHints(InlayHintParams request, Document document, SourceText sourceText, InlayHint[] hints)
    {
        if (_inlayHintCache.Count >= MaxCachedInlayHintEntries)
            _inlayHintCache.Clear();

        var key = CreateInlayCacheKey(document);
        var updatedHints = _inlayHintCache.AddOrUpdate(
            key,
            hints.ToImmutableArray(),
            (_, existing) => MergeCachedInlayHints(existing, hints, request.Range));

        _latestInlayHintDocumentCache[request.TextDocument.Uri.ToString()] = new InlayHintDocumentCacheEntry(
            document.Version,
            document.Project.Version,
            sourceText,
            updatedHints);

        RecordInlayPresentationDecision(
            "InlayPresentationCached",
            request.TextDocument.Uri,
            document,
            request.Range,
            updatedHints.Length,
            $"outcome=Cached incomingCount={hints.Length}");
    }

    private void RecordDiagnosticsDecision(
        string eventName,
        DocumentUri uri,
        DocumentStore.DiagnosticSnapshotKey snapshot,
        DocumentStore.DiagnosticLane lane,
        string reason,
        int? editorVersion,
        string extraDetail)
    {
        var detail = $"snapshotUri={snapshot.Uri} projectId={snapshot.ProjectId} documentVersion={snapshot.DocumentVersion} projectVersion={snapshot.ProjectVersion} lane={lane} {extraDetail}";
        RecordEvent(
            "dispatcherWorkItemEvent",
            uri,
            editorVersion,
            eventName,
            reason,
            snapshot: null,
            extraDetail: detail);
    }

    private void RecordDiagnosticsPresentationDecision(
        string eventName,
        DocumentUri uri,
        DocumentStore.DiagnosticLane lane,
        int? editorVersion,
        DocumentStore.DiagnosticSnapshotKey? snapshotKey,
        ImmutableArray<Diagnostic> diagnostics,
        string extraDetail)
    {
        var snapshotDetail = snapshotKey is { } snapshot
            ? $"snapshotUri={snapshot.Uri} projectId={snapshot.ProjectId} documentVersion={snapshot.DocumentVersion} projectVersion={snapshot.ProjectVersion}"
            : "snapshot=<none>";
        var detail = $"{snapshotDetail} lane={lane} count={diagnostics.Length} summary={RavenTextDocumentSyncHandler.SummarizeDiagnosticsForLog(diagnostics)} {extraDetail}";
        RecordEvent(
            "dispatcherWorkItemEvent",
            uri,
            editorVersion,
            eventName,
            reason: $"presentation:{lane}",
            snapshot: null,
            extraDetail: detail);
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
            .Where(RavenTextDocumentSyncHandler.CanCarryForwardAnalyzerDiagnostic)
            .ToImmutableArray();
        if (analyzerDiagnostics.IsDefaultOrEmpty)
        {
            _lastPublishedAnalyzerDiagnostics.TryRemove(uri, out _);
            return;
        }

        _lastPublishedAnalyzerDiagnostics[uri] = new RavenTextDocumentSyncHandler.VersionedDiagnostics(version, snapshotKey, analyzerDiagnostics);
    }

    private void RecordInlayPresentationDecision(
        string eventName,
        DocumentUri uri,
        Document document,
        LspRange range,
        int count,
        string extraDetail)
    {
        var snapshot = CreateSnapshot(uri, document, documentSession: 0);
        RecordWorkItemEvent(
            eventName,
            uri,
            reason: "presentation:inlayHint",
            snapshot,
            extraDetail: $"range={range.Start.Line}:{range.Start.Character}-{range.End.Line}:{range.End.Character} count={count} {extraDetail}");
    }

    private static InlayHintDocumentCacheKey CreateInlayCacheKey(Document document)
        => new(document.FilePath ?? document.Name, document.Version, document.Project.Version);

    private static ImmutableArray<InlayHint> MergeCachedInlayHints(
        ImmutableArray<InlayHint> existing,
        InlayHint[] incoming,
        LspRange requestRange)
    {
        if (existing.IsDefaultOrEmpty)
            return incoming.ToImmutableArray();

        var merged = existing
            .Where(hint => !IsInRange(hint.Position, requestRange))
            .ToList();
        var seen = merged
            .Select(static hint => (hint.Position.Line, hint.Position.Character, hint.Label.String))
            .ToHashSet();

        foreach (var hint in incoming)
        {
            if (seen.Add((hint.Position.Line, hint.Position.Character, hint.Label.String)))
                merged.Add(hint);
        }

        return merged.ToImmutableArray();
    }

    private static bool TryCreateStickyHints(
        SourceText oldText,
        SourceText newText,
        LspRange requestRange,
        ImmutableArray<InlayHint> oldHints,
        out InlayHint[] hints)
    {
        hints = [];

        var changes = newText.GetTextChanges(oldText);
        if (changes.Count == 0)
        {
            hints = oldHints
                .Where(hint => IsInRange(hint.Position, requestRange))
                .ToArray();
            return hints.Length > 0;
        }

        var translated = new List<InlayHint>();
        foreach (var hint in oldHints)
        {
            if (!TryTranslateHint(oldText, newText, changes, hint, out var translatedHint))
                continue;

            if (IsInRange(translatedHint.Position, requestRange))
                translated.Add(translatedHint);
        }

        hints = translated.ToArray();
        return hints.Length > 0;
    }

    private static bool TryTranslateHint(
        SourceText oldText,
        SourceText newText,
        IReadOnlyList<TextChange> changes,
        InlayHint hint,
        [System.Diagnostics.CodeAnalysis.NotNullWhen(true)] out InlayHint? translatedHint)
    {
        translatedHint = null;
        var oldOffset = PositionHelper.ToOffset(oldText, hint.Position);
        if (!TryTranslateOffset(changes, oldOffset, out var newOffset))
            return false;

        var newPosition = PositionHelper.ToRange(newText, new TextSpan(newOffset, 0)).Start;
        var translatedTextEdits = TranslateTextEdits(oldText, newText, changes, hint.TextEdits);
        if (hint.TextEdits is not null && translatedTextEdits is null)
            return false;

        translatedHint = new InlayHint
        {
            Position = newPosition,
            Label = hint.Label,
            Kind = hint.Kind,
            Tooltip = hint.Tooltip,
            PaddingLeft = hint.PaddingLeft,
            PaddingRight = hint.PaddingRight,
            TextEdits = translatedTextEdits,
            Data = hint.Data
        };
        return true;
    }

    private static bool TryTranslateDiagnostic(
        SourceText oldText,
        SourceText newText,
        Diagnostic diagnostic,
        [System.Diagnostics.CodeAnalysis.NotNullWhen(true)] out Diagnostic? translatedDiagnostic)
    {
        translatedDiagnostic = null;

        var changes = newText.GetTextChanges(oldText);
        var oldStart = PositionHelper.ToOffset(oldText, diagnostic.Range.Start);
        var oldEnd = PositionHelper.ToOffset(oldText, diagnostic.Range.End);
        var oldSpan = TextSpan.FromBounds(oldStart, oldEnd);
        if (changes.Any(change => TextChangeTouchesDiagnosticSpan(change, oldSpan)))
            return false;

        if (!TryTranslateOffset(changes, oldStart, out var newStart) ||
            !TryTranslateOffset(changes, oldEnd, out var newEnd))
        {
            return false;
        }

        translatedDiagnostic = new Diagnostic
        {
            Code = diagnostic.Code,
            CodeDescription = diagnostic.CodeDescription,
            Data = diagnostic.Data,
            Message = diagnostic.Message,
            Range = PositionHelper.ToRange(newText, TextSpan.FromBounds(newStart, newEnd)),
            RelatedInformation = diagnostic.RelatedInformation,
            Severity = diagnostic.Severity,
            Source = diagnostic.Source,
            Tags = diagnostic.Tags
        };
        return true;
    }

    private static bool TextChangeTouchesDiagnosticSpan(TextChange change, TextSpan diagnosticSpan)
    {
        if (change.Span.IntersectsWith(diagnosticSpan))
            return true;

        var diagnosticEnd = diagnosticSpan.Start + diagnosticSpan.Length;
        var changeEnd = change.Span.Start + change.Span.Length;

        if (diagnosticSpan.Length == 0)
            return change.Span.Start <= diagnosticSpan.Start && diagnosticSpan.Start <= changeEnd;

        if (change.Span.Length == 0)
            return diagnosticSpan.Start <= change.Span.Start && change.Span.Start <= diagnosticEnd;

        return false;
    }

    private static Container<TextEdit>? TranslateTextEdits(
        SourceText oldText,
        SourceText newText,
        IReadOnlyList<TextChange> changes,
        Container<TextEdit>? textEdits)
    {
        if (textEdits is null)
            return null;

        var translated = new List<TextEdit>();
        foreach (var textEdit in textEdits)
        {
            var oldStart = PositionHelper.ToOffset(oldText, textEdit.Range.Start);
            var oldEnd = PositionHelper.ToOffset(oldText, textEdit.Range.End);
            if (!TryTranslateOffset(changes, oldStart, out var newStart) ||
                !TryTranslateOffset(changes, oldEnd, out var newEnd))
            {
                return null;
            }

            translated.Add(new TextEdit
            {
                Range = PositionHelper.ToRange(newText, TextSpan.FromBounds(newStart, newEnd)),
                NewText = textEdit.NewText
            });
        }

        return new Container<TextEdit>(translated);
    }

    private static bool TryTranslateOffset(IReadOnlyList<TextChange> changes, int oldOffset, out int newOffset)
    {
        var accumulatedDelta = 0;

        foreach (var change in changes.OrderBy(static change => change.Span.Start))
        {
            var oldStart = change.Span.Start;
            var oldEnd = change.Span.End;

            if (oldOffset < oldStart)
            {
                newOffset = oldOffset + accumulatedDelta;
                return true;
            }

            if (oldOffset <= oldEnd)
            {
                newOffset = oldStart + accumulatedDelta + change.NewText.Length;
                return true;
            }

            accumulatedDelta += change.NewText.Length - change.Span.Length;
        }

        newOffset = oldOffset + accumulatedDelta;
        return true;
    }

    private static bool IsInRange(Position position, LspRange range)
    {
        if (position.Line < range.Start.Line || position.Line > range.End.Line)
            return false;

        if (position.Line == range.Start.Line && position.Character < range.Start.Character)
            return false;

        if (position.Line == range.End.Line && position.Character > range.End.Character)
            return false;

        return true;
    }

    private static string FormatCurrentSnapshotDetail(string prefix, WorkspaceSnapshotKey? currentSnapshot)
        => currentSnapshot is { } current
            ? $"{prefix} currentEpoch={current.WorkspaceEpoch} currentDocumentVersion={current.DocumentVersion} currentProjectVersion={current.ProjectVersion}"
            : $"{prefix} currentSnapshot=<missing>";

    private void RecordEvent(
        string operation,
        DocumentUri uri,
        int? editorVersion,
        string eventName,
        string reason,
        WorkspaceSnapshotKey? snapshot,
        string? extraDetail)
    {
        var sequence = Interlocked.Increment(ref _eventSequence);
        var detail = $"event={eventName} reason={reason}";
        if (snapshot is { } value)
        {
            detail += $" sequence={sequence} epoch={value.WorkspaceEpoch} projectId={value.ProjectId} documentSession={value.DocumentSession} documentVersion={value.DocumentVersion} projectVersion={value.ProjectVersion}";
        }
        else
        {
            detail += $" sequence={sequence} epoch={CurrentEpoch}";
        }

        if (!string.IsNullOrWhiteSpace(extraDetail))
            detail += $" {extraDetail}";

        EnqueueEvent(new LanguageServerDispatcherEvent(
            sequence,
            operation,
            eventName,
            uri,
            reason,
            editorVersion,
            snapshot,
            CurrentEpoch,
            extraDetail));

        LanguageServerPerformanceInstrumentation.RecordOperation(
            operation,
            uri,
            editorVersion,
            0,
            detail: $"{uri} {detail}");

        _logger.LogDebug(
            "Dispatcher {EventName} for {Uri}: reason={Reason}, editorVersion={EditorVersion}, detail={Detail}.",
            eventName,
            uri,
            reason,
            editorVersion?.ToString() ?? "<none>",
            detail);
    }

    private void EnqueueEvent(LanguageServerDispatcherEvent dispatcherEvent)
    {
        _recentEvents.Enqueue(dispatcherEvent);
        while (_recentEvents.Count > MaxRecentEvents)
            _recentEvents.TryDequeue(out _);
    }
}

internal readonly record struct LanguageServerDispatcherEvent(
    long Sequence,
    string Operation,
    string EventName,
    DocumentUri Uri,
    string Reason,
    int? EditorVersion,
    WorkspaceSnapshotKey? Snapshot,
    long WorkspaceEpoch,
    string? Detail);

internal readonly record struct DiagnosticPresentationResult(
    ImmutableArray<Diagnostic> Diagnostics,
    bool ShouldPublish);

internal readonly record struct InlayHintDocumentCacheKey(
    string Uri,
    VersionStamp DocumentVersion,
    VersionStamp ProjectVersion);

internal readonly record struct InlayHintDocumentCacheEntry(
    VersionStamp DocumentVersion,
    VersionStamp ProjectVersion,
    SourceText SourceText,
    ImmutableArray<InlayHint> Hints);

internal readonly record struct WorkspaceSnapshotKey(
    DocumentUri Uri,
    ProjectId ProjectId,
    VersionStamp DocumentVersion,
    VersionStamp ProjectVersion,
    long DocumentSession,
    long WorkspaceEpoch);
