using System.Collections.Concurrent;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.IO;

using Microsoft.Extensions.Logging;

using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

using CodeDiagnostic = Raven.CodeAnalysis.Diagnostic;
using CodeDiagnosticSeverity = Raven.CodeAnalysis.DiagnosticSeverity;
using LspDiagnostic = OmniSharp.Extensions.LanguageServer.Protocol.Models.Diagnostic;
using LspDiagnosticSeverity = OmniSharp.Extensions.LanguageServer.Protocol.Models.DiagnosticSeverity;
using LspRange = OmniSharp.Extensions.LanguageServer.Protocol.Models.Range;
using SyntaxTree = Raven.CodeAnalysis.Syntax.SyntaxTree;

namespace Raven.LanguageServer;

internal sealed class DocumentStore
{
    private const double SlowCompilerGateThresholdMs = 100;
    private const double SlowAnalysisContextThresholdMs = 100;
    private const double SlowSemanticModelMaterializationThresholdMs = 100;
    private const double SlowDiagnosticsThresholdMs = 150;
    private const double SlowWarmAnalysisThresholdMs = 250;

    internal readonly record struct DocumentAnalysisContext(
        Document Document,
        Compilation Compilation,
        SyntaxTree SyntaxTree,
        Raven.CodeAnalysis.Text.SourceText SourceText);

    internal readonly record struct DiagnosticsComputationResult(
        IReadOnlyList<LspDiagnostic> Diagnostics,
        bool WasSkipped);

    internal enum DocumentDiagnosticsMode
    {
        Full,
        SyntaxOnly
    }

    private static readonly HashSet<string> UnnecessaryDiagnosticIds = new(StringComparer.OrdinalIgnoreCase)
    {
        CompilerDiagnostics.UnreachableCodeDetected.Id,
        Raven.CodeAnalysis.Diagnostics.UnusedVariableAnalyzer.DiagnosticId
    };

    private readonly WorkspaceManager _workspaceManager;
    private readonly ILogger<DocumentStore> _logger;
    private readonly SemaphoreSlim _compilerAccessGate = new(1, 1);
    private readonly ConcurrentDictionary<string, SemaphoreSlim> _documentSemanticGates = new(StringComparer.Ordinal);
    private readonly ConcurrentDictionary<DocumentAnalysisCacheKey, CachedDocumentAnalysis> _documentAnalysisCache = new();

    public DocumentStore(WorkspaceManager workspaceManager, ILogger<DocumentStore> logger)
    {
        _workspaceManager = workspaceManager;
        _logger = logger;
    }

    public Document UpsertDocument(DocumentUri uri, string text)
    {
        InvalidateDocumentAnalysis(uri);
        return _workspaceManager.UpsertDocument(uri, text);
    }

    public bool TryGetDocument(DocumentUri uri, [NotNullWhen(true)] out Document? document)
        => _workspaceManager.TryGetDocument(uri, out document);

    public bool TryGetDocumentContext(
        DocumentUri uri,
        [NotNullWhen(true)] out Document? document,
        [NotNullWhen(true)] out Compilation? compilation)
        => _workspaceManager.TryGetDocumentContext(uri, out document, out compilation);

    public async Task<DocumentAnalysisContext?> GetAnalysisContextAsync(DocumentUri uri, CancellationToken cancellationToken)
    {
        var analysis = await GetOrCreateDocumentAnalysisAsync(uri, cancellationToken).ConfigureAwait(false);
        return analysis?.Context;
    }

    internal async Task<SemanticModel?> GetSemanticModelAsync(DocumentUri uri, CancellationToken cancellationToken)
    {
        var context = await GetAnalysisContextAsync(uri, cancellationToken).ConfigureAwait(false);
        if (context is null)
            return null;

        var stopwatch = Stopwatch.StartNew();
        var setupBefore = context.Value.Compilation.PerformanceInstrumentation.Setup.CaptureSnapshot();
        var semanticModel = context.Value.Compilation.GetSemanticModel(context.Value.SyntaxTree);
        stopwatch.Stop();

        if (stopwatch.Elapsed.TotalMilliseconds >= SlowSemanticModelMaterializationThresholdMs)
        {
            var setupAfter = context.Value.Compilation.PerformanceInstrumentation.Setup.CaptureSnapshot();
            var setupDelta = CompilerSetupInstrumentation.Subtract(setupAfter, setupBefore);
            _logger.LogInformation(
                "Slow semantic model materialization for {Uri}: total={TotalMs:F1}ms setupDelta=[{SetupDelta}].",
                uri,
                stopwatch.Elapsed.TotalMilliseconds,
                CompilerSetupInstrumentation.FormatDelta(setupDelta));
        }

        return semanticModel;
    }

    private async Task<CachedDocumentAnalysis?> GetOrCreateDocumentAnalysisAsync(DocumentUri uri, CancellationToken cancellationToken)
    {
        var stopwatch = Stopwatch.StartNew();
        double compilationLookupMs = 0;
        double syntaxTreeMs = 0;
        double sourceTextMs = 0;

        if (!TryGetDocumentContext(uri, out var document, out var compilation) ||
            document is null ||
            compilation is null)
        {
            return null;
        }
        compilationLookupMs = stopwatch.Elapsed.TotalMilliseconds;

        var cacheKey = new DocumentAnalysisCacheKey(
            uri.ToString(),
            document.Project.Id,
            document.Id,
            document.Version,
            document.Project.Version);
        if (_documentAnalysisCache.TryGetValue(cacheKey, out var cachedAnalysis))
            return cachedAnalysis;

        var syntaxTree = await document.GetSyntaxTreeAsync(cancellationToken).ConfigureAwait(false);
        if (syntaxTree is null)
            return null;
        syntaxTreeMs = stopwatch.Elapsed.TotalMilliseconds - compilationLookupMs;

        var sourceText = await document.GetTextAsync(cancellationToken).ConfigureAwait(false);
        sourceTextMs = stopwatch.Elapsed.TotalMilliseconds - compilationLookupMs - syntaxTreeMs;
        if (!compilation.SyntaxTrees.Contains(syntaxTree))
        {
            syntaxTree = FindMatchingCompilationSyntaxTree(compilation, syntaxTree, document.FilePath);
            if (syntaxTree is null)
                return null;

            sourceText = syntaxTree.GetText() ?? sourceText;
        }

        stopwatch.Stop();
        if (stopwatch.Elapsed.TotalMilliseconds >= SlowAnalysisContextThresholdMs)
        {
            _logger.LogInformation(
                "Slow analysis context for {Uri}: total={TotalMs:F1}ms compilationLookup={CompilationLookupMs:F1}ms syntaxTree={SyntaxTreeMs:F1}ms sourceText={SourceTextMs:F1}ms.",
                uri,
                stopwatch.Elapsed.TotalMilliseconds,
                compilationLookupMs,
                syntaxTreeMs,
                sourceTextMs);
        }

        var analysis = new CachedDocumentAnalysis(new DocumentAnalysisContext(document, compilation, syntaxTree, sourceText));
        _documentAnalysisCache[cacheKey] = analysis;
        return analysis;
    }

    public bool TryGetCompilation(DocumentUri uri, [NotNullWhen(true)] out Compilation? compilation)
        => _workspaceManager.TryGetCompilation(uri, out compilation);

    public bool RemoveDocument(DocumentUri uri)
    {
        InvalidateDocumentAnalysis(uri);
        return _workspaceManager.RemoveDocument(uri);
    }

    public async ValueTask<IDisposable> EnterCompilerAccessAsync(
        CancellationToken cancellationToken,
        string? purpose = null,
        DocumentUri? uri = null)
    {
        var stopwatch = Stopwatch.StartNew();
        await _compilerAccessGate.WaitAsync(cancellationToken).ConfigureAwait(false);
        stopwatch.Stop();

        if (stopwatch.Elapsed.TotalMilliseconds >= SlowCompilerGateThresholdMs)
        {
            _logger.LogInformation(
                "Slow compiler gate wait for {Purpose} {Uri}: wait={WaitMs:F1}ms.",
                purpose ?? "unknown",
                uri?.ToString() ?? "<none>",
                stopwatch.Elapsed.TotalMilliseconds);
        }

        return new CompilerAccessLease(_compilerAccessGate);
    }

    public async ValueTask<IDisposable?> TryEnterCompilerAccessAsync(
        CancellationToken cancellationToken,
        string? purpose = null,
        DocumentUri? uri = null)
    {
        var stopwatch = Stopwatch.StartNew();
        var acquired = await _compilerAccessGate.WaitAsync(0, cancellationToken).ConfigureAwait(false);
        stopwatch.Stop();

        if (!acquired)
        {
            _logger.LogDebug(
                "Skipped compiler gate acquisition for {Purpose} {Uri}: gate busy.",
                purpose ?? "unknown",
                uri?.ToString() ?? "<none>");
            return null;
        }

        if (stopwatch.Elapsed.TotalMilliseconds >= SlowCompilerGateThresholdMs)
        {
            _logger.LogInformation(
                "Slow compiler gate wait for {Purpose} {Uri}: wait={WaitMs:F1}ms.",
                purpose ?? "unknown",
                uri?.ToString() ?? "<none>",
                stopwatch.Elapsed.TotalMilliseconds);
        }

        return new CompilerAccessLease(_compilerAccessGate);
    }

    public async ValueTask<IDisposable> EnterDocumentSemanticAccessAsync(
        DocumentUri uri,
        CancellationToken cancellationToken,
        string? purpose = null)
    {
        var gate = _documentSemanticGates.GetOrAdd(uri.ToString(), static _ => new SemaphoreSlim(1, 1));
        var stopwatch = Stopwatch.StartNew();
        await gate.WaitAsync(cancellationToken).ConfigureAwait(false);
        stopwatch.Stop();

        if (stopwatch.Elapsed.TotalMilliseconds >= SlowCompilerGateThresholdMs)
        {
            _logger.LogInformation(
                "Slow document semantic gate wait for {Purpose} {Uri}: wait={WaitMs:F1}ms.",
                purpose ?? "unknown",
                uri,
                stopwatch.Elapsed.TotalMilliseconds);
        }

        return new CompilerAccessLease(gate);
    }

    public async ValueTask<IDisposable?> TryEnterDocumentSemanticAccessAsync(
        DocumentUri uri,
        CancellationToken cancellationToken,
        string? purpose = null)
    {
        var gate = _documentSemanticGates.GetOrAdd(uri.ToString(), static _ => new SemaphoreSlim(1, 1));
        var stopwatch = Stopwatch.StartNew();
        var acquired = await gate.WaitAsync(0, cancellationToken).ConfigureAwait(false);
        stopwatch.Stop();

        if (!acquired)
        {
            _logger.LogDebug(
                "Skipped document semantic gate acquisition for {Purpose} {Uri}: gate busy.",
                purpose ?? "unknown",
                uri);
            return null;
        }

        if (stopwatch.Elapsed.TotalMilliseconds >= SlowCompilerGateThresholdMs)
        {
            _logger.LogInformation(
                "Slow document semantic gate wait for {Purpose} {Uri}: wait={WaitMs:F1}ms.",
                purpose ?? "unknown",
                uri,
                stopwatch.Elapsed.TotalMilliseconds);
        }

        return new CompilerAccessLease(gate);
    }

    public async Task<IReadOnlyList<LspDiagnostic>> GetDiagnosticsAsync(
        DocumentUri uri,
        Func<bool>? shouldSkipWork,
        CancellationToken cancellationToken)
    {
        var result = await GetDiagnosticsCoreAsync(uri, shouldSkipWork, allowBusySkip: false, DocumentDiagnosticsMode.Full, cancellationToken).ConfigureAwait(false);
        return result.Diagnostics;
    }

    internal Task<DiagnosticsComputationResult> TryGetDiagnosticsAsync(
        DocumentUri uri,
        DocumentDiagnosticsMode mode,
        Func<bool>? shouldSkipWork,
        CancellationToken cancellationToken)
        => GetDiagnosticsCoreAsync(uri, shouldSkipWork, allowBusySkip: true, mode, cancellationToken);

    private async Task<DiagnosticsComputationResult> GetDiagnosticsCoreAsync(
        DocumentUri uri,
        Func<bool>? shouldSkipWork,
        bool allowBusySkip,
        DocumentDiagnosticsMode mode,
        CancellationToken cancellationToken)
    {
        var stopwatch = Stopwatch.StartNew();
        double gateWaitMs = 0;
        double syntaxTreeMs = 0;
        double diagnosticsFetchMs = 0;
        string? diagnosticsSetupDelta = null;
        string? diagnosticsBinderDelta = null;
        var busySkipped = false;
        var semanticBusySkipped = false;
        var documentOnlyDiagnostics = false;

        try
        {
            if (shouldSkipWork?.Invoke() == true)
                return new DiagnosticsComputationResult(Array.Empty<LspDiagnostic>(), WasSkipped: true);

            var useBusySkip = allowBusySkip && mode == DocumentDiagnosticsMode.SyntaxOnly;

            IDisposable? lease = null;
            if (useBusySkip)
            {
                lease = await TryEnterCompilerAccessAsync(cancellationToken, "diagnostics", uri).ConfigureAwait(false);
                if (lease is null)
                {
                    busySkipped = true;
                    return new DiagnosticsComputationResult(Array.Empty<LspDiagnostic>(), WasSkipped: true);
                }
            }
            else
            {
                lease = await EnterCompilerAccessAsync(cancellationToken, "diagnostics", uri).ConfigureAwait(false);
            }

            using var _ = lease;
            gateWaitMs = stopwatch.Elapsed.TotalMilliseconds;
            if (shouldSkipWork?.Invoke() == true)
                return new DiagnosticsComputationResult(Array.Empty<LspDiagnostic>(), WasSkipped: true);
            var analysis = await GetOrCreateDocumentAnalysisAsync(uri, cancellationToken).ConfigureAwait(false);
            if (analysis is null)
                return new DiagnosticsComputationResult(Array.Empty<LspDiagnostic>(), WasSkipped: false);
            var context = analysis.Context;
            var syntaxTree = context.SyntaxTree;
            syntaxTreeMs = stopwatch.Elapsed.TotalMilliseconds - gateWaitMs;
            if (shouldSkipWork?.Invoke() == true)
                return new DiagnosticsComputationResult(Array.Empty<LspDiagnostic>(), WasSkipped: true);

            IReadOnlyCollection<CodeDiagnostic> diagnosticsForProject;
            if (allowBusySkip)
            {
                IDisposable? semanticLease = null;
                if (mode == DocumentDiagnosticsMode.Full)
                {
                    semanticLease = useBusySkip
                        ? await TryEnterDocumentSemanticAccessAsync(uri, cancellationToken, "diagnostics-semantic").ConfigureAwait(false)
                        : await EnterDocumentSemanticAccessAsync(uri, cancellationToken, "diagnostics-semantic").ConfigureAwait(false);
                    if (semanticLease is null)
                    {
                        semanticBusySkipped = true;
                        return new DiagnosticsComputationResult(Array.Empty<LspDiagnostic>(), WasSkipped: true);
                    }
                }

                using var __ = semanticLease;
                if (mode == DocumentDiagnosticsMode.SyntaxOnly)
                {
                    diagnosticsForProject = analysis.GetSyntaxDiagnostics();
                }
                else
                {
                    var setupBefore = context.Compilation.PerformanceInstrumentation.Setup.CaptureSnapshot();
                    var binderBefore = context.Compilation.PerformanceInstrumentation.BinderReentry.CaptureSnapshot();
                    diagnosticsForProject = analysis.GetDiagnostics();
                    var setupAfter = context.Compilation.PerformanceInstrumentation.Setup.CaptureSnapshot();
                    var binderAfter = context.Compilation.PerformanceInstrumentation.BinderReentry.CaptureSnapshot();
                    diagnosticsSetupDelta = CompilerSetupInstrumentation.FormatDelta(
                        CompilerSetupInstrumentation.Subtract(setupAfter, setupBefore));
                    diagnosticsBinderDelta = BinderReentryInstrumentation.FormatDelta(
                        BinderReentryInstrumentation.Subtract(binderAfter, binderBefore));
                }
                documentOnlyDiagnostics = true;
            }
            else
            {
                if (!_workspaceManager.TryGetDiagnostics(uri, out var fullProjectDiagnostics, cancellationToken: cancellationToken))
                    return new DiagnosticsComputationResult(Array.Empty<LspDiagnostic>(), WasSkipped: false);

                diagnosticsForProject = fullProjectDiagnostics;
            }

            diagnosticsFetchMs = stopwatch.Elapsed.TotalMilliseconds - gateWaitMs - syntaxTreeMs;
            if (shouldSkipWork?.Invoke() == true)
                return new DiagnosticsComputationResult(Array.Empty<LspDiagnostic>(), WasSkipped: true);

            var diagnostics = diagnosticsForProject
                .Where(d => ShouldReport(d, syntaxTree))
                .Select(MapDiagnostic)
                .ToArray();

            stopwatch.Stop();
            if (stopwatch.Elapsed.TotalMilliseconds >= SlowDiagnosticsThresholdMs)
            {
                if (diagnosticsSetupDelta is not null || diagnosticsBinderDelta is not null)
                {
                    _logger.LogInformation(
                        "Slow diagnostics for {Uri}: total={TotalMs:F1}ms gateWait={GateWaitMs:F1}ms syntaxTree={SyntaxTreeMs:F1}ms diagnosticsFetch={DiagnosticsFetchMs:F1}ms count={Count} setupDelta=[{SetupDelta}] binderDelta=[{BinderDelta}].",
                        uri,
                        stopwatch.Elapsed.TotalMilliseconds,
                        gateWaitMs,
                        syntaxTreeMs,
                        diagnosticsFetchMs,
                        diagnostics.Length,
                        diagnosticsSetupDelta ?? "<none>",
                        diagnosticsBinderDelta ?? "<none>");
                }
                else
                {
                    _logger.LogInformation(
                        "Slow diagnostics for {Uri}: total={TotalMs:F1}ms gateWait={GateWaitMs:F1}ms syntaxTree={SyntaxTreeMs:F1}ms diagnosticsFetch={DiagnosticsFetchMs:F1}ms count={Count}.",
                        uri,
                        stopwatch.Elapsed.TotalMilliseconds,
                        gateWaitMs,
                        syntaxTreeMs,
                        diagnosticsFetchMs,
                        diagnostics.Length);
                }
            }

            return new DiagnosticsComputationResult(diagnostics, WasSkipped: false);
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            return new DiagnosticsComputationResult(Array.Empty<LspDiagnostic>(), WasSkipped: true);
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Diagnostic computation failed for {Uri}.", uri);
            return new DiagnosticsComputationResult(Array.Empty<LspDiagnostic>(), WasSkipped: false);
        }
        finally
        {
            stopwatch.Stop();
            LanguageServerPerformanceInstrumentation.RecordOperation(
                busySkipped
                    || semanticBusySkipped
                    ? "computeDiagnosticsSkipped"
                    : documentOnlyDiagnostics
                        ? mode == DocumentDiagnosticsMode.SyntaxOnly
                            ? "computeSyntaxDiagnostics"
                            : "computeDocumentDiagnostics"
                        : "computeDiagnostics",
                uri,
                null,
                stopwatch.Elapsed.TotalMilliseconds,
                stages:
                [
                    new LanguageServerPerformanceInstrumentation.StageTiming("gateWait", gateWaitMs),
                    new LanguageServerPerformanceInstrumentation.StageTiming("syntaxTree", syntaxTreeMs),
                    new LanguageServerPerformanceInstrumentation.StageTiming("diagnosticsFetch", diagnosticsFetchMs)
                ]);
        }
    }

    public Task<IReadOnlyList<LspDiagnostic>> GetDiagnosticsAsync(DocumentUri uri, CancellationToken cancellationToken)
        => GetDiagnosticsAsync(uri, shouldSkipWork: null, cancellationToken);

    public async Task WarmAnalysisAsync(DocumentUri uri, Func<bool>? shouldSkipWork, CancellationToken cancellationToken)
    {
        var stopwatch = Stopwatch.StartNew();
        double gateWaitMs = 0;
        double analysisContextMs = 0;
        double semanticModelMs = 0;

        try
        {
            if (shouldSkipWork?.Invoke() == true)
                return;

            using var lease = await TryEnterCompilerAccessAsync(cancellationToken, "warmAnalysis", uri).ConfigureAwait(false);
            if (lease is null)
                return;
            gateWaitMs = stopwatch.Elapsed.TotalMilliseconds;

            var stageStopwatch = Stopwatch.StartNew();
            var analysis = await GetOrCreateDocumentAnalysisAsync(uri, cancellationToken).ConfigureAwait(false);
            analysisContextMs = stageStopwatch.Elapsed.TotalMilliseconds;
            if (analysis is null)
                return;

            if (shouldSkipWork?.Invoke() == true)
                return;

            stageStopwatch.Restart();
            var setupBefore = analysis.Context.Compilation.PerformanceInstrumentation.Setup.CaptureSnapshot();
            _ = analysis.GetSemanticModel();
            semanticModelMs = stageStopwatch.Elapsed.TotalMilliseconds;
            if (semanticModelMs >= SlowSemanticModelMaterializationThresholdMs)
            {
                var setupAfter = analysis.Context.Compilation.PerformanceInstrumentation.Setup.CaptureSnapshot();
                var setupDelta = CompilerSetupInstrumentation.Subtract(setupAfter, setupBefore);
                _logger.LogInformation(
                    "Warm analysis semantic model setup for {Uri}: semanticModel={SemanticModelMs:F1}ms setupDelta=[{SetupDelta}].",
                    uri,
                    semanticModelMs,
                    CompilerSetupInstrumentation.FormatDelta(setupDelta));
            }
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
        }
        catch (Exception ex)
        {
            _logger.LogDebug(ex, "Background analysis warmup failed for {Uri}.", uri);
        }
        finally
        {
            stopwatch.Stop();
            if (stopwatch.Elapsed.TotalMilliseconds >= SlowWarmAnalysisThresholdMs)
            {
                _logger.LogInformation(
                    "Slow analysis warmup for {Uri}: total={TotalMs:F1}ms gateWait={GateWaitMs:F1}ms context={ContextMs:F1}ms semanticModel={SemanticModelMs:F1}ms.",
                    uri,
                    stopwatch.Elapsed.TotalMilliseconds,
                    gateWaitMs,
                    analysisContextMs,
                    semanticModelMs);
            }

            LanguageServerPerformanceInstrumentation.RecordOperation(
                "warmAnalysis",
                uri,
                null,
                stopwatch.Elapsed.TotalMilliseconds,
                stages:
                [
                    new LanguageServerPerformanceInstrumentation.StageTiming("gateWait", gateWaitMs),
                    new LanguageServerPerformanceInstrumentation.StageTiming("analysisContext", analysisContextMs),
                    new LanguageServerPerformanceInstrumentation.StageTiming("semanticModel", semanticModelMs)
                ]);
        }
    }

    internal static bool ShouldReport(CodeDiagnostic diagnostic, SyntaxTree syntaxTree)
    {
        if (diagnostic.Location.SourceTree == syntaxTree)
            return true;

        var syntaxTreePath = syntaxTree.FilePath;
        if (string.IsNullOrWhiteSpace(syntaxTreePath))
            return false;

        if (PathsEqual(diagnostic.Location.SourceTree?.FilePath, syntaxTreePath))
            return true;

        var lineSpanPath = diagnostic.Location.GetLineSpan().Path;
        return PathsEqual(lineSpanPath, syntaxTreePath);
    }

    private static LspDiagnostic MapDiagnostic(CodeDiagnostic diagnostic)
    {
        var lineSpan = diagnostic.Location.GetLineSpan();
        var range = new LspRange(
            new Position(lineSpan.StartLinePosition.Line, lineSpan.StartLinePosition.Character),
            new Position(lineSpan.EndLinePosition.Line, lineSpan.EndLinePosition.Character));

        return new LspDiagnostic
        {
            Message = diagnostic.GetMessage(),
            Code = diagnostic.Id,
            Severity = MapSeverity(diagnostic.Severity),
            Source = "raven",
            Range = range,
            Tags = MapTags(diagnostic)
        };
    }

    internal static Container<DiagnosticTag>? MapTags(CodeDiagnostic diagnostic)
    {
        if (UnnecessaryDiagnosticIds.Contains(diagnostic.Id) ||
            (string.Equals(diagnostic.Id, Raven.CodeAnalysis.Diagnostics.UnusedMethodAnalyzer.DiagnosticId, StringComparison.OrdinalIgnoreCase) &&
             diagnostic.Properties.TryGetValue(Raven.CodeAnalysis.Diagnostics.UnusedMethodAnalyzer.UnnecessaryDiagnosticProperty, out var value) &&
             string.Equals(value, bool.TrueString, StringComparison.OrdinalIgnoreCase)))
        {
            return new Container<DiagnosticTag>(DiagnosticTag.Unnecessary);
        }

        return null;
    }

    private static LspDiagnosticSeverity? MapSeverity(CodeDiagnosticSeverity severity)
        => severity switch
        {
            CodeDiagnosticSeverity.Error => LspDiagnosticSeverity.Error,
            CodeDiagnosticSeverity.Warning => LspDiagnosticSeverity.Warning,
            CodeDiagnosticSeverity.Info => LspDiagnosticSeverity.Information,
            CodeDiagnosticSeverity.Hidden => LspDiagnosticSeverity.Hint,
            _ => null
        };

    private static bool PathsEqual(string? left, string? right)
    {
        if (string.IsNullOrWhiteSpace(left) || string.IsNullOrWhiteSpace(right))
            return false;

        return string.Equals(
            NormalizePath(left),
            NormalizePath(right),
            StringComparison.OrdinalIgnoreCase);
    }

    private static string NormalizePath(string path)
    {
        var fullPath = Path.GetFullPath(path);
        return fullPath.TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar);
    }

    private static SyntaxTree? FindMatchingCompilationSyntaxTree(Compilation compilation, SyntaxTree syntaxTree, string? documentFilePath)
    {
        if (compilation.SyntaxTrees.Contains(syntaxTree))
            return syntaxTree;

        var candidatePath = documentFilePath ?? syntaxTree.FilePath;
        if (!string.IsNullOrWhiteSpace(candidatePath))
        {
            var normalizedCandidatePath = NormalizePath(candidatePath);
            foreach (var compilationSyntaxTree in compilation.SyntaxTrees)
            {
                if (string.IsNullOrWhiteSpace(compilationSyntaxTree.FilePath))
                    continue;

                if (string.Equals(
                        NormalizePath(compilationSyntaxTree.FilePath),
                        normalizedCandidatePath,
                        StringComparison.OrdinalIgnoreCase))
                {
                    return compilationSyntaxTree;
                }
            }
        }

        return null;
    }

    private void InvalidateDocumentAnalysis(DocumentUri uri)
    {
        var uriText = uri.ToString();
        foreach (var key in _documentAnalysisCache.Keys)
        {
            if (string.Equals(key.Uri, uriText, StringComparison.Ordinal))
                _documentAnalysisCache.TryRemove(key, out _);
        }
    }

    private sealed class CompilerAccessLease : IDisposable
    {
        private SemaphoreSlim? _gate;

        public CompilerAccessLease(SemaphoreSlim gate)
        {
            _gate = gate;
        }

        public void Dispose()
        {
            var gate = Interlocked.Exchange(ref _gate, null);
            gate?.Release();
        }
    }

    private readonly record struct DocumentAnalysisCacheKey(
        string Uri,
        ProjectId ProjectId,
        DocumentId DocumentId,
        VersionStamp DocumentVersion,
        VersionStamp ProjectVersion);

    private sealed class CachedDocumentAnalysis
    {
        private readonly Lazy<SemanticModel> _semanticModel;
        private readonly Lazy<IReadOnlyCollection<CodeDiagnostic>> _syntaxDiagnostics;
        private readonly Lazy<IReadOnlyCollection<CodeDiagnostic>> _diagnostics;

        public CachedDocumentAnalysis(DocumentAnalysisContext context)
        {
            Context = context;
            _semanticModel = new Lazy<SemanticModel>(
                () => context.Compilation.GetSemanticModel(context.SyntaxTree),
                LazyThreadSafetyMode.ExecutionAndPublication);
            _syntaxDiagnostics = new Lazy<IReadOnlyCollection<CodeDiagnostic>>(
                () => ComputeSyntaxDiagnostics(context),
                LazyThreadSafetyMode.ExecutionAndPublication);
            _diagnostics = new Lazy<IReadOnlyCollection<CodeDiagnostic>>(
                () => ComputeDiagnostics(context, GetSemanticModel()),
                LazyThreadSafetyMode.ExecutionAndPublication);
        }

        public DocumentAnalysisContext Context { get; }

        public SemanticModel GetSemanticModel()
            => _semanticModel.Value;

        public IReadOnlyCollection<CodeDiagnostic> GetSyntaxDiagnostics()
            => _syntaxDiagnostics.Value;

        public IReadOnlyCollection<CodeDiagnostic> GetDiagnostics()
            => _diagnostics.Value;

        private static IReadOnlyCollection<CodeDiagnostic> ComputeSyntaxDiagnostics(DocumentAnalysisContext context)
        {
            var diagnostics = new HashSet<CodeDiagnostic>();
            foreach (var diagnostic in context.SyntaxTree.GetDiagnostics())
            {
                var mapped = context.Compilation.ApplyCompilationOptions(diagnostic, reportSuppressedDiagnostics: false);
                if (mapped is not null)
                    diagnostics.Add(mapped);
            }

            return diagnostics;
        }

        private static IReadOnlyCollection<CodeDiagnostic> ComputeDiagnostics(DocumentAnalysisContext context, SemanticModel semanticModel)
        {
            var diagnostics = new HashSet<CodeDiagnostic>(ComputeSyntaxDiagnostics(context));
            foreach (var diagnostic in semanticModel.GetDiagnostics())
            {
                var mapped = context.Compilation.ApplyCompilationOptions(diagnostic, reportSuppressedDiagnostics: false);
                if (mapped is not null)
                    diagnostics.Add(mapped);
            }

            return diagnostics;
        }
    }
}
