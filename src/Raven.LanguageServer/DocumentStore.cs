using System.Diagnostics.CodeAnalysis;
using System.IO;
using System.Diagnostics;

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
    private const double SlowDiagnosticsThresholdMs = 150;
    private const double SlowWarmAnalysisThresholdMs = 250;

    internal readonly record struct DocumentAnalysisContext(
        Document Document,
        Compilation Compilation,
        SyntaxTree SyntaxTree,
        Raven.CodeAnalysis.Text.SourceText SourceText);

    private static readonly HashSet<string> UnnecessaryDiagnosticIds = new(StringComparer.OrdinalIgnoreCase)
    {
        CompilerDiagnostics.UnreachableCodeDetected.Id,
        Raven.CodeAnalysis.Diagnostics.UnusedVariableAnalyzer.DiagnosticId
    };

    private readonly WorkspaceManager _workspaceManager;
    private readonly ILogger<DocumentStore> _logger;
    private readonly SemaphoreSlim _compilerAccessGate = new(1, 1);

    public DocumentStore(WorkspaceManager workspaceManager, ILogger<DocumentStore> logger)
    {
        _workspaceManager = workspaceManager;
        _logger = logger;
    }

    public Document UpsertDocument(DocumentUri uri, string text)
        => _workspaceManager.UpsertDocument(uri, text);

    public bool TryGetDocument(DocumentUri uri, [NotNullWhen(true)] out Document? document)
        => _workspaceManager.TryGetDocument(uri, out document);

    public bool TryGetDocumentContext(
        DocumentUri uri,
        [NotNullWhen(true)] out Document? document,
        [NotNullWhen(true)] out Compilation? compilation)
        => _workspaceManager.TryGetDocumentContext(uri, out document, out compilation);

    public async Task<DocumentAnalysisContext?> GetAnalysisContextAsync(DocumentUri uri, CancellationToken cancellationToken)
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

        return new DocumentAnalysisContext(document, compilation, syntaxTree, sourceText);
    }

    public bool TryGetCompilation(DocumentUri uri, [NotNullWhen(true)] out Compilation? compilation)
        => _workspaceManager.TryGetCompilation(uri, out compilation);

    public bool RemoveDocument(DocumentUri uri)
        => _workspaceManager.RemoveDocument(uri);

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

    public async Task<IReadOnlyList<LspDiagnostic>> GetDiagnosticsAsync(DocumentUri uri, CancellationToken cancellationToken)
    {
        var stopwatch = Stopwatch.StartNew();
        double gateWaitMs = 0;
        double syntaxTreeMs = 0;
        double diagnosticsFetchMs = 0;

        try
        {
            using var _ = await EnterCompilerAccessAsync(cancellationToken, "diagnostics", uri).ConfigureAwait(false);
            gateWaitMs = stopwatch.Elapsed.TotalMilliseconds;
            if (!TryGetDocument(uri, out var document))
                return Array.Empty<LspDiagnostic>();

            var syntaxTree = await document.GetSyntaxTreeAsync(cancellationToken).ConfigureAwait(false);
            if (syntaxTree is null)
                return Array.Empty<LspDiagnostic>();
            syntaxTreeMs = stopwatch.Elapsed.TotalMilliseconds - gateWaitMs;

            if (!_workspaceManager.TryGetDiagnostics(uri, out var diagnosticsForProject, cancellationToken: cancellationToken))
                return Array.Empty<LspDiagnostic>();
            diagnosticsFetchMs = stopwatch.Elapsed.TotalMilliseconds - gateWaitMs - syntaxTreeMs;

            var diagnostics = diagnosticsForProject
                .Where(d => ShouldReport(d, syntaxTree))
                .Select(MapDiagnostic)
                .ToArray();

            stopwatch.Stop();
            if (stopwatch.Elapsed.TotalMilliseconds >= SlowDiagnosticsThresholdMs)
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

            return diagnostics;
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            return Array.Empty<LspDiagnostic>();
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Diagnostic computation failed for {Uri}.", uri);
            return Array.Empty<LspDiagnostic>();
        }
    }

    public async Task WarmAnalysisAsync(DocumentUri uri, CancellationToken cancellationToken)
    {
        var stopwatch = Stopwatch.StartNew();
        double gateWaitMs = 0;
        double analysisContextMs = 0;
        double semanticModelMs = 0;

        try
        {
            using var lease = await EnterCompilerAccessAsync(cancellationToken, "warmAnalysis", uri).ConfigureAwait(false);
            gateWaitMs = stopwatch.Elapsed.TotalMilliseconds;

            var stageStopwatch = Stopwatch.StartNew();
            var context = await GetAnalysisContextAsync(uri, cancellationToken).ConfigureAwait(false);
            analysisContextMs = stageStopwatch.Elapsed.TotalMilliseconds;
            if (context is null)
                return;

            var compilation = context.Value.Compilation;
            var syntaxTree = context.Value.SyntaxTree;
            stageStopwatch.Restart();
            _ = compilation.GetSemanticModel(syntaxTree);
            semanticModelMs = stageStopwatch.Elapsed.TotalMilliseconds;
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
}
