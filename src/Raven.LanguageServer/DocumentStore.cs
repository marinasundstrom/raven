using System.Diagnostics.CodeAnalysis;

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

    public bool TryGetCompilation(DocumentUri uri, [NotNullWhen(true)] out Compilation? compilation)
        => _workspaceManager.TryGetCompilation(uri, out compilation);

    public bool RemoveDocument(DocumentUri uri)
        => _workspaceManager.RemoveDocument(uri);

    public async ValueTask<IDisposable> EnterCompilerAccessAsync(CancellationToken cancellationToken)
    {
        await _compilerAccessGate.WaitAsync(cancellationToken).ConfigureAwait(false);
        return new CompilerAccessLease(_compilerAccessGate);
    }

    public async Task<IReadOnlyList<LspDiagnostic>> GetDiagnosticsAsync(DocumentUri uri, CancellationToken cancellationToken)
    {
        try
        {
            using var _ = await EnterCompilerAccessAsync(cancellationToken).ConfigureAwait(false);
            if (!TryGetDocument(uri, out var document))
                return Array.Empty<LspDiagnostic>();

            var syntaxTree = await document.GetSyntaxTreeAsync(cancellationToken).ConfigureAwait(false);
            if (syntaxTree is null)
                return Array.Empty<LspDiagnostic>();

            if (!_workspaceManager.TryGetDiagnostics(uri, out var diagnosticsForProject, cancellationToken: cancellationToken))
                return Array.Empty<LspDiagnostic>();

            var diagnostics = diagnosticsForProject
                .Where(d => ShouldReport(d, syntaxTree))
                .Select(MapDiagnostic)
                .ToArray();

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

    private static bool ShouldReport(CodeDiagnostic diagnostic, SyntaxTree syntaxTree)
    {
        if (diagnostic.Location.SourceTree is null)
            return false;

        return diagnostic.Location.SourceTree == syntaxTree;
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
            Range = range
        };
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
