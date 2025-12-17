using System.Collections.Concurrent;
using System.Diagnostics.CodeAnalysis;
using System.IO;
using Microsoft.Extensions.Logging;
using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Text;
using DiagnosticSeverity = Raven.CodeAnalysis.DiagnosticSeverity;
using LspDiagnostic = OmniSharp.Extensions.LanguageServer.Protocol.Models.Diagnostic;
using LspDiagnosticSeverity = OmniSharp.Extensions.LanguageServer.Protocol.Models.DiagnosticSeverity;

namespace Raven.LanguageServer;

internal sealed class DocumentStore
{
    private readonly RavenWorkspace _workspace;
    private readonly ProjectId _projectId;
    private readonly ILogger<DocumentStore> _logger;
    private readonly ConcurrentDictionary<DocumentUri, DocumentId> _documents = new();

    public DocumentStore(RavenWorkspace workspace, ILogger<DocumentStore> logger)
    {
        _workspace = workspace;
        _logger = logger;

        var options = new CompilationOptions(OutputKind.ConsoleApplication);
        _projectId = _workspace.AddProject("RavenLanguageServer", compilationOptions: options);
        AddFrameworkReferences();
    }

    public Compilation GetCompilation() => _workspace.GetCompilation(_projectId);

    public Document UpsertDocument(DocumentUri uri, string text)
    {
        var sourceText = SourceText.From(text);
        var name = Path.GetFileName(uri.GetFileSystemPath()) ?? uri.GetFileSystemPath() ?? "document.rav";
        var solution = _workspace.CurrentSolution;

        if (_documents.TryGetValue(uri, out var documentId))
        {
            solution = solution.WithDocumentText(documentId, sourceText);
        }
        else
        {
            documentId = DocumentId.CreateNew(_projectId);
            solution = solution.AddDocument(documentId, name, sourceText, uri.GetFileSystemPath());
            _documents[uri] = documentId;
        }

        _workspace.TryApplyChanges(solution);
        return _workspace.CurrentSolution.GetDocument(documentId)!;
    }

    public bool TryGetDocument(DocumentUri uri, [NotNullWhen(true)] out Document? document)
    {
        if (_documents.TryGetValue(uri, out var documentId))
        {
            document = _workspace.CurrentSolution.GetDocument(documentId);
            return document is not null;
        }

        document = null;
        return false;
    }

    public async Task<IReadOnlyList<LspDiagnostic>> GetDiagnosticsAsync(DocumentUri uri, CancellationToken cancellationToken)
    {
        if (!TryGetDocument(uri, out var document))
            return Array.Empty<LspDiagnostic>();

        var syntaxTree = await document.GetSyntaxTreeAsync(cancellationToken).ConfigureAwait(false);
        if (syntaxTree is null)
            return Array.Empty<LspDiagnostic>();

        var compilation = GetCompilation();
        var diagnostics = compilation.GetDiagnostics(cancellationToken: cancellationToken)
            .Where(d => ShouldReport(d, syntaxTree))
            .Select(MapDiagnostic)
            .ToArray();

        return diagnostics;
    }

    private void AddFrameworkReferences()
    {
        try
        {
            var solution = _workspace.CurrentSolution;
            var version = TargetFrameworkResolver.ResolveLatestInstalledVersion();

            foreach (var referencePath in TargetFrameworkResolver.GetReferenceAssemblies(version))
            {
                if (!File.Exists(referencePath))
                    continue;

                var reference = MetadataReference.CreateFromFile(referencePath);
                solution = solution.AddMetadataReference(_projectId, reference);
            }

            _workspace.TryApplyChanges(solution);
        }
        catch (Exception ex)
        {
            _logger.LogWarning(ex, "Failed to load reference assemblies for the workspace.");
        }
    }

    private static bool ShouldReport(Diagnostic diagnostic, SyntaxTree syntaxTree)
    {
        if (diagnostic.Location.SourceTree is null)
            return false;

        return diagnostic.Location.SourceTree == syntaxTree;
    }

    private static LspDiagnostic MapDiagnostic(Diagnostic diagnostic)
    {
        var lineSpan = diagnostic.Location.GetLineSpan();
        var range = new Range(
            new Position(lineSpan.StartLinePosition.Line, lineSpan.StartLinePosition.Character),
            new Position(lineSpan.EndLinePosition.Line, lineSpan.EndLinePosition.Character));

        return new LspDiagnostic
        {
            Message = diagnostic.GetMessage(),
            Severity = MapSeverity(diagnostic.Severity),
            Source = "raven",
            Range = range
        };
    }

    private static LspDiagnosticSeverity? MapSeverity(DiagnosticSeverity severity)
        => severity switch
        {
            DiagnosticSeverity.Error => LspDiagnosticSeverity.Error,
            DiagnosticSeverity.Warning => LspDiagnosticSeverity.Warning,
            DiagnosticSeverity.Info => LspDiagnosticSeverity.Information,
            DiagnosticSeverity.Hidden => LspDiagnosticSeverity.Hint,
            _ => null
        };
}
