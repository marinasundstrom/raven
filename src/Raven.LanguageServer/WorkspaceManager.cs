using System.Collections.Concurrent;
using System.IO;
using System.Linq;
using Microsoft.Extensions.Logging;
using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Text;

namespace Raven.LanguageServer;

internal sealed class WorkspaceManager
{
    private readonly RavenWorkspace _workspace;
    private readonly ILogger<WorkspaceManager> _logger;
    private readonly object _gate = new();
    private readonly Dictionary<string, ProjectId> _projectsByRoot = new(StringComparer.OrdinalIgnoreCase);
    private readonly ConcurrentDictionary<DocumentUri, OwnedDocument> _documents = new();
    private ProjectId? _fallbackProjectId;

    public WorkspaceManager(RavenWorkspace workspace, ILogger<WorkspaceManager> logger)
    {
        _workspace = workspace;
        _logger = logger;
    }

    public void Initialize(InitializeParams request)
    {
        var roots = ResolveRoots(request);
        lock (_gate)
        {
            _projectsByRoot.Clear();
            _fallbackProjectId = null;

            foreach (var root in roots)
            {
                var projectId = CreateProjectForRoot(root);
                _projectsByRoot[root] = projectId;
            }

            if (_projectsByRoot.Count == 0)
                _fallbackProjectId = CreateFallbackProject();
        }

        _logger.LogInformation("Workspace initialized with {RootCount} root(s).", roots.Count);
    }

    public Document UpsertDocument(DocumentUri uri, string text)
    {
        var sourceText = SourceText.From(text);
        var filePath = uri.GetFileSystemPath();
        var name = Path.GetFileName(filePath) ?? filePath ?? "document.rav";

        lock (_gate)
        {
            var ownerProject = ResolveProjectForUri(uri);
            var solution = _workspace.CurrentSolution;

            if (_documents.TryGetValue(uri, out var existing))
            {
                if (existing.ProjectId == ownerProject)
                {
                    solution = solution.WithDocumentText(existing.DocumentId, sourceText);
                    _workspace.TryApplyChanges(solution);
                    return _workspace.CurrentSolution.GetDocument(existing.DocumentId)!;
                }

                solution = solution.RemoveDocument(existing.DocumentId);
                _documents.TryRemove(uri, out _);
            }

            var documentId = DocumentId.CreateNew(ownerProject);
            solution = solution.AddDocument(documentId, name, sourceText, filePath);
            _workspace.TryApplyChanges(solution);
            _documents[uri] = new OwnedDocument(documentId, ownerProject);

            return _workspace.CurrentSolution.GetDocument(documentId)!;
        }
    }

    public bool TryGetDocument(DocumentUri uri, out Document? document)
    {
        if (_documents.TryGetValue(uri, out var ownedDocument))
        {
            document = _workspace.CurrentSolution.GetDocument(ownedDocument.DocumentId);
            return document is not null;
        }

        document = null;
        return false;
    }

    public bool TryGetCompilation(DocumentUri uri, out Compilation? compilation)
    {
        if (_documents.TryGetValue(uri, out var ownedDocument))
        {
            compilation = _workspace.GetCompilation(ownedDocument.ProjectId);
            return true;
        }

        compilation = null;
        return false;
    }

    public bool RemoveDocument(DocumentUri uri)
    {
        if (!_documents.TryRemove(uri, out var ownedDocument))
            return false;

        lock (_gate)
        {
            var solution = _workspace.CurrentSolution.RemoveDocument(ownedDocument.DocumentId);
            _workspace.TryApplyChanges(solution);
        }

        return true;
    }

    private List<string> ResolveRoots(InitializeParams request)
    {
        var roots = new List<string>();

        if (request.WorkspaceFolders?.Any() == true)
        {
            foreach (var folder in request.WorkspaceFolders)
            {
                AddIfValid(roots, folder.Uri.GetFileSystemPath());
            }
        }

        if (roots.Count == 0)
            AddIfValid(roots, request.RootUri?.GetFileSystemPath());

        return roots;
    }

    private void AddIfValid(List<string> roots, string? path)
    {
        if (string.IsNullOrWhiteSpace(path))
            return;

        var normalized = NormalizePath(path);
        if (!roots.Contains(normalized, StringComparer.OrdinalIgnoreCase))
            roots.Add(normalized);
    }

    private ProjectId ResolveProjectForUri(DocumentUri uri)
    {
        var documentPath = uri.GetFileSystemPath();
        if (string.IsNullOrWhiteSpace(documentPath))
            return EnsureFallbackProject();

        var normalizedPath = NormalizePath(documentPath);
        var bestRoot = _projectsByRoot.Keys
            .Where(root => IsWithinRoot(normalizedPath, root))
            .OrderByDescending(root => root.Length)
            .FirstOrDefault();

        if (bestRoot is not null)
            return _projectsByRoot[bestRoot];

        return EnsureFallbackProject();
    }

    private ProjectId EnsureFallbackProject()
    {
        if (_fallbackProjectId is null)
            _fallbackProjectId = CreateFallbackProject();
        return _fallbackProjectId.Value;
    }

    private ProjectId CreateFallbackProject()
        => CreateProject("RavenLanguageServer");

    private ProjectId CreateProjectForRoot(string root)
    {
        var directoryName = Path.GetFileName(root.TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar));
        if (string.IsNullOrWhiteSpace(directoryName))
            directoryName = "RavenWorkspace";

        return CreateProject($"Raven_{directoryName}");
    }

    private ProjectId CreateProject(string name)
    {
        var compilationOptions = new CompilationOptions(OutputKind.ConsoleApplication)
            .WithMembersPublicByDefault(true);
        var projectId = _workspace.AddProject(name, compilationOptions: compilationOptions);

        var solution = _workspace.CurrentSolution;
        var version = TargetFrameworkResolver.ResolveLatestInstalledVersion();
        foreach (var referencePath in TargetFrameworkResolver.GetReferenceAssemblies(version))
        {
            if (!File.Exists(referencePath))
                continue;

            solution = solution.AddMetadataReference(projectId, MetadataReference.CreateFromFile(referencePath));
        }

        _workspace.TryApplyChanges(solution);
        return projectId;
    }

    private static string NormalizePath(string path)
        => Path.GetFullPath(path)
            .TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar);

    private static bool IsWithinRoot(string documentPath, string rootPath)
    {
        if (documentPath.Equals(rootPath, StringComparison.OrdinalIgnoreCase))
            return true;

        var prefix = rootPath + Path.DirectorySeparatorChar;
        return documentPath.StartsWith(prefix, StringComparison.OrdinalIgnoreCase);
    }

    private readonly record struct OwnedDocument(DocumentId DocumentId, ProjectId ProjectId);
}
