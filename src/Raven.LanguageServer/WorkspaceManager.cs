using System.Collections.Concurrent;
using System.Collections.Immutable;
using System.IO;
using System.Linq;

using Microsoft.Extensions.Logging;

using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using CodeDiagnostic = Raven.CodeAnalysis.Diagnostic;

namespace Raven.LanguageServer;

internal sealed class WorkspaceManager
{
    private static readonly string MacroShadowOutputRoot = Path.Combine(Path.GetTempPath(), "raven-ls-macros");

    private readonly RavenWorkspace _workspace;
    private readonly ILogger<WorkspaceManager> _logger;
    private readonly object _gate = new();
    private readonly ImmutableArray<CodeFixProvider> _builtInCodeFixProviders;
    private readonly ImmutableArray<CodeRefactoringProvider> _builtInCodeRefactoringProviders;
    private readonly Dictionary<string, ProjectId> _projectsByRoot = new(StringComparer.OrdinalIgnoreCase);
    private readonly ConcurrentDictionary<DocumentUri, OwnedDocument> _documents = new();
    private readonly ConcurrentDictionary<ProjectId, CachedDiagnostics> _diagnosticsCache = new();
    private ImmutableArray<string> _workspaceRoots = ImmutableArray<string>.Empty;
    private ProjectId? _fallbackProjectId;

    public WorkspaceManager(RavenWorkspace workspace, ILogger<WorkspaceManager> logger)
        : this(
            workspace,
            logger,
            BuiltInCodeFixProviders.CreateDefault(),
            BuiltInCodeRefactoringProviders.CreateDefault())
    {
    }

    internal WorkspaceManager(
        RavenWorkspace workspace,
        ILogger<WorkspaceManager> logger,
        ImmutableArray<CodeFixProvider> builtInCodeFixProviders,
        ImmutableArray<CodeRefactoringProvider> builtInCodeRefactoringProviders)
    {
        _workspace = workspace;
        _logger = logger;
        _builtInCodeFixProviders = builtInCodeFixProviders;
        _builtInCodeRefactoringProviders = builtInCodeRefactoringProviders;
    }

    public void Initialize(InitializeParams request)
    {
        var roots = ResolveRoots(request);
        _workspaceRoots = roots.ToImmutableArray();
        InitializeCore(roots);
        _logger.LogInformation("Workspace initialized with {RootCount} root(s).", roots.Count);
    }

    private void InitializeCore(IReadOnlyList<string> roots)
    {
        lock (_gate)
        {
            _workspace.OpenSolution(_workspace.CreateSolution());
            _projectsByRoot.Clear();
            _fallbackProjectId = null;
            _documents.Clear();
            _diagnosticsCache.Clear();
            var loadedProjects = new Dictionary<string, ProjectId>(StringComparer.OrdinalIgnoreCase);

            foreach (var root in roots)
            {
                if (TryOpenProjectsForRoot(root, loadedProjects, out var projectId))
                    _projectsByRoot[root] = projectId;
                else
                    _projectsByRoot[root] = CreateProjectForRoot(root);
            }

            if (_projectsByRoot.Count == 0)
                _fallbackProjectId = CreateFallbackProject();
        }
    }

    public void ReloadForWatchedFiles(IEnumerable<FileEvent> changes)
    {
        ArgumentNullException.ThrowIfNull(changes);

        var changedPaths = changes
            .Select(change => change.Uri?.GetFileSystemPath())
            .Where(path => !string.IsNullOrWhiteSpace(path))
            .Select(path => NormalizePath(path!))
            .ToArray();

        if (changedPaths.Length == 0 || _workspaceRoots.Length == 0)
            return;

        var shouldReload = changedPaths.Any(path =>
        {
            var extension = Path.GetExtension(path);
            return string.Equals(extension, ".rvnproj", StringComparison.OrdinalIgnoreCase) ||
                   string.Equals(extension, ".csproj", StringComparison.OrdinalIgnoreCase) ||
                   string.Equals(extension, ".fsproj", StringComparison.OrdinalIgnoreCase) ||
                   RavenFileExtensions.HasRavenExtension(path);
        });

        if (!shouldReload)
            return;

        lock (_gate)
        {
            var openDocuments = new List<ReloadDocumentState>();
            foreach (var pair in _documents)
            {
                var document = _workspace.CurrentSolution.GetDocument(pair.Value.DocumentId);
                if (document is null)
                    continue;

                openDocuments.Add(new ReloadDocumentState(
                    pair.Key,
                    document.Text.ToString(),
                    pair.Value.IsProjectDocument));
            }

            InitializeCore(_workspaceRoots);

            foreach (var openDocument in openDocuments)
            {
                _ = UpsertDocument(openDocument.Uri, openDocument.Text);
            }
        }
    }

    private bool TryOpenProjectsForRoot(string root, Dictionary<string, ProjectId> loadedProjects, out ProjectId projectId)
    {
        var projectSystem = _workspace.Services.ProjectSystemService;
        if (projectSystem is null)
        {
            _logger.LogWarning("No project system service is available. Falling back to inferred workspace for root '{Root}'.", root);
            projectId = default;
            return false;
        }

        var projectFilePaths = FindWorkspaceProjectFiles(root, projectSystem);
        if (projectFilePaths.Length == 0)
        {
            projectId = default;
            return false;
        }

        var stack = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        var loadedProjectPathsForRoot = new List<string>();

        foreach (var projectFilePath in projectFilePaths)
        {
            try
            {
                _ = OpenProjectWithReferences(projectFilePath, projectSystem, loadedProjects, stack);
                loadedProjectPathsForRoot.Add(projectFilePath);
            }
            catch (Exception ex)
            {
                stack.Remove(NormalizePath(projectFilePath));
                _logger.LogWarning(
                    ex,
                    "Failed to open Raven project '{ProjectFilePath}' for root '{Root}'. Continuing with remaining projects.",
                    projectFilePath,
                    root);
            }
        }

        var successfullyLoadedCandidates = projectFilePaths
            .Where(path => loadedProjects.ContainsKey(NormalizePath(path)))
            .ToArray();

        if (successfullyLoadedCandidates.Length == 0)
        {
            _logger.LogWarning("Failed to open any Raven project(s) for root '{Root}'. Falling back to inferred workspace.", root);
            projectId = default;
            return false;
        }

        var primaryProjectPath = SelectPrimaryProjectPath(root, successfullyLoadedCandidates);
        projectId = loadedProjects[NormalizePath(primaryProjectPath)];
        _logger.LogInformation(
            "Opened {ProjectCount} Raven project(s) for root '{Root}'. Primary project: '{ProjectFilePath}'.",
            successfullyLoadedCandidates.Length,
            root,
            primaryProjectPath);
        return true;
    }

    private ProjectId OpenProjectWithReferences(
        string projectFilePath,
        IProjectSystemService projectSystem,
        Dictionary<string, ProjectId> loadedProjects,
        HashSet<string> stack)
    {
        var normalizedProjectPath = NormalizePath(projectFilePath);
        if (loadedProjects.TryGetValue(normalizedProjectPath, out var existing))
            return existing;

        if (!stack.Add(normalizedProjectPath))
            throw new InvalidOperationException($"Detected cyclic project references involving '{normalizedProjectPath}'.");

        foreach (var referencedProjectPath in projectSystem.GetProjectReferencePaths(normalizedProjectPath)
                     .Where(projectSystem.CanOpenProject))
            _ = OpenProjectWithReferences(referencedProjectPath, projectSystem, loadedProjects, stack);

        var projectId = projectSystem.OpenProject(_workspace, normalizedProjectPath);
        EnsureRavenCoreReference(projectId);
        EnsureBuiltInAnalyzers(projectId);
        loadedProjects[normalizedProjectPath] = projectId;
        stack.Remove(normalizedProjectPath);
        return projectId;
    }

    private void EnsureRavenCoreReference(ProjectId projectId)
    {
        var project = _workspace.CurrentSolution.GetProject(projectId);
        if (project is null)
            return;

        var hasRavenCoreReference = project.MetadataReferences
            .OfType<PortableExecutableReference>()
            .Any(static reference =>
                string.Equals(Path.GetFileName(reference.FilePath), "Raven.Core.dll", StringComparison.OrdinalIgnoreCase));

        if (hasRavenCoreReference)
            return;

        var preferredTfm = project.TargetFramework ?? TargetFrameworkResolver.ResolveLatestInstalledVersion().Moniker.ToTfm();
        var ravenCoreReferencePath = ResolveRavenCoreReferencePath(preferredTfm);
        if (string.IsNullOrWhiteSpace(ravenCoreReferencePath))
        {
            _logger.LogWarning(
                "Unable to locate a valid Raven.Core metadata reference for project '{ProjectName}'.",
                project.Name);
            return;
        }

        var solution = _workspace.CurrentSolution.AddMetadataReference(projectId, MetadataReference.CreateFromFile(ravenCoreReferencePath));
        _workspace.TryApplyChanges(solution);
        _logger.LogDebug(
            "Added Raven.Core metadata reference '{ReferencePath}' for opened project '{ProjectName}'.",
            ravenCoreReferencePath,
            project.Name);
    }

    private void EnsureBuiltInAnalyzers(ProjectId projectId)
    {
        var project = _workspace.CurrentSolution.GetProject(projectId);
        if (project is null)
            return;

        var updatedProject = project.AddBuiltInAnalyzers(enableSuggestions: true);
        if (_workspace.TryApplyChanges(updatedProject.Solution))
        {
            _logger.LogDebug(
                "Registered built-in analyzers for project '{ProjectName}'.",
                project.Name);
        }
    }

    internal static string[] FindWorkspaceProjectFiles(string root, IProjectSystemService projectSystem)
    {
        if (!Directory.Exists(root))
            return [];

        var candidates = Directory
            .EnumerateFiles(root, "*.*proj", SearchOption.AllDirectories)
            .Where(projectSystem.CanOpenProject)
            .OrderBy(path => path, StringComparer.OrdinalIgnoreCase)
            .ToArray();

        return candidates;
    }

    internal static string SelectPrimaryProjectPath(string root, IReadOnlyList<string> candidates)
    {
        if (candidates.Count == 0)
            throw new InvalidOperationException("At least one project candidate is required.");

        var normalizedRoot = NormalizePath(root);
        var directoryName = Path.GetFileName(normalizedRoot);
        var topLevelCandidates = candidates
            .Where(path => string.Equals(
                NormalizePath(Path.GetDirectoryName(path) ?? string.Empty),
                normalizedRoot,
                StringComparison.OrdinalIgnoreCase))
            .ToArray();

        var candidateSet = topLevelCandidates.Length > 0 ? topLevelCandidates : candidates.ToArray();

        if (!string.IsNullOrWhiteSpace(directoryName))
        {
            var preferred = candidateSet.FirstOrDefault(path =>
                string.Equals(Path.GetFileNameWithoutExtension(path), directoryName, StringComparison.OrdinalIgnoreCase));
            if (preferred is not null)
                return preferred;
        }

        return candidateSet
            .OrderBy(path => GetDirectoryDepth(normalizedRoot, path))
            .ThenBy(path => path, StringComparer.OrdinalIgnoreCase)
            .First();
    }

    public Document UpsertDocument(DocumentUri uri, string text)
    {
        var sourceText = SourceText.From(text);
        var filePath = uri.GetFileSystemPath();
        var name = Path.GetFileName(filePath) ?? filePath ?? $"document{RavenFileExtensions.Raven}";

        lock (_gate)
        {
            var ownerProject = ResolveProjectForUri(uri);
            var solution = _workspace.CurrentSolution;
            var normalizedFilePath = !string.IsNullOrWhiteSpace(filePath) ? NormalizePath(filePath) : null;
            OwnedDocument? staleOwnedDocument = null;

            if (_documents.TryGetValue(uri, out var existing))
            {
                if (existing.ProjectId == ownerProject)
                {
                    solution = solution.WithDocumentText(existing.DocumentId, sourceText);
                    _workspace.TryApplyChanges(solution);
                    var updatedDocument = _workspace.CurrentSolution.GetDocument(existing.DocumentId)!;
                    _documents[uri] = new OwnedDocument(updatedDocument.Id, ownerProject, updatedDocument.Version, IsProjectDocument: existing.IsProjectDocument);
                    _diagnosticsCache.TryRemove(ownerProject, out _);
                    RefreshMacroConsumersForProject(ownerProject);
                    return updatedDocument;
                }

                staleOwnedDocument = existing;
                _documents.TryRemove(uri, out _);
            }

            if (normalizedFilePath is not null &&
                TryFindExistingDocument(solution, ownerProject, normalizedFilePath, out var existingDocument, out var existingOwnerProject))
            {
                solution = solution.WithDocumentText(existingDocument.Id, sourceText);
                if (staleOwnedDocument is { IsProjectDocument: false } stale
                    && stale.DocumentId != existingDocument.Id
                    && solution.GetDocument(stale.DocumentId) is not null)
                {
                    solution = solution.RemoveDocument(stale.DocumentId);
                }
                _workspace.TryApplyChanges(solution);
                var updatedDocument = _workspace.CurrentSolution.GetDocument(existingDocument.Id)!;
                _documents[uri] = new OwnedDocument(updatedDocument.Id, existingOwnerProject, updatedDocument.Version, IsProjectDocument: true);
                _diagnosticsCache.TryRemove(existingOwnerProject, out _);
                if (staleOwnedDocument is { } staleOwner && staleOwner.ProjectId != existingOwnerProject)
                    _diagnosticsCache.TryRemove(staleOwner.ProjectId, out _);
                RefreshMacroConsumersForProject(existingOwnerProject);
                return updatedDocument;
            }

            if (staleOwnedDocument is { IsProjectDocument: false } staleDocument
                && solution.GetDocument(staleDocument.DocumentId) is not null)
            {
                solution = solution.RemoveDocument(staleDocument.DocumentId);
            }

            var documentId = DocumentId.CreateNew(ownerProject);
            solution = solution.AddDocument(documentId, name, sourceText, filePath);
            _workspace.TryApplyChanges(solution);
            var addedDocument = _workspace.CurrentSolution.GetDocument(documentId)!;
            _documents[uri] = new OwnedDocument(documentId, ownerProject, addedDocument.Version, IsProjectDocument: false);
            _diagnosticsCache.TryRemove(ownerProject, out _);

            return addedDocument;
        }
    }

    private static bool TryFindExistingDocument(
        Solution solution,
        ProjectId preferredProjectId,
        string normalizedFilePath,
        out Document document,
        out ProjectId ownerProjectId)
    {
        // Prefer a document that is already part of the resolved owner project.
        var preferredProject = solution.GetProject(preferredProjectId);
        if (preferredProject is not null)
        {
            var preferredMatch = preferredProject.Documents.FirstOrDefault(doc =>
                !string.IsNullOrWhiteSpace(doc.FilePath) &&
                string.Equals(NormalizePath(doc.FilePath), normalizedFilePath, StringComparison.OrdinalIgnoreCase));
            if (preferredMatch is not null)
            {
                document = preferredMatch;
                ownerProjectId = preferredProjectId;
                return true;
            }
        }

        foreach (var project in solution.Projects)
        {
            var match = project.Documents.FirstOrDefault(doc =>
                !string.IsNullOrWhiteSpace(doc.FilePath) &&
                string.Equals(NormalizePath(doc.FilePath), normalizedFilePath, StringComparison.OrdinalIgnoreCase));
            if (match is not null)
            {
                document = match;
                ownerProjectId = project.Id;
                return true;
            }
        }

        document = null!;
        ownerProjectId = default;
        return false;
    }

    public bool TryGetDocument(DocumentUri uri, out Document? document)
    {
        if (TryResolveOwnedDocument(uri, out var ownedDocument))
        {
            document = _workspace.CurrentSolution.GetDocument(ownedDocument.DocumentId);
            return document is not null;
        }

        document = null;
        return false;
    }

    public bool TryGetDocumentContext(DocumentUri uri, out Document? document, out Compilation? compilation)
    {
        lock (_gate)
        {
            if (TryResolveOwnedDocument(uri, out var ownedDocument))
            {
                var solution = _workspace.CurrentSolution;
                document = solution.GetDocument(ownedDocument.DocumentId);
                if (document is not null)
                {
                    compilation = _workspace.GetCompilation(document.Project);
                    return true;
                }
            }
        }

        document = null;
        compilation = null;
        return false;
    }

    public bool TryGetCompilation(DocumentUri uri, out Compilation? compilation)
    {
        lock (_gate)
        {
            if (TryResolveOwnedDocument(uri, out var ownedDocument))
            {
                var project = _workspace.CurrentSolution.GetProject(ownedDocument.ProjectId);
                if (project is not null)
                {
                    compilation = _workspace.GetCompilation(project);
                    return true;
                }
            }
        }

        compilation = null;
        return false;
    }

    public bool TryGetDiagnostics(
        DocumentUri uri,
        out ImmutableArray<CodeDiagnostic> diagnostics,
        CompilationWithAnalyzersOptions? analyzerOptions = null,
        CancellationToken cancellationToken = default)
    {
        if (_documents.TryGetValue(uri, out var ownedDocument))
        {
            var project = _workspace.CurrentSolution.GetProject(ownedDocument.ProjectId);
            if (project is not null && analyzerOptions is null &&
                _diagnosticsCache.TryGetValue(ownedDocument.ProjectId, out var cached) &&
                cached.Version == project.Version)
            {
                diagnostics = cached.Diagnostics;
                return true;
            }

            diagnostics = _workspace.GetDiagnostics(ownedDocument.ProjectId, analyzerOptions, cancellationToken);
            if (project is not null && analyzerOptions is null)
            {
                _diagnosticsCache[ownedDocument.ProjectId] = new CachedDiagnostics(project.Version, diagnostics);
            }
            return true;
        }

        diagnostics = ImmutableArray<CodeDiagnostic>.Empty;
        return false;
    }

    public bool TryGetCodeFixes(
        DocumentUri uri,
        out ImmutableArray<CodeFix> codeFixes,
        CompilationWithAnalyzersOptions? analyzerOptions = null,
        CancellationToken cancellationToken = default)
    {
        if (TryResolveOwnedDocument(uri, out var ownedDocument))
        {
            codeFixes = _workspace
                .GetCodeFixes(ownedDocument.ProjectId, _builtInCodeFixProviders, analyzerOptions, cancellationToken)
                .Where(fix => fix.DocumentId == ownedDocument.DocumentId)
                .ToImmutableArray();
            return true;
        }

        codeFixes = ImmutableArray<CodeFix>.Empty;
        return false;
    }

    public bool TryGetRefactorings(
        DocumentUri uri,
        TextSpan span,
        out ImmutableArray<CodeRefactoring> refactorings,
        CancellationToken cancellationToken = default)
    {
        if (_documents.TryGetValue(uri, out var ownedDocument))
        {
            refactorings = _workspace
                .GetRefactorings(ownedDocument.DocumentId, _builtInCodeRefactoringProviders, span, cancellationToken);
            return true;
        }

        refactorings = ImmutableArray<CodeRefactoring>.Empty;
        return false;
    }

    public bool RemoveDocument(DocumentUri uri)
    {
        if (!_documents.TryRemove(uri, out var ownedDocument))
            return false;

        lock (_gate)
        {
            if (ownedDocument.IsProjectDocument)
            {
                var document = _workspace.CurrentSolution.GetDocument(ownedDocument.DocumentId);
                if (document?.FilePath is { } filePath && File.Exists(filePath))
                {
                    var sourceText = SourceText.From(File.ReadAllText(filePath));
                    var solution = _workspace.CurrentSolution.WithDocumentText(ownedDocument.DocumentId, sourceText);
                    _workspace.TryApplyChanges(solution);
                    _diagnosticsCache.TryRemove(ownedDocument.ProjectId, out _);
                    RefreshMacroConsumersForProject(ownedDocument.ProjectId);
                }
            }
            else
            {
                var solution = _workspace.CurrentSolution.RemoveDocument(ownedDocument.DocumentId);
                _workspace.TryApplyChanges(solution);
                _diagnosticsCache.TryRemove(ownedDocument.ProjectId, out _);
            }
        }

        return true;
    }

    private void RefreshMacroConsumersForProject(ProjectId changedProjectId)
    {
        var changedProject = _workspace.CurrentSolution.GetProject(changedProjectId);
        if (changedProject?.FilePath is null)
            return;

        var sourceProjectPath = NormalizePath(changedProject.FilePath);
        var consumers = _workspace.CurrentSolution.Projects
            .Where(project => project.MacroReferences.Any(reference =>
                !string.IsNullOrWhiteSpace(reference.SourceProjectFilePath) &&
                string.Equals(NormalizePath(reference.SourceProjectFilePath), sourceProjectPath, StringComparison.OrdinalIgnoreCase)))
            .ToArray();

        if (consumers.Length == 0)
            return;

        try
        {
            var outputPath = EmitMacroProjectOutput(changedProject);
            var solution = _workspace.CurrentSolution;

            foreach (var consumer in consumers)
            {
                var updatedReferences = consumer.MacroReferences
                    .Select(reference =>
                        !string.IsNullOrWhiteSpace(reference.SourceProjectFilePath) &&
                        string.Equals(NormalizePath(reference.SourceProjectFilePath), sourceProjectPath, StringComparison.OrdinalIgnoreCase)
                            ? MacroReference.CreateFromFile(outputPath, changedProject.FilePath)
                            : reference)
                    .ToArray();

                solution = solution.WithMacroReferences(consumer.Id, updatedReferences);
                _diagnosticsCache.TryRemove(consumer.Id, out _);
            }

            _workspace.TryApplyChanges(solution);
        }
        catch (Exception ex)
        {
            _logger.LogWarning(
                ex,
                "Failed to refresh macro consumers for source project '{ProjectFilePath}'.",
                changedProject.FilePath);
        }
    }

    private string EmitMacroProjectOutput(Project macroProject)
    {
        if (string.IsNullOrWhiteSpace(macroProject.FilePath))
            throw new InvalidOperationException("Macro project file path is required.");

        var evaluation = MsBuildProjectEvaluator.Evaluate(macroProject.FilePath, RavenProjectConventions.Default, macroProject.TargetFramework);
        var outputPath = GetShadowMacroOutputPath(macroProject, evaluation.AssemblyName);

        Directory.CreateDirectory(Path.GetDirectoryName(outputPath)!);

        try
        {
            using var peStream = File.Create(outputPath);
            using var pdbStream = File.Create(Path.ChangeExtension(outputPath, ".pdb"));
            var emitResult = _workspace.GetCompilation(macroProject.Id).Emit(peStream, pdbStream);
            if (!emitResult.Success)
                throw new InvalidOperationException(string.Join(Environment.NewLine, emitResult.Diagnostics.Select(static diagnostic => diagnostic.ToString())));
        }
        catch
        {
            TryDeleteFile(outputPath);
            TryDeleteFile(Path.ChangeExtension(outputPath, ".pdb"));
            throw;
        }

        return outputPath;
    }

    private static string GetShadowMacroOutputPath(Project macroProject, string assemblyName)
    {
        var projectIdentity = Path.GetFileNameWithoutExtension(macroProject.FilePath)
            ?? macroProject.Id.ToString();
        var versionSegment = macroProject.Version.ToString().Replace(Path.DirectorySeparatorChar, '_')
            .Replace(Path.AltDirectorySeparatorChar, '_')
            .Replace(':', '_');
        var directory = Path.Combine(MacroShadowOutputRoot, projectIdentity, versionSegment);
        return Path.Combine(directory, $"{assemblyName}.dll");
    }

    private static void TryDeleteFile(string path)
    {
        try
        {
            if (File.Exists(path))
                File.Delete(path);
        }
        catch
        {
        }
    }

    private bool TryResolveOwnedDocument(DocumentUri uri, out OwnedDocument ownedDocument)
    {
        if (_documents.TryGetValue(uri, out ownedDocument))
        {
            var currentDocument = _workspace.CurrentSolution.GetDocument(ownedDocument.DocumentId);
            var currentProject = _workspace.CurrentSolution.GetProject(ownedDocument.ProjectId);
            if (currentDocument is not null &&
                currentProject is not null &&
                currentDocument.Project.Id == ownedDocument.ProjectId &&
                currentDocument.Version == ownedDocument.Version)
                return true;

            _documents.TryRemove(uri, out _);
        }

        var filePath = uri.GetFileSystemPath();
        if (string.IsNullOrWhiteSpace(filePath))
        {
            ownedDocument = default;
            return false;
        }

        var normalizedFilePath = NormalizePath(filePath);

        lock (_gate)
        {
            var preferredProjectId = _projectsByRoot.Values.FirstOrDefault();
            if (preferredProjectId != default &&
                TryFindExistingDocument(_workspace.CurrentSolution, preferredProjectId, normalizedFilePath, out var existingDocument, out var ownerProjectId))
            {
                ownedDocument = new OwnedDocument(existingDocument.Id, ownerProjectId, existingDocument.Version, IsProjectDocument: true);
                _documents[uri] = ownedDocument;
                return true;
            }

            foreach (var project in _workspace.CurrentSolution.Projects)
            {
                var match = project.Documents.FirstOrDefault(doc =>
                    !string.IsNullOrWhiteSpace(doc.FilePath) &&
                    string.Equals(NormalizePath(doc.FilePath), normalizedFilePath, StringComparison.OrdinalIgnoreCase));
                if (match is null)
                    continue;

                ownedDocument = new OwnedDocument(match.Id, project.Id, match.Version, IsProjectDocument: true);
                _documents[uri] = ownedDocument;
                return true;
            }
        }

        ownedDocument = default;
        return false;
    }

    public IReadOnlyList<Project> GetProjectsSnapshot()
    {
        lock (_gate)
        {
            return _workspace.CurrentSolution.Projects.ToArray();
        }
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

        var ravenCoreReferencePath = ResolveRavenCoreReferencePath(version.Moniker.ToTfm());
        if (ravenCoreReferencePath is not null)
        {
            solution = solution.AddMetadataReference(projectId, MetadataReference.CreateFromFile(ravenCoreReferencePath));
            _logger.LogDebug("Added Raven.Core metadata reference '{ReferencePath}' for project '{ProjectName}'.", ravenCoreReferencePath, name);
        }
        else
        {
            _logger.LogWarning("Unable to locate a valid Raven.Core metadata reference for project '{ProjectName}'.", name);
        }

        _workspace.TryApplyChanges(solution);
        EnsureBuiltInAnalyzers(projectId);
        return projectId;
    }

    private string? ResolveRavenCoreReferencePath(string preferredTfm)
    {
        foreach (var candidate in EnumerateRavenCoreCandidates(preferredTfm))
        {
            if (IsValidMetadataReferencePath(candidate))
                return candidate;
        }

        return null;
    }

    private IEnumerable<string> EnumerateRavenCoreCandidates(string preferredTfm)
    {
        var tfms = new[] { preferredTfm, "net11.0", "net10.0" }
            .Where(tfm => !string.IsNullOrWhiteSpace(tfm))
            .Distinct(StringComparer.OrdinalIgnoreCase)
            .ToArray();

        var roots = new[]
        {
            TryFindRepositoryRoot(Directory.GetCurrentDirectory()),
            TryFindRepositoryRoot(AppContext.BaseDirectory),
        }
        .Where(path => !string.IsNullOrWhiteSpace(path))
        .Cast<string>()
        .Distinct(StringComparer.OrdinalIgnoreCase)
        .ToArray();

        var candidates = new List<string>();

        foreach (var root in roots)
        {
            foreach (var tfm in tfms)
            {
                candidates.Add(Path.Combine(root, "src", "Raven.Core", "bin", "Debug", tfm, "Raven.Core.dll"));
                candidates.Add(Path.Combine(root, "src", "Raven.Core", "bin", "Debug", tfm, tfm, "Raven.Core.dll"));
            }
        }

        candidates.Add(Path.Combine(AppContext.BaseDirectory, "Raven.Core.dll"));

        return candidates.Distinct(StringComparer.OrdinalIgnoreCase);
    }

    private static string? TryFindRepositoryRoot(string startPath)
    {
        if (string.IsNullOrWhiteSpace(startPath))
            return null;

        var current = Directory.Exists(startPath)
            ? new DirectoryInfo(startPath)
            : new DirectoryInfo(Path.GetDirectoryName(startPath)!);

        while (current is not null)
        {
            var solutionPath = Path.Combine(current.FullName, "Raven.sln");
            if (File.Exists(solutionPath))
                return current.FullName;

            current = current.Parent;
        }

        return null;
    }

    private static bool IsValidMetadataReferencePath(string path)
    {
        try
        {
            var fileInfo = new FileInfo(path);
            if (!fileInfo.Exists || fileInfo.Length == 0)
                return false;

            _ = MetadataReference.CreateFromFile(path);
            return true;
        }
        catch
        {
            return false;
        }
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

    private static int GetDirectoryDepth(string normalizedRoot, string projectFilePath)
    {
        var projectDirectory = NormalizePath(Path.GetDirectoryName(projectFilePath) ?? string.Empty);
        if (string.Equals(projectDirectory, normalizedRoot, StringComparison.OrdinalIgnoreCase))
            return 0;

        var relative = Path.GetRelativePath(normalizedRoot, projectDirectory);
        if (string.IsNullOrWhiteSpace(relative) || relative == ".")
            return 0;

        return relative
            .Split([Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar], StringSplitOptions.RemoveEmptyEntries)
            .Length;
    }

    private readonly record struct OwnedDocument(DocumentId DocumentId, ProjectId ProjectId, VersionStamp Version, bool IsProjectDocument);
    private readonly record struct ReloadDocumentState(DocumentUri Uri, string Text, bool IsProjectDocument);
    private readonly record struct CachedDiagnostics(VersionStamp Version, ImmutableArray<CodeDiagnostic> Diagnostics);
}
