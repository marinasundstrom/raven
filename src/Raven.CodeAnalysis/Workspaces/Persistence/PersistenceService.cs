using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

/// <summary>
/// Provides solution and project persistence for workspaces.
/// </summary>
public class PersistenceService
{
    public virtual Solution OpenSolution(string filePath, Workspace workspace)
    {
        if (workspace is RavenWorkspace raven)
            return SolutionFile.Open(filePath, raven);
        throw new NotSupportedException("Solution persistence requires a RavenWorkspace.");
    }

    public virtual void SaveSolution(Solution solution, string filePath)
        => SolutionFile.Save(solution, filePath);

    public virtual ProjectId OpenProject(Workspace workspace, string projectFilePath)
    {
        if (workspace is not RavenWorkspace raven)
            throw new NotSupportedException("Project persistence requires a RavenWorkspace.");
        var projInfo = ProjectFile.Load(projectFilePath);
        var projectId = raven.AddProject(projInfo.Info.Name, projectFilePath, projInfo.Info.AssemblyName, projInfo.Info.CompilationOptions);
        var solution = workspace.CurrentSolution;
        foreach (var doc in projInfo.Info.Documents)
        {
            var docId = DocumentId.CreateNew(projectId);
            solution = solution.AddDocument(docId, doc.Name, doc.Text, doc.FilePath);
        }

        var tfm = projInfo.Info.TargetFramework ?? raven.DefaultTargetFramework;
        foreach (var reference in raven.GetFrameworkReferences(tfm))
            solution = solution.AddMetadataReference(projectId, reference);

        foreach (var metadataReferencePath in projInfo.MetadataReferences)
            solution = solution.AddMetadataReference(projectId, MetadataReference.CreateFromFile(metadataReferencePath));

        var packageReferences = NuGetPackageResolver.ResolveReferences(projectFilePath, tfm, projInfo.PackageReferences);
        foreach (var packageReference in packageReferences)
            solution = solution.AddMetadataReference(projectId, packageReference);

        foreach (var refPath in projInfo.ProjectReferences)
        {
            var full = Path.IsPathRooted(refPath) ? refPath : Path.GetFullPath(Path.Combine(Path.GetDirectoryName(projectFilePath)!, refPath));
            var refProj = raven.CurrentSolution.Projects.FirstOrDefault(p => string.Equals(p.FilePath, full, StringComparison.OrdinalIgnoreCase));
            if (refProj is not null)
                solution = solution.AddProjectReference(projectId, new ProjectReference(refProj.Id));
        }
        workspace.TryApplyChanges(solution);
        return projectId;
    }

    public virtual void SaveProject(Project project, string filePath)
        => ProjectFile.Save(project, filePath);

    /// <summary>
    /// Enables file system watching for project documents. Caller is responsible
    /// for disposing the returned handle to stop watching.
    /// </summary>
    public virtual IDisposable EnableFileWatching(RavenWorkspace workspace)
    {
        if (workspace is null) throw new ArgumentNullException(nameof(workspace));
        return new FileWatcher(workspace);
    }

    private sealed class FileWatcher : IDisposable
    {
        private readonly RavenWorkspace _workspace;
        private readonly Dictionary<ProjectId, FileSystemWatcher> _watchers = new();

        public FileWatcher(RavenWorkspace workspace)
        {
            _workspace = workspace;
            foreach (var project in workspace.CurrentSolution.Projects)
                WatchProject(project);
            _workspace.WorkspaceChanged += OnWorkspaceChanged;
        }

        private static bool IsSource(string path) =>
            RavenFileExtensions.HasRavenExtension(path);

        private void WatchProject(Project project)
        {
            var path = project.FilePath;
            if (string.IsNullOrEmpty(path))
                return;
            var dir = Path.GetDirectoryName(path)!;
            var watcher = new FileSystemWatcher(dir)
            {
                NotifyFilter = NotifyFilters.FileName | NotifyFilters.LastWrite,
                IncludeSubdirectories = true,
                EnableRaisingEvents = true,
                Filter = "*.*"
            };
            watcher.Created += (s, e) => OnChanged(project.Id, e);
            watcher.Changed += (s, e) => OnChanged(project.Id, e);
            watcher.Deleted += (s, e) => OnDeleted(project.Id, e);
            watcher.Renamed += (s, e) => OnRenamed(project.Id, e);
            _watchers[project.Id] = watcher;
        }

        private void OnWorkspaceChanged(object? sender, WorkspaceChangeEventArgs e)
        {
            if (e.Kind == WorkspaceChangeKind.ProjectAdded && e.ProjectId is { } pid)
            {
                var project = _workspace.CurrentSolution.GetProject(pid);
                if (project is not null)
                    WatchProject(project);
            }
            else if (e.Kind == WorkspaceChangeKind.ProjectRemoved && e.ProjectId is { } rid)
            {
                if (_watchers.Remove(rid, out var watcher))
                    watcher.Dispose();
            }
        }

        private void OnChanged(ProjectId projectId, FileSystemEventArgs e)
        {
            if (!IsSource(e.FullPath))
                return;
            try
            {
                var solution = _workspace.CurrentSolution;
                var project = solution.GetProject(projectId);
                if (project is null)
                    return;
                var doc = project.Documents.FirstOrDefault(d => string.Equals(d.FilePath, e.FullPath, StringComparison.OrdinalIgnoreCase));
                var text = SourceText.From(File.ReadAllText(e.FullPath));
                if (doc is null)
                {
                    var docId = DocumentId.CreateNew(projectId);
                    solution = solution.AddDocument(docId, Path.GetFileName(e.FullPath), text, e.FullPath);
                }
                else
                {
                    solution = solution.WithDocumentText(doc.Id, text);
                }
                _workspace.TryApplyChanges(solution);
            }
            catch
            {
                // ignore file read errors
            }
        }

        private void OnDeleted(ProjectId projectId, FileSystemEventArgs e)
        {
            if (!IsSource(e.FullPath))
                return;
            var solution = _workspace.CurrentSolution;
            var project = solution.GetProject(projectId);
            var doc = project?.Documents.FirstOrDefault(d => string.Equals(d.FilePath, e.FullPath, StringComparison.OrdinalIgnoreCase));
            if (doc is null)
                return;
            solution = solution.RemoveDocument(doc.Id);
            _workspace.TryApplyChanges(solution);
        }

        private void OnRenamed(ProjectId projectId, RenamedEventArgs e)
        {
            OnDeleted(projectId, new FileSystemEventArgs(WatcherChangeTypes.Deleted, Path.GetDirectoryName(e.OldFullPath)!, Path.GetFileName(e.OldFullPath)));
            OnChanged(projectId, new FileSystemEventArgs(WatcherChangeTypes.Created, Path.GetDirectoryName(e.FullPath)!, Path.GetFileName(e.FullPath)));
        }

        public void Dispose()
        {
            _workspace.WorkspaceChanged -= OnWorkspaceChanged;
            foreach (var watcher in _watchers.Values)
                watcher.Dispose();
            _watchers.Clear();
        }
    }
}
