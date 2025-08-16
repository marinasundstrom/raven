using System.Collections.Generic;
using System.Linq;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

/// <summary>
/// Coordinates a <see cref="Solution"/> instance and raises events when it changes.
/// </summary>
public class Workspace
{
    private Solution _currentSolution;
    private readonly Dictionary<ProjectId, ProjectCompilationState> _projectCompilations = new();

    protected Workspace(string kind)
    {
        Kind = kind;
        _currentSolution = new Solution();
    }

    public string Kind { get; }

    public event EventHandler<WorkspaceChangeEventArgs>? WorkspaceChanged;

    public Solution CurrentSolution => _currentSolution;

    /// <summary>Attempts to apply a new solution and raises the appropriate change event.</summary>
    public bool TryApplyChanges(Solution newSolution)
    {
        if (newSolution is null) throw new ArgumentNullException(nameof(newSolution));
        var oldSolution = _currentSolution;
        if (ReferenceEquals(oldSolution, newSolution)) return true;

        var (kind, projectId, documentId) = ComputeChangeKind(oldSolution, newSolution);
        _currentSolution = newSolution;

        // drop compilation caches for removed projects
        var removed = _projectCompilations.Keys.Where(id => newSolution.GetProject(id) is null).ToList();
        foreach (var id in removed)
            _projectCompilations.Remove(id);

        OnWorkspaceChanged(new WorkspaceChangeEventArgs(kind, oldSolution, newSolution, projectId, documentId));
        return true;
    }

    private static (WorkspaceChangeKind kind, ProjectId? projectId, DocumentId? documentId)
        ComputeChangeKind(Solution oldSolution, Solution newSolution)
    {
        foreach (var proj in newSolution.Projects)
        {
            var oldProj = oldSolution.GetProject(proj.Id);
            if (oldProj is null)
                return (WorkspaceChangeKind.ProjectAdded, proj.Id, null);

            if (oldProj.Version != proj.Version)
            {
                foreach (var doc in proj.Documents)
                {
                    var oldDoc = oldProj.GetDocument(doc.Id);
                    if (oldDoc is null)
                        return (WorkspaceChangeKind.DocumentAdded, proj.Id, doc.Id);
                    if (oldDoc.Version != doc.Version)
                        return (WorkspaceChangeKind.DocumentChanged, proj.Id, doc.Id);
                }
                return (WorkspaceChangeKind.ProjectChanged, proj.Id, null);
            }
        }
        return (WorkspaceChangeKind.SolutionChanged, null, null);
    }

    protected virtual void OnWorkspaceChanged(WorkspaceChangeEventArgs e)
        => WorkspaceChanged?.Invoke(this, e);

    /// <summary>
    /// Gets a <see cref="Compilation"/> for the specified project. Results are cached
    /// and incrementally rebuilt when documents change.
    /// </summary>
    public Compilation GetCompilation(ProjectId projectId)
    {
        var project = CurrentSolution.GetProject(projectId)
            ?? throw new ArgumentException("Project not found", nameof(projectId));

        if (!_projectCompilations.TryGetValue(projectId, out var state))
        {
            return BuildCompilation(project);
        }

        if (state.Version == project.Version)
            return state.Compilation;

        return BuildCompilation(project, state);
    }

    private Compilation BuildCompilation(Project project, ProjectCompilationState? state = null)
    {
        state ??= new ProjectCompilationState();

        var syntaxTrees = new List<SyntaxTree>();
        var documentStates = state.DocumentStates;
        var presentDocs = new HashSet<DocumentId>();

        foreach (var doc in project.Documents)
        {
            presentDocs.Add(doc.Id);
            if (documentStates.TryGetValue(doc.Id, out var docState) && docState.Version == doc.Version)
            {
                syntaxTrees.Add(docState.SyntaxTree);
            }
            else
            {
                var text = doc.GetTextAsync().Result;
                var tree = SyntaxTree.ParseText(text, options: null, path: doc.FilePath ?? doc.Name);
                syntaxTrees.Add(tree);
                documentStates[doc.Id] = new DocumentState(doc.Version, tree);
            }
        }

        // remove cached documents no longer present
        var toRemove = documentStates.Keys.Where(id => !presentDocs.Contains(id)).ToList();
        foreach (var id in toRemove)
            documentStates.Remove(id);

        var compilation = Compilation.Create(project.Name, syntaxTrees.ToArray());

        state.Version = project.Version;
        state.Compilation = compilation;

        _projectCompilations[project.Id] = state;
        return compilation;
    }

    private sealed class ProjectCompilationState
    {
        public VersionStamp Version;
        public Compilation Compilation = null!;
        public Dictionary<DocumentId, DocumentState> DocumentStates { get; } = new();
    }

    private sealed record DocumentState(VersionStamp Version, SyntaxTree SyntaxTree);
}
