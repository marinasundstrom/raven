using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;

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
        : this(kind, HostServices.Default)
    {
    }

    protected Workspace(string kind, HostServices services)
    {
        Kind = kind;
        Services = services ?? throw new ArgumentNullException(nameof(services));
        _currentSolution = new Solution(services, this);
    }

    public string Kind { get; }

    /// <summary>Services available to this workspace.</summary>
    public HostServices Services { get; }

    public event EventHandler<WorkspaceChangeEventArgs>? WorkspaceChanged;

    public Solution CurrentSolution => _currentSolution;

    /// <summary>Creates a new empty <see cref="Solution"/> using the workspace services.</summary>
    public Solution CreateSolution() => new Solution(Services, this);

    /// <summary>Opens the specified <see cref="Solution"/> as the current solution.</summary>
    public void OpenSolution(Solution solution)
    {
        if (solution is null) throw new ArgumentNullException(nameof(solution));
        if (!ReferenceEquals(solution.Services, Services))
            throw new InvalidOperationException("Solution was created with different host services.");
        if (!ReferenceEquals(solution.Workspace, this))
            throw new InvalidOperationException("Solution was created with different workspace.");

        TryApplyChanges(solution);
    }

    /// <summary>Attempts to apply a new solution and raises the appropriate change event.</summary>
    public bool TryApplyChanges(Solution newSolution)
    {
        if (newSolution is null) throw new ArgumentNullException(nameof(newSolution));
        if (!ReferenceEquals(newSolution.Services, Services))
            throw new InvalidOperationException("Solution was created with different host services.");
        if (!ReferenceEquals(newSolution.Workspace, this))
            throw new InvalidOperationException("Solution was created with different workspace.");
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
                foreach (var oldDoc in oldProj.Documents)
                {
                    if (proj.GetDocument(oldDoc.Id) is null)
                        return (WorkspaceChangeKind.DocumentRemoved, proj.Id, oldDoc.Id);
                }
                return (WorkspaceChangeKind.ProjectChanged, proj.Id, null);
            }
        }
        foreach (var oldProj in oldSolution.Projects)
        {
            if (newSolution.GetProject(oldProj.Id) is null)
                return (WorkspaceChangeKind.ProjectRemoved, oldProj.Id, null);
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
        return GetCompilation(projectId, new HashSet<ProjectId>());
    }

    private Compilation GetCompilation(ProjectId projectId, HashSet<ProjectId> building)
    {
        var project = CurrentSolution.GetProject(projectId)
            ?? throw new ArgumentException("Project not found", nameof(projectId));

        if (!building.Add(projectId))
            throw new InvalidOperationException("Circular project reference detected.");

        try
        {
            if (!_projectCompilations.TryGetValue(projectId, out var state))
            {
                return BuildCompilation(project, null, building);
            }

            if (state.Version == project.Version)
                return state.Compilation;

            return BuildCompilation(project, state, building);
        }
        finally
        {
            building.Remove(projectId);
        }
    }

    private Compilation BuildCompilation(Project project, ProjectCompilationState? state, HashSet<ProjectId> building)
    {
        state ??= new ProjectCompilationState();

        var syntaxTrees = new List<SyntaxTree>();
        var documentStates = state.DocumentStates;
        var presentDocs = new HashSet<DocumentId>();

        foreach (var doc in project.Documents)
        {
            presentDocs.Add(doc.Id);
            var tree = doc.SyntaxTree;
            if (tree is null)
                continue;
            if (documentStates.TryGetValue(doc.Id, out var docState) && docState.Version == doc.Version)
            {
                syntaxTrees.Add(docState.SyntaxTree);
            }
            else
            {
                syntaxTrees.Add(tree);
                documentStates[doc.Id] = new DocumentState(doc.Version, tree);
            }
        }

        // remove cached documents no longer present
        var toRemove = documentStates.Keys.Where(id => !presentDocs.Contains(id)).ToList();
        foreach (var id in toRemove)
            documentStates.Remove(id);

        var references = new List<MetadataReference>();
        references.AddRange(project.MetadataReferences);

        foreach (var projRef in project.ProjectReferences)
        {
            var compRef = GetCompilation(projRef.ProjectId, building).ToMetadataReference();
            references.Add(compRef);
        }

        var compilation = Compilation.Create(project.AssemblyName ?? project.Name,
            syntaxTrees.ToArray(), references.ToArray(), project.CompilationOptions);

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

    /// <summary>
    /// Gets diagnostics for the specified project, including analyzer diagnostics.
    /// </summary>
    public ImmutableArray<Diagnostic> GetDiagnostics(
        ProjectId projectId,
        CompilationWithAnalyzersOptions? analyzerOptions = null,
        CancellationToken cancellationToken = default)
    {
        var project = CurrentSolution.GetProject(projectId)
            ?? throw new ArgumentException("Project not found", nameof(projectId));

        var compilation = GetCompilation(projectId);
        var diagnostics = compilation.GetDiagnostics(analyzerOptions, cancellationToken).ToHashSet();

        if (project.CompilationOptions?.RunAnalyzers != false)
        {
            foreach (var reference in project.AnalyzerReferences)
            {
                foreach (var analyzer in reference.GetAnalyzers())
                {
                    var isInternalAnalyzer = AnalyzerDiagnosticIdValidator.IsInternalAnalyzer(analyzer);

                    foreach (var diagnostic in analyzer.Analyze(compilation, cancellationToken))
                    {
                        AnalyzerDiagnosticIdValidator.Validate(analyzer, diagnostic, isInternalAnalyzer);

                        var mapped = compilation.ApplyCompilationOptions(diagnostic, analyzerOptions?.ReportSuppressedDiagnostics ?? false);
                        if (mapped is not null)
                            diagnostics.Add(mapped);
                    }
                }
            }
        }

        return diagnostics.OrderBy(d => d.Location).ToImmutableArray();
    }
}
