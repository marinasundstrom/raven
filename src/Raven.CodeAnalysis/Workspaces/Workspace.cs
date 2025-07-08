namespace Raven.CodeAnalysis;

public class Workspace : IDisposable
{
    protected Workspace(string? kind)
    {
        Kind = kind;
    }

    public virtual string? Kind { get; }

    public Solution CurrentSolution { get; protected set; } = new(SolutionId.CreateNew(), "");

    public event EventHandler<WorkspaceChangeEventArgs>? WorkspaceChanged;

    public async Task<Solution> OpenSolutionAsync(string solutionPath, CancellationToken cancellationToken = default)
    {
        // TODO: Replace with actual loading logic from disk
        var solution = new Solution(SolutionId.CreateNew(), "Bar"); // Simulated load
        CurrentSolution = solution;
        WorkspaceChanged?.Invoke(this, new WorkspaceChangeEventArgs(
            WorkspaceChangeKind.SolutionAdded,
            solution,
            oldSolution: null,
            projectId: null,
            documentId: null
        ));
        return solution;
    }

    public bool TryApplyChanges(Solution solution)
    {
        var changes = DetectChanges(CurrentSolution, solution);
        CurrentSolution = solution;

        foreach (var change in changes)
        {
            WorkspaceChanged?.Invoke(this, change);
        }

        return true;
    }

    private IEnumerable<WorkspaceChangeEventArgs> DetectChanges(Solution oldSolution, Solution newSolution)
    {
        var changes = new List<WorkspaceChangeEventArgs>();

        var oldProjects = oldSolution.Projects.ToDictionary(p => p.Id);
        var newProjects = newSolution.Projects.ToDictionary(p => p.Id);

        var addedProjects = newProjects.Keys.Except(oldProjects.Keys);
        foreach (var added in addedProjects)
        {
            changes.Add(new WorkspaceChangeEventArgs(
                WorkspaceChangeKind.ProjectAdded, newSolution, oldSolution, added, null));
        }

        var removedProjects = oldProjects.Keys.Except(newProjects.Keys);
        foreach (var removed in removedProjects)
        {
            changes.Add(new WorkspaceChangeEventArgs(
                WorkspaceChangeKind.ProjectRemoved, newSolution, oldSolution, removed, null));
        }

        var commonProjects = oldProjects.Keys.Intersect(newProjects.Keys);
        foreach (var projectId in commonProjects)
        {
            var oldProject = oldProjects[projectId];
            var newProject = newProjects[projectId];

            var oldDocs = oldProject.Documents.ToDictionary(d => d.Id);
            var newDocs = newProject.Documents.ToDictionary(d => d.Id);

            var addedDocs = newDocs.Keys.Except(oldDocs.Keys);
            foreach (var addedDoc in addedDocs)
            {
                changes.Add(new WorkspaceChangeEventArgs(
                    WorkspaceChangeKind.DocumentAdded, newSolution, oldSolution, projectId, addedDoc));
            }

            var removedDocs = oldDocs.Keys.Except(newDocs.Keys);
            foreach (var removedDoc in removedDocs)
            {
                changes.Add(new WorkspaceChangeEventArgs(
                    WorkspaceChangeKind.DocumentRemoved, newSolution, oldSolution, projectId, removedDoc));
            }

            var commonDocs = oldDocs.Keys.Intersect(newDocs.Keys);
            foreach (var docId in commonDocs)
            {
                var oldText = oldDocs[docId].Text;
                var newText = newDocs[docId].Text;

                if (!string.Equals(oldText, newText, StringComparison.Ordinal))
                {
                    changes.Add(new WorkspaceChangeEventArgs(
                        WorkspaceChangeKind.DocumentChanged, newSolution, oldSolution, projectId, docId));
                }
            }

            // TODO: Detect metadata changes like name, references, etc.
        }

        return changes;
    }

    public void Dispose()
    {
        // Clean up if needed
    }
}
