namespace Raven.CodeAnalysis;

public class Workspace : IDisposable
{
    protected Workspace(string? kind)
    {
        Kind = kind;
    }

    public virtual string? Kind { get; }

    public Solution CurrentSolution { get; protected set; }

    public event EventHandler<WorkspaceChangeEventArgs>? WorkspaceChanged;

    public async Task<Solution> OpenSolutionAsync(string solutionPath, CancellationToken cancellationToken = default)
    {
        // Simulate loading a solution from file
        var solution = new Solution(); // Implement actual loading logic
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
        // Detect changes (e.g., added/removed/updated documents or projects)
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

        // Detect project additions
        var addedProjects = newSolution.Projects.Select(x => x.Id).Except(oldSolution.Projects.Select(x => x.Id));
        foreach (var added in addedProjects)
        {
            changes.Add(new WorkspaceChangeEventArgs(
                WorkspaceChangeKind.ProjectAdded, newSolution, oldSolution, added, null));
        }

        // Detect project removals
        var removedProjects = oldSolution.Projects.Select(x => x.Id).Except(newSolution.Projects.Select(x => x.Id));
        foreach (var removed in removedProjects)
        {
            changes.Add(new WorkspaceChangeEventArgs(
                WorkspaceChangeKind.ProjectRemoved, newSolution, oldSolution, removed, null));
        }

        // Detect changes in existing projects
        foreach (var project in newSolution.Projects)
        {
            var oldProject = oldSolution.Projects.FirstOrDefault(p => p.Id == project.Id);
            if (oldProject == null) continue;

            // Detect added documents
            var addedDocuments = project.Documents.Select(x => x.Id).Except(oldProject.Documents.Select(x => x.Id));
            foreach (var addedDoc in addedDocuments)
            {
                changes.Add(new WorkspaceChangeEventArgs(
                    WorkspaceChangeKind.DocumentAdded, newSolution, oldSolution, project.Id, addedDoc));
            }

            // Detect removed documents
            var removedDocuments = oldProject.Documents.Select(x => x.Id).Except(project.Documents.Select(x => x.Id));
            foreach (var removedDoc in removedDocuments)
            {
                changes.Add(new WorkspaceChangeEventArgs(
                    WorkspaceChangeKind.DocumentRemoved, newSolution, oldSolution, project.Id, removedDoc));
            }

            // Detect updated documents
            foreach (var document in project.Documents)
            {
                var oldDocument = oldProject.Documents.FirstOrDefault(d => d.Id == document.Id);
                if (oldDocument == null) continue;

                if (oldDocument != null && !document.SourceTextEquals(oldDocument))
                {
                    changes.Add(new WorkspaceChangeEventArgs(
                        WorkspaceChangeKind.DocumentChanged, newSolution, oldSolution, project.Id, document.Id));
                }
            }

            // Detect additional document changes (if applicable)
            var addedAdditionalDocs = project.AdditionalDocuments.Select(x => x.Id).Except(oldProject.AdditionalDocuments.Select(x => x.Id));
            foreach (var addedDoc in addedAdditionalDocs)
            {
                changes.Add(new WorkspaceChangeEventArgs(
                    WorkspaceChangeKind.AdditionalDocumentAdded, newSolution, oldSolution, project.Id, addedDoc));
            }

            var removedAdditionalDocs = oldProject.AdditionalDocuments.Select(x => x.Id).Except(project.AdditionalDocuments.Select(x => x.Id));
            foreach (var removedDoc in removedAdditionalDocs)
            {
                changes.Add(new WorkspaceChangeEventArgs(
                    WorkspaceChangeKind.AdditionalDocumentRemoved, newSolution, oldSolution, project.Id, removedDoc));
            }
        }

        return changes;
    }

    public void Dispose()
    {
        // Clean up resources or event handlers
        WorkspaceChanged = null;
    }
}
