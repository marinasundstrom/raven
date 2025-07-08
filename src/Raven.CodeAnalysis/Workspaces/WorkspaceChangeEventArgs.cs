namespace Raven.CodeAnalysis;

public class WorkspaceChangeEventArgs : EventArgs
{
    public WorkspaceChangeEventArgs(WorkspaceChangeKind kind, Solution newSolution, Solution oldSolution, ProjectId? projectId, DocumentId? documentId)
    {
        Kind = kind;
        NewSolution = newSolution;
        OldSolution = oldSolution;
        ProjectId = projectId;
        DocumentId = documentId;
    }

    public WorkspaceChangeKind Kind { get; }
    public Solution NewSolution { get; }
    public Solution OldSolution { get; }
    public ProjectId? ProjectId { get; }
    public DocumentId? DocumentId { get; }
}
