
using System;

namespace Raven.CodeAnalysis
{
    public sealed class WorkspaceChangeEventArgs : EventArgs
    {
        public WorkspaceChangeEventArgs(
            WorkspaceChangeKind kind,
            Solution oldSolution,
            Solution newSolution,
            ProjectId? projectId = null,
            DocumentId? documentId = null)
        {
            Kind = kind;
            OldSolution = oldSolution;
            NewSolution = newSolution;
            ProjectId = projectId;
            DocumentId = documentId;
        }

        public WorkspaceChangeKind Kind { get; }
        public Solution OldSolution { get; }
        public Solution NewSolution { get; }
        public ProjectId? ProjectId { get; }
        public DocumentId? DocumentId { get; }
    }
}
