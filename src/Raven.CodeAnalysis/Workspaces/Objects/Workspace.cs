using System;
using System.Linq;

namespace Raven.CodeAnalysis;

/// <summary>
/// Coordinates a <see cref="Solution"/> and raises events when changes are applied.
/// </summary>
public class Workspace
{
    private Solution _currentSolution;

    public Workspace(string kind)
    {
        var info = new SolutionInfo(
            new SolutionInfo.SolutionAttributes(SolutionId.CreateNew(), string.Empty, VersionStamp.Create()),
            Enumerable.Empty<ProjectInfo>());
        _currentSolution = new Solution(info);
        Kind = kind;
    }

    public string Kind { get; }

    public event EventHandler<WorkspaceChangeEventArgs>? WorkspaceChanged;

    public Solution CurrentSolution => _currentSolution;

    /// <summary>
    /// Tries to apply the given solution as the current one and raises a WorkspaceChanged event.
    /// </summary>
    public bool TryApplyChanges(Solution newSolution)
    {
        if (newSolution == null) throw new ArgumentNullException(nameof(newSolution));
        var oldSolution = _currentSolution;
        if (ReferenceEquals(oldSolution, newSolution)) return true;

        _currentSolution = newSolution;
        OnWorkspaceChanged(new WorkspaceChangeEventArgs(
                                newSolution.ChangeKind,
                                oldSolution,
                                newSolution,
                                newSolution.LastProjectId,
                                newSolution.LastDocumentId));
        return true;
    }

    protected virtual void OnWorkspaceChanged(WorkspaceChangeEventArgs e) =>
        WorkspaceChanged?.Invoke(this, e);
}
