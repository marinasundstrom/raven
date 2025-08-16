using System;

namespace Raven.CodeAnalysis;

/// <summary>
/// Coordinates a <see cref="Solution"/> and raises events when changes are applied.
/// </summary>
public class Workspace
{
    private Solution _currentSolution;

    public Workspace(Solution initialSolution)
    {
        _currentSolution = initialSolution;
    }

    public event EventHandler<WorkspaceChangeEventArgs>? WorkspaceChanged;

    public Solution CurrentSolution => _currentSolution;

    /// <summary>
    /// Tries to apply the given solution as the current one. If the versions differ we raise a WorkspaceChanged event.
    /// </summary>
    public bool TryApplyChanges(Solution newSolution)
    {
        if (newSolution == null) throw new ArgumentNullException(nameof(newSolution));
        var oldSolution = _currentSolution;
        if (ReferenceEquals(oldSolution, newSolution)) return true;

        _currentSolution = newSolution;
        var state = newSolution.State;
        OnWorkspaceChanged(new WorkspaceChangeEventArgs(
            state.ChangeKind,
            oldSolution,
            newSolution,
            state.ProjectId,
            state.DocumentId));
        return true;
    }

    protected virtual void OnWorkspaceChanged(WorkspaceChangeEventArgs e) =>
        WorkspaceChanged?.Invoke(this, e);
}
