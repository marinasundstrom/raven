using System;

namespace Raven.CodeAnalysis;

/// <summary>
/// Provides application-wide services for workspaces and solutions.
/// </summary>
public class HostServices
{
    public HostServices(
        SyntaxTreeProvider syntaxTreeProvider,
        PersistenceService? persistenceService = null,
        IProjectSystemService? projectSystemService = null,
        IWorkspaceEventSink? workspaceEventSink = null)
    {
        SyntaxTreeProvider = syntaxTreeProvider ?? throw new ArgumentNullException(nameof(syntaxTreeProvider));
        PersistenceService = persistenceService;
        ProjectSystemService = projectSystemService;
        WorkspaceEventSink = workspaceEventSink;
    }

    /// <summary>The <see cref="SyntaxTreeProvider"/> used to parse documents.</summary>
    public SyntaxTreeProvider SyntaxTreeProvider { get; }

    /// <summary>The <see cref="PersistenceService"/> used to save and open workspaces.</summary>
    public PersistenceService? PersistenceService { get; }

    /// <summary>Project-system service used to open/save project files.</summary>
    public IProjectSystemService? ProjectSystemService { get; }

    /// <summary>Receives workspace-internal timing and diagnostic events.</summary>
    public IWorkspaceEventSink? WorkspaceEventSink { get; }

    /// <summary>Gets a default instance of <see cref="HostServices"/>.</summary>
    public static HostServices Default { get; } = new HostServices(new SyntaxTreeProvider());
}

public interface IWorkspaceEventSink
{
    void Report(WorkspaceEvent workspaceEvent);
}

public sealed record WorkspaceEvent(
    string Operation,
    string? ProjectName,
    string? DocumentPath,
    double ElapsedMilliseconds,
    string Detail);
