using Raven.CodeAnalysis;

namespace Raven.Compiler.Core;

public sealed class CompilerWorkspaceSession
{
    internal CompilerWorkspaceSession(
        RavenWorkspace workspace,
        ProjectId projectId,
        Project project,
        Compilation compilation,
        string targetFramework)
    {
        Workspace = workspace;
        ProjectId = projectId;
        Project = project;
        Compilation = compilation;
        TargetFramework = targetFramework;
    }

    public RavenWorkspace Workspace { get; }

    public ProjectId ProjectId { get; }

    public Project Project { get; }

    public Compilation Compilation { get; }

    public string TargetFramework { get; }

    public IReadOnlyList<Diagnostic> GetDiagnostics()
        => Workspace.GetDiagnostics(ProjectId);
}
