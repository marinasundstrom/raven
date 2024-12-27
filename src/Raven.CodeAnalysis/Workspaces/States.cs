namespace Raven.CodeAnalysis;

public class ProjectState
{
    private ProjectInfo ProjectInfo { get; }

    public ProjectState(ProjectInfo projectInfo)
    {
        ProjectInfo = projectInfo;
    }

    public ProjectId Id => ProjectInfo.Id;
    public string Name => ProjectInfo.Name;
    public string Language => ProjectInfo.Language;
    public string AssemblyName => ProjectInfo.AssemblyName;
    public IEnumerable<DocumentInfo> Documents { get; internal set; }
    public IEnumerable<DocumentInfo> AdditionalDocuments { get; internal set; }
    public IEnumerable<ProjectReference> ProjectReferences { get; internal set; }

    internal ProjectState With(
        ProjectId id,
        VersionStamp version,
        string name,
        string assemblyName,
        string language,
        string? filePath = null
    )
    {
        return new ProjectState(ProjectInfo.With(
            id,
            version,
            name,
            assemblyName,
            language,
            filePath
        ));
    }
}


public class SolutionState
{
    private SolutionInfo SolutionInfo { get; }

    public SolutionState(SolutionInfo solutionInfo)
    {
        SolutionInfo = solutionInfo;
    }

    public SolutionId Id => SolutionInfo.Id;

    public VersionStamp Version => SolutionInfo.Version;

    public string? FilePath => SolutionInfo.FilePath;

    public IReadOnlyList<ProjectInfo> Projects { get; }
}