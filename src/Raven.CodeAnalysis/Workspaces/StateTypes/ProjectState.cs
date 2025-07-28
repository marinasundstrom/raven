namespace Raven.CodeAnalysis;

sealed class ProjectState
{
    public ProjectInfo ProjectInfo { get; }

    public ProjectState(ProjectInfo projectInfo, TextDocumentStates<DocumentState> documentStates)
    {
        ProjectInfo = projectInfo.WithDocuments([]);
        DocumentStates = documentStates;
    }

    public ProjectId Id => ProjectInfo.Id;
    public string Name => ProjectInfo.Name;
    public VersionStamp Version => ProjectInfo.Version;
    public ParseOptions? ParseOptions => ProjectInfo.ParseOptions;
    public string? DefaultNamespace => ProjectInfo.DefaultNamespace;
    public string? OutputFilePath => ProjectInfo.OutputFilePath;

    public IReadOnlyList<MetadataReference> MetadataReferences => ProjectInfo.MetadataReferences;
    public IReadOnlyList<ProjectReference> ProjectReferences => ProjectInfo.ProjectReferences;

    public TextDocumentStates<DocumentState> DocumentStates { get; }

    public ProjectState WithMetadataReferences(IEnumerable<MetadataReference> enumerable)
    {
        return With(ProjectInfo.WithVersion(Version.GetNewerVersion()), DocumentStates);
    }

    public ProjectState WithProjectReferences(IEnumerable<ProjectReference> enumerable)
    {
        return With(ProjectInfo.WithVersion(Version.GetNewerVersion()), DocumentStates);
    }

    private ProjectState With(ProjectInfo? projectInfo = null, TextDocumentStates<DocumentState>? documentStates = null)
    {
        return new ProjectState(projectInfo ?? ProjectInfo, documentStates ?? DocumentStates);
    }
}