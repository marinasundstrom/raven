using System.Collections.Immutable;
using System.Linq;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

/// <summary>
/// Public immutable representation of a set of projects.  The heavy lifting is
/// done by <see cref="SolutionState"/> which stores the actual data.  Each
/// mutation returns a new <see cref="Solution"/> with a different underlying
/// state, allowing cheap incremental updates.
/// </summary>
public sealed class Solution
{
    internal SolutionState State { get; }

    public Solution(SolutionInfo info)
        : this(CreateStateFromInfo(info))
    {
    }

    private Solution(SolutionState state)
    {
        State = state;
    }

    public SolutionId Id => State.Id;
    public VersionStamp Version => State.Version;

    public IEnumerable<Project> Projects =>
        State.ProjectIds.Select(id => GetProject(id)!).Where(p => p is not null);

    public Project? GetProject(ProjectId id)
    {
        var state = State.GetProjectState(id);
        return state is null ? null : new Project(this, state);
    }

    public Document? GetDocument(DocumentId id)
    {
        var docState = State.GetDocumentState(id);
        if (docState is null) return null;
        var projectState = State.GetProjectState(id.ProjectId)!;
        var project = new Project(this, projectState);
        return new Document(project, docState);
    }

    public bool ContainsProject(ProjectId id) => State.ContainsProject(id);

    public Solution AddProject(string name)
    {
        var projectId = ProjectId.CreateNew(Id);
        var info = new ProjectInfo(
            new ProjectInfo.ProjectAttributes(projectId, name, VersionStamp.Create()),
            Enumerable.Empty<DocumentInfo>());
        var newState = State.AddProject(info);
        return new Solution(newState);
    }

    public Solution AddProject(ProjectInfo projectInfo)
    {
        var newState = State.AddProject(projectInfo);
        return new Solution(newState);
    }

    public Solution RemoveProject(ProjectId projectId)
    {
        var newState = State.RemoveProject(projectId);
        return new Solution(newState);
    }

    public Solution AddDocument(ProjectId projectId, string name, SourceText text)
    {
        var docId = DocumentId.CreateNew(projectId);
        var docInfo = DocumentInfo.Create(docId, name, text);
        var newState = State.AddDocument(projectId, docInfo);
        return new Solution(newState);
    }

    private static SolutionState CreateStateFromInfo(SolutionInfo info)
    {
        var projectStates = info.Projects
            .Select(pi => new ProjectState(pi, CreateDocumentStates(pi)))
            .ToImmutableDictionary(ps => ps.Id);
        return new SolutionState(info.Attributes,
                                 projectStates.Keys.ToList(),
                                 projectStates,
                                 WorkspaceChangeKind.SolutionAdded);
    }

    private static TextDocumentStates<DocumentState> CreateDocumentStates(ProjectInfo pi)
    {
        var docs = pi.Documents
            .Select(di => new DocumentState(di.Attributes, new TextAndVersionSource(), new ParseOptions()))
            .ToImmutableList();
        return new TextDocumentStates<DocumentState>(docs);
    }
}
