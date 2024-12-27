using System.Text.Json;

using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

public class Solution
{
    private readonly SolutionInfo _solutionInfo;
    private IEnumerable<Project> _projects;

    public Solution()
    {
        Workspace = new AdhocWorkspace();
        _solutionInfo = new SolutionInfo(
            new SolutionAttributes(
                SolutionId.CreateNewId(),
                VersionStamp.Create(),
                null), []);
    }

    public Solution(Workspace workspace, SolutionInfo solutionInfo)
    {
        Workspace = workspace;
        _solutionInfo = solutionInfo;
    }

    public SolutionId Id => _solutionInfo.Id;

    public IEnumerable<Project> Projects => _projects = _solutionInfo.Projects.Select(d => new Project(this, d));

    public Workspace Workspace { get; }

    public Solution AddDocument(DocumentId documentId, string name, SourceText text,
        IEnumerable<string>? folders = default, string? filePath = default)
    {
        // Create a new Document instance
        var d = new DocumentInfo(
            new DocumentAttributes(
                id: documentId,
                name: name,
                folders: folders?.ToList(),
                sourceCodeKind: SourceCodeKind.Regular,
                filePath: filePath, // Initialize as needed
                false,
                false
            // sourceText: text
            // Default kind
            )
        );
        var document = new Document(project, d);

        // Clone the current projects list
        var updatedProjects = Projects.ToList();

        // Find the target project
        var projectIndex = updatedProjects.FindIndex(p => p.Id == documentId.ProjectId);
        if (projectIndex == -1)
            throw new InvalidOperationException($"Project with ID {documentId.ProjectId} not found.");

        var targetProject = updatedProjects[projectIndex];

        // Add the document to the project's document list
        var updatedDocuments = targetProject.Documents.ToList();
        updatedDocuments.Add(document);

        // Create a new instance of the project with updated documents
        var updatedProject = new Project(
            id: targetProject.Id, name: targetProject.Name, targetProject.AssemblyName, targetProject.Language, documents: updatedDocuments,
            additionalDocuments: targetProject.AdditionalDocuments, targetProject.ProjectReferences);

        // Replace the old project with the updated project
        updatedProjects[projectIndex] = updatedProject;

        // Return a new solution instance with the updated projects list
        return new Solution(Id, updatedProjects);
    }

    public Solution AddProject(ProjectId projectId, string name, string assemblyName, string language)
    {
        var version = VersionStamp.Create();
        var project = ProjectInfo.Create(projectId, version, name, assemblyName, language, null, null, null, [], [], [], []);

        return new Solution(
            Workspace,
            _solutionInfo.WithProjects());
    }

    public Document GetDocument(DocumentId documentId)
    {
        return Projects
            .SelectMany(project => project.Documents)
            .FirstOrDefault(doc => doc.Id == documentId)
            ?? throw new InvalidOperationException("Document not found.");
    }

    public async Task<IEnumerable<Diagnostic>> GetDiagnosticsAsync(CancellationToken cancellationToken = default)
    {
        var diagnostics = new List<Diagnostic>();
        foreach (var project in Projects)
        {
            diagnostics.AddRange(await project.GetDiagnosticsAsync(cancellationToken));
        }
        return diagnostics;
    }

    // TODO: Replace with a more standard way
    public Solution UpdateDocument(DocumentId documentId, SourceText newText, string? newName = null)
    {
        // Clone the current projects list
        var updatedProjects = Projects.ToList();

        // Find the target project
        var projectIndex = updatedProjects.FindIndex(p => p.Id == documentId.ProjectId);
        if (projectIndex == -1)
            throw new InvalidOperationException($"Project with ID {documentId.ProjectId} not found.");

        var targetProject = updatedProjects[projectIndex];

        // Clone the project's document list
        var updatedDocuments = targetProject.Documents.ToList();

        // Find the target document
        var documentIndex = updatedDocuments.FindIndex(d => d.Id == documentId);
        if (documentIndex == -1)
            throw new InvalidOperationException($"Document with ID {documentId} not found in project {documentId.ProjectId}.");

        var targetDocument = updatedDocuments[documentIndex];

        // Create an updated document instance
        var updatedDocument = targetDocument.WithText(newText);
        if (newName != null)
            updatedDocument = updatedDocument.WithName(newName);

        // Replace the old document with the updated one
        updatedDocuments[documentIndex] = updatedDocument;

        return UpdateProject(targetProject, updatedDocuments, updatedProjects, projectIndex);
    }

    private Solution UpdateProject(Project targetProject, List<Document> updatedDocuments, List<Project> updatedProjects, int projectIndex)
    {
        // Create a new project with updated documents

        var p = new ProjectInfo(targetProject._projectInfo.Attributes, null, )

        var updatedProject = new Project(
            targetProject.Id, targetProject.Name, targetProject.AssemblyName, targetProject.Language, updatedDocuments,
            targetProject.AdditionalDocuments, targetProject.ProjectReferences);

        // Replace the old project with the updated project
        updatedProjects[projectIndex] = updatedProject;

        // Return a new solution instance with the updated projects
        return new Solution(Id, updatedProjects);
    }

    public Solution WithProject(Project project)
    {
        var updatedProjects = Projects.Select(proj =>
            proj.Id == project.Id ? project : proj
        ).ToList();

        return new Solution(Workspace, _solutionInfo.WithProjects(updatedProjects));
    }

    public string ToJson()
    {
        return JsonSerializer.Serialize(this);
    }

    public static Solution FromJson(string json)
    {
        return JsonSerializer.Deserialize<Solution>(json)!;
    }
}