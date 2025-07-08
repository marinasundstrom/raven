using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

public static class SolutionExtensions
{
    public static Solution AddProject(this Solution solution, ProjectId id, string name, string assemblyName, string language)
    {
        var attributes = new ProjectAttributes(name, []);
        var project = new Project(id, attributes);
        return solution.AddProject(project);
    }

    public static Solution AddDocument(this Solution solution, DocumentId id, string name, SourceText text, string? filePath = null)
    {
        var projectId = id.ProjectId;
        var document = new Document(id, new DocumentAttributes(name, text.ToString()));
        var project = solution.GetProject(projectId);
        if (project == null) return solution;

        var updatedProject = project.AddDocument(document);
        return solution.WithProject(updatedProject);
    }

    public static Solution UpdateDocument(this Solution solution, DocumentId id, SourceText text, string? newName = null)
    {
        var oldDoc = solution.GetDocument(id);
        if (oldDoc == null) return solution;

        var newDoc = newName != null
            ? oldDoc.WithText(text.ToString()).WithName(newName)
            : oldDoc.WithText(text.ToString());

        return solution.WithDocument(newDoc);
    }
}
