namespace Raven.CodeAnalysis;

public static class VersionUtilities
{
    public static bool HasProjectChanged(Solution oldSolution, Solution newSolution, ProjectId projectId)
    {
        var oldProject = oldSolution.GetProject(projectId);
        var newProject = newSolution.GetProject(projectId);

        if (oldProject is null || newProject is null)
            return false;

        return oldProject.Version != newProject.Version;
    }

    public static bool HasDocumentChanged(Solution oldSolution, Solution newSolution, DocumentId documentId)
    {
        var oldDoc = oldSolution.GetDocument(documentId);
        var newDoc = newSolution.GetDocument(documentId);

        if (oldDoc is null || newDoc is null)
            return false;

        return oldDoc.GetTextAsync().Result != newDoc.GetTextAsync().Result;
    }
}
