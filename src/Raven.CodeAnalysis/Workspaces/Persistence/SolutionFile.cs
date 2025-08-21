using System;
using System.IO;
using System.Linq;
using System.Xml.Linq;

namespace Raven.CodeAnalysis;

internal static class SolutionFile
{
    public static void Save(Solution solution, string filePath)
    {
        if (solution is null) throw new ArgumentNullException(nameof(solution));
        var dir = Path.GetDirectoryName(filePath)!;
        Directory.CreateDirectory(dir);
        var projects = solution.Projects.Select(p =>
        {
            var projectPath = p.FilePath ?? Path.Combine(dir, $"{p.Name}.ravenproj");
            ProjectFile.Save(p, projectPath);
            var relative = Path.GetRelativePath(dir, projectPath);
            return new XElement("Project", new XAttribute("Path", relative));
        });
        var doc = new XDocument(new XElement("Solution", projects));
        doc.Save(filePath);
    }

    public static Solution Open(string filePath, RavenWorkspace workspace)
    {
        if (workspace is null) throw new ArgumentNullException(nameof(workspace));
        var dir = Path.GetDirectoryName(filePath)!;
        var xdoc = XDocument.Load(filePath);
        var root = xdoc.Root ?? throw new InvalidDataException("Invalid solution file.");
        var solution = new Solution(workspace.Services, workspace);
        foreach (var projElem in root.Elements("Project"))
        {
            var rel = (string?)projElem.Attribute("Path") ?? throw new InvalidDataException("Project path missing.");
            var projPath = Path.Combine(dir, rel);
            var projInfo = ProjectFile.Load(projPath);
            var projId = ProjectId.CreateNew(solution.Id);
            solution = solution.AddProject(projId, projInfo.Name, projPath, projInfo.TargetFramework);
            foreach (var doc in projInfo.Documents)
            {
                var docId = DocumentId.CreateNew(projId);
                solution = solution.AddDocument(docId, doc.Name, doc.Text, doc.FilePath);
            }
            var tfm = projInfo.TargetFramework ?? workspace.DefaultTargetFramework;
            foreach (var reference in workspace.GetFrameworkReferences(tfm))
                solution = solution.AddMetadataReference(projId, reference);
        }
        return solution;
    }
}
