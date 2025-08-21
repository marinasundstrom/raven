using System;
using System.Collections.Generic;
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
        var loaded = new List<(ProjectId Id, ProjectFile.ProjectFileInfo Info, string Path)>();
        foreach (var projElem in root.Elements("Project"))
        {
            var rel = (string?)projElem.Attribute("Path") ?? throw new InvalidDataException("Project path missing.");
            var projPath = Path.Combine(dir, rel);
            var projInfo = ProjectFile.Load(projPath);
            var projId = ProjectId.CreateNew(solution.Id);
            solution = solution.AddProject(projId, projInfo.Info.Name, projPath, projInfo.Info.TargetFramework, projInfo.Info.AssemblyName, projInfo.Info.CompilationOptions);
            foreach (var doc in projInfo.Info.Documents)
            {
                var docId = DocumentId.CreateNew(projId);
                solution = solution.AddDocument(docId, doc.Name, doc.Text, doc.FilePath);
            }
            var tfm = projInfo.Info.TargetFramework ?? workspace.DefaultTargetFramework;
            foreach (var reference in workspace.GetFrameworkReferences(tfm))
                solution = solution.AddMetadataReference(projId, reference);
            loaded.Add((projId, projInfo, projPath));
        }

        var pathMap = loaded.ToDictionary(x => x.Path, x => x.Id, StringComparer.OrdinalIgnoreCase);
        foreach (var (id, info, projPath) in loaded)
        {
            foreach (var refPath in info.ProjectReferences)
            {
                var full = Path.IsPathRooted(refPath) ? refPath : Path.GetFullPath(Path.Combine(Path.GetDirectoryName(projPath)!, refPath));
                if (pathMap.TryGetValue(full, out var refId))
                    solution = solution.AddProjectReference(id, new ProjectReference(refId));
            }
        }
        return solution;
    }
}
