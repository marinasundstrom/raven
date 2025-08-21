using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Xml.Linq;

using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

internal static class ProjectFile
{
    public static void Save(Project project, string filePath)
    {
        if (project is null) throw new ArgumentNullException(nameof(project));

        var dir = Path.GetDirectoryName(filePath)!;
        Directory.CreateDirectory(dir);

        foreach (var document in project.Documents)
        {
            var path = document.FilePath;
            if (string.IsNullOrEmpty(path))
                path = Path.Combine(dir, document.Name.EndsWith(".rav", StringComparison.OrdinalIgnoreCase) || document.Name.EndsWith(".rvn", StringComparison.OrdinalIgnoreCase) ? document.Name : document.Name + ".rav");
            else if (!Path.IsPathRooted(path))
                path = Path.Combine(dir, path);

            Directory.CreateDirectory(Path.GetDirectoryName(path)!);
            File.WriteAllText(path, document.Text.ToString());
        }

        var projectElement = new XElement("Project",
            new XAttribute("Name", project.Name),
            project.TargetFramework is string tfm ? new XAttribute("TargetFramework", tfm) : null);

        var doc = new XDocument(projectElement);
        doc.Save(filePath);
    }

    public static ProjectInfo Load(string filePath)
    {
        var xdoc = XDocument.Load(filePath);
        var root = xdoc.Root ?? throw new InvalidDataException("Invalid project file.");
        var name = (string?)root.Attribute("Name") ?? Path.GetFileNameWithoutExtension(filePath);
        var targetFramework = (string?)root.Attribute("TargetFramework");
        var tempSolutionId = SolutionId.CreateNew();
        var projectId = ProjectId.CreateNew(tempSolutionId);
        var projectDir = Path.GetDirectoryName(filePath)!;

        var docElements = root.Elements("Document").ToList();
        IEnumerable<string> paths;
        if (docElements.Count > 0)
        {
            paths = docElements.Select(e =>
            {
                var attr = (string?)e.Attribute("FilePath") ?? (string?)e.Attribute("Path");
                if (attr is null)
                    throw new InvalidDataException("Document path missing.");
                return Path.IsPathRooted(attr) ? attr : Path.Combine(projectDir, attr);
            });
        }
        else
        {
            var rav = Directory.EnumerateFiles(projectDir, "*.rav");
            var rvn = Directory.EnumerateFiles(projectDir, "*.rvn");
            paths = rav.Concat(rvn);
        }

        var documents = paths.Select(p =>
        {
            var docId = DocumentId.CreateNew(projectId);
            var text = SourceText.From(File.ReadAllText(p));
            return DocumentInfo.Create(docId, Path.GetFileName(p), text, p);
        }).ToList();

        var attrInfo = new ProjectInfo.ProjectAttributes(projectId, name, VersionStamp.Create());
        return new ProjectInfo(attrInfo, documents, filePath: filePath, targetFramework: targetFramework);
    }
}
