using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Xml.Linq;

using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

internal static class ProjectFile
{
    internal sealed record ProjectFileInfo(ProjectInfo Info, ImmutableArray<string> ProjectReferences);

    public static void Save(Project project, string filePath)
    {
        if (project is null) throw new ArgumentNullException(nameof(project));

        var dir = Path.GetDirectoryName(filePath)!;
        Directory.CreateDirectory(dir);

        foreach (var document in project.Documents)
        {
            var path = document.FilePath;
            if (string.IsNullOrEmpty(path))
                path = Path.Combine(dir, RavenFileExtensions.HasRavenExtension(document.Name) ? document.Name : document.Name + RavenFileExtensions.Raven);
            else if (!Path.IsPathRooted(path))
                path = Path.Combine(dir, path);

            Directory.CreateDirectory(Path.GetDirectoryName(path)!);
            File.WriteAllText(path, document.Text.ToString());
        }

        var targetFramework = project.TargetFramework;
        var projectElement = new XElement("Project",
            new XAttribute("Name", project.Name),
            targetFramework is string tfm ? new XAttribute("TargetFramework", tfm) : null,
            project.AssemblyName is string asm ? new XAttribute("Output", asm) : null,
            project.CompilationOptions is { } opts ? new XAttribute("OutputKind", opts.OutputKind) : null);

        foreach (var projRef in project.ProjectReferences)
        {
            var refProj = project.Solution.GetProject(projRef.ProjectId);
            var refPath = refProj?.FilePath;
            if (string.IsNullOrEmpty(refPath))
                continue;
            var rel = Path.GetRelativePath(dir, refPath);
            projectElement.Add(new XElement("ProjectReference", new XAttribute("Path", rel)));
        }

        var doc = new XDocument(projectElement);
        doc.Save(filePath);
    }

    public static ProjectFileInfo Load(string filePath)
    {
        var xdoc = XDocument.Load(filePath);
        var root = xdoc.Root ?? throw new InvalidDataException("Invalid project file.");
        var name = (string?)root.Attribute("Name") ?? Path.GetFileNameWithoutExtension(filePath);
        var targetFramework = (string?)root.Attribute("TargetFramework");
        var output = (string?)root.Attribute("Output");
        var outputKindAttr = (string?)root.Attribute("OutputKind");
        CompilationOptions? options = null;
        if (outputKindAttr is string ok && Enum.TryParse<OutputKind>(ok, out var kind))
            options = new CompilationOptions(kind);
        else
            options = new CompilationOptions(OutputKind.ConsoleApplication);
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
            paths = RavenFileExtensions.All.SelectMany(ext => Directory.EnumerateFiles(projectDir, $"*{ext}"));
        }

        var documents = paths.Select(p =>
        {
            var docId = DocumentId.CreateNew(projectId);
            var text = SourceText.From(File.ReadAllText(p));
            return DocumentInfo.Create(docId, Path.GetFileName(p), text, p);
        }).ToList();

        var projectRefs = root.Elements("ProjectReference")
            .Select(e => (string?)e.Attribute("Path") ?? throw new InvalidDataException("ProjectReference path missing."))
            .Select(p => Path.IsPathRooted(p) ? p : Path.GetFullPath(Path.Combine(projectDir, p)))
            .ToImmutableArray();

        var attrInfo = new ProjectInfo.ProjectAttributes(projectId, name, VersionStamp.Create());
        var info = new ProjectInfo(attrInfo, documents, filePath: filePath, targetFramework: targetFramework, compilationOptions: options, assemblyName: output);
        return new ProjectFileInfo(info, projectRefs);
    }
}
