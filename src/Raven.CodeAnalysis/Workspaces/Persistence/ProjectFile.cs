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
    internal sealed record PackageReferenceInfo(string Id, string Version);
    internal sealed record FrameworkReferenceInfo(string Name);

    internal sealed record ProjectFileInfo(
        ProjectInfo Info,
        ImmutableArray<string> ProjectReferences,
        ImmutableArray<string> MetadataReferences,
        ImmutableArray<string> MacroReferences,
        ImmutableArray<PackageReferenceInfo> PackageReferences,
        ImmutableArray<FrameworkReferenceInfo> FrameworkReferences,
        string Configuration);

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
            project.AssemblyName is string asm ? new XAttribute("Output", asm) : null);

        if (project.CompilationOptions is { } opts)
        {
            projectElement.Add(new XAttribute("OutputKind", opts.OutputKind));
            projectElement.Add(new XAttribute("AllowUnsafe", opts.AllowUnsafe));
            projectElement.Add(new XAttribute("AllowGlobalStatements", opts.AllowGlobalStatements));
            if (opts.MembersPublicByDefaultConfigured)
                projectElement.Add(new XAttribute("MembersPublicByDefault", opts.MembersPublicByDefault));
        }

        foreach (var projRef in project.ProjectReferences)
        {
            var refProj = project.Solution.GetProject(projRef.ProjectId);
            var refPath = refProj?.FilePath;
            if (string.IsNullOrEmpty(refPath))
                continue;
            var rel = Path.GetRelativePath(dir, refPath);
            projectElement.Add(new XElement("ProjectReference", new XAttribute("Path", rel)));
        }

        foreach (var macroRef in project.MacroReferences)
        {
            if (string.IsNullOrWhiteSpace(macroRef.Display) || !File.Exists(macroRef.Display))
                continue;

            var rel = Path.GetRelativePath(dir, macroRef.Display);
            projectElement.Add(new XElement("RavenMacro", new XAttribute("Path", rel)));
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
        var enableDefaultRavItemsAttr = (string?)root.Attribute("EnableDefaultRavItems");
        var enableDefaultRavItems = true;
        if (enableDefaultRavItemsAttr is string enabledText && bool.TryParse(enabledText, out var parsedEnableDefaultRavItems))
            enableDefaultRavItems = parsedEnableDefaultRavItems;
        var configuration = (string?)root.Attribute("Configuration");
        configuration = RavenProjectConventions.Default.NormalizeConfiguration(configuration);
        var outputKindAttr = (string?)root.Attribute("OutputKind");
        var allowUnsafeAttr = (string?)root.Attribute("AllowUnsafe");
        var allowGlobalStatementsAttr = (string?)root.Attribute("AllowGlobalStatements");
        var membersPublicByDefaultAttr = (string?)root.Attribute("MembersPublicByDefault");
        CompilationOptions? options = null;
        if (outputKindAttr is string ok && Enum.TryParse<OutputKind>(ok, out var kind))
            options = new CompilationOptions(kind);
        else
            options = new CompilationOptions(OutputKind.ConsoleApplication);

        if (allowUnsafeAttr is string au && bool.TryParse(au, out var allowUnsafe))
            options = options.WithAllowUnsafe(allowUnsafe);

        if (allowGlobalStatementsAttr is string ags && bool.TryParse(ags, out var allowGlobalStatements))
            options = options.WithAllowGlobalStatements(allowGlobalStatements);

        if (membersPublicByDefaultAttr is string mpbd && bool.TryParse(mpbd, out var membersPublicByDefault))
            options = options.WithMembersPublicByDefault(membersPublicByDefault);
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
        else if (enableDefaultRavItems)
        {
            var conventions = RavenProjectConventions.Default;
            paths = RavenFileExtensions.All.SelectMany(ext =>
                    Directory.EnumerateFiles(projectDir, $"*{ext}", SearchOption.AllDirectories))
                .Where(p => conventions.IsImplicitSourceFile(projectDir, p))
                .OrderBy(path => path, StringComparer.OrdinalIgnoreCase);
        }
        else
        {
            paths = Array.Empty<string>();
        }

        var documents = paths.Select(p =>
        {
            var docId = DocumentId.CreateNew(projectId);
            var text = SourceText.From(File.ReadAllText(p));
            return DocumentInfo.Create(docId, Path.GetFileName(p), text, p);
        }).ToList();

        options = EditorConfigDiagnosticOptions.ApplyDiagnosticSeverityOptions(
            options,
            filePath,
            documents.Select(d => d.FilePath));

        var projectRefs = root.Elements("ProjectReference")
            .Select(e => (string?)e.Attribute("Path") ?? throw new InvalidDataException("ProjectReference path missing."))
            .Select(p => Path.IsPathRooted(p) ? p : Path.GetFullPath(Path.Combine(projectDir, p)))
            .ToImmutableArray();

        var metadataRefs = root.Elements("Reference")
            .Select(e => (string?)e.Attribute("Path") ?? (string?)e.Attribute("Include") ?? throw new InvalidDataException("Reference path missing."))
            .Select(p => Path.IsPathRooted(p) ? p : Path.GetFullPath(Path.Combine(projectDir, p)))
            .ToImmutableArray();

        var packageRefs = root.Elements("PackageReference")
            .Select(e =>
            {
                var id = (string?)e.Attribute("Include") ?? (string?)e.Attribute("Id");
                var version = (string?)e.Attribute("Version") ?? (string?)e.Element("Version");
                if (string.IsNullOrWhiteSpace(id))
                    throw new InvalidDataException("PackageReference Include is missing.");
                if (string.IsNullOrWhiteSpace(version))
                    throw new InvalidDataException($"PackageReference '{id}' version is missing.");
                return new PackageReferenceInfo(id, version);
            })
            .ToImmutableArray();

        var frameworkRefs = root.Elements("FrameworkReference")
            .Select(e => (string?)e.Attribute("Include") ?? (string?)e.Attribute("Name"))
            .Where(static value => !string.IsNullOrWhiteSpace(value))
            .Select(static value => new FrameworkReferenceInfo(value!))
            .ToImmutableArray();

        var macroRefs = root.Elements("RavenMacro")
            .Select(e => (string?)e.Attribute("Path") ?? (string?)e.Attribute("Include") ?? throw new InvalidDataException("RavenMacro path missing."))
            .Select(p => Path.IsPathRooted(p) ? p : Path.GetFullPath(Path.Combine(projectDir, p)))
            .ToImmutableArray();

        var attrInfo = new ProjectInfo.ProjectAttributes(projectId, name, VersionStamp.Create());
        var info = new ProjectInfo(attrInfo, documents, filePath: filePath, analyzerReferences: null, targetFramework: targetFramework, compilationOptions: options, assemblyName: output);
        return new ProjectFileInfo(info, projectRefs, metadataRefs, macroRefs, packageRefs, frameworkRefs, configuration);
    }
}
