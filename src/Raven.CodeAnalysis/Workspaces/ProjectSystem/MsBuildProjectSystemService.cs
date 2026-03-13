using System;
using System.IO;
using System.Linq;
using System.Xml.Linq;

using Raven.CodeAnalysis.Macros;

namespace Raven.CodeAnalysis;

public sealed class MsBuildProjectSystemService : IProjectSystemService
{
    private readonly RavenProjectConventions _conventions;

    public MsBuildProjectSystemService()
        : this(RavenProjectConventions.Default)
    {
    }

    public MsBuildProjectSystemService(RavenProjectConventions conventions)
    {
        _conventions = conventions ?? throw new ArgumentNullException(nameof(conventions));
    }

    public bool CanOpenProject(string projectFilePath)
    {
        if (!File.Exists(projectFilePath))
            return false;

        if (!TryReadProjectDocument(projectFilePath, out var document))
            return false;

        var extension = Path.GetExtension(projectFilePath);
        if (string.Equals(extension, ".rvnproj", StringComparison.OrdinalIgnoreCase))
            return string.Equals(document.Root?.Name.LocalName, "Project", StringComparison.OrdinalIgnoreCase);

        return IsRavenMsBuildProject(document);
    }

    public IReadOnlyList<string> GetProjectReferencePaths(string projectFilePath)
    {
        MsBuildLocatorRegistration.EnsureRegistered();
        return MsBuildProjectEvaluator.Evaluate(projectFilePath, _conventions).ProjectReferencePaths;
    }

    public ProjectId OpenProject(Workspace workspace, string projectFilePath)
    {
        if (workspace is not RavenWorkspace raven)
            throw new NotSupportedException("Project persistence requires a RavenWorkspace.");

        MsBuildLocatorRegistration.EnsureRegistered();
        var evaluation = MsBuildProjectEvaluator.Evaluate(projectFilePath, _conventions);
        var projectId = raven.AddProject(
            evaluation.Name,
            projectFilePath,
            evaluation.AssemblyName,
            evaluation.CompilationOptions,
            evaluation.TargetFramework);

        var solution = workspace.CurrentSolution;
        foreach (var document in evaluation.Documents)
        {
            var documentId = DocumentId.CreateNew(projectId);
            solution = solution.AddDocument(documentId, document.Name, document.Text, document.FilePath);
        }

        solution = ProjectSystemGeneratedDocumentHelper.AddGeneratedTargetFrameworkAttributeDocumentIfNeeded(
            solution,
            projectId,
            evaluation.GeneratedSourceDirectory,
            _conventions.GetTargetFrameworkAttributeFileName(evaluation.Name),
            evaluation.TargetFramework);

        var tfm = evaluation.TargetFramework ?? raven.DefaultTargetFramework;
        foreach (var reference in raven.GetFrameworkReferences(tfm))
            solution = solution.AddMetadataReference(projectId, reference);

        foreach (var metadataReferencePath in evaluation.MetadataReferencePaths)
            solution = solution.AddMetadataReference(projectId, MetadataReference.CreateFromFile(metadataReferencePath));

        foreach (var macroReferencePath in evaluation.MacroReferencePaths)
        {
            var resolvedMacroReferencePath = ResolveMacroReferencePath(macroReferencePath, evaluation, raven);
            solution = solution.AddMacroReference(projectId, MacroReference.CreateFromFile(resolvedMacroReferencePath));
        }

        var packageReferences = NuGetPackageResolver.ResolveReferences(
            projectFilePath,
            tfm,
            evaluation.PackageReferences,
            evaluation.FrameworkReferences);

        foreach (var packageReference in packageReferences)
            solution = solution.AddMetadataReference(projectId, packageReference);

        foreach (var referencedProjectPath in evaluation.ProjectReferencePaths)
        {
            var loadedProject = raven.CurrentSolution.Projects.FirstOrDefault(
                project => string.Equals(project.FilePath, referencedProjectPath, StringComparison.OrdinalIgnoreCase));

            if (loadedProject is not null)
            {
                solution = solution.AddProjectReference(projectId, new ProjectReference(loadedProject.Id));
                continue;
            }

            var metadataPath = MsBuildProjectEvaluator.TryResolveReferencedProjectOutputPath(
                referencedProjectPath,
                evaluation.Configuration,
                evaluation.TargetFramework);

            if (!string.IsNullOrWhiteSpace(metadataPath) && File.Exists(metadataPath))
                solution = solution.AddMetadataReference(projectId, MetadataReference.CreateFromFile(metadataPath));
        }

        workspace.TryApplyChanges(solution);
        return projectId;
    }

    public void SaveProject(Project project, string filePath)
    {
        ArgumentNullException.ThrowIfNull(project);
        ArgumentException.ThrowIfNullOrWhiteSpace(filePath);

        var projectDirectory = Path.GetDirectoryName(filePath) ?? Environment.CurrentDirectory;
        Directory.CreateDirectory(projectDirectory);

        foreach (var document in project.Documents.Where(static doc => ShouldPersistDocument(doc)))
        {
            var path = document.FilePath;
            if (string.IsNullOrWhiteSpace(path))
                path = Path.Combine(projectDirectory, RavenFileExtensions.HasRavenExtension(document.Name) ? document.Name : document.Name + RavenFileExtensions.Raven);
            else if (!Path.IsPathRooted(path))
                path = Path.Combine(projectDirectory, path);

            Directory.CreateDirectory(Path.GetDirectoryName(path)!);
            File.WriteAllText(path, document.Text.ToString());
        }

        var projectDocument = File.Exists(filePath)
            ? XDocument.Load(filePath, LoadOptions.PreserveWhitespace)
            : new XDocument(new XElement("Project"));
        var root = projectDocument.Root ?? new XElement("Project");
        if (projectDocument.Root is null)
            projectDocument.Add(root);

        UpdateProperty(root, "AssemblyName", project.AssemblyName);
        UpdateProperty(root, "TargetFramework", project.TargetFramework);
        UpdateProperty(root, "OutputType", MapOutputType(project.CompilationOptions?.OutputKind ?? OutputKind.ConsoleApplication));
        UpdateProperty(root, "AllowUnsafeBlocks", (project.CompilationOptions?.AllowUnsafe ?? false).ToString().ToLowerInvariant());
        UpdateProperty(root, "RavenAllowGlobalStatements", (project.CompilationOptions?.AllowGlobalStatements ?? true).ToString().ToLowerInvariant());

        if (project.CompilationOptions?.MembersPublicByDefaultConfigured == true)
        {
            var membersPublicByDefault = project.CompilationOptions.MembersPublicByDefault.ToString().ToLowerInvariant();
            UpdateProperty(root, "MembersPublicByDefault", membersPublicByDefault);
            RemoveProperty(root, "RavenMembersPublicByDefault");
        }
        else
        {
            RemoveProperty(root, "MembersPublicByDefault");
            RemoveProperty(root, "RavenMembersPublicByDefault");
        }

        RewriteRavenCompileItems(root, project, projectDirectory);
        RewriteManagedProjectReferences(root, project, projectDirectory);
        RewriteMacroReferences(root, project, projectDirectory);

        projectDocument.Save(filePath);
    }

    internal static bool IsRavenMsBuildProject(XDocument document)
    {
        var root = document.Root;
        if (root is null || !string.Equals(root.Name.LocalName, "Project", StringComparison.OrdinalIgnoreCase))
            return false;

        var sdk = (string?)root.Attribute("Sdk");
        if (!string.IsNullOrWhiteSpace(sdk) &&
            sdk.Contains("Raven", StringComparison.OrdinalIgnoreCase))
        {
            return true;
        }

        return root.Descendants().Any(static element =>
            string.Equals(element.Name.LocalName, "RavenCompile", StringComparison.OrdinalIgnoreCase));
    }

    private static bool TryReadProjectDocument(string projectFilePath, out XDocument document)
    {
        try
        {
            document = XDocument.Load(projectFilePath, LoadOptions.PreserveWhitespace);
            return true;
        }
        catch
        {
            document = null!;
            return false;
        }
    }

    private static bool ShouldPersistDocument(Document document)
    {
        if (!RavenFileExtensions.HasRavenExtension(document.Name) &&
            (document.FilePath is null || !RavenFileExtensions.HasRavenExtension(document.FilePath)))
        {
            return false;
        }

        if (string.IsNullOrWhiteSpace(document.FilePath))
            return true;

        var filePath = document.FilePath!;
        var matchesGeneratedTargetFrameworkDocument = RavenFileExtensions.All.Any(
            ext => filePath.EndsWith($".TargetFrameworkAttribute.g{ext}", StringComparison.OrdinalIgnoreCase));
        if (!matchesGeneratedTargetFrameworkDocument)
            return true;

        var normalizedPath = filePath.Replace('\\', '/');
        var segments = normalizedPath.Split('/', StringSplitOptions.RemoveEmptyEntries);
        return !segments.Any(segment => segment.Equals("obj", StringComparison.OrdinalIgnoreCase));
    }

    private string ResolveMacroReferencePath(
        string macroReferencePath,
        MsBuildProjectEvaluationResult requestingProject,
        RavenWorkspace workspace)
    {
        var extension = Path.GetExtension(macroReferencePath);
        if (!IsProjectFileExtension(extension))
            return macroReferencePath;

        if (string.Equals(extension, ".rvnproj", StringComparison.OrdinalIgnoreCase))
            return BuildRavenMacroProject(macroReferencePath, requestingProject, workspace);

        var metadataPath = MsBuildProjectEvaluator.TryResolveReferencedProjectOutputPath(
            macroReferencePath,
            requestingProject.Configuration,
            requestingProject.TargetFramework);

        if (!string.IsNullOrWhiteSpace(metadataPath) && File.Exists(metadataPath))
            return metadataPath;

        throw new FileNotFoundException($"Could not resolve macro assembly output for project '{macroReferencePath}'.", macroReferencePath);
    }

    private string BuildRavenMacroProject(
        string projectFilePath,
        MsBuildProjectEvaluationResult requestingProject,
        RavenWorkspace workspace)
    {
        var macroEvaluation = MsBuildProjectEvaluator.Evaluate(projectFilePath, _conventions, requestingProject.TargetFramework);
        var effectiveTargetFramework = macroEvaluation.TargetFramework ?? requestingProject.TargetFramework ?? workspace.DefaultTargetFramework;
        var outputPath = GetRavenMacroOutputPath(projectFilePath, macroEvaluation.Configuration, effectiveTargetFramework, macroEvaluation.AssemblyName);
        var rebuildInputs = GetRavenMacroRebuildInputs(macroEvaluation).ToArray();

        if (NeedsRebuild(projectFilePath, outputPath, rebuildInputs))
        {
            Directory.CreateDirectory(Path.GetDirectoryName(outputPath)!);

            var macroWorkspace = RavenWorkspace.Create(
                targetFramework: requestingProject.TargetFramework ?? workspace.DefaultTargetFramework,
                projectSystemService: new CompositeProjectSystemService(
                    new RavenProjectSystemService(),
                    new MsBuildProjectSystemService(_conventions)));

            var macroProjectId = macroWorkspace.OpenProject(projectFilePath);
            var macroCompilation = macroWorkspace.GetCompilation(macroProjectId);

            using var peStream = File.Create(outputPath);
            using var pdbStream = File.Create(Path.ChangeExtension(outputPath, ".pdb"));
            var emitResult = macroCompilation.Emit(peStream, pdbStream);
            if (!emitResult.Success)
            {
                var diagnosticText = string.Join(Environment.NewLine, emitResult.Diagnostics.Select(static diagnostic => diagnostic.ToString()));
                throw new InvalidOperationException($"Failed to build macro project '{projectFilePath}'.{Environment.NewLine}{diagnosticText}");
            }
        }

        return outputPath;
    }

    internal static string GetRavenMacroOutputPath(
        string projectFilePath,
        string configuration,
        string? targetFramework,
        string assemblyName)
    {
        var outputDirectory = Path.Combine(
            Path.GetDirectoryName(projectFilePath) ?? Environment.CurrentDirectory,
            "bin",
            configuration);

        if (!string.IsNullOrWhiteSpace(targetFramework))
            outputDirectory = Path.Combine(outputDirectory, targetFramework);

        return Path.Combine(outputDirectory, $"{assemblyName}.dll");
    }

    internal static IEnumerable<string> GetRavenMacroRebuildInputs(MsBuildProjectEvaluationResult evaluation)
    {
        foreach (var document in evaluation.Documents)
        {
            if (!string.IsNullOrWhiteSpace(document.FilePath))
                yield return document.FilePath!;
        }

        foreach (var metadataReferencePath in evaluation.MetadataReferencePaths)
            yield return metadataReferencePath;

        foreach (var projectReferencePath in evaluation.ProjectReferencePaths)
        {
            yield return projectReferencePath;

            var referencedOutput = MsBuildProjectEvaluator.TryResolveReferencedProjectOutputPath(
                projectReferencePath,
                evaluation.Configuration,
                evaluation.TargetFramework);

            if (!string.IsNullOrWhiteSpace(referencedOutput))
                yield return referencedOutput!;
        }

        foreach (var macroReferencePath in evaluation.MacroReferencePaths)
        {
            yield return macroReferencePath;

            var extension = Path.GetExtension(macroReferencePath);
            if (!IsProjectFileExtension(extension))
                continue;

            var referencedOutput = string.Equals(extension, ".rvnproj", StringComparison.OrdinalIgnoreCase)
                ? GetRavenMacroOutputPathForProject(macroReferencePath, evaluation.Configuration, evaluation.TargetFramework)
                : MsBuildProjectEvaluator.TryResolveReferencedProjectOutputPath(
                    macroReferencePath,
                    evaluation.Configuration,
                    evaluation.TargetFramework);

            if (!string.IsNullOrWhiteSpace(referencedOutput))
                yield return referencedOutput!;
        }
    }

    private static string GetRavenMacroOutputPathForProject(
        string projectFilePath,
        string configuration,
        string? targetFramework)
    {
        var macroEvaluation = MsBuildProjectEvaluator.Evaluate(projectFilePath, RavenProjectConventions.Default, targetFramework);
        return GetRavenMacroOutputPath(projectFilePath, configuration, macroEvaluation.TargetFramework ?? targetFramework, macroEvaluation.AssemblyName);
    }

    internal static bool NeedsRebuild(string projectFilePath, string outputPath, IEnumerable<string?> sourcePaths)
    {
        if (!File.Exists(outputPath))
            return true;

        var outputWriteTime = File.GetLastWriteTimeUtc(outputPath);
        if (File.GetLastWriteTimeUtc(projectFilePath) > outputWriteTime)
            return true;

        foreach (var sourcePath in sourcePaths)
        {
            if (string.IsNullOrWhiteSpace(sourcePath) || !File.Exists(sourcePath))
                continue;

            if (File.GetLastWriteTimeUtc(sourcePath) > outputWriteTime)
                return true;
        }

        return false;
    }

    private static bool IsProjectFileExtension(string extension)
        => string.Equals(extension, ".rvnproj", StringComparison.OrdinalIgnoreCase)
            || string.Equals(extension, ".csproj", StringComparison.OrdinalIgnoreCase)
            || string.Equals(extension, ".fsproj", StringComparison.OrdinalIgnoreCase);

    private static void RewriteRavenCompileItems(XElement root, Project project, string projectDirectory)
    {
        var ravenCompileElements = root
            .Descendants()
            .Where(static element => string.Equals(element.Name.LocalName, "RavenCompile", StringComparison.OrdinalIgnoreCase))
            .ToArray();

        foreach (var element in ravenCompileElements)
            element.Remove();

        var documents = project.Documents
            .Where(ShouldPersistDocument)
            .Select(doc => doc.FilePath ?? doc.Name)
            .Where(static path => !string.IsNullOrWhiteSpace(path))
            .Select(path => Path.IsPathRooted(path!)
                ? Path.GetRelativePath(projectDirectory, path!)
                : path!)
            .Distinct(StringComparer.OrdinalIgnoreCase)
            .OrderBy(static path => path, StringComparer.OrdinalIgnoreCase)
            .ToArray();

        if (documents.Length == 0)
            return;

        var itemGroup = new XElement(root.GetDefaultNamespace() + "ItemGroup");
        foreach (var path in documents)
            itemGroup.Add(new XElement(root.GetDefaultNamespace() + "RavenCompile", new XAttribute("Include", path)));

        root.Add(itemGroup);
    }

    private static void RewriteManagedProjectReferences(XElement root, Project project, string projectDirectory)
    {
        var projectSystem = project.Solution.Services.ProjectSystemService;
        var managedReferenceElements = root
            .Descendants()
            .Where(static element => string.Equals(element.Name.LocalName, "ProjectReference", StringComparison.OrdinalIgnoreCase))
            .Where(element =>
            {
                var include = (string?)element.Attribute("Include");
                if (string.IsNullOrWhiteSpace(include))
                    return false;

                var path = Path.IsPathRooted(include)
                    ? include
                    : Path.GetFullPath(Path.Combine(projectDirectory, include));

                return projectSystem?.CanOpenProject(path) == true;
            })
            .ToArray();

        foreach (var element in managedReferenceElements)
            element.Remove();

        var references = project.ProjectReferences
            .Select(reference => project.Solution.GetProject(reference.ProjectId))
            .Where(static referencedProject => referencedProject?.FilePath is not null)
            .Select(referencedProject => Path.GetRelativePath(projectDirectory, referencedProject!.FilePath!))
            .Distinct(StringComparer.OrdinalIgnoreCase)
            .OrderBy(static path => path, StringComparer.OrdinalIgnoreCase)
            .ToArray();

        if (references.Length == 0)
            return;

        var itemGroup = new XElement(root.GetDefaultNamespace() + "ItemGroup");
        foreach (var path in references)
            itemGroup.Add(new XElement(root.GetDefaultNamespace() + "ProjectReference", new XAttribute("Include", path)));

        root.Add(itemGroup);
    }

    private static void RewriteMacroReferences(XElement root, Project project, string projectDirectory)
    {
        var macroReferenceElements = root
            .Descendants()
            .Where(static element => string.Equals(element.Name.LocalName, "RavenMacro", StringComparison.OrdinalIgnoreCase))
            .ToArray();

        foreach (var element in macroReferenceElements)
            element.Remove();

        var references = project.MacroReferences
            .Select(static reference => reference.Display)
            .Where(static display => !string.IsNullOrWhiteSpace(display) && File.Exists(display))
            .Select(path => Path.GetRelativePath(projectDirectory, path))
            .Distinct(StringComparer.OrdinalIgnoreCase)
            .OrderBy(static path => path, StringComparer.OrdinalIgnoreCase)
            .ToArray();

        if (references.Length == 0)
            return;

        var itemGroup = new XElement(root.GetDefaultNamespace() + "ItemGroup");
        foreach (var path in references)
            itemGroup.Add(new XElement(root.GetDefaultNamespace() + "RavenMacro", new XAttribute("Include", path)));

        root.Add(itemGroup);
    }

    private static void UpdateProperty(XElement root, string name, string? value)
    {
        if (string.IsNullOrWhiteSpace(value))
        {
            RemoveProperty(root, name);
            return;
        }

        var property = root
            .Elements()
            .Where(static element => string.Equals(element.Name.LocalName, "PropertyGroup", StringComparison.OrdinalIgnoreCase))
            .Elements()
            .FirstOrDefault(element => string.Equals(element.Name.LocalName, name, StringComparison.OrdinalIgnoreCase));

        if (property is not null)
        {
            property.Value = value;
            return;
        }

        var propertyGroup = root
            .Elements()
            .FirstOrDefault(static element => string.Equals(element.Name.LocalName, "PropertyGroup", StringComparison.OrdinalIgnoreCase));

        propertyGroup ??= AddPropertyGroup(root);
        propertyGroup.Add(new XElement(root.GetDefaultNamespace() + name, value));
    }

    private static void RemoveProperty(XElement root, string name)
    {
        var properties = root
            .Elements()
            .Where(static element => string.Equals(element.Name.LocalName, "PropertyGroup", StringComparison.OrdinalIgnoreCase))
            .Elements()
            .Where(element => string.Equals(element.Name.LocalName, name, StringComparison.OrdinalIgnoreCase))
            .ToArray();

        foreach (var property in properties)
            property.Remove();
    }

    private static XElement AddPropertyGroup(XElement root)
    {
        var propertyGroup = new XElement(root.GetDefaultNamespace() + "PropertyGroup");
        root.AddFirst(propertyGroup);
        return propertyGroup;
    }

    private static string MapOutputType(OutputKind outputKind)
        => outputKind == OutputKind.DynamicallyLinkedLibrary ? "Library" : "Exe";
}
