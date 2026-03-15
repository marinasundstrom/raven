using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Xml.Linq;

using Microsoft.Build.Evaluation;

using Raven.CodeAnalysis.Text;

using MSBuildProject = Microsoft.Build.Evaluation.Project;

namespace Raven.CodeAnalysis;

internal static class MsBuildProjectEvaluator
{
    public static MsBuildProjectEvaluationResult Evaluate(
        string projectFilePath,
        RavenProjectConventions conventions,
        string? requestedTargetFramework = null)
    {
        var initialEvaluation = LoadProject(projectFilePath, globalProperties: null);
        var configuration = GetNormalizedConfiguration(initialEvaluation, conventions);
        var targetFramework = string.IsNullOrWhiteSpace(requestedTargetFramework)
            ? GetEffectiveTargetFramework(initialEvaluation)
            : requestedTargetFramework;

        var globalProperties = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)
        {
            ["Configuration"] = configuration,
            ["DesignTimeBuild"] = "true",
            ["BuildingInsideVisualStudio"] = "true",
            ["SkipCompilerExecution"] = "true",
            ["ProvideCommandLineArgs"] = "true"
        };

        if (!string.IsNullOrWhiteSpace(targetFramework))
            globalProperties["TargetFramework"] = targetFramework;

        var project = LoadProject(projectFilePath, globalProperties);
        if (string.IsNullOrWhiteSpace(targetFramework))
            targetFramework = GetEffectiveTargetFramework(project);

        var projectDirectory = Path.GetDirectoryName(projectFilePath) ?? Environment.CurrentDirectory;
        var documents = project.GetItems("RavenCompile")
            .Select(item => GetFullPath(projectDirectory, item))
            .Where(static path => File.Exists(path))
            .Distinct(StringComparer.OrdinalIgnoreCase)
            .OrderBy(static path => path, StringComparer.OrdinalIgnoreCase)
            .Select(path =>
            {
                var documentId = DocumentId.CreateNew(ProjectId.CreateNew(SolutionId.CreateNew()));
                return DocumentInfo.Create(
                    documentId,
                    Path.GetFileName(path),
                    SourceText.From(File.ReadAllText(path)),
                    path);
            })
            .ToImmutableArray();

        var metadataReferencePaths = project.GetItems("Reference")
            .Select(item => item.GetMetadataValue("HintPath"))
            .Where(static value => !string.IsNullOrWhiteSpace(value))
            .Select(path => Path.IsPathRooted(path!)
                ? path!
                : Path.GetFullPath(Path.Combine(projectDirectory, path!)))
            .Where(File.Exists)
            .Distinct(StringComparer.OrdinalIgnoreCase)
            .ToImmutableArray();

        var projectReferencePaths = project.GetItems("ProjectReference")
            .Select(item => GetFullPath(projectDirectory, item))
            .Where(File.Exists)
            .Distinct(StringComparer.OrdinalIgnoreCase)
            .ToImmutableArray();

        var macroReferencePaths = project.GetItems("RavenMacro")
            .Select(item => GetFullPath(projectDirectory, item))
            .Distinct(StringComparer.OrdinalIgnoreCase)
            .ToImmutableArray();

        var packageReferences = project.GetItems("PackageReference")
            .Select(item => new ProjectFile.PackageReferenceInfo(
                item.EvaluatedInclude,
                item.GetMetadataValue("Version")))
            .Where(static item =>
                !string.IsNullOrWhiteSpace(item.Id) &&
                !string.IsNullOrWhiteSpace(item.Version))
            .ToImmutableArray();

        var frameworkReferences = project.GetItems("FrameworkReference")
            .Select(item => new ProjectFile.FrameworkReferenceInfo(item.EvaluatedInclude))
            .Where(static item => !string.IsNullOrWhiteSpace(item.Name))
            .ToImmutableArray();

        var outputType = project.GetPropertyValue("OutputType");
        var allowUnsafe = GetBooleanProperty(project, "AllowUnsafe") ?? GetBooleanProperty(project, "AllowUnsafeBlocks") ?? false;
        var allowGlobalStatements = GetBooleanProperty(project, "AllowGlobalStatements")
            ?? GetBooleanProperty(project, "RavenAllowGlobalStatements")
            ?? true;
        var membersPublicByDefault = GetBooleanProperty(project, "MembersPublicByDefault")
            ?? GetBooleanProperty(project, "RavenMembersPublicByDefault");

        var compilationOptions = new CompilationOptions(ParseOutputKind(outputType))
            .WithAllowUnsafe(allowUnsafe)
            .WithAllowGlobalStatements(allowGlobalStatements);

        if (membersPublicByDefault is bool configuredMembersPublicByDefault)
            compilationOptions = compilationOptions.WithMembersPublicByDefault(configuredMembersPublicByDefault);

        var intermediateOutputPath = project.GetPropertyValue("IntermediateOutputPath");
        var generatedSourceDirectory = GetGeneratedSourceDirectory(projectDirectory, intermediateOutputPath, configuration, conventions);
        var name = GetProjectName(project, projectFilePath);
        var assemblyName = GetPropertyOrDefault(project, "AssemblyName", Path.GetFileNameWithoutExtension(projectFilePath));
        var documentationOptions = new ProjectDocumentationOptions(
            GenerateXmlDocumentation: GetBooleanProperty(project, "GenerateDocumentationFile") ?? false,
            GenerateMarkdownDocumentation: GetBooleanProperty(project, "GenerateMarkdownDocumentationFile") ?? false,
            XmlDocumentationFile: GetOptionalProperty(project, "DocumentationFile"),
            MarkdownDocumentationOutputPath: GetOptionalProperty(project, "MarkdownDocumentationOutputPath"));

        return new MsBuildProjectEvaluationResult(
            name,
            assemblyName,
            targetFramework,
            configuration,
            compilationOptions,
            documents,
            metadataReferencePaths,
            projectReferencePaths,
            macroReferencePaths,
            packageReferences,
            frameworkReferences,
            generatedSourceDirectory,
            documentationOptions);
    }

    public static string? TryResolveReferencedProjectOutputPath(
        string projectFilePath,
        string configuration,
        string? requestedTargetFramework)
    {
        if (!File.Exists(projectFilePath))
            return null;

        if (!TryReadProjectDocument(projectFilePath, out var document))
            return null;

        if (MsBuildProjectSystemService.IsRavenMsBuildProject(document))
            return null;

        var globalProperties = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)
        {
            ["Configuration"] = configuration,
            ["DesignTimeBuild"] = "true",
            ["BuildingInsideVisualStudio"] = "true",
            ["SkipCompilerExecution"] = "true"
        };

        if (!string.IsNullOrWhiteSpace(requestedTargetFramework))
            globalProperties["TargetFramework"] = requestedTargetFramework!;

        var project = LoadProject(projectFilePath, globalProperties);
        var targetPath = project.GetPropertyValue("TargetPath");
        return string.IsNullOrWhiteSpace(targetPath) ? null : Path.GetFullPath(targetPath);
    }

    private static MSBuildProject LoadProject(string projectFilePath, IDictionary<string, string>? globalProperties)
    {
        var projectCollection = globalProperties is null
            ? new ProjectCollection()
            : new ProjectCollection(globalProperties);

        return new MSBuildProject(projectFilePath, globalProperties, toolsVersion: null, projectCollection);
    }

    private static string GetProjectName(MSBuildProject project, string projectFilePath)
    {
        return GetPropertyOrDefault(
            project,
            "RootNamespace",
            Path.GetFileNameWithoutExtension(projectFilePath));
    }

    private static string GetGeneratedSourceDirectory(
        string projectDirectory,
        string? intermediateOutputPath,
        string configuration,
        RavenProjectConventions conventions)
    {
        if (string.IsNullOrWhiteSpace(intermediateOutputPath))
            return conventions.GetGeneratedSourceDirectory(projectDirectory, configuration);

        var fullIntermediateOutputPath = Path.IsPathRooted(intermediateOutputPath)
            ? intermediateOutputPath
            : Path.GetFullPath(Path.Combine(projectDirectory, intermediateOutputPath));

        return Path.Combine(fullIntermediateOutputPath, "raven", "generated");
    }

    private static string GetEffectiveTargetFramework(MSBuildProject project)
    {
        var targetFramework = project.GetPropertyValue("TargetFramework");
        if (!string.IsNullOrWhiteSpace(targetFramework))
            return targetFramework;

        var targetFrameworks = project.GetPropertyValue("TargetFrameworks");
        if (string.IsNullOrWhiteSpace(targetFrameworks))
            return string.Empty;

        return targetFrameworks
            .Split(';', StringSplitOptions.RemoveEmptyEntries | StringSplitOptions.TrimEntries)
            .FirstOrDefault() ?? string.Empty;
    }

    private static string GetNormalizedConfiguration(MSBuildProject project, RavenProjectConventions conventions)
        => conventions.NormalizeConfiguration(GetPropertyOrDefault(project, "Configuration", conventions.DefaultConfiguration));

    private static string GetFullPath(string projectDirectory, ProjectItem item)
    {
        var fullPath = item.GetMetadataValue("FullPath");
        if (!string.IsNullOrWhiteSpace(fullPath))
            return Path.GetFullPath(fullPath);

        var evaluatedInclude = item.EvaluatedInclude;
        return Path.IsPathRooted(evaluatedInclude)
            ? evaluatedInclude
            : Path.GetFullPath(Path.Combine(projectDirectory, evaluatedInclude));
    }

    private static bool? GetBooleanProperty(MSBuildProject project, string propertyName)
    {
        var value = project.GetPropertyValue(propertyName);
        return bool.TryParse(value, out var parsed) ? parsed : null;
    }

    private static string GetPropertyOrDefault(MSBuildProject project, string propertyName, string defaultValue)
    {
        var value = project.GetPropertyValue(propertyName);
        return string.IsNullOrWhiteSpace(value) ? defaultValue : value;
    }

    private static string? GetOptionalProperty(MSBuildProject project, string propertyName)
    {
        var value = project.GetPropertyValue(propertyName);
        return string.IsNullOrWhiteSpace(value) ? null : value;
    }

    private static OutputKind ParseOutputKind(string outputType)
    {
        if (string.Equals(outputType, "Library", StringComparison.OrdinalIgnoreCase))
            return OutputKind.DynamicallyLinkedLibrary;

        return OutputKind.ConsoleApplication;
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
}

internal readonly record struct MsBuildProjectEvaluationResult(
    string Name,
    string AssemblyName,
    string? TargetFramework,
    string Configuration,
    CompilationOptions CompilationOptions,
    ImmutableArray<DocumentInfo> Documents,
    ImmutableArray<string> MetadataReferencePaths,
    ImmutableArray<string> ProjectReferencePaths,
    ImmutableArray<string> MacroReferencePaths,
    ImmutableArray<ProjectFile.PackageReferenceInfo> PackageReferences,
    ImmutableArray<ProjectFile.FrameworkReferenceInfo> FrameworkReferences,
    string GeneratedSourceDirectory,
    ProjectDocumentationOptions DocumentationOptions);
