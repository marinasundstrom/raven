using System.Xml.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Text;

namespace Raven.Compiler.Core;

public static class CompilerWorkspaceFactory
{
    public static CompilerWorkspaceSession Create(CompilerWorkspaceOptions workspaceOptions)
    {
        if (workspaceOptions.InputPaths.Count == 0)
            throw new ArgumentException("At least one input path is required.", nameof(workspaceOptions));

        var sourceFiles = workspaceOptions.InputPaths
            .Select(Path.GetFullPath)
            .ToArray();

        foreach (var sourceFile in sourceFiles)
        {
            if (!File.Exists(sourceFile))
                throw new FileNotFoundException($"Input file '{sourceFile}' does not exist.", sourceFile);
        }

        var projectFileInput = sourceFiles.Length == 1 && RavenFileExtensions.HasProjectExtension(sourceFiles[0])
            ? sourceFiles[0]
            : null;
        var projectTargetFramework = projectFileInput is null ? null : TryReadProjectTargetFramework(projectFileInput);
        var targetFramework = workspaceOptions.TargetFramework
            ?? projectTargetFramework
            ?? TargetFrameworkUtil.Resolve(AppContext.TargetFrameworkName);
        var version = TargetFrameworkResolver.ResolveVersion(targetFramework);
        var options = CreateCompilationOptions(workspaceOptions);

        var workspace = RavenWorkspace.Create(
            targetFramework: targetFramework,
            projectSystemService: workspaceOptions.RestoreProjectReferences
                ? null
                : new CompositeProjectSystemService(
                    new RavenProjectSystemService(),
                    new MsBuildProjectSystemService(RavenProjectConventions.Default, resolvePackageReferences: false)));

        ProjectId projectId;
        Project project;
        if (projectFileInput is not null)
        {
            projectId = workspace.OpenProject(projectFileInput);
            project = workspace.CurrentSolution.GetProject(projectId)!;

            if (workspaceOptions.TargetFramework is not null)
            {
                var frameworkReferences = TargetFrameworkResolver.GetReferenceAssemblies(version)
                    .Select(MetadataReference.CreateFromFile)
                    .ToArray();
                foreach (var reference in frameworkReferences)
                    project = project.AddMetadataReference(reference);
            }
        }
        else
        {
            var assemblyName = Path.GetFileNameWithoutExtension(sourceFiles[0]);
            projectId = workspace.AddProject(assemblyName, compilationOptions: options);
            project = workspace.CurrentSolution.GetProject(projectId)!;

            foreach (var filePath in sourceFiles)
            {
                using var file = File.OpenRead(filePath);
                var sourceText = SourceText.From(file);
                var document = project.AddDocument(Path.GetFileName(filePath), sourceText, filePath);
                project = document.Project;
            }

            if (!sourceFiles.Any(static filePath => Path.GetFileName(filePath).EndsWith(".Prelude.g.rvn", StringComparison.OrdinalIgnoreCase)))
            {
                var preludeName = $"{assemblyName}.Prelude.g.rvn";
                var preludeDirectory = Path.GetDirectoryName(sourceFiles[0]) ?? Environment.CurrentDirectory;
                var preludeDocument = project.AddDocument(
                    preludeName,
                    RavenPrelude.CreateDefaultSourceText(),
                    Path.Combine(preludeDirectory, preludeName));
                project = preludeDocument.Project;
            }

            var frameworkReferences = TargetFrameworkResolver.GetReferenceAssemblies(version)
                .Select(MetadataReference.CreateFromFile)
                .ToArray();
            foreach (var reference in frameworkReferences)
                project = project.AddMetadataReference(reference);
        }

        foreach (var referencePath in workspaceOptions.AdditionalReferences)
        {
            var fullPath = Path.GetFullPath(referencePath);
            project = project.AddMetadataReference(MetadataReference.CreateFromFile(fullPath));
        }

        if (project.CompilationOptions is { } projectOptions)
        {
            options = projectOptions
                .WithSpecificDiagnosticOptions(options.SpecificDiagnosticOptions)
                .WithAllowUnsafe(options.AllowUnsafe)
                .WithAllowGlobalStatements(options.AllowGlobalStatements)
                .WithAllowNamespaceMembers(options.AllowNamespaceMembers)
                .WithAllowNamespaceMemberImports(options.AllowNamespaceMemberImports)
                .WithRuntimeAsync(options.UseRuntimeAsync)
                .WithEmbedCoreTypes(options.EmbedCoreTypes)
                .WithEnableSuggestions(options.EnableSuggestions);

            if (workspaceOptions.MembersPublicByDefault is bool membersPublicByDefault)
                options = options.WithMembersPublicByDefault(membersPublicByDefault);
        }

        project = project
            .WithCompilationOptions(options)
            .AddBuiltInAnalyzers(options.EnableSuggestions);

        workspace.TryApplyChanges(project.Solution);
        project = workspace.CurrentSolution.GetProject(projectId)!;
        var compilation = workspace.GetCompilation(projectId);

        return new CompilerWorkspaceSession(workspace, projectId, project, compilation, targetFramework);
    }

    private static CompilationOptions CreateCompilationOptions(CompilerWorkspaceOptions workspaceOptions)
    {
        var options = new CompilationOptions(workspaceOptions.OutputKind)
            .WithAllowUnsafe(workspaceOptions.AllowUnsafe)
            .WithAllowGlobalStatements(workspaceOptions.AllowGlobalStatements)
            .WithAllowNamespaceMembers(workspaceOptions.AllowNamespaceMembers)
            .WithAllowNamespaceMemberImports(workspaceOptions.AllowNamespaceMemberImports)
            .WithRuntimeAsync(workspaceOptions.UseRuntimeAsync)
            .WithEmbedCoreTypes(workspaceOptions.EmbedCoreTypes)
            .WithEnableSuggestions(workspaceOptions.EnableSuggestions);

        if (workspaceOptions.MembersPublicByDefault is bool membersPublicByDefault)
            options = options.WithMembersPublicByDefault(membersPublicByDefault);

        return options;
    }

    private static string? TryReadProjectTargetFramework(string projectFilePath)
    {
        try
        {
            var xdoc = XDocument.Load(projectFilePath);
            return xdoc.Root?
                .Descendants()
                .FirstOrDefault(element => string.Equals(element.Name.LocalName, "TargetFramework", StringComparison.OrdinalIgnoreCase))
                ?.Value;
        }
        catch
        {
            return null;
        }
    }

}
