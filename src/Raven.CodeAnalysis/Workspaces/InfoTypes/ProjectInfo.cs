using System.Collections.Immutable;

using Raven.CodeAnalysis.Macros;

namespace Raven.CodeAnalysis;

/// <summary>Immutable metadata about a project.</summary>
public sealed class ProjectInfo
{
    public ProjectInfo(
        ProjectAttributes attributes,
        IEnumerable<DocumentInfo> documents,
        IEnumerable<ProjectReference>? projectReferences = null,
        IEnumerable<MetadataReference>? metadataReferences = null,
        IEnumerable<MacroReference>? macroReferences = null,
        IEnumerable<AnalyzerReference>? analyzerReferences = null,
        string? filePath = null,
        string? targetFramework = null,
        CompilationOptions? compilationOptions = null,
        string? assemblyName = null,
        ProjectDocumentationOptions? documentationOptions = null,
        IEnumerable<GeneratorReference>? generatorReferences = null,
        ParseOptions? parseOptions = null,
        string? compilerGeneratedFilesOutputPath = null)
    {
        Attributes = attributes;
        Documents = documents.ToImmutableArray();
        ProjectReferences = projectReferences?.ToImmutableArray() ?? ImmutableArray<ProjectReference>.Empty;
        MetadataReferences = metadataReferences?.ToImmutableArray() ?? ImmutableArray<MetadataReference>.Empty;
        MacroReferences = macroReferences?.ToImmutableArray() ?? ImmutableArray<MacroReference>.Empty;
        AnalyzerReferences = analyzerReferences?.ToImmutableArray() ?? ImmutableArray<AnalyzerReference>.Empty;
        GeneratorReferences = generatorReferences?.ToImmutableArray() ?? ImmutableArray<GeneratorReference>.Empty;
        FilePath = filePath;
        TargetFramework = targetFramework;
        CompilationOptions = compilationOptions;
        AssemblyName = assemblyName;
        DocumentationOptions = documentationOptions;
        ParseOptions = parseOptions?.Snapshot();
        CompilerGeneratedFilesOutputPath = compilerGeneratedFilesOutputPath;
    }

    public ProjectAttributes Attributes { get; }
    public ProjectId Id => Attributes.Id;
    public string Name => Attributes.Name;
    public VersionStamp Version => Attributes.Version;

    public ImmutableArray<DocumentInfo> Documents { get; }

    public ImmutableArray<ProjectReference> ProjectReferences { get; }
    public ImmutableArray<MetadataReference> MetadataReferences { get; }
    public ImmutableArray<MacroReference> MacroReferences { get; }
    public ImmutableArray<AnalyzerReference> AnalyzerReferences { get; }
    public ImmutableArray<GeneratorReference> GeneratorReferences { get; }
    public CompilationOptions? CompilationOptions { get; }
    public string? AssemblyName { get; }
    public ProjectDocumentationOptions? DocumentationOptions { get; }
    public ParseOptions? ParseOptions { get; }
    public string? CompilerGeneratedFilesOutputPath { get; }
    public string? DefaultNamespace { get; internal set; }
    public string? FilePath { get; }
    public string? TargetFramework { get; }

    public ProjectInfo WithDocuments(IEnumerable<DocumentInfo> docs) =>
        new(Attributes, docs, ProjectReferences, MetadataReferences, MacroReferences, AnalyzerReferences, FilePath, TargetFramework, CompilationOptions, AssemblyName, DocumentationOptions, GeneratorReferences, ParseOptions, CompilerGeneratedFilesOutputPath);

    public ProjectInfo WithVersion(VersionStamp version)
    {
        var newAttributes = Attributes with
        {
            Version = version
        };
        return new ProjectInfo(newAttributes, Documents, ProjectReferences, MetadataReferences, MacroReferences, AnalyzerReferences, FilePath, TargetFramework, CompilationOptions, AssemblyName, DocumentationOptions, GeneratorReferences, ParseOptions, CompilerGeneratedFilesOutputPath);
    }

    public ProjectInfo WithProjectReferences(IEnumerable<ProjectReference> projectReferences) =>
        new(Attributes, Documents, projectReferences, MetadataReferences, MacroReferences, AnalyzerReferences, FilePath, TargetFramework, CompilationOptions, AssemblyName, DocumentationOptions, GeneratorReferences, ParseOptions, CompilerGeneratedFilesOutputPath);

    public ProjectInfo WithMetadataReferences(IEnumerable<MetadataReference> metadataReferences) =>
        new(Attributes, Documents, ProjectReferences, metadataReferences, MacroReferences, AnalyzerReferences, FilePath, TargetFramework, CompilationOptions, AssemblyName, DocumentationOptions, GeneratorReferences, ParseOptions, CompilerGeneratedFilesOutputPath);

    public ProjectInfo WithMacroReferences(IEnumerable<MacroReference> macroReferences) =>
        new(Attributes, Documents, ProjectReferences, MetadataReferences, macroReferences, AnalyzerReferences, FilePath, TargetFramework, CompilationOptions, AssemblyName, DocumentationOptions, GeneratorReferences, ParseOptions, CompilerGeneratedFilesOutputPath);

    public ProjectInfo WithCompilationOptions(CompilationOptions? compilationOptions) =>
        new(Attributes, Documents, ProjectReferences, MetadataReferences, MacroReferences, AnalyzerReferences, FilePath, TargetFramework, compilationOptions, AssemblyName, DocumentationOptions, GeneratorReferences, ParseOptions, CompilerGeneratedFilesOutputPath);

    public ProjectInfo WithAnalyzerReferences(IEnumerable<AnalyzerReference> analyzerReferences) =>
        new(Attributes, Documents, ProjectReferences, MetadataReferences, MacroReferences, analyzerReferences, FilePath, TargetFramework, CompilationOptions, AssemblyName, DocumentationOptions, GeneratorReferences, ParseOptions, CompilerGeneratedFilesOutputPath);

    public ProjectInfo WithGeneratorReferences(IEnumerable<GeneratorReference> generatorReferences) =>
        new(Attributes, Documents, ProjectReferences, MetadataReferences, MacroReferences, AnalyzerReferences, FilePath, TargetFramework, CompilationOptions, AssemblyName, DocumentationOptions, generatorReferences, ParseOptions, CompilerGeneratedFilesOutputPath);

    public ProjectInfo WithTargetFramework(string? targetFramework) =>
        new(Attributes, Documents, ProjectReferences, MetadataReferences, MacroReferences, AnalyzerReferences, FilePath, targetFramework, CompilationOptions, AssemblyName, DocumentationOptions, GeneratorReferences, ParseOptions, CompilerGeneratedFilesOutputPath);

    public ProjectInfo WithDocumentationOptions(ProjectDocumentationOptions? documentationOptions) =>
        new(Attributes, Documents, ProjectReferences, MetadataReferences, MacroReferences, AnalyzerReferences, FilePath, TargetFramework, CompilationOptions, AssemblyName, documentationOptions, GeneratorReferences, ParseOptions, CompilerGeneratedFilesOutputPath);

    public ProjectInfo WithParseOptions(ParseOptions? parseOptions) =>
        new(
            Attributes,
            Documents.Select(static document => document.WithParseOptionsChanged()),
            ProjectReferences,
            MetadataReferences,
            MacroReferences,
            AnalyzerReferences,
            FilePath,
            TargetFramework,
            CompilationOptions,
            AssemblyName,
            DocumentationOptions,
            GeneratorReferences,
            parseOptions,
            CompilerGeneratedFilesOutputPath);

    public ProjectInfo WithCompilerGeneratedFilesOutputPath(string? path) =>
        new(Attributes, Documents, ProjectReferences, MetadataReferences, MacroReferences, AnalyzerReferences, FilePath, TargetFramework, CompilationOptions, AssemblyName, DocumentationOptions, GeneratorReferences, ParseOptions, path);

    public sealed record ProjectAttributes(ProjectId Id, string Name, VersionStamp Version);
}
