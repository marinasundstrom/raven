using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

/// <summary>
/// Represents a project within a <see cref="Solution"/>. Acts as a facade over
/// immutable <see cref="ProjectInfo"/> data and owns a set of <see cref="Document"/>s.
/// </summary>
public sealed class Project
{
    private readonly ProjectInfo _info;
    private readonly Solution _solution;
    private ImmutableDictionary<DocumentId, Document> _documentCache = ImmutableDictionary<DocumentId, Document>.Empty;
    private readonly ImmutableDictionary<DocumentId, DocumentInfo> _documentInfos;

    internal Project(ProjectInfo info, Solution solution)
    {
        _info = info ?? throw new ArgumentNullException(nameof(info));
        _solution = solution ?? throw new ArgumentNullException(nameof(solution));
        _documentInfos = info.Documents.ToImmutableDictionary(d => d.Id);
    }

    /// <summary>The solution that contains this project.</summary>
    public Solution Solution => _solution;

    /// <summary>The identifier for this project.</summary>
    public ProjectId Id => _info.Id;

    /// <summary>The name of the project.</summary>
    public string Name => _info.Name;

    /// <summary>The current version of the project.</summary>
    public VersionStamp Version => _info.Version;

    /// <summary>The path to the project file on disk.</summary>
    public string? FilePath => _info.FilePath;

    /// <summary>The target framework for this project, if any.</summary>
    public string? TargetFramework => _info.TargetFramework;

    /// <summary>The compilation options for this project.</summary>
    public CompilationOptions? CompilationOptions => _info.CompilationOptions;

    /// <summary>The explicit assembly name for this project, if any.</summary>
    public string? AssemblyName => _info.AssemblyName;

    /// <summary>All documents in the project.</summary>
    public IEnumerable<Document> Documents => _documentInfos.Values.Select(info => GetDocument(info.Id)!);

    /// <summary>Project-to-project references.</summary>
    public IReadOnlyList<ProjectReference> ProjectReferences => _info.ProjectReferences;

    /// <summary>Metadata references for this project.</summary>
    public IReadOnlyList<MetadataReference> MetadataReferences => _info.MetadataReferences;

    /// <summary>An analyzer references for this project.</summary>
    public IReadOnlyList<AnalyzerReference> AnalyzerReferences => _info.AnalyzerReferences;

    /// <summary>Gets a document by its identifier.</summary>
    public Document? GetDocument(DocumentId id)
    {
        if (!_documentInfos.TryGetValue(id, out var info))
            return null;
        if (!_documentCache.TryGetValue(id, out var doc))
        {
            doc = new Document(info, this);
            _documentCache = _documentCache.Add(id, doc);
        }
        return doc;
    }

    /// <summary>
    /// Adds a new document to this project and returns the resulting <see cref="Document"/>.
    /// </summary>
    /// <remarks>
    /// The returned document belongs to a new <see cref="Solution"/> that contains the
    /// added document. To update a <see cref="Workspace"/>, apply the returned document's
    /// solution via <c>workspace.TryApplyChanges(document.Project.Solution)</c>.
    /// </remarks>
    public Document AddDocument(DocumentId id, string name, SourceText text, string? filePath = null)
    {
        if (id.ProjectId != Id)
            throw new ArgumentException("DocumentId must belong to this project", nameof(id));

        var newSolution = _solution.AddDocument(id, name, text, filePath);
        return newSolution.GetDocument(id)!;
    }

    /// <summary>
    /// Adds a new document with an automatically generated identifier.
    /// </summary>
    public Document AddDocument(string name, SourceText text, string? filePath = null)
    {
        var id = DocumentId.CreateNew(Id);
        return AddDocument(id, name, text, filePath);
    }

    /// <summary>
    /// Adds a metadata reference to this project and returns the resulting <see cref="Document"/>
    /// </summary>
    public Project AddMetadataReference(MetadataReference metadataReference)
    {
        return Solution.AddMetadataReference(Id, metadataReference).GetProject(Id);
    }

    /// <summary>Adds an analyzer reference to this project.</summary>
    public Project AddAnalyzerReference(AnalyzerReference analyzerReference)
    {
        return Solution.AddAnalyzerReference(Id, analyzerReference).GetProject(Id);
    }

    /// <summary>
    /// Creates a new project with the specified <see cref="CompilationOptions"/>.
    /// </summary>
    public Project WithCompilationOptions(CompilationOptions? options)
    {
        var newSolution = Solution.WithCompilationOptions(Id, options);
        return newSolution.GetProject(Id)!;
    }

    internal Project AddDocument(DocumentInfo info)
    {
        var newInfos = _documentInfos.Add(info.Id, info);
        var newInfo = _info.WithDocuments(newInfos.Values).WithVersion(_info.Version.GetNewerVersion());
        return new Project(newInfo, _solution);
    }

    internal Project WithDocument(DocumentInfo info)
    {
        var newInfos = _documentInfos.SetItem(info.Id, info);
        var newInfo = _info.WithDocuments(newInfos.Values).WithVersion(_info.Version.GetNewerVersion());
        return new Project(newInfo, _solution);
    }
}
