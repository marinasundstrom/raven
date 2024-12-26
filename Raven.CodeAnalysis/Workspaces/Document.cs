using System.Text.Json;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

public class Document : TextDocument
{
    public Document(Project project, DocumentInfo documentInfo)
    {
        Project = project;
        _documentInfo = documentInfo;
    }

    public SourceCodeKind SourceCodeKind { get; set; }

    public bool SupportsSemanticModel { get; set; }

    public bool SupportsSyntaxTree { get; set; }
    public Project Project { get; }

    public async Task<SemanticModel> GetSemanticModelAsync(CancellationToken cancellationToken = default)
    {
        var compilation = await Project.CreateCompilationAsync(cancellationToken);
        var syntaxTree = await GetSyntaxTreeAsync(cancellationToken);
        return compilation.GetSemanticModel(syntaxTree);
    }

    private SyntaxTree? _cachedSyntaxTree;
    private readonly DocumentInfo _documentInfo;

    public async Task<SyntaxTree> GetSyntaxTreeAsync(CancellationToken cancellationToken = default)
    {
        if (_cachedSyntaxTree == null)
        {
            _cachedSyntaxTree = SyntaxFactory.ParseSyntaxTree(_sourceText, filePath: this.FilePath);
        }
        else if (_cachedSyntaxTree.GetText() != _sourceText)
        {
            _cachedSyntaxTree = _cachedSyntaxTree.WithChangedText(_sourceText);
        }

        return _cachedSyntaxTree;
    }

    public async Task<SyntaxNode?> GetSyntaxRootAsync(CancellationToken cancellationToken = default)
    {
        var syntaxTree = await GetSyntaxTreeAsync(cancellationToken);
        return syntaxTree.GetRoot();
    }

    public Task<VersionStamp> GetSyntaxVersionAsync(CancellationToken cancellationToken = default)
    {
        // Use timestamp from file or source text for versioning
        return Task.FromResult(VersionStamp.Create());
    }

    public Document WithFilePath(string filePath)
    {
        return new Document(Id, Name, filePath, Folders, SourceCodeKind, SupportsSemanticModel, SupportsSyntaxTree,
            _cachedSyntaxTree, _sourceText);
    }

    public Document WithFolders(IEnumerable<string> folderPaths)
    {
        return new Document(Id, Name, FilePath, folderPaths, SourceCodeKind, SupportsSemanticModel, SupportsSyntaxTree,
            _cachedSyntaxTree, _sourceText);
    }

    public Document WithName(string name)
    {
        return new Document(Id, name, FilePath, Folders, SourceCodeKind, SupportsSemanticModel, SupportsSyntaxTree,
            _cachedSyntaxTree, _sourceText);
    }

    public Document WithSourceCodeKind(SourceCodeKind kind)
    {
        return new Document(Id, Name, FilePath, Folders, kind, SupportsSemanticModel, SupportsSyntaxTree,
            _cachedSyntaxTree, _sourceText);
    }

    /*
    public Document WithSyntaxRoot(SyntaxNode root)
    {
        return new Document(Id, Name, FilePath, Folders, SourceCodeKind, SupportsSemanticModel, SupportsSyntaxTree,
            _cachedSyntaxTree, _sourceText);
        
        return new Document
        {
            Id = this.Id,
            Name = this.Name,
            FilePath = this.FilePath,
            Folders = this.Folders,
            SourceCodeKind = this.SourceCodeKind,
            SupportsSemanticModel = this.SupportsSemanticModel,
            SupportsSyntaxTree = this.SupportsSyntaxTree,
            _cachedSyntaxTree = this._cachedSyntaxTree,
            _sourceText = this._sourceText,
        };
    }
    */

    public Document WithText(SourceText sourceText)
    {
        // Update the current DocumentInfo with the new text
        var updatedDocumentInfo = _documentInfo.With(
            Id,
            Name,
            Folders.ToList(),
            SourceCodeKind,
            FilePath,
            isGenerated: false,
            designTimeOnly: false
        );

        // Replace the current Document in the parent Project
        var updatedProject = Project.WithDocument(this.Id, updatedDocumentInfo);

        // Replace the updated Project in the parent Solution
        var updatedSolution = Project.Solution.WithProject(updatedProject);

        // Create and return the new Document
        return new Document(updatedProject, updatedDocumentInfo)
        {
            _cachedSyntaxTree = _cachedSyntaxTree?.WithChangedText(sourceText),
            _sourceText = sourceText,
            SourceCodeKind = SourceCodeKind,
            SupportsSemanticModel = SupportsSemanticModel,
            SupportsSyntaxTree = SupportsSyntaxTree
        };
    }

    public string ToJson()
    {
        return JsonSerializer.Serialize(this);
    }

    public static Document FromJson(string json)
    {
        return JsonSerializer.Deserialize<Document>(json)!;
    }

    public bool SourceTextEquals(Document other)
    {
        return _sourceText?.ContentEquals(other._sourceText) ?? false;
    }

    public IEnumerable<TextChange> GetTextChangesAsync(Document oldDocument, CancellationToken cancellationToken = default)
    {
        return _sourceText.GetTextChanges(oldDocument._sourceText);
    }
}
