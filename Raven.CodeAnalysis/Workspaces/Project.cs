using System.Text.Json;

namespace Raven.CodeAnalysis;

public class Project
{
    internal ProjectState State { get; }

    private IEnumerable<Document> _documents;
    private IEnumerable<Document> _additionalDocuments;
    private List<ProjectReference> _projectReferences;

    internal Project(Solution solution, ProjectState state)
    {
        Solution = solution;
        State = state;
    }

    public ProjectId Id => State.Id;

    public string Name => State.Name;

    public Solution Solution { get; }

    public string Language => State.Language;

    public string AssemblyName => State.AssemblyName;

    public IEnumerable<Document> Documents => _documents = State.Documents.Select(d => new Document(this, d));

    public IEnumerable<TextDocument> AdditionalDocuments => _additionalDocuments = State.AdditionalDocuments.Select(d => new Document(this, d));

    public IEnumerable<ProjectReference> ProjectReferences => _projectReferences = State.ProjectReferences.ToList();

    public Project AddAdditionalDocument(string name, string filePath)
    {
        var documentId = DocumentId.CreateNewId(Id);
        var newDocument = new TextDocument
        {
            Id = documentId,
            Name = name,
            FilePath = filePath
        };

        var updatedDocs = AdditionalDocuments.ToList();
        updatedDocs.Add(newDocument);

        return new Project(Id, Name, AssemblyName, Language, Documents, updatedDocs, []);
    }

    public TextDocument GetAdditionalDocument(DocumentId id)
    {
        return AdditionalDocuments.FirstOrDefault(doc => doc.Id == id)
            ?? throw new InvalidOperationException("Additional document not found.");
    }

    public Project RemoveAdditionalDocument(DocumentId id)
    {
        var updatedDocs = AdditionalDocuments.Where(doc => doc.Id != id).ToList();
        return new Project(Id, Name, AssemblyName, Language, Documents, updatedDocs, ProjectReferences);
    }

    public async Task<Compilation> CreateCompilationAsync(CancellationToken cancellationToken = default)
    {
        // Generate a compilation for this project
        var syntaxTrees = await Task.WhenAll(Documents.Select(x => x.GetSyntaxTreeAsync(cancellationToken)));
        return Compilation.Create(Name, syntaxTrees);
    }

    public async Task<IEnumerable<Diagnostic>> GetDiagnosticsAsync(CancellationToken cancellationToken = default)
    {
        var compilation = await CreateCompilationAsync(cancellationToken);
        return compilation.GetDiagnostics();
    }

    public Project WithDocument(DocumentId documentId, DocumentInfo documentInfo)
    {
        var updatedDocuments = Documents.Select(doc =>
            doc.Id == documentId ? new Document(this, documentInfo) : doc
        ).ToList();

        return new Project(Solution, Id, Name, AssemblyName, Language, CompilationOptions, ParseOptions, updatedDocuments);
    }

    public string ToJson()
    {
        return JsonSerializer.Serialize(this);
    }

    public static Project FromJson(string json)
    {
        return JsonSerializer.Deserialize<Project>(json)!;
    }
}
