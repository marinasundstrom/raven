using System;
using System.Threading;
using System.Threading.Tasks;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

/// <summary>
/// Represents a source document within a project. Acts as a facade over immutable
/// <see cref="DocumentInfo"/> data and is owned by a <see cref="Project"/>.
/// </summary>
public sealed class Document
{
    private readonly DocumentInfo _info;
    private readonly Project _project;
    private SyntaxTree? _syntaxTree;

    internal Document(DocumentInfo info, Project project)
    {
        _info = info ?? throw new ArgumentNullException(nameof(info));
        _project = project ?? throw new ArgumentNullException(nameof(project));
    }

    /// <summary>The project that contains this document.</summary>
    public Project Project => _project;

    /// <summary>The solution that owns this document.</summary>
    public Solution Solution => _project.Solution;

    /// <summary>The identifier for this document.</summary>
    public DocumentId Id => _info.Id;

    /// <summary>The name of the document.</summary>
    public string Name => _info.Name;

    /// <summary>The path to the document on disk if any.</summary>
    public string? FilePath => _info.FilePath;

    /// <summary>The current version of the text.</summary>
    public VersionStamp Version => _info.Version;

    internal SourceText Text => _info.Text;

    internal DocumentInfo Info => _info;

    internal SyntaxTree? SyntaxTree
    {
        get
        {
            if (_syntaxTree is null)
                _syntaxTree = Solution.Services.SyntaxTreeProvider.TryParse(Name, Text, FilePath);
            return _syntaxTree;
        }
    }

    /// <summary>Asynchronously gets the text of the document.</summary>
    public Task<SourceText> GetTextAsync(CancellationToken cancellationToken = default)
        => Task.FromResult(Text);

    /// <summary>Asynchronously gets the syntax tree of the document, if any.</summary>
    public Task<SyntaxTree?> GetSyntaxTreeAsync(CancellationToken cancellationToken = default)
        => Task.FromResult(SyntaxTree);

    /// <summary>Creates a new document with updated text using the owning solution.</summary>
    public Document WithText(SourceText newText)
    {
        if (newText is null) throw new ArgumentNullException(nameof(newText));
        var newSolution = Solution.WithDocumentText(Id, newText);
        return newSolution.GetDocument(Id)!;
    }
}
