using System.Threading;
using System.Threading.Tasks;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

/// <summary>
/// Represents a source document within a project. Instances are immutable and
/// any change results in a new <see cref="Document"/> instance.
/// </summary>
public sealed class Document
{
    internal Document(DocumentId id, string name, SourceText text, SyntaxTree? syntaxTree, string? filePath, VersionStamp version)
    {
        Id = id;
        Name = name;
        Text = text;
        SyntaxTree = syntaxTree;
        FilePath = filePath;
        Version = version;
    }

    /// <summary>The identifier for this document.</summary>
    public DocumentId Id { get; }

    /// <summary>The name of the document.</summary>
    public string Name { get; }

    /// <summary>The path to the document on disk if any.</summary>
    public string? FilePath { get; }

    /// <summary>The current version of the text.</summary>
    public VersionStamp Version { get; }

    internal SourceText Text { get; }

    internal SyntaxTree? SyntaxTree { get; }

    /// <summary>Asynchronously gets the text of the document.</summary>
    public Task<SourceText> GetTextAsync(CancellationToken cancellationToken = default)
        => Task.FromResult(Text);

    /// <summary>Asynchronously gets the syntax tree of the document, if any.</summary>
    public Task<SyntaxTree?> GetSyntaxTreeAsync(CancellationToken cancellationToken = default)
        => Task.FromResult(SyntaxTree);

    /// <summary>Creates a new document with updated text and a new version stamp.</summary>
    public Document WithText(SourceText newText)
    {
        if (newText is null) throw new ArgumentNullException(nameof(newText));
        SyntaxTree? newTree = null;
        if (SyntaxTree is not null)
        {
            try
            {
                newTree = SyntaxTree.WithChangedText(newText);
            }
            catch
            {
                newTree = SyntaxTree.ParseText(newText, path: FilePath ?? Name);
            }
        }
        return new Document(Id, Name, newText, newTree, FilePath, Version.GetNewerVersion());
    }
}
