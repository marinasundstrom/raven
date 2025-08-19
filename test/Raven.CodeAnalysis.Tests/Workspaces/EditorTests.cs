using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Editing;
using Raven.CodeAnalysis.Text;
using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class EditorTests
{
    [Fact]
    public async Task InsertText_ShouldUpdateDocument()
    {
        var document = CreateDocument("Hello World");
        var editor = Editor.Create(document);
        editor.Insert(5, ",");
        var changed = editor.GetChangedDocument();
        var text = await changed.GetTextAsync();
        Assert.Equal("Hello, World", text.ToString());
    }

    [Fact]
    public async Task ReplaceText_ShouldUpdateDocument()
    {
        var document = CreateDocument("Hello World");
        var editor = Editor.Create(document);
        editor.Replace(new TextSpan(6, 5), "Universe");
        var changed = editor.GetChangedDocument();
        var text = await changed.GetTextAsync();
        Assert.Equal("Hello Universe", text.ToString());
    }

    [Fact]
    public async Task RemoveText_ShouldUpdateDocument()
    {
        var document = CreateDocument("Hello World");
        var editor = Editor.Create(document);
        editor.Remove(new TextSpan(5, 1));
        var changed = editor.GetChangedDocument();
        var text = await changed.GetTextAsync();
        Assert.Equal("HelloWorld", text.ToString());
    }

    private static Document CreateDocument(string text)
    {
        var source = SourceText.From(text);
        var solutionId = SolutionId.CreateNew();
        var projectId = ProjectId.CreateNew(solutionId);
        var documentId = DocumentId.CreateNew(projectId);
        var tree = SyntaxTree.ParseText(source, path: "Test");
        return new Document(documentId, "Test", source, tree, null, VersionStamp.Create());
    }
}

