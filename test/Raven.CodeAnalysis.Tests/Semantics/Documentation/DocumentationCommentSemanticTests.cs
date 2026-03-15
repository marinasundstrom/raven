using System.Linq;

using Raven.CodeAnalysis.Documentation;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class DocumentationCommentSemanticTests : CompilationTestBase
{
    [Fact]
    public void PropertyDocumentationComment_AttachesToPropertySymbol()
    {
        const string source = """
class C {
    /// Property docs
    val Species: string {
        get {
            return "Homo sapiens"
        }
    }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var propertyDeclaration = tree.GetRoot().DescendantNodes().OfType<PropertyDeclarationSyntax>().Single();
        var property = Assert.IsAssignableFrom<IPropertySymbol>(model.GetDeclaredSymbol(propertyDeclaration));

        var comment = property.GetDocumentationComment();

        Assert.NotNull(comment);
        Assert.Equal(DocumentationFormat.Markdown, comment!.Format);
        Assert.Equal("Property docs", comment.Content);
    }

    [Fact]
    public void MarkdownDocumentationComment_ParsesStructuredTags()
    {
        const string source = """
class C {
    /// Parses a widget title.
    ///
    /// Additional details live in the main body.
    ///
    /// @param text Input text to parse.
    /// @returns The parsed title.
    /// @remarks This is culture-invariant.
    func ParseTitle(text: string) -> string => text
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var methodDeclaration = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var method = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(methodDeclaration));

        var comment = method.GetDocumentationComment();

        Assert.NotNull(comment);
        Assert.Equal(DocumentationFormat.Markdown, comment!.Format);
        Assert.Equal("Parses a widget title.\n\nAdditional details live in the main body.", comment.Body);
        Assert.Equal(3, comment.Tags.Length);
        Assert.Equal(DocumentationTagKind.Param, comment.Tags[0].Kind);
        Assert.Equal("text", comment.Tags[0].Argument);
        Assert.Equal("Input text to parse.", comment.Tags[0].Content);
        Assert.Equal(DocumentationTagKind.Returns, comment.Tags[1].Kind);
        Assert.Equal("The parsed title.", comment.Tags[1].Content);
        Assert.Equal(DocumentationTagKind.Remarks, comment.Tags[2].Kind);
        Assert.Equal("This is culture-invariant.", comment.Tags[2].Content);
    }
}
