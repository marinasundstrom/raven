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
}
