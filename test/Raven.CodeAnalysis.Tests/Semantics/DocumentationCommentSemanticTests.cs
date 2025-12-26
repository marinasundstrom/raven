using System.Linq;

using Raven.CodeAnalysis.Documentation;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class DocumentationCommentSemanticTests : CompilationTestBase
{
    [Fact]
    public void FieldDocumentationComment_AttachesToFieldSymbol()
    {
        const string source = """
class C {
    /// Field docs
    public val species = "Homo sapiens"
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();
        var field = Assert.IsAssignableFrom<IFieldSymbol>(model.GetDeclaredSymbol(declarator));

        var comment = field.GetDocumentationComment();

        Assert.NotNull(comment);
        Assert.Equal(DocumentationFormat.Markdown, comment!.Format);
        Assert.Equal("Field docs", comment.Content);
    }
}
