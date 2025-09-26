using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class EnumDeclarationParserTests
{
    [Fact]
    public void EnumDeclaration_WithAttributeList_ParsesAttributes()
    {
        var source = "[Flags] enum Colors { Red }";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<EnumDeclarationSyntax>(Assert.Single(root.Members));

        var attributeList = Assert.Single(declaration.AttributeLists);
        var attribute = Assert.Single(attributeList.Attributes);

        var name = Assert.IsType<IdentifierNameSyntax>(attribute.Name);
        Assert.Equal("Flags", name.Identifier.Text);
        Assert.Empty(tree.GetDiagnostics());
    }
}
