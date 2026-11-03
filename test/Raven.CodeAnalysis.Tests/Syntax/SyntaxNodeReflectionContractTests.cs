using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "Shared")]
public sealed class SyntaxNodeReflectionContractTests
{
    [Fact]
    public void TokenListProperty_RequiresActualTokenMembership()
    {
        var declaration = SyntaxTree.ParseText("public class C {}").GetRoot()
            .DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        Assert.Equal("Modifiers", declaration.GetPropertyNameForListItem(declaration.Modifiers[0]));
        Assert.Null(declaration.GetPropertyNameForListItem(SyntaxFactory.Identifier("unrelated")));
    }

    [Fact]
    public void TypedSeparatedList_RecognizesSeparatorsAndChildTokens()
    {
        var list = SyntaxTree.ParseText("func M(first: int, second: int) {}").GetRoot()
            .DescendantNodes().OfType<ParameterListSyntax>().Single();
        Assert.Equal("Parameters", list.GetPropertyNameForListItem(list.Parameters.GetSeparators().Single()));
        Assert.Equal("Parameters", list.GetPropertyNameForListItem(list.Parameters[0].Identifier));
        Assert.Null(list.GetPropertyNameForListItem(SyntaxFactory.Identifier("unrelated")));
    }

    [Fact]
    public void TypedNodeList_RecognizesTokensOfItsImmediateItems()
    {
        var root = SyntaxTree.ParseText("class First {}\nclass Second {}").GetRoot();
        var declaration = root.Members.OfType<ClassDeclarationSyntax>().First();
        Assert.Equal("Members", root.GetPropertyNameForListItem(declaration.Identifier));
    }
}
