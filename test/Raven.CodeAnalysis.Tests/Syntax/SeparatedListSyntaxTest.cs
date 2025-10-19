namespace Raven.CodeAnalysis.Syntax.Tests;

using static Raven.CodeAnalysis.Syntax.SyntaxFactory;

public class SeparatedListSyntaxTest(ITestOutputHelper testOutputHelper)
{
    [Fact]
    public void Create_Empty()
    {
        var separatedSyntaxList = SeparatedList<ParameterSyntax>();

        separatedSyntaxList.Count.ShouldBe(0);
    }

    [Fact]
    public void Create_WithOneNode()
    {
        var separatedSyntaxList = SeparatedList<ParameterSyntax>([
            SyntaxFactory.Parameter(EmptyList<AttributeListSyntax>(), IdentifierToken("a")),
        ]);

        separatedSyntaxList.Count.ShouldBe(1);
        separatedSyntaxList.GetSeparators().Count().ShouldBe(0);
        separatedSyntaxList.GetWithSeparators().Count().ShouldBe(1);
    }

    [Fact]
    public void Create_WithOneNodeAndOneSeparator()
    {
        var separatedSyntaxList = SeparatedList<ParameterSyntax>([
            SyntaxFactory.Parameter(EmptyList<AttributeListSyntax>(), IdentifierToken("a")),
            CommaToken
        ]);

        separatedSyntaxList.Count.ShouldBe(1);
        separatedSyntaxList.GetSeparators().Count().ShouldBe(1);
        separatedSyntaxList.GetWithSeparators().Count().ShouldBe(2);
    }

    [Fact]
    public void Create_WithTwoNodesAndOneSeparator()
    {
        var separatedSyntaxList = SeparatedList<ParameterSyntax>([
            SyntaxFactory.Parameter(EmptyList<AttributeListSyntax>(), IdentifierToken("a")),
            CommaToken,
            SyntaxFactory.Parameter(EmptyList<AttributeListSyntax>(), IdentifierToken("b"))
        ]);

        separatedSyntaxList.Count.ShouldBe(2);
        separatedSyntaxList.GetSeparators().Count().ShouldBe(1);
        separatedSyntaxList.GetWithSeparators().Count().ShouldBe(3);
    }
}
