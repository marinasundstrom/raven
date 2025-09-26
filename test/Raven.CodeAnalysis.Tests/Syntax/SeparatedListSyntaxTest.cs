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
            SyntaxFactory.Parameter(SyntaxTokenList.Empty, IdentifierToken("a"), null, null),
        ]);

        separatedSyntaxList.Count.ShouldBe(1);
        separatedSyntaxList.GetSeparators().Count().ShouldBe(0);
        separatedSyntaxList.GetWithSeparators().Count().ShouldBe(1);
    }

    [Fact]
    public void Create_WithOneNodeAndOneSeparator()
    {
        var separatedSyntaxList = SeparatedList<ParameterSyntax>([
            SyntaxFactory.Parameter(SyntaxTokenList.Empty, IdentifierToken("a"), null, null),
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
            SyntaxFactory.Parameter(SyntaxTokenList.Empty, IdentifierToken("a"), null, null),
            CommaToken,
            SyntaxFactory.Parameter(SyntaxTokenList.Empty, IdentifierToken("b"), null, null)
        ]);

        separatedSyntaxList.Count.ShouldBe(2);
        separatedSyntaxList.GetSeparators().Count().ShouldBe(1);
        separatedSyntaxList.GetWithSeparators().Count().ShouldBe(3);
    }
}
