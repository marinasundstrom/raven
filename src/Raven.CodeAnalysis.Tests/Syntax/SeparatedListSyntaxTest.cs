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
            new ParameterSyntax(IdentifierName("a"), null),
        ]);

        separatedSyntaxList.Count.ShouldBe(1);
        separatedSyntaxList.GetSeparators().Count().ShouldBe(0);
        separatedSyntaxList.GetWithSeparators().Count().ShouldBe(1);
    }

    [Fact]
    public void Create_WithOneNodeAndOneSeparator()
    {
        var separatedSyntaxList = SeparatedList<ParameterSyntax>([
            new ParameterSyntax(IdentifierName("a"), null),
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
            new ParameterSyntax(IdentifierName("a"), null),
            CommaToken,
            new ParameterSyntax(IdentifierName("b"), null)
        ]);

        separatedSyntaxList.Count.ShouldBe(2);
        separatedSyntaxList.GetSeparators().Count().ShouldBe(1);
        separatedSyntaxList.GetWithSeparators().Count().ShouldBe(3);
    }
}