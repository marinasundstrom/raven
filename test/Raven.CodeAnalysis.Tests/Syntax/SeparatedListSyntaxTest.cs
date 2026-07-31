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
    public void DefaultValue_IsAnEmptyList()
    {
        var separatedSyntaxList = default(SeparatedSyntaxList<ParameterSyntax>);

        separatedSyntaxList.Count.ShouldBe(0);
        separatedSyntaxList.SeparatorCount.ShouldBe(0);
        separatedSyntaxList.ShouldBeEmpty();
        separatedSyntaxList.GetWithSeparators().ShouldBeEmpty();
    }

    [Fact]
    public void Create_WithOneNode()
    {
        var separatedSyntaxList = SeparatedList<ParameterSyntax>([
            SyntaxFactory.Parameter(List<AttributeListSyntax>(), Identifier("a")),
        ]);

        separatedSyntaxList.Count.ShouldBe(1);
        separatedSyntaxList.GetSeparators().Count().ShouldBe(0);
        separatedSyntaxList.GetWithSeparators().Count().ShouldBe(1);
    }

    [Fact]
    public void Create_WithOneNodeAndOneSeparator()
    {
        var separatedSyntaxList = SeparatedList<ParameterSyntax>([
            SyntaxFactory.Parameter(List<AttributeListSyntax>(), Identifier("a")),
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
            SyntaxFactory.Parameter(List<AttributeListSyntax>(), Identifier("a")),
            CommaToken,
            SyntaxFactory.Parameter(List<AttributeListSyntax>(), Identifier("b"))
        ]);

        separatedSyntaxList.Count.ShouldBe(2);
        separatedSyntaxList.GetSeparators().Count().ShouldBe(1);
        separatedSyntaxList.GetWithSeparators().Count().ShouldBe(3);
    }

    [Fact]
    public void ParsedSeparator_PreservesParentAndSourcePosition()
    {
        const string source = "func Measure(first: int, second: int) {}";
        var tree = SyntaxTree.ParseText(source);
        var parameterList = tree.GetRoot().DescendantNodes().OfType<ParameterListSyntax>().Single();

        var separator = parameterList.Parameters.GetSeparator(0);

        separator.Text.ShouldBe(",");
        separator.SpanStart.ShouldBe(source.IndexOf(',', StringComparison.Ordinal));
        separator.Parent.ShouldBeSameAs(parameterList);
        separator.SyntaxTree.ShouldBeSameAs(tree);
    }
}
