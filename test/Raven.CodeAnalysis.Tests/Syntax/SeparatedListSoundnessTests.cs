namespace Raven.CodeAnalysis.Syntax.Tests;

[Trait("Target", "Shared")]
public sealed class SeparatedListSoundnessTests
{
    [Theory]
    [InlineData(0, false)]
    [InlineData(1, false)]
    [InlineData(2, false)]
    [InlineData(3, false)]
    [InlineData(1, true)]
    [InlineData(2, true)]
    public void AlternatingNodesAndSeparatorsAreSound(int count, bool trailingSeparator)
    {
        var items = new List<SyntaxNodeOrToken>();
        for (var i = 0; i < count; i++)
        {
            items.Add(SyntaxFactory.IdentifierName("item" + i));
            if (i + 1 < count || trailingSeparator)
                items.Add(SyntaxFactory.CommaToken);
        }
        var list = new SeparatedSyntaxList<IdentifierNameSyntax>(items.ToArray());
        Assert.True(list.IsSoundSeparatedList(SyntaxKind.CommaToken));
        Assert.True(default(SeparatedSyntaxList<IdentifierNameSyntax>).IsSoundSeparatedList(SyntaxKind.CommaToken));
    }

    [Theory]
    [InlineData(1)]
    [InlineData(2)]
    public void TrailingSeparatorCanBeRetrievedByIndex(int count)
    {
        var items = new List<SyntaxNodeOrToken>();
        for (var i = 0; i < count; i++)
        {
            items.Add(SyntaxFactory.IdentifierName("item" + i));
            items.Add(SyntaxFactory.CommaToken);
        }
        var list = new SeparatedSyntaxList<IdentifierNameSyntax>(items.ToArray());
        Assert.Equal(count, list.SeparatorCount);
        Assert.Equal(SyntaxKind.CommaToken, list.GetSeparator(count - 1).Kind);
        Assert.Throws<IndexOutOfRangeException>(() => list.GetSeparator(-1));
        Assert.Throws<IndexOutOfRangeException>(() => list.GetSeparator(count));
        Assert.Throws<IndexOutOfRangeException>(() => default(SeparatedSyntaxList<IdentifierNameSyntax>).GetSeparator(0));
    }

    [Fact]
    public void RejectsMalformedSlotsIncludingTheEndOfTheList()
    {
        var name = SyntaxFactory.IdentifierName("item");
        var comma = SyntaxFactory.CommaToken;
        SyntaxNodeOrToken[][] malformed =
        [
            [comma],
            [name, name],
            [name, comma, comma],
            [name, comma, name, SyntaxFactory.SemicolonToken],
            [name, comma, SyntaxFactory.Block()]
        ];
        foreach (var items in malformed)
            Assert.False(new SeparatedSyntaxList<IdentifierNameSyntax>(items).IsSoundSeparatedList(SyntaxKind.CommaToken));
    }
}
