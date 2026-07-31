namespace Raven.CodeAnalysis.Syntax.Tests;

public sealed class SyntaxListContractTests
{
    [Fact]
    public void DefaultValue_IsAnEmptyMutableValue()
    {
        var list = default(SyntaxList<BlockSyntax>);
        var block = SyntaxFactory.Block();

        Assert.Empty(list);
        Assert.Equal(0, list.Count);
        Assert.Equal(default, list.Span);
        Assert.Equal(default, list.FullSpan);

        var withBlock = list.Add(block);
        Assert.Single(withBlock);
        Assert.Null(withBlock[0].Parent);
        Assert.Equal(block, withBlock[0]);

        var emptyAgain = withBlock.Remove(block);
        Assert.Empty(emptyAgain);
    }

    [Fact]
    public void ChildList_ReusesProjectedChildrenWithCorrectOwnership()
    {
        var tree = SyntaxTree.ParseText("func Main() {}");
        var root = tree.GetRoot();
        var childList = root.ChildNodesAndTokens();

        var firstEnumeration = childList.ToArray();
        var secondEnumeration = childList.ToArray();

        Assert.NotEmpty(firstEnumeration);
        Assert.Equal(firstEnumeration.Length, secondEnumeration.Length);
        Assert.All(firstEnumeration, child => Assert.Same(root, child.Parent));
        Assert.All(
            firstEnumeration.Zip(secondEnumeration),
            pair => Assert.Same(pair.First, pair.Second));
    }

    [Fact]
    public void ReflectedNodeProperties_CacheMaterializedGroupsAndSlots()
    {
        var root = SyntaxTree.ParseText("func Main() {}").GetRoot();
        var properties = root.GetChildrenGroupedByProperty();

        var firstGroups = properties.Properties;
        var secondGroups = properties.Properties;

        Assert.Same(firstGroups, secondGroups);
        Assert.True(properties.SlotCount > 0);
        Assert.Same(properties[0], properties[0]);
    }
}
