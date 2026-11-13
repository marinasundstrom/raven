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
        Assert.Equal("Modifiers", SyntaxNodeReflectionExtensions.GetPropertyNameForListItem(declaration, declaration.Modifiers[0]));
        Assert.Null(SyntaxNodeReflectionExtensions.GetPropertyNameForListItem(declaration, SyntaxFactory.Identifier("unrelated")));
    }

    [Fact]
    public void TypedSeparatedList_RecognizesSeparatorsAndChildTokens()
    {
        var list = SyntaxTree.ParseText("func M(first: int, second: int) {}").GetRoot()
            .DescendantNodes().OfType<ParameterListSyntax>().Single();
        Assert.Equal("Parameters", SyntaxNodeReflectionExtensions.GetPropertyNameForListItem(list, list.Parameters.GetSeparators().Single()));
        Assert.Equal("Parameters", SyntaxNodeReflectionExtensions.GetPropertyNameForListItem(list, list.Parameters[0].Identifier));
        Assert.Null(SyntaxNodeReflectionExtensions.GetPropertyNameForListItem(list, SyntaxFactory.Identifier("unrelated")));
    }

    [Fact]
    public void TypedNodeList_RecognizesTokensOfItsImmediateItems()
    {
        var root = SyntaxTree.ParseText("class First {}\nclass Second {}").GetRoot();
        var declaration = root.Members.OfType<ClassDeclarationSyntax>().First();
        Assert.Equal("Members", SyntaxNodeReflectionExtensions.GetPropertyNameForListItem(root, declaration.Identifier));
    }
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void NodePropertiesMaterializesOnceAndCachesSlotCountAndLookup(bool countFirst)
    {
        var enumerations = 0;
        var first = new SyntaxNodeReflectionExtensions.Child { SlotIndex = 2, Item = SyntaxFactory.Identifier("first") };
        var second = new SyntaxNodeReflectionExtensions.Child { SlotIndex = 5, Item = SyntaxFactory.Identifier("second") };
        var group = new SyntaxNodeReflectionExtensions.ChildGroup { PropertyName = "Items", IsList = true, Items = new[] { first, second } };
        var properties = new SyntaxNodeReflectionExtensions.NodeProperties(Groups());
        Assert.Equal(0, enumerations);
        if (countFirst)
            Assert.Equal(2, properties.SlotCount);
        var materialized = properties.Properties;
        Assert.Same(group, Assert.Single(materialized));
        Assert.Same(materialized, properties.Properties);
        Assert.Equal(2, properties.SlotCount);
        Assert.Equal(2, properties.SlotCount);
        Assert.Same(first, properties[2]);
        Assert.Same(second, properties[5]);
        Assert.Same(first, properties[2]);
        Assert.Equal(1, enumerations);

        IEnumerable<SyntaxNodeReflectionExtensions.ChildGroup> Groups()
        {
            enumerations++;
            yield return group;
        }
    }

    [Fact]
    public void EmptyNodePropertiesCacheZeroAndRetainMissingSlotBehavior()
    {
        var properties = new SyntaxNodeReflectionExtensions.NodeProperties([]);
        Assert.Equal(0, properties.SlotCount);
        Assert.Empty(properties.Properties);
        Assert.Same(properties.Properties, properties.Properties);
        Assert.Equal(0, properties.SlotCount);
        Assert.Throws<KeyNotFoundException>(() => properties[0]);
    }
}
