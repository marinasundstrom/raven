using System.Reflection;

using NodesShared;

namespace Raven.GeneratorParity.Tests;

public class NodeModelTests
{
    [Fact]
    [Trait("Target", "DotNet")]
    public void LoadsSelectedImplementation()
    {
        var expected = typeof(NodeModelTests).Assembly.GetCustomAttributes<AssemblyMetadataAttribute>()
            .Single(a => a.Key == "GeneratorImplementationAssembly").Value;
        foreach (var type in new[] { typeof(SyntaxNodeModel), typeof(SyntaxHierarchyModel), typeof(SlotModel),
            typeof(FactoryDefinitionModel), typeof(FactoryOverloadModel), typeof(FactoryParameterModel), typeof(FactoryDefaultModel), typeof(Serialization) })
            Assert.Equal(expected, type.Assembly.GetName().Name);
        Assert.Equal("SerializeAsXml", Assert.Single(typeof(Serialization).GetMethods(
            BindingFlags.Public | BindingFlags.Static | BindingFlags.DeclaredOnly)).Name);
        Assert.False(typeof(SyntaxNodeModel).IsSealed);
        Assert.False(typeof(SlotModel).IsSealed);
    }

    [Theory]
    [Trait("Target", "Shared")]
    [InlineData("Token", null, "Token")]
    [InlineData("List", "Expression", "List<Expression>")]
    [InlineData("SeparatedList", "Argument", "SeparatedList<Argument>")]
    public void FullTypeNamePreservesSlotShape(string type, string? element, string expected)
        => Assert.Equal(expected, new SlotModel { Type = type, ElementType = element }.FullTypeName);

    [Fact]
    [Trait("Target", "Shared")]
    public void MutableAggregateModelsKeepIndependentCollectionsAndIdentity()
    {
        var left = new SyntaxNodeModel();
        var right = new SyntaxNodeModel();
        left.Slots.Add(new SlotModel { Name = "Value" });
        left.AdditionalPermittedTypes.Add("OtherSyntax");
        Assert.Empty(right.Slots);
        Assert.Empty(right.AdditionalPermittedTypes);
        Assert.NotEqual(left, right);
        Assert.Empty(new SyntaxHierarchyModel().AdditionalPermittedTypes);
        Assert.Empty(new FactoryDefinitionModel().Overloads);
        Assert.Empty(new FactoryOverloadModel().Parameters);
        Assert.Empty(new FactoryOverloadModel().Defaults);
    }
}
