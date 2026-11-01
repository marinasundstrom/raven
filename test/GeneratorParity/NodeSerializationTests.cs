using System.Text;
using System.Xml;
using System.Xml.Linq;

using NodesShared;

namespace Raven.GeneratorParity.Tests;

[Trait("Target", "Shared")]
public class NodeSerializationTests
{
    [Fact]
    public async Task EmptyModelProducesEmptyRootAndLeavesWriterOpen()
    {
        var result = await Serialize([], []);
        Assert.Equal("Model", result.Name.LocalName);
        Assert.Empty(result.Elements());
    }

    [Fact]
    public async Task RetainsHierarchyNodeAndSlotMetadata()
    {
        var result = await Serialize([new SyntaxNodeModel {
            Name = "Sample", Inherits = "Expression", IsAbstract = true, HasExplicitKind = true,
            AdditionalPermittedTypes = ["ExtraSyntax"],
            Slots = [new SlotModel { Name = "Keyword", Type = "Token", DefaultToken = "LetKeyword",
                IsOptionalToken = true, IsNullable = true, IsInherited = true, IsAbstract = true }]
        }], [new SyntaxHierarchyModel { Name = "Expression", ModelBase = "SyntaxNode", AdditionalPermittedTypes = ["CustomSyntax"] }]);
        var hierarchy = result.Element("Hierarchy")!;
        Assert.Equal("SyntaxNode", hierarchy.Attribute("ModelBase")?.Value);
        Assert.Equal("CustomSyntax", hierarchy.Element("PermittedType")?.Attribute("Name")?.Value);
        var node = result.Element("Node")!;
        Assert.Equal("Expression", node.Attribute("Inherits")?.Value);
        Assert.Equal("true", node.Attribute("IsAbstract")?.Value);
        Assert.Equal("true", node.Attribute("HasExplicitKind")?.Value);
        Assert.Equal("ExtraSyntax", node.Element("PermittedType")?.Attribute("Name")?.Value);
        var slot = node.Element("Slot")!;
        Assert.Equal("LetKeyword", slot.Attribute("DefaultToken")?.Value);
        foreach (var flag in new[] { "IsOptionalToken", "IsNullable", "IsInherited", "IsAbstract" })
            Assert.Equal("true", slot.Attribute(flag)?.Value);
    }

    [Theory]
    [InlineData("List", "Expression", "List", "Expression")]
    [InlineData("SeparatedList", "Argument", "SeparatedList", "Argument")]
    [InlineData("List<Expression>", null, "List", "Expression")]
    [InlineData("SeparatedList<Argument>", null, "SeparatedList", "Argument")]
    public async Task CollectionElementTypeSurvivesSerialization(string type, string? elementType, string expectedType, string expectedElement)
    {
        var result = await Serialize([new SyntaxNodeModel { Name = "Container", Slots = [new SlotModel {
            Name = "Items", Type = type, ElementType = elementType }] }], []);
        var slot = result.Element("Node")!.Element("Slot")!;
        Assert.Equal(expectedType, slot.Attribute("Type")?.Value);
        Assert.Equal(expectedElement, slot.Attribute("ElementType")?.Value);
    }

    [Fact]
    public async Task DefaultAttributesAreOmittedAndTextIsEscaped()
    {
        var result = await Serialize([new SyntaxNodeModel { Name = "A&B", Slots = [new SlotModel { Name = "x<y", Type = "Token" }] }], []);
        var node = result.Element("Node")!;
        Assert.Equal("A&B", node.Attribute("Name")?.Value);
        Assert.Equal("x<y", node.Element("Slot")!.Attribute("Name")?.Value);
        Assert.Null(node.Attribute("Inherits"));
        Assert.Null(node.Attribute("IsAbstract"));
        Assert.Null(node.Element("Slot")!.Attribute("IsNullable"));
    }

    [Fact]
    public async Task RepositoryModelRetainsEveryCollectionSlotElementType()
    {
        var source = XDocument.Load(Path.Combine(AppContext.BaseDirectory, "Model.xml"));
        var nodes = source.Descendants("Node").Select(node => new SyntaxNodeModel
        {
            Name = node.Attribute("Name")!.Value,
            Slots = node.Elements("Slot").Select(slot => new SlotModel
            {
                Name = slot.Attribute("Name")!.Value,
                Type = slot.Attribute("Type")!.Value,
                ElementType = slot.Attribute("ElementType")?.Value
            }).ToList()
        }).ToList();
        var result = await Serialize(nodes, []);
        var expected = source.Descendants("Slot").Where(slot => slot.Attribute("ElementType") is not null)
            .Select(slot => ((string?)slot.Parent!.Attribute("Name"), (string?)slot.Attribute("Name"), (string?)slot.Attribute("ElementType"))).ToArray();
        var actual = result.Descendants("Slot").Where(slot => slot.Attribute("ElementType") is not null)
            .Select(slot => ((string?)slot.Parent!.Attribute("Name"), (string?)slot.Attribute("Name"), (string?)slot.Attribute("ElementType"))).ToArray();
        Assert.NotEmpty(expected);
        Assert.Equal(expected, actual);
    }

    [Fact]
    public async Task ElementTypeChangeChangesSerializedGeneratorInput()
    {
        var node = new SyntaxNodeModel { Name = "Container", Slots = [new SlotModel { Name = "Items", Type = "List", ElementType = "Expression" }] };
        var before = await Serialize([node], []);
        node.Slots[0].ElementType = "Statement";
        var after = await Serialize([node], []);
        Assert.False(XNode.DeepEquals(before, after));
    }

    private static async Task<XElement> Serialize(List<SyntaxNodeModel> nodes, List<SyntaxHierarchyModel> hierarchies)
    {
        var text = new StringBuilder();
        using var writer = XmlWriter.Create(text, new XmlWriterSettings { Async = true, OmitXmlDeclaration = true });
        await Serialization.SerializeAsXml(nodes, hierarchies, writer);
        await writer.FlushAsync();
        var root = XElement.Parse(text.ToString());
        Assert.NotEqual(WriteState.Closed, writer.WriteState);
        return root;
    }
}
