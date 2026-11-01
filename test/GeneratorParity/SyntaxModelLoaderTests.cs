using Raven.Generators;

namespace Raven.GeneratorParity.Tests;

[Trait("Target", "DotNet")]
public class SyntaxModelLoaderTests
{
    [Fact]
    public void NodesAndHierarchiesPreserveOrderDefaultsAndFlags()
    {
        using var input = new XmlInput("""
            <Model>
              <Hierarchy Name="Root" ModelBase="Node"><PermittedType Name="ExtraSyntax" /></Hierarchy>
              <Node Name="First" Inherits="Node" IsAbstract="true" HasExplicitKind="true">
                <PermittedType Name="CustomSyntax" />
                <Slot Name="Items" Type="List" ElementType="Expression" IsNullable="true" IsInherited="true" IsAbstract="true" />
                <Slot Name="Keyword" Type="Token" DefaultToken="LetKeyword" IsOptionalToken="true" />
              </Node>
              <Node Name="Second" IsAbstract="invalid"><Slot Name="Child" Type="Node" /></Node>
            </Model>
            """);
        var nodes = SyntaxModelLoader.LoadSyntaxNodesFromXml(input.Path);
        Assert.Equal(new[] { "First", "Second" }, nodes.Select(n => n.Name));
        Assert.Equal("Node", nodes[0].Inherits);
        Assert.True(nodes[0].IsAbstract);
        Assert.True(nodes[0].HasExplicitKind);
        Assert.Equal("CustomSyntax", Assert.Single(nodes[0].AdditionalPermittedTypes));
        var slot = nodes[0].Slots[0];
        Assert.Equal("List<Expression>", slot.FullTypeName);
        Assert.True(slot.IsNullable && slot.IsInherited && slot.IsAbstract);
        Assert.True(nodes[0].Slots[1].IsOptionalToken);
        Assert.Equal("LetKeyword", nodes[0].Slots[1].DefaultToken);
        Assert.Equal("", nodes[1].Inherits);
        Assert.False(nodes[1].IsAbstract);
        Assert.False(nodes[1].HasExplicitKind);
        Assert.Null(nodes[1].Slots[0].ElementType);
        var hierarchy = Assert.Single(SyntaxModelLoader.LoadSyntaxHierarchiesFromXml(input.Path));
        Assert.Equal("Root", hierarchy.Name);
        Assert.Equal("Node", hierarchy.ModelBase);
        Assert.Equal("ExtraSyntax", Assert.Single(hierarchy.AdditionalPermittedTypes));
    }

    [Fact]
    public void FactoriesPreserveAliasesSummariesParametersAndDefaults()
    {
        using var input = new XmlInput("""
            <Factories><Factory Node="Container">
              <Overload Alias="Alias" Summary="Text &amp; metadata">
                <Parameter Slot="Left" /><Parameter Slot="Right" />
                <Default Slot="Keyword" Token="LetKeyword" /><Default Slot="Body" Null="true" />
              </Overload>
              <Overload />
            </Factory></Factories>
            """);
        var factory = Assert.Single(SyntaxModelLoader.LoadFactoriesFromXml(input.Path));
        Assert.Equal("Container", factory.Node);
        Assert.Equal(2, factory.Overloads.Count);
        var overload = factory.Overloads[0];
        Assert.Equal("Alias", overload.Alias);
        Assert.Equal("Text & metadata", overload.Summary);
        Assert.Equal(new[] { "Left", "Right" }, overload.Parameters.Select(p => p.Slot));
        Assert.Equal("LetKeyword", overload.Defaults[0].Token);
        Assert.False(overload.Defaults[0].Null);
        Assert.True(overload.Defaults[1].Null);
        Assert.Null(factory.Overloads[1].Alias);
        Assert.Empty(factory.Overloads[1].Defaults);
    }

    [Theory]
    [InlineData("12", 12)]
    [InlineData("-1", -1)]
    [InlineData("invalid", null)]
    [InlineData("2147483648", null)]
    public void TokensRetainFlagsAndTolerantPrecedenceParsing(string precedence, int? expected)
    {
        using var input = new XmlInput($"""
            <Tokens><TokenKind Name="PlusToken" Text="&lt;&amp;" Value="value" IsReservedWord="true" IsTrivia="TRUE" IsUnaryOperator="true" IsBinaryOperator="true" Precedence="{precedence}" /><TokenKind Name="Empty" /></Tokens>
            """);
        var tokens = SyntaxModelLoader.LoadTokenKindsFromXml(input.Path);
        Assert.Equal(2, tokens.Count);
        Assert.Equal(new TokenKindModel("PlusToken", "<&", "value", true, true, true, true, expected), tokens[0]);
        Assert.Equal(new TokenKindModel("Empty"), tokens[1]);
    }

    [Fact]
    public void KindsRetainInputOrderAndDuplicates()
    {
        using var input = new XmlInput("""
            <Kinds><NodeKind Name="Zed" Type="Binary" /><NodeKind Name="Alpha" Type="Unary" /><NodeKind Name="Zed" Type="Binary" /></Kinds>
            """);
        Assert.Equal(new[] { new NodeKindModel("Zed", "Binary"), new NodeKindModel("Alpha", "Unary"), new NodeKindModel("Zed", "Binary") },
            SyntaxModelLoader.LoadNodeKindsFromXml(input.Path));
    }

    [Fact]
    public void OnlyFactoryFileIsOptional()
    {
        var missing = System.IO.Path.Combine(System.IO.Path.GetTempPath(), Guid.NewGuid() + ".xml");
        Assert.Empty(SyntaxModelLoader.LoadFactoriesFromXml(missing));
        Assert.Throws<FileNotFoundException>(() => SyntaxModelLoader.LoadSyntaxNodesFromXml(missing));
        Assert.Throws<FileNotFoundException>(() => SyntaxModelLoader.LoadSyntaxHierarchiesFromXml(missing));
        Assert.Throws<FileNotFoundException>(() => SyntaxModelLoader.LoadTokenKindsFromXml(missing));
        Assert.Throws<FileNotFoundException>(() => SyntaxModelLoader.LoadNodeKindsFromXml(missing));
    }

    [Theory]
    [InlineData("node", "<Model><Node /></Model>", "Node missing Name")]
    [InlineData("node", "<Model><Node Name='A'><Slot Name='B' /></Node></Model>", "Slot missing Type")]
    [InlineData("hierarchy", "<Model><Hierarchy /></Model>", "Hierarchy missing Name")]
    [InlineData("factory", "<Factories><Factory /></Factories>", "Factory missing Node")]
    [InlineData("factory", "<Factories><Factory Node='A'><Overload><Parameter /></Overload></Factory></Factories>", "Factory parameter missing Slot")]
    [InlineData("token", "<Tokens><TokenKind /></Tokens>", "TokenKind missing Name")]
    [InlineData("kind", "<Kinds><NodeKind Name='A' /></Kinds>", "NodeKind missing Type")]
    public void MissingRequiredAttributesKeepSpecificErrors(string loader, string xml, string expected)
    {
        using var input = new XmlInput(xml);
        var error = Assert.Throws<Exception>(() =>
        {
            switch (loader)
            {
                case "node": SyntaxModelLoader.LoadSyntaxNodesFromXml(input.Path); break;
                case "hierarchy": SyntaxModelLoader.LoadSyntaxHierarchiesFromXml(input.Path); break;
                case "factory": SyntaxModelLoader.LoadFactoriesFromXml(input.Path); break;
                case "token": SyntaxModelLoader.LoadTokenKindsFromXml(input.Path); break;
                case "kind": SyntaxModelLoader.LoadNodeKindsFromXml(input.Path); break;
            }
        });
        Assert.Equal(expected, error.Message);
    }

    private sealed class XmlInput : IDisposable
    {
        public string Path { get; } = System.IO.Path.GetTempFileName();
        public XmlInput(string xml) => File.WriteAllText(Path, xml);
        public void Dispose() => File.Delete(Path);
    }
}
