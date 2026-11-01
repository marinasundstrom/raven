using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

using NodesShared;

using Raven.Generators;

namespace Raven.GeneratorParity.Tests;

[Trait("Target", "DotNet")]
public class RedNodeGeneratorTests
{
    [Fact]
    public void ConcreteNodeExposesSlotsUpdatesAndVisitorDispatch()
    {
        var root = RedNodeGenerator.GenerateRedNode(SampleNode(), []);
        Assert.Empty(root.GetDiagnostics());
        var type = Assert.Single(root.DescendantNodes().OfType<ClassDeclarationSyntax>());
        Assert.True(type.Modifiers.Any(SyntaxKind.SealedKeyword));
        Assert.Equal("ContainerSyntax", type.Identifier.Text);
        Assert.Equal(new[] { "Keyword", "Child", "Items" }, type.Members.OfType<PropertyDeclarationSyntax>().Select(p => p.Identifier.Text));
        var methods = type.Members.OfType<MethodDeclarationSyntax>().ToArray();
        Assert.Equal(2, methods.Count(m => m.Identifier.Text == "Accept"));
        Assert.Contains(methods, m => m.Identifier.Text == "GetNodeSlot");
        Assert.Contains(methods, m => m.Identifier.Text == "WithChild");
        Assert.Contains(methods, m => m.Identifier.Text == "WithItems");
        Assert.Contains("return this", methods.Single(m => m.Identifier.Text == "Update").ToFullString());
    }

    [Fact]
    public void AbstractNodeRetainsClosedHierarchyWithoutFactory()
    {
        var node = new SyntaxNodeModel
        {
            Name = "Expression",
            Inherits = "Node",
            IsAbstract = true,
            Slots = [new() { Name = "Child", Type = "Expression", IsAbstract = true }]
        };
        var root = RedNodeGenerator.GenerateRedNode(node, ["LiteralSyntax"]);
        var type = Assert.Single(root.DescendantNodes().OfType<ClassDeclarationSyntax>());
        Assert.True(type.Modifiers.Any(SyntaxKind.AbstractKeyword));
        Assert.True(Assert.Single(type.Members.OfType<PropertyDeclarationSyntax>()).Modifiers.Any(SyntaxKind.AbstractKeyword));
        Assert.Contains("typeof(LiteralSyntax)", root.ToFullString());
        Assert.Null(RedNodeGenerator.GenerateRedFactoryMethod(node));
        var hierarchy = RedNodeGenerator.GenerateClosedHierarchyDeclaration("ExpressionSyntax", ["LiteralSyntax"]);
        Assert.Contains("typeof(LiteralSyntax)", hierarchy.ToFullString());
    }

    [Fact]
    public void ConvenienceFactoryDefaultsTokensAndNullableChildren()
    {
        var root = RedNodeGenerator.GenerateRedFactoryMethod(SampleNode())!;
        Assert.Empty(root.GetDiagnostics());
        var methods = root.DescendantNodes().OfType<MethodDeclarationSyntax>().ToArray();
        Assert.Equal(2, methods.Length);
        Assert.Equal(new[] { "items" }, methods[0].ParameterList.Parameters.Select(p => p.Identifier.Text));
        Assert.Contains("Token(SyntaxKind.IfKeyword)", methods[0].ToFullString());
        Assert.Contains("null", methods[0].ToFullString());
        Assert.Equal(new[] { "keyword", "child", "items" }, methods[1].ParameterList.Parameters.Select(p => p.Identifier.Text));
    }

    [Fact]
    public void ExplicitFactoryKeepsAliasSummaryAndParameterOrder()
    {
        var definition = new FactoryDefinitionModel
        {
            Node = "Container",
            Overloads = [new()
        {
            Alias = "NamedContainer", Summary = "Creates a container.",
            Parameters = [new() { Slot = "Items" }],
            Defaults = [new() { Slot = "Keyword", Token = "IfKeyword" }, new() { Slot = "Child", Null = true }]
        }]
        };
        var root = RedNodeGenerator.GenerateRedFactoryMethod(SampleNode(), definition)!;
        var methods = root.DescendantNodes().OfType<MethodDeclarationSyntax>().ToArray();
        var alias = methods.Single(m => m.Identifier.Text == "NamedContainer");
        Assert.Equal("items", Assert.Single(alias.ParameterList.Parameters).Identifier.Text);
        Assert.Contains("Creates a container.", root.ToFullString());
        Assert.Contains("new ContainerSyntax(Token(SyntaxKind.IfKeyword), null, items)", alias.ToFullString());
    }

    private static SyntaxNodeModel SampleNode() => new()
    {
        Name = "Container",
        Inherits = "Expression",
        Slots = [new() { Name = "Keyword", Type = "Token", DefaultToken = "IfKeyword" },
            new() { Name = "Child", Type = "Expression", IsNullable = true },
            new() { Name = "Items", Type = "List", ElementType = "Expression" }]
    };
}
