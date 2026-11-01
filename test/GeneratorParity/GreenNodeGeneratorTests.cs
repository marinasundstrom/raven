using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

using NodesShared;

using Raven.Generators;

namespace Raven.GeneratorParity.Tests;

[Trait("Target", "DotNet")]
public class GreenNodeGeneratorTests
{
    [Fact]
    public void ConcreteNodeHasOrderedSlotsKindAndVisitorHooks()
    {
        var node = SampleNode();
        var root = CSharpSyntaxTree.ParseText(GreenNodeGenerator.GenerateGreenNode(node, new())).GetRoot();
        Assert.DoesNotContain(root.GetDiagnostics(), d => d.Severity == Microsoft.CodeAnalysis.DiagnosticSeverity.Error);
        var type = Assert.Single(root.DescendantNodes().OfType<ClassDeclarationSyntax>());
        Assert.Equal("ContainerSyntax", type.Identifier.Text);
        Assert.Equal("ExpressionSyntax", type.BaseList!.Types[0].Type.ToString());
        var constructor = type.Members.OfType<ConstructorDeclarationSyntax>().Single(c => c.Modifiers.Any(SyntaxKind.PublicKeyword));
        Assert.Equal(new[] { "kind", "keyword", "child", "items", "diagnostics", "annotations" }, constructor.ParameterList.Parameters.Select(p => p.Identifier.Text));
        var slots = type.Members.OfType<PropertyDeclarationSyntax>().ToArray();
        Assert.Equal(new[] { "Keyword", "Child", "Items" }, slots.Select(p => p.Identifier.Text));
        Assert.Contains("GetSlot<ExpressionSyntax?>(1)", slots[1].ToFullString().Replace(" ", ""));
        Assert.Contains("GetSlot<SyntaxList>(2)", slots[2].ToFullString().Replace(" ", ""));
        Assert.True(slots[1].Modifiers.Any(SyntaxKind.OverrideKeyword));
        var methods = type.Members.OfType<MethodDeclarationSyntax>().ToArray();
        Assert.Equal(2, methods.Count(m => m.Identifier.Text == "Accept"));
        Assert.Contains(methods, m => m.Identifier.Text == "CreateRed");
        Assert.Contains(methods, m => m.Identifier.Text == "With");
        var update = methods.Single(m => m.Identifier.Text == "Update");
        Assert.Contains("Kind != kind", update.ToFullString());
        Assert.Contains("return this", update.ToFullString());
    }

    [Fact]
    public void AbstractNodeDeclaresAbstractPropertiesWithoutConcreteHelpers()
    {
        var node = new SyntaxNodeModel
        {
            Name = "Expression",
            Inherits = "Node",
            IsAbstract = true,
            Slots = [new() { Name = "Child", Type = "Expression", IsAbstract = true }, new() { Name = "Ignored", Type = "Token" }]
        };
        var root = CSharpSyntaxTree.ParseText(GreenNodeGenerator.GenerateGreenNode(node, new())).GetRoot();
        Assert.DoesNotContain(root.GetDiagnostics(), d => d.Severity == Microsoft.CodeAnalysis.DiagnosticSeverity.Error);
        var type = Assert.Single(root.DescendantNodes().OfType<ClassDeclarationSyntax>());
        Assert.True(type.Modifiers.Any(SyntaxKind.AbstractKeyword));
        Assert.True(Assert.Single(type.Members.OfType<PropertyDeclarationSyntax>()).Modifiers.Any(SyntaxKind.AbstractKeyword));
        Assert.Empty(type.Members.OfType<MethodDeclarationSyntax>());
        Assert.Null(GreenNodeGenerator.GenerateFactoryMethod(node));
    }

    [Fact]
    public void FactoryIncludesMinimalNullableOverloadAndMetadataDefaults()
    {
        var factory = GreenNodeGenerator.GenerateFactoryMethod(SampleNode());
        Assert.Empty(factory.GetDiagnostics());
        var methods = factory.DescendantNodes().OfType<MethodDeclarationSyntax>().ToArray();
        Assert.Equal(2, methods.Length);
        Assert.Equal(new[] { "kind", "keyword", "items" }, methods[0].ParameterList.Parameters.Select(p => p.Identifier.Text));
        Assert.Equal(new[] { "kind", "keyword", "child", "items", "diagnostics", "annotations" }, methods[1].ParameterList.Parameters.Select(p => p.Identifier.Text));
        Assert.Contains("Container(kind, keyword, null, items, null, null)", methods[0].ToFullString());
        Assert.Equal("null", methods[1].ParameterList.Parameters[^1].Default!.Value.ToString());
        Assert.Equal("null", methods[1].ParameterList.Parameters[^2].Default!.Value.ToString());
    }

    private static SyntaxNodeModel SampleNode() => new()
    {
        Name = "Container",
        Inherits = "Expression",
        HasExplicitKind = true,
        Slots = [new() { Name = "Keyword", Type = "Token" },
            new() { Name = "Child", Type = "Expression", IsNullable = true, IsInherited = true },
            new() { Name = "Items", Type = "List", ElementType = "Expression" }]
    };
}
