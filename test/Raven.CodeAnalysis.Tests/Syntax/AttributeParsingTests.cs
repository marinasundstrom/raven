using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class AttributeParsingTests : DiagnosticTestBase
{
    [Fact]
    public void Attribute_WithNamedArgumentUsingColon_Parses()
    {
        const string code = """
            [JsonPolymorphic(TypeDiscriminatorPropertyName: "type")]
            class Widget {}
            """;

        var tree = SyntaxTree.ParseText(code);
        var declaration = tree.GetRoot()
            .DescendantNodes()
            .OfType<ClassDeclarationSyntax>()
            .Single();

        var attribute = Assert.Single(Assert.Single(declaration.AttributeLists).Attributes);
        var argument = Assert.Single(attribute.ArgumentList!.Arguments);

        Assert.Equal("TypeDiscriminatorPropertyName", argument.NameColon!.Name.Identifier.Text);
        Assert.Equal(SyntaxKind.ColonToken, argument.NameColon.ColonToken.Kind);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void Attribute_WithNamedArgumentUsingEquals_ReportsDiagnostic()
    {
        const string code = """
            [JsonPolymorphic(TypeDiscriminatorPropertyName = "type")]
            class Widget {}
            """;

        var tree = SyntaxTree.ParseText(code);
        var diagnostics = tree.GetDiagnostics();

        var diagnostic = Assert.Single(diagnostics);
        Assert.Equal("RAV1003", diagnostic.Descriptor.Id);
        Assert.Contains(":", diagnostic.GetMessage());
    }

    [Fact]
    public void ConstructorDeclaration_WithAttributeList_ParsesAttributes()
    {
        const string code = """
            class Widget {
                [Primary]
                public init() {}
            }
            """;

        var tree = SyntaxTree.ParseText(code);
        var constructor = tree.GetRoot()
            .DescendantNodes()
            .OfType<ConstructorDeclarationSyntax>()
            .Single();

        var attributeList = Assert.Single(constructor.AttributeLists);
        var attribute = Assert.Single(attributeList.Attributes);

        var name = Assert.IsType<IdentifierNameSyntax>(attribute.Name);
        Assert.Equal("Primary", name.Identifier.Text);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void FieldDeclaration_WithAttributeList_ParsesAttributes()
    {
        const string code = """
            class Widget {
                [Field]
                let value: int = 0
            }
            """;

        var tree = SyntaxTree.ParseText(code);
        var field = tree.GetRoot()
            .DescendantNodes()
            .OfType<FieldDeclarationSyntax>()
            .Single();

        var attributeList = Assert.Single(field.AttributeLists);
        var attribute = Assert.Single(attributeList.Attributes);

        var name = Assert.IsType<IdentifierNameSyntax>(attribute.Name);
        Assert.Equal("Field", name.Identifier.Text);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void PropertyDeclaration_WithAttributeList_ParsesAttributes()
    {
        const string code = """
            class Widget {
                [Data]
                public Value: int { get => 0 }
            }
            """;

        var tree = SyntaxTree.ParseText(code);
        var property = tree.GetRoot()
            .DescendantNodes()
            .OfType<PropertyDeclarationSyntax>()
            .Single();

        var attributeList = Assert.Single(property.AttributeLists);
        var attribute = Assert.Single(attributeList.Attributes);

        var name = Assert.IsType<IdentifierNameSyntax>(attribute.Name);
        Assert.Equal("Data", name.Identifier.Text);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void IndexerDeclaration_WithAttributeList_ParsesAttributes()
    {
        const string code = """
            class Widget {
                var data: int
                [Indexed]
                public self[index: int]: int { get => data }
            }
            """;

        var tree = SyntaxTree.ParseText(code);
        var indexer = tree.GetRoot()
            .DescendantNodes()
            .OfType<IndexerDeclarationSyntax>()
            .Single();

        var attributeList = Assert.Single(indexer.AttributeLists);
        var attribute = Assert.Single(attributeList.Attributes);

        var name = Assert.IsType<IdentifierNameSyntax>(attribute.Name);
        Assert.Equal("Indexed", name.Identifier.Text);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void EnumMemberDeclaration_WithAttributeList_ParsesAttributes()
    {
        const string code = """
            enum Colors {
                [Primary]
                Red,
            }
            """;

        var tree = SyntaxTree.ParseText(code);
        var member = tree.GetRoot()
            .DescendantNodes()
            .OfType<EnumMemberDeclarationSyntax>()
            .Single();

        var attributeList = Assert.Single(member.AttributeLists);
        var attribute = Assert.Single(attributeList.Attributes);

        var name = Assert.IsType<IdentifierNameSyntax>(attribute.Name);
        Assert.Equal("Primary", name.Identifier.Text);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void GlobalStatement_WithAttributeList_ParsesAttributes()
    {
        const string code = """
            [Entry]
            let value: int = 0
            """;

        var tree = SyntaxTree.ParseText(code);
        var global = Assert.IsType<GlobalStatementSyntax>(Assert.Single(tree.GetRoot().Members));

        var attributeList = Assert.Single(global.AttributeLists);
        var attribute = Assert.Single(attributeList.Attributes);

        var name = Assert.IsType<IdentifierNameSyntax>(attribute.Name);
        Assert.Equal("Entry", name.Identifier.Text);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void NamespaceDeclaration_WithAttributeList_ParsesAttributes()
    {
        const string code = """
            [MyNamespace]
            namespace Samples {
            }
            """;

        var tree = SyntaxTree.ParseText(code);
        var declaration = Assert.IsType<NamespaceDeclarationSyntax>(Assert.Single(tree.GetRoot().Members));

        var attributeList = Assert.Single(declaration.AttributeLists);
        var attribute = Assert.Single(attributeList.Attributes);

        var name = Assert.IsType<IdentifierNameSyntax>(attribute.Name);
        Assert.Equal("MyNamespace", name.Identifier.Text);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void FileScopedNamespaceDeclaration_WithAttributeList_ParsesAttributes()
    {
        const string code = """
            [MyNamespace]
            namespace Samples;
            let value: int = 0
            """;

        var tree = SyntaxTree.ParseText(code);
        var declaration = Assert.IsType<FileScopedNamespaceDeclarationSyntax>(Assert.Single(tree.GetRoot().Members));

        var attributeList = Assert.Single(declaration.AttributeLists);
        var attribute = Assert.Single(attributeList.Attributes);

        var name = Assert.IsType<IdentifierNameSyntax>(attribute.Name);
        Assert.Equal("MyNamespace", name.Identifier.Text);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void CompilationUnit_WithAssemblyAttribute_ParsesTarget()
    {
        const string code = """
            [assembly: MyAttr]
            import System
            """;

        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();

        var attributeList = Assert.Single(root.AttributeLists);
        Assert.NotNull(attributeList.Target);
        Assert.Equal("assembly", attributeList.Target!.Identifier.Text);

        var attribute = Assert.Single(attributeList.Attributes);
        var name = Assert.IsType<IdentifierNameSyntax>(attribute.Name);
        Assert.Equal("MyAttr", name.Identifier.Text);
        Assert.Empty(tree.GetDiagnostics());
    }
}
