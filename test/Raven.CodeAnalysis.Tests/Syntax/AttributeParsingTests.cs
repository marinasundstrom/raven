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
                val value: int = 0
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
                var data: int;
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
            val value: int = 0
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
            val value: int = 0
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

    [Fact]
    public void CompilationUnit_AssemblyAttributeSeparatedByBlankLine_IsCompilationAttribute()
    {
        const string code = """
            import System.Runtime.Versioning.*

            [assembly: TargetFramework(".NETCoreApp,Version=v9.0")]

            class Widget {}
            """;

        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();

        var rootAttributeList = Assert.Single(root.AttributeLists);
        Assert.Equal("assembly", rootAttributeList.Target?.Identifier.Text);

        var declaration = Assert.IsType<ClassDeclarationSyntax>(Assert.Single(root.Members));
        Assert.Empty(declaration.AttributeLists);
    }

    [Fact]
    public void CompilationUnit_AssemblyAttributeWithoutBlankLine_IsCompilationAttribute()
    {
        const string code = """
            import System.Runtime.Versioning.*
            [assembly: TargetFramework(".NETCoreApp,Version=v9.0")]
            class Widget {}
            """;

        var tree = SyntaxTree.ParseText(code);
        var root = tree.GetRoot();
        var rootAttributeList = Assert.Single(root.AttributeLists);
        Assert.Equal("assembly", rootAttributeList.Target?.Identifier.Text);

        var declaration = Assert.IsType<ClassDeclarationSyntax>(Assert.Single(root.Members));
        Assert.Empty(declaration.AttributeLists);
    }

    [Fact]
    public void MethodDeclaration_WithReturnAttributeTarget_ParsesTarget()
    {
        const string code = """
            class Widget {
                [return: A, B("Test")]
                public M() -> int => 42
            }
            """;

        var tree = SyntaxTree.ParseText(code);
        var method = tree.GetRoot()
            .DescendantNodes()
            .OfType<MethodDeclarationSyntax>()
            .Single();

        var attributeList = Assert.Single(method.AttributeLists);
        Assert.NotNull(attributeList.Target);
        Assert.Equal("return", attributeList.Target!.Identifier.Text);
        Assert.Equal(2, attributeList.Attributes.Count);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void FunctionStatement_WithAttributeList_ParsesAttributes()
    {
        const string code = """
            [MyAttr]
            func Compute() -> int {
                return 1
            }
            """;

        var tree = SyntaxTree.ParseText(code);
        var function = tree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .Single();

        var attributeList = Assert.Single(function.AttributeLists);
        Assert.Null(attributeList.Target);
        var attribute = Assert.Single(attributeList.Attributes);
        var name = Assert.IsType<IdentifierNameSyntax>(attribute.Name);
        Assert.Equal("MyAttr", name.Identifier.Text);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void FunctionStatement_WithReturnAttributeTarget_ParsesTarget()
    {
        const string code = """
            [return: Result]
            func Compute() -> int {
                return 1
            }
            """;

        var tree = SyntaxTree.ParseText(code);
        var function = tree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .Single();

        var attributeList = Assert.Single(function.AttributeLists);
        Assert.NotNull(attributeList.Target);
        Assert.Equal("return", attributeList.Target!.Identifier.Text);
        var attribute = Assert.Single(attributeList.Attributes);
        var name = Assert.IsType<IdentifierNameSyntax>(attribute.Name);
        Assert.Equal("Result", name.Identifier.Text);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void LocalFunctionStatement_WithLeadingAttributeList_ParsesAttributes()
    {
        const string code = """
            func Outer() {
                [return: A, B("Test")]
                [FromBody]
                func Inner(content: string) -> string {
                    return content
                }
            }
            """;

        var tree = SyntaxTree.ParseText(code);
        var function = tree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .Single(f => f.Identifier.Text == "Inner");

        Assert.Equal(2, function.AttributeLists.Count);

        var returnList = function.AttributeLists[0];
        Assert.NotNull(returnList.Target);
        Assert.Equal("return", returnList.Target!.Identifier.Text);
        Assert.Equal(2, returnList.Attributes.Count);

        var parameterList = function.AttributeLists[1];
        Assert.Null(parameterList.Target);
        Assert.Single(parameterList.Attributes);

        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void DelegateDeclaration_WithReturnAttributeTarget_ParsesTarget()
    {
        const string code = """
            class C {
                [return: FormatterResult]
                public delegate Formatter(value: string) -> string
            }
            """;

        var tree = SyntaxTree.ParseText(code);
        var declaration = tree.GetRoot()
            .DescendantNodes()
            .OfType<DelegateDeclarationSyntax>()
            .Single();

        var attributeList = Assert.Single(declaration.AttributeLists);
        Assert.NotNull(attributeList.Target);
        Assert.Equal("return", attributeList.Target!.Identifier.Text);
        var attribute = Assert.Single(attributeList.Attributes);
        var name = Assert.IsType<IdentifierNameSyntax>(attribute.Name);
        Assert.Equal("FormatterResult", name.Identifier.Text);
        Assert.Empty(tree.GetDiagnostics());
    }
}
