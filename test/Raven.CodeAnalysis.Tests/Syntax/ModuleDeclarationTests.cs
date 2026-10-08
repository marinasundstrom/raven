using System.Linq;

using Raven.CodeAnalysis.Tests;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class ModuleDeclarationTests
{
    [Theory]
    [InlineData("module Example.Math { public class Item {} }")]
    [InlineData("module Example.Math\npublic class Item {}")]
    public void ModuleUsesNamespaceShapeAndBindsQualifiedNames(string source)
    {
        var tree = SyntaxTree.ParseText(source);
        Assert.Empty(tree.GetDiagnostics());
        var declaration = Assert.Single(tree.GetRoot().DescendantNodes().OfType<BaseNamespaceDeclarationSyntax>());
        var compilation = Compilation.Create("UnrelatedPackage", [tree], TestMetadataReferences.Default);
        var module = Assert.IsAssignableFrom<INamespaceSymbol>(compilation.GetSemanticModel(tree).GetDeclaredSymbol(declaration));
        Assert.True(module.IsModule);
        Assert.Equal("Example.Math", module.ToMetadataName());
        Assert.NotNull(compilation.GetTypeByMetadataName("Example.Math.Item"));
    }

    [Fact]
    public void ModuleImportsAndNestedModulesUseExistingLookup()
    {
        var declarations = SyntaxTree.ParseText("module Example { module Models { public class Item {} } }");
        var consumer = SyntaxTree.ParseText("import Example.Models.*\nmodule Consumer\npublic class Holder { val Value: Item? => null }");
        var compilation = Compilation.Create("Package", [declarations, consumer], TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var item = compilation.GetTypeByMetadataName("Example.Models.Item");
        Assert.True(item!.ContainingNamespace!.IsModule);
        Assert.Contains("Item", compilation.GetTypeByMetadataName("Consumer.Holder")!.GetMembers("Value").OfType<IPropertySymbol>().Single().Type.ToDisplayString());
    }

    [Fact]
    public void OrdinaryNamespaceRemainsNamespace()
    {
        var tree = SyntaxTree.ParseText("namespace Example { public class Item {} }");
        var compilation = Compilation.Create("Package", [tree], TestMetadataReferences.Default);
        Assert.False(compilation.GetTypeByMetadataName("Example.Item")!.ContainingNamespace!.IsModule);
    }
}
