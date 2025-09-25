using System.IO;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class PartialClassTests : CompilationTestBase
{
    [Fact]
    public void PartialClassDeclarations_MergeMembers()
    {
        const string source = """
partial class Container {
    let x: int = 0;
};

partial class Container {
    let y: int = 0;
};
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");

        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success);

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var firstDeclaration = Assert.IsType<ClassDeclarationSyntax>(root.Members[0]);
        var secondDeclaration = Assert.IsType<ClassDeclarationSyntax>(root.Members[1]);

        var firstSymbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(firstDeclaration));
        var secondSymbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(secondDeclaration));

        Assert.Same(firstSymbol, secondSymbol);
        Assert.Single(firstSymbol.GetMembers("x"));
        Assert.Single(firstSymbol.GetMembers("y"));
    }

    [Fact]
    public void DuplicateClassDeclarationsWithoutPartial_ProduceDiagnostic()
    {
        const string source = """
class C {};
class C {};
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");

        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);

        Assert.False(result.Success);
        var diagnostic = Assert.Single(result.Diagnostics);
        Assert.Equal("RAV0600", diagnostic.Descriptor.Id);
    }

    [Fact]
    public void PartialClassMissingModifier_ProducesDiagnostic()
    {
        const string source = """
class Sample {};
partial class Sample {};
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");

        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);

        Assert.False(result.Success);
        var diagnostic = Assert.Single(result.Diagnostics);
        Assert.Equal("RAV0601", diagnostic.Descriptor.Id);
    }
}
