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
    val x: int = 0;
};

partial class Container {
    val y: int = 0;
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
    public void PartialClassDeclarationsAcrossSyntaxTrees_MergeMembers()
    {
        const string sourceA = """
partial class Container {
    val x: int = 0;
};
""";

        const string sourceB = """
partial class Container {
    val y: int = 0;
};
""";

        var treeA = SyntaxTree.ParseText(sourceA);
        var treeB = SyntaxTree.ParseText(sourceB);

        var compilation = CreateCompilation(new[] { treeA, treeB }, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");

        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success);

        var modelA = compilation.GetSemanticModel(treeA);
        var modelB = compilation.GetSemanticModel(treeB);

        var rootA = treeA.GetRoot();
        var rootB = treeB.GetRoot();

        var declarationA = Assert.IsType<ClassDeclarationSyntax>(Assert.Single(rootA.Members));
        var declarationB = Assert.IsType<ClassDeclarationSyntax>(Assert.Single(rootB.Members));

        var symbolA = Assert.IsAssignableFrom<INamedTypeSymbol>(modelA.GetDeclaredSymbol(declarationA));
        var symbolB = Assert.IsAssignableFrom<INamedTypeSymbol>(modelB.GetDeclaredSymbol(declarationB));

        Assert.Same(symbolA, symbolB);
        Assert.Single(symbolA.GetMembers("x"));
        Assert.Single(symbolA.GetMembers("y"));
    }

    [Fact]
    public void PartialClassDeclarationsAcrossSyntaxTrees_InMetadataNamespace_MergeMembers()
    {
        const string sourceA = """
namespace System;

partial class Container {
    val x: int = 0;
};
""";

        const string sourceB = """
namespace System;

partial class Container {
    val y: int = 0;
};
""";

        var treeA = SyntaxTree.ParseText(sourceA);
        var treeB = SyntaxTree.ParseText(sourceB);

        var compilation = CreateCompilation(new[] { treeA, treeB }, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");

        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success);

        var modelA = compilation.GetSemanticModel(treeA);
        var modelB = compilation.GetSemanticModel(treeB);

        var declarationA = Assert.IsType<ClassDeclarationSyntax>(Assert.Single(treeA.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>()));
        var declarationB = Assert.IsType<ClassDeclarationSyntax>(Assert.Single(treeB.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>()));

        var symbolA = Assert.IsAssignableFrom<INamedTypeSymbol>(modelA.GetDeclaredSymbol(declarationA));
        var symbolB = Assert.IsAssignableFrom<INamedTypeSymbol>(modelB.GetDeclaredSymbol(declarationB));

        Assert.Same(symbolA, symbolB);
        Assert.Equal(2, symbolA.DeclaringSyntaxReferences.Length);
        Assert.Single(symbolA.GetMembers("x"));
        Assert.Single(symbolA.GetMembers("y"));
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

}
