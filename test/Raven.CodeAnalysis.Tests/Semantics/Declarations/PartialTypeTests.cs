using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class PartialTypeTests : CompilationTestBase
{
    [Fact]
    public void PartialInterfaceDeclarationsAcrossSyntaxTrees_MergeMembers()
    {
        const string sourceA = """
partial interface IValueProvider {
    func GetA() -> int;
}
""";

        const string sourceB = """
partial interface IValueProvider {
    func GetB() -> int;
}
""";

        var treeA = SyntaxTree.ParseText(sourceA);
        var treeB = SyntaxTree.ParseText(sourceB);

        var compilation = CreateCompilation(new[] { treeA, treeB }, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");
        compilation.EnsureSetup();
        Assert.Empty(compilation.GetDiagnostics());

        var modelA = compilation.GetSemanticModel(treeA);
        var modelB = compilation.GetSemanticModel(treeB);
        var declA = Assert.IsType<InterfaceDeclarationSyntax>(Assert.Single(treeA.GetRoot().Members));
        var declB = Assert.IsType<InterfaceDeclarationSyntax>(Assert.Single(treeB.GetRoot().Members));
        var symbolA = Assert.IsAssignableFrom<INamedTypeSymbol>(modelA.GetDeclaredSymbol(declA));
        var symbolB = Assert.IsAssignableFrom<INamedTypeSymbol>(modelB.GetDeclaredSymbol(declB));

        Assert.Same(symbolA, symbolB);
        Assert.Single(symbolA.GetMembers("GetA"));
        Assert.Single(symbolA.GetMembers("GetB"));
    }

    [Fact]
    public void PartialTypeDeclarations_WithDifferentAccessibility_ReportDiagnostic()
    {
        const string source = """
public partial class C { }
internal partial class C { }
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor.Id == "RAV0602");
    }

    [Fact]
    public void PartialTypeDeclarations_WithDifferentTypeParameters_ReportDiagnostic()
    {
        const string source = """
partial class Box<T> { }
partial class Box<U, V> { }
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor.Id == "RAV0603");
    }
}
