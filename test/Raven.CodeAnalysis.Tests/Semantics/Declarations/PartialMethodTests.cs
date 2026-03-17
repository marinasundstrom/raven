using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class PartialMethodTests : CompilationTestBase
{
    [Fact]
    public void PartialMethodDefinitionAndImplementation_MergeIntoSingleSymbol()
    {
        const string source = """
partial class C {
    partial func M(value: int) -> int;
}

partial class C {
    partial func M(value: int) -> int {
        return value;
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");
        compilation.EnsureSetup();
        Assert.Empty(compilation.GetDiagnostics());

        var methods = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().ToArray();
        Assert.Equal(2, methods.Length);

        var model = compilation.GetSemanticModel(tree);
        var first = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(methods[0]));
        var second = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(methods[1]));
        var containingType = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol((TypeDeclarationSyntax)methods[0].Parent!));

        Assert.Same(first, second);
        Assert.Single(containingType.GetMembers("M").OfType<IMethodSymbol>());
    }

    [Fact]
    public void PartialMethodWithoutImplementation_ReportsDiagnostic()
    {
        const string source = """
partial class C {
    partial func M() -> unit;
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor.Id == "RAV0604");
    }

    [Fact]
    public void PartialMethodWithoutDefinition_ReportsDiagnostic()
    {
        const string source = """
partial class C {
    partial func M() -> unit { }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor.Id == "RAV0605");
    }

    [Fact]
    public void PartialMethodInNonPartialType_ReportsDiagnostic()
    {
        const string source = """
class C {
    partial func M() -> unit;
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor.Id == "RAV0606");
    }
}
