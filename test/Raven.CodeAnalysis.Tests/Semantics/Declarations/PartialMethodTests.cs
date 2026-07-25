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

    [Theory]
    [InlineData("scoped ", "")]
    [InlineData("", "scoped ")]
    public void PartialMethodParameters_MustHaveMatchingScopedContracts(
        string definitionModifier,
        string implementationModifier)
    {
        var source = $$"""
partial class C {
    partial func M({{definitionModifier}}value: System.Span<int>) -> unit;
    partial func M({{implementationModifier}}value: System.Span<int>) -> unit { }
}
""";

        var (compilation, _) = CreateCompilation(source);

        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor.Id == "RAV0359");
    }

    [Fact]
    public void PartialMethodParameters_WithMatchingScopedContracts_AreMerged()
    {
        const string source = """
partial class C {
    partial func M(scoped value: System.Span<int>) -> unit;
    partial func M(scoped value: System.Span<int>) -> unit { }
}
""";

        var (compilation, tree) = CreateCompilation(source);
        var declarations = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().ToArray();
        var model = compilation.GetSemanticModel(tree);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor.Id == "RAV0359");
        Assert.Same(
            model.GetDeclaredSymbol(declarations[0]),
            model.GetDeclaredSymbol(declarations[1]));
    }
}
