using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class PartialPropertyTests : CompilationTestBase
{
    [Fact]
    public void PartialPropertyDefinitionAndImplementation_MergeIntoSingleSymbol()
    {
        const string source = """
partial class C {
    partial var Name: string {
        get;
        set;
    }
}

partial class C {
    partial var Name: string {
        get => field;
        set => field = value;
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");
        compilation.EnsureSetup();
        Assert.Empty(compilation.GetDiagnostics());

        var properties = tree.GetRoot().DescendantNodes().OfType<PropertyDeclarationSyntax>().ToArray();
        Assert.Equal(2, properties.Length);

        var model = compilation.GetSemanticModel(tree);
        var first = Assert.IsAssignableFrom<IPropertySymbol>(model.GetDeclaredSymbol(properties[0]));
        var second = Assert.IsAssignableFrom<IPropertySymbol>(model.GetDeclaredSymbol(properties[1]));
        var containingType = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol((TypeDeclarationSyntax)properties[0].Parent!));

        Assert.Same(first, second);
        Assert.Single(containingType.GetMembers("Name").OfType<IPropertySymbol>());
    }

    [Fact]
    public void PartialPropertyWithoutImplementation_ReportsDiagnostic()
    {
        const string source = """
partial class C {
    partial var Name: string {
        get;
        set;
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor.Id == "RAV0607");
    }

    [Fact]
    public void PartialPropertyWithoutDefinition_ReportsDiagnostic()
    {
        const string source = """
partial class C {
    partial var Name: string {
        get => field;
        set => field = value;
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor.Id == "RAV0609");
    }

    [Fact]
    public void PartialPropertyInNonPartialType_ReportsDiagnostic()
    {
        const string source = """
class C {
    partial var Name: string {
        get;
        set;
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor.Id == "RAV0612");
    }

    [Fact]
    public void PartialPropertyImplementationCannotBeAutoProperty()
    {
        const string source = """
partial class C {
    partial var Name: string {
        get;
        set;
    }
}

partial class C {
    partial var Name: string = ""
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor.Id == "RAV0613");
    }
}
