using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class PartialEventTests : CompilationTestBase
{
    [Fact]
    public void PartialEventDefinitionAndImplementation_MergeIntoSingleSymbol()
    {
        const string source = """
partial class C {
    partial event Changed: System.Action;
}

partial class C {
    partial event Changed: System.Action {
        add { }
        remove { }
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");
        compilation.EnsureSetup();
        Assert.Empty(compilation.GetDiagnostics());

        var events = tree.GetRoot().DescendantNodes().OfType<EventDeclarationSyntax>().ToArray();
        Assert.Equal(2, events.Length);

        var model = compilation.GetSemanticModel(tree);
        var first = Assert.IsAssignableFrom<IEventSymbol>(model.GetDeclaredSymbol(events[0]));
        var second = Assert.IsAssignableFrom<IEventSymbol>(model.GetDeclaredSymbol(events[1]));
        var containingType = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol((TypeDeclarationSyntax)events[0].Parent!));

        Assert.Same(first, second);
        Assert.Single(containingType.GetMembers("Changed").OfType<IEventSymbol>());
    }

    [Fact]
    public void PartialEventWithoutImplementation_ReportsDiagnostic()
    {
        const string source = """
partial class C {
    partial event Changed: System.Action;
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor.Id == "RAV0614");
    }

    [Fact]
    public void PartialEventWithoutDefinition_ReportsDiagnostic()
    {
        const string source = """
partial class C {
    partial event Changed: System.Action {
        add { }
        remove { }
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor.Id == "RAV0615");
    }

    [Fact]
    public void PartialEventInNonPartialType_ReportsDiagnostic()
    {
        const string source = """
class C {
    partial event Changed: System.Action;
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor.Id == "RAV0616");
    }
}
