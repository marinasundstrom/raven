using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class NamespaceDirectiveTests
{
    [Fact]
    public void FileScopedNamespaceDirective_AppliesToSynthesizedProgram()
    {
        const string source = """
        namespace Samples

        System.Console.WriteLine("hi");
        """;

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "app",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var diagnostics = compilation.GetDiagnostics();
        Assert.Empty(diagnostics);

        var programType = compilation.GetTypeByMetadataName("Samples.Program");
        Assert.NotNull(programType);

        var containingNamespace = programType!.ContainingNamespace;
        Assert.NotNull(containingNamespace);
        Assert.Equal("Samples", containingNamespace!.ToString());
    }
}

