using System.IO;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class PartialMemberModifierTests : CompilationTestBase
{
    [Fact]
    public void PartialModifier_OnFieldAndMethod_ReportsDiagnostic()
    {
        const string source = """
class C {
    partial val x: int = 1;
    partial M() -> unit { }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");

        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);

        Assert.False(result.Success);
        var partialDiagnostics = result.Diagnostics.Where(d => d.Descriptor.Id == "RAV0332").ToArray();
        Assert.Equal(2, partialDiagnostics.Length);
    }

    [Fact]
    public void PartialModifier_OnConstructor_ReportsDiagnostic()
    {
        const string source = """
class C {
    partial init() { }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");

        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);

        Assert.False(result.Success);
        var diagnostic = Assert.Single(result.Diagnostics.Where(d => d.Descriptor.Id == "RAV0332"));
        Assert.Contains("partial", diagnostic.ToString());
    }
}
