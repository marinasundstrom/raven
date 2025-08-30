using System.IO;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ClassInheritanceTests : CompilationTestBase
{
    [Fact]
    public void SealedBaseClass_DerivationProducesDiagnostic()
    {
        var source = """
class Parent {};
class Derived : Parent {};
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.False(result.Success);
        var diagnostic = Assert.Single(result.Diagnostics);
        Assert.Equal("RAV0306", diagnostic.Descriptor.Id);
    }
}
