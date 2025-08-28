using System.IO;
using System.Linq;
using System.Reflection;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ClassInheritanceTests
{
    [Fact]
    public void SealedBaseClass_DerivationProducesDiagnostic()
    {
        var source = """
class Parent {};
class Derived : Parent {};
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("lib", [tree], new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.False(result.Success);
        var diagnostic = Assert.Single(result.Diagnostics);
        Assert.Equal("RAV0306", diagnostic.Descriptor.Id);
    }
}
