using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class PartialClassDiagnosticsTests : CompilationTestBase
{
    [Fact]
    public void DuplicateClassDeclarationsWithoutPartial_ProduceDiagnostic()
    {
        const string source = """
class C {};
class C {};
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), assemblyName: "lib");
        var diagnostics = compilation.GetDiagnostics();
        var diagnostic = Assert.Single(diagnostics);
        Assert.Equal(CompilerDiagnostics.TypeAlreadyDefined.Id, diagnostic.Descriptor.Id);
    }
}
