using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class SemanticModelDiagnosticsTests
{
    [Fact]
    public void GetDiagnostics_CollectsMethodBodyDiagnostics()
    {
        var source = """
class Program {
    void M() {
        1();
    }
}
""";
        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", new[] { syntaxTree }, new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(syntaxTree);

        var diagnostics = model.GetDiagnostics();

        Assert.Contains(diagnostics, d => d.Descriptor == SemanticDiagnostics.InvalidInvocation);
    }
}
