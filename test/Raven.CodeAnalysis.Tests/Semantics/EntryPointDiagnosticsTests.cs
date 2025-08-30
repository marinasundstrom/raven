using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class EntryPointDiagnosticsTests
{
    [Fact(Skip = "Requires reference assemblies in this environment")]
    public void ConsoleApp_WithoutMain_ProducesDiagnostic()
    {
        var tree = SyntaxTree.ParseText("");
        var compilation = Compilation.Create("app", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));
        var diagnostics = compilation.GetDiagnostics();
        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint);
    }
}
