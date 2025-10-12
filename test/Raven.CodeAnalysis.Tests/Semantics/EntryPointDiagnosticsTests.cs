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

    [Fact]
    public void ConsoleApp_WithMultipleMainMethods_ProducesAmbiguousDiagnostic()
    {
        var code = """
class Program {
    static Main() -> unit {
        return;
    }
}

class Helper {
    static Main() -> unit {
        return;
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create("app", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.EntryPointIsAmbiguous);
        Assert.DoesNotContain(diagnostics, d => d.Descriptor == CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint);
    }

    [Fact]
    public void TopLevelStatements_WithUserDefinedMain_ProducesAmbiguousDiagnostic()
    {
        var topLevel = SyntaxTree.ParseText("let x = 0");
        var mainClass = SyntaxTree.ParseText("""
class App {
    static Main() -> unit {
        return;
    }
}
""");

        var compilation = Compilation.Create("app", new[] { topLevel, mainClass }, TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.EntryPointIsAmbiguous);
        Assert.DoesNotContain(diagnostics, d => d.Descriptor == CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint);
    }
}
