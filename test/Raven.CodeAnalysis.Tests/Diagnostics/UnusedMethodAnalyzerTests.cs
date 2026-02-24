using System.Linq;

using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class UnusedMethodAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void ConsoleApplication_UnusedMethod_ReportsDiagnostic()
    {
        const string code = """
val c = C()
c.Used()

class C {
    public func Used() -> () { }
    public func Unused() -> () { }
}
""";

        var verifier = CreateAnalyzerVerifier<UnusedMethodAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(UnusedMethodAnalyzer.DiagnosticId)
                    .WithLocation(6, 17)
                    .WithArguments("Unused")
            ],
            disabledDiagnostics:
            [
                CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id
            ]);

        verifier.Verify();
    }

    [Fact]
    public void ConsoleApplication_EntryPoint_IsNotReported()
    {
        const string code = """
class Program {
    static func Main() -> int { 0 }
}
""";

        var verifier = CreateAnalyzerVerifier<UnusedMethodAnalyzer>(
            code,
            disabledDiagnostics:
            [
                CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id
            ]);

        verifier.Verify();
    }

    [Fact]
    public void Library_PublicUnusedMethod_DoesNotReportDiagnostic()
    {
        const string code = """
class C {
    public func Unused() -> () { }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "lib",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var diagnostics = new UnusedMethodAnalyzer()
            .Analyze(compilation)
            .Where(d => d.Id == UnusedMethodAnalyzer.DiagnosticId)
            .ToArray();

        Assert.Empty(diagnostics);
    }

    [Fact]
    public void Library_InternalUnusedMethod_ReportsDiagnostic()
    {
        const string code = """
class C {
    internal func Unused() -> () { }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "lib",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var diagnostics = new UnusedMethodAnalyzer()
            .Analyze(compilation)
            .Where(d => d.Id == UnusedMethodAnalyzer.DiagnosticId)
            .ToArray();

        Assert.Single(diagnostics);
        Assert.Equal("Unused", diagnostics[0].GetMessageArgs().FirstOrDefault()?.ToString());
    }
}
