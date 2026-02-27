using System.Linq;

using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class UnusedPropertyAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void ConsoleApplication_UnusedProperty_ReportsDiagnostic()
    {
        const string code = """
val x = 0

class C {
    public val Name: string = "Raven"
}
""";

        var verifier = CreateAnalyzerVerifier<UnusedPropertyAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(UnusedPropertyAnalyzer.DiagnosticId)
                    .WithLocation(4, 16)
                    .WithArguments("Name")
            ],
            disabledDiagnostics:
            [
                CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id
            ]);

        verifier.Verify();
    }

    [Fact]
    public void ConsoleApplication_UsedProperty_DoesNotReportDiagnostic()
    {
        const string code = """
val c = C()
val x = c.Name

class C {
    public val Name: string = "Raven"
}
""";

        var verifier = CreateAnalyzerVerifier<UnusedPropertyAnalyzer>(
            code,
            disabledDiagnostics:
            [
                CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id
            ]);

        verifier.Verify();
    }

    [Fact]
    public void Library_PublicUnusedProperty_DoesNotReportDiagnostic()
    {
        const string code = """
class C {
    public val Name: string = "Raven"
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "lib",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var diagnostics = new UnusedPropertyAnalyzer()
            .Analyze(compilation)
            .Where(d => d.Id == UnusedPropertyAnalyzer.DiagnosticId)
            .ToArray();

        Assert.Empty(diagnostics);
    }

    [Fact]
    public void PrivateStoredProperty_AccessedViaIdentifier_DoesNotReportDiagnostic()
    {
        // Regression: private stored property syntax should resolve as property usage in symbol info.
        const string code = """
val x = 0

class Counter {
    private var count: int = 0
    private var name: string = ""

    func Increment() -> () {
        count = count + 1
    }

    func GetName() -> string => name
}
""";

        var verifier = CreateAnalyzerVerifier<UnusedPropertyAnalyzer>(
            code,
            disabledDiagnostics:
            [
                CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id
            ]);

        verifier.Verify();
    }

    [Fact]
    public void PrivateStoredProperty_AccessedViaMemberAccess_DoesNotReportDiagnostic()
    {
        // Regression: member access to stored properties should resolve directly as property usage.
        const string code = """
val x = 0

class Person {
    private var age: int = 0
    private var roles: System.Collections.Generic.List<string> = []

    init(a: int) {
        self.age = a
    }

    func AddRole(role: string) -> () {
        roles.Add(role)
    }
}
""";

        var verifier = CreateAnalyzerVerifier<UnusedPropertyAnalyzer>(
            code,
            disabledDiagnostics:
            [
                CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id
            ]);

        verifier.Verify();
    }

    [Fact]
    public void PrivateStoredProperty_NeverAccessed_ReportsDiagnostic()
    {
        // A private stored property that is genuinely never read or written should still warn.
        const string code = """
class Foo {
    private val unused: int = 42
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "lib",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var diagnostics = new UnusedPropertyAnalyzer()
            .Analyze(compilation)
            .Where(d => d.Id == UnusedPropertyAnalyzer.DiagnosticId)
            .ToArray();

        Assert.Single(diagnostics);
        Assert.Equal("unused", diagnostics[0].GetMessageArgs().FirstOrDefault()?.ToString());
    }

    [Fact]
    public void Library_InternalUnusedProperty_ReportsDiagnostic()
    {
        const string code = """
class C {
    internal val Name: string = "Raven"
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "lib",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var diagnostics = new UnusedPropertyAnalyzer()
            .Analyze(compilation)
            .Where(d => d.Id == UnusedPropertyAnalyzer.DiagnosticId)
            .ToArray();

        Assert.Single(diagnostics);
        Assert.Equal("Name", diagnostics[0].GetMessageArgs().FirstOrDefault()?.ToString());
    }
}
