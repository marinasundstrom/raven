using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class VarCanBeLetAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void VarLocal_NotReassigned_ReportsDiagnostic()
    {
        const string code = """
class C {
    public func M() -> unit {
        var count = 0
    }
}
""";

        var verifier = CreateAnalyzerVerifier<VarCanBeLetAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(VarCanBeLetAnalyzer.DiagnosticId)
                    .WithLocation(3, 9)
                    .WithArguments("count")
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void VarLocal_Reassigned_NoDiagnostic()
    {
        const string code = """
class C {
    public func M() -> unit {
        var count = 0
        count = 1
    }
}
""";

        var verifier = CreateAnalyzerVerifier<VarCanBeLetAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void VarGlobal_NotReassigned_ReportsDiagnostic()
    {
        const string code = "var count = 0";

        var verifier = CreateAnalyzerVerifier<VarCanBeLetAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(VarCanBeLetAnalyzer.DiagnosticId)
                    .WithLocation(1, 1)
                    .WithArguments("count")
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void VarGlobal_Reassigned_NoDiagnostic()
    {
        const string code = """
var count = 0
count = 1
""";

        var verifier = CreateAnalyzerVerifier<VarCanBeLetAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void VarLocal_ReassignedByTupleDeconstruction_NoDiagnostic()
    {
        const string code = """
class C {
    public func M() -> unit {
        var a = 1, b = 2
        (b, a) = (a, b)
    }
}
""";

        var verifier = CreateAnalyzerVerifier<VarCanBeLetAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void VarLocal_ReassignedBySequenceDeconstruction_NoDiagnostic()
    {
        const string code = """
class C {
    public func M() -> unit {
        var first = 0, second = 0, third = 0
        [first, second, third] = [1, 2, 3]
    }
}
""";

        var verifier = CreateAnalyzerVerifier<VarCanBeLetAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void VarLocal_CapturedByBlockLambda_NoDiagnostic()
    {
        const string code = """
class C {
    public func M() -> unit {
        var count = 0
        val read = func () -> int {
            return count
        }

        Consume(read())
    }

    private func Consume(value: int) -> unit { }
}
""";

        var verifier = CreateAnalyzerVerifier<VarCanBeLetAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void VarLocal_PassedAsOutArgument_NoDiagnostic()
    {
        const string code = """
class C {
    public func M(arg: string) -> unit {
        var total = 0

        int.TryParse(arg, out total)
    }
}
""";

        var verifier = CreateAnalyzerVerifier<VarCanBeLetAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id],
            frameworkProjectionMode: FrameworkProjectionMode.None);

        verifier.Verify();
    }

    [Fact]
    public void VarLocal_ReassignedInsideStatementBodiedLoops_NoDiagnostic()
    {
        const string code = """
class C {
    public func M(values: int[]) -> unit {
        var total = 0

        for value in values
            total += value

        while total < 100
            total += 1
    }
}
""";

        var verifier = CreateAnalyzerVerifier<VarCanBeLetAnalyzer>(
            code,
            expectedDiagnostics: [],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void VarLocal_WithGenericExtensionLambdaChain_DoesNotRecurseDuringCaptureAnalysis()
    {
        const string code = """
import System.*
import System.Linq.*
import System.Collections.Generic.*
import System.Linq.Expressions.*

class User(var Name: string, var IsActive: bool)

class C {
    public func Run(users: IQueryable<User>) -> unit {
        var query = users
            |> Where(user => user.IsActive)
            |> Select(user => user.Name)
    }
}
""";

        var verifier = CreateAnalyzerVerifier<VarCanBeLetAnalyzer>(
            code,
            expectedDiagnostics:
            [
                new DiagnosticResult(VarCanBeLetAnalyzer.DiagnosticId)
                    .WithLocation(10, 9)
                    .WithArguments("query")
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void OwnerWithoutVarLocals_SkipsSemanticSymbolLookup()
    {
        var instrumentation = new PerformanceInstrumentation();
        const string code = """
class C {
    public func M() -> unit {
        val count = 0
        Print(count)
    }

    private func Print(value: int) -> unit {
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "lib",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                    .WithPerformanceInstrumentation(instrumentation))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var before = instrumentation.SemanticQuery.CaptureSnapshot();
        var diagnostics = new VarCanBeLetAnalyzer()
            .Analyze(compilation)
            .Where(d => d.Id == VarCanBeLetAnalyzer.DiagnosticId)
            .ToArray();
        var delta = SemanticQueryInstrumentation.Subtract(
            instrumentation.SemanticQuery.CaptureSnapshot(),
            before);

        Assert.Empty(diagnostics);
        Assert.Equal(0, delta.SymbolInfoQueries);
    }

    [Fact]
    public void NonCandidateAssignments_AreSkippedBeforeSemanticSymbolLookup()
    {
        var instrumentation = new PerformanceInstrumentation();
        const string code = """
class C {
    private var field: int = 0

    public func M() -> unit {
        var count = 0
        self.field = 1
    }
}
""";

        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "lib",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                    .WithPerformanceInstrumentation(instrumentation))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var before = instrumentation.SemanticQuery.CaptureSnapshot();
        var diagnostics = new VarCanBeLetAnalyzer()
            .Analyze(compilation)
            .Where(d => d.Id == VarCanBeLetAnalyzer.DiagnosticId)
            .ToArray();
        var delta = SemanticQueryInstrumentation.Subtract(
            instrumentation.SemanticQuery.CaptureSnapshot(),
            before);

        var diagnostic = Assert.Single(diagnostics);
        Assert.Equal("count", diagnostic.GetMessageArgs().Single());
        Assert.Equal(0, delta.SymbolInfoQueries);
    }
}
