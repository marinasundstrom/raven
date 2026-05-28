using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class UnusedLocalAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void Analyzer_RegistersGlobalAndOwnerScopedSyntaxActions()
    {
        var analyzer = new UnusedLocalAnalyzer();

        Assert.True(analyzer.TryEnsureInitialized());
        Assert.Equal(2, analyzer.SyntaxNodeActions.Count);
        Assert.All(analyzer.SyntaxNodeActions, registration => Assert.Equal(SyntaxNodeAnalysisScope.Document, registration.Scope));
        Assert.Contains(analyzer.SyntaxNodeActions, registration => registration.Kinds.SequenceEqual([SyntaxKind.CompilationUnit]));
        var expectedKinds = new[]
            {
                SyntaxKind.MethodDeclaration,
                SyntaxKind.FunctionStatement,
                SyntaxKind.ConstructorDeclaration,
                SyntaxKind.OperatorDeclaration,
                SyntaxKind.ConversionOperatorDeclaration,
                SyntaxKind.SimpleFunctionExpression,
                SyntaxKind.ParenthesizedFunctionExpression
            }
            .OrderBy(static kind => (int)kind)
            .ToArray();

        Assert.Contains(analyzer.SyntaxNodeActions, registration => registration.Kinds.SequenceEqual(expectedKinds));
    }

    [Fact]
    public void UnusedLocal_ReportsDiagnostic()
    {
        const string code = """
class C {
    public func M() -> unit {
        val count = 0
    }
}
""";

        var diagnostic = Assert.Single(Analyze(code));

        Assert.Equal("Value 'count' is never used.", diagnostic.GetMessage());
        Assert.Equal("count", diagnostic.GetMessageArgs().FirstOrDefault()?.ToString());
    }

    [Fact]
    public void ReadLocal_DoesNotReportDiagnostic()
    {
        const string code = """
class C {
    public func M() -> unit {
        val count = 0
        Print(count)
    }

    private func Print(value: int) -> unit { }
}
""";

        Assert.Empty(Analyze(code));
    }

    [Fact]
    public void LocalPassedToImportedNamespaceFunction_IsUsed()
    {
        const string mainCode = """
import Utilities.*

func Main() {
    val x = A(42)
    A(x)

    use test = Test2()
}
""";
        const string utilitiesCode = """
namespace Utilities

import System.*

func A(x: int) -> int {
    42 + x
}

func Test2() -> IDisposable {
    return default
}
""";

        var diagnostics = Analyze(mainCode, utilitiesCode);

        var diagnostic = Assert.Single(diagnostics);
        Assert.Equal("Value 'test' is never used.", diagnostic.GetMessage());
    }

    [Fact]
    public void UnusedParameter_IsNotReportedByLocalAnalyzer()
    {
        const string code = """
class C {
    public func M(value: int) -> unit {
    }
}
""";

        Assert.Empty(Analyze(code));
    }

    [Fact]
    public void UseLocalReadInScope_DoesNotReportDiagnostic()
    {
        const string code = """
import System.*

class Resource : IDisposable {
    public func Dispose() -> unit {
    }
}

class C {
    public func M() -> unit {
        use resource = Resource()
        resource.Dispose()
    }
}
""";

        Assert.Empty(Analyze(code));
    }

    [Fact]
    public void UseLocalReadInInBlock_DoesNotReportDiagnostic()
    {
        const string code = """
import System.*

class Resource : IDisposable {
    public func Dispose() -> unit {
    }
}

class C {
    public func M() -> unit {
        use resource = Resource() in {
            resource.Dispose()
        }
    }
}
""";

        Assert.Empty(Analyze(code));
    }

    [Fact]
    public void UnusedUseLocal_ReportsDiagnostic()
    {
        const string code = """
import System.*

class Resource : IDisposable {
    public func Dispose() -> unit {
    }
}

class C {
    public func M() -> unit {
        use resource = Resource()
    }
}
""";

        var diagnostic = Assert.Single(Analyze(code));

        Assert.Equal("Value 'resource' is never used.", diagnostic.GetMessage());
        Assert.Equal("resource", diagnostic.GetMessageArgs().FirstOrDefault()?.ToString());
    }

    [Fact]
    public void NonCandidateIdentifiers_AreSkippedBeforeSemanticSymbolLookup()
    {
        var instrumentation = new PerformanceInstrumentation();
        const string code = """
class C {
    public func M() -> unit {
        val count = 0
        val text = "Raven"
        Print(1)
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
        var diagnostics = new UnusedLocalAnalyzer()
            .Analyze(compilation)
            .Where(d => d.Id == UnusedLocalAnalyzer.DiagnosticId)
            .ToArray();
        var delta = SemanticQueryInstrumentation.Subtract(
            instrumentation.SemanticQuery.CaptureSnapshot(),
            before);

        Assert.Equal(2, diagnostics.Length);
        Assert.Equal(0, delta.SymbolInfoQueries);
    }

    private static Diagnostic[] Analyze(string code)
        => Analyze([code]);

    private static Diagnostic[] Analyze(params string[] sources)
    {
        var trees = sources.Select(static source => SyntaxTree.ParseText(source)).ToArray();
        var compilation = Compilation.Create(
                "lib",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(trees)
            .AddReferences(TestMetadataReferences.Default);

        return new UnusedLocalAnalyzer()
            .Analyze(compilation)
            .Where(d => d.Id == UnusedLocalAnalyzer.DiagnosticId)
            .ToArray();
    }
}
