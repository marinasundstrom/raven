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

    private static Diagnostic[] Analyze(string code)
    {
        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "lib",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        return new UnusedLocalAnalyzer()
            .Analyze(compilation)
            .Where(d => d.Id == UnusedLocalAnalyzer.DiagnosticId)
            .ToArray();
    }
}
