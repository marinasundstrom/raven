using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class UnusedVariableAnalyzerTests : AnalyzerTestBase
{
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

        var diagnostics = Analyze(code);

        var diagnostic = Assert.Single(diagnostics);
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
    public void CapturedLocal_DoesNotReportDiagnostic()
    {
        const string code = """
class C {
    public func M() -> unit {
        val count = 0
        val print = func () => count
        print()
    }
}
""";

        Assert.Empty(Analyze(code));
    }

    [Fact]
    public void DeconstructionDeclaration_ReportsUnusedBindings()
    {
        const string code = """
class C {
    public func M() -> unit {
        val [first, second] = [1, 2]
        Print(first)
    }

    private func Print(value: int) -> unit { }
}
""";

        var diagnostics = Analyze(code);

        var diagnostic = Assert.Single(diagnostics);
        Assert.Equal("second", diagnostic.GetMessageArgs().FirstOrDefault()?.ToString());
    }

    [Fact]
    public void IfPatternBinding_UsedInsideBody_DoesNotReportDiagnostic()
    {
        const string code = """
class C {
    public func M() -> unit {
        if val x = 1 {
            Print(x)
        }
    }

    private func Print(value: int) -> unit { }
}
""";

        Assert.Empty(Analyze(code));
    }

    [Fact]
    public void SequenceDeclarationBindings_UsedAcrossWrites_DoNotReportDiagnostic()
    {
        const string code = """
class C {
    public func M() -> unit {
        val values = [1, 2, 3]
        val [first, second, ...rest] = values

        Print(first)
        Print(second)

        for item in rest {
            Print(item)
        }
    }

    private func Print(value: int) -> unit { }
}
""";

        Assert.Empty(Analyze(code));
    }

    [Fact]
    public void ForPatternBinding_UsedInsideLoopBody_DoesNotReportDiagnostic()
    {
        const string code = """
class C {
    public func M() -> unit {
        val points = [(1, 2), (3, 4)]

        for val (x, > 0) in points {
            Print(x)
        }
    }

    private func Print(value: int) -> unit { }
}
""";

        Assert.Empty(Analyze(code));
    }

    [Fact]
    public void ForTupleDeconstructionBindings_UsedInsideLoopBody_DoesNotReportDiagnostic()
    {
        const string code = """
class C {
    public func M() -> unit {
        val dict = ["a": 1, "b": 2]

        for val (key, value) in dict {
            PrintPair(key, value)
        }
    }

    private func PrintPair(key: string, value: int) -> unit { }
}
""";

        Assert.Empty(Analyze(code));
    }

    [Fact]
    public void SpreadSource_DoesNotReportDiagnostic()
    {
        const string code = """
class C {
    public func M() -> unit {
        val dict = ["a": 1, "b": 2, "c": 3]
        val dict2 = ["e": 5, ...dict]
    }
}
""";

        var diagnostics = Analyze(code);

        var diagnostic = Assert.Single(diagnostics);
        Assert.Equal("dict2", diagnostic.GetMessageArgs().FirstOrDefault()?.ToString());
    }

    [Fact]
    public void BareIdentifierValuePatternInMatch_DoesNotReportDiagnostic()
    {
        const string code = """
class C {
    public func M() -> string {
        val expected = 1
        val actual = 1

        val result = actual match {
            expected => "ok"
            _ => "no"
        }

        return result
    }
}
""";

        Assert.Empty(Analyze(code));
    }

    [Fact]
    public void BareIdentifierValuePatternInIsExpression_DoesNotReportDiagnostic()
    {
        const string code = """
class C {
    public func M() -> unit {
        val expected = 1
        val actual = 1
        val matches = actual is expected

        Print(matches)
    }

    private func Print(value: bool) -> unit { }
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

        return new UnusedVariableAnalyzer()
            .Analyze(compilation)
            .Where(d => d.Id == UnusedVariableAnalyzer.DiagnosticId)
            .ToArray();
    }
}
