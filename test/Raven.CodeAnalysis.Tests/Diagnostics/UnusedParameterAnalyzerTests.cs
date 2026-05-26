using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class UnusedParameterAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void Analyzer_RegistersMethodSymbolAction()
    {
        var analyzer = new UnusedParameterAnalyzer();

        Assert.True(analyzer.TryEnsureInitialized());
        Assert.Empty(analyzer.SyntaxNodeActions);

        var registration = Assert.Single(analyzer.SymbolActions);
        Assert.Equal([SymbolKind.Method], registration.Kinds.ToArray());
    }

    [Fact]
    public void UnusedMethodParameter_ReportsDiagnostic()
    {
        const string code = """
class C {
    public func M(value: int) -> unit {
    }
}
""";

        var diagnostic = Assert.Single(Analyze(code));

        Assert.Equal("Parameter 'value' is never used.", diagnostic.GetMessage());
        Assert.Equal("value", diagnostic.GetMessageArgs().FirstOrDefault()?.ToString());
    }

    [Fact]
    public void UnusedLocal_IsNotReportedByParameterAnalyzer()
    {
        const string code = """
class C {
    public func M() -> unit {
        val count = 0
    }
}
""";

        Assert.Empty(Analyze(code));
    }

    [Fact]
    public void ConstructorParametersAssignedToFields_DoNotReportDiagnostic()
    {
        const string code = """
class C {
    private val name: string
    private val count: int

    init(name: string, count: int) {
        self.name = name
        self.count = count
    }
}
""";

        Assert.Empty(Analyze(code));
    }

    [Fact]
    public void ConstructorParametersAssignedToGenericFields_DoNotReportDiagnostic()
    {
        const string code = """
import System.Collections.Generic.*

class UiControl {
}

class UiStackPanel : UiControl {
    private val children: List<UiControl>
    private val spacing: double

    init(children: List<UiControl>, spacing: double) {
        self.children = children
        self.spacing = spacing
    }

    val Children: IEnumerable<UiControl> => children
    val Spacing: double => spacing
}
""";

        Assert.Empty(Analyze(code));
    }

    [Fact]
    public void ConstructorParametersAssignedToFields_AfterCompilerDiagnostics_DoNotReportDiagnostic()
    {
        const string code = """
class C {
    private val name: string
    private val count: int

    init(name: string, count: int) {
        self.name = name
        self.count = count
    }
}
""";

        Assert.Empty(AnalyzeAfterCompilerDiagnostics(code));
    }

    [Fact]
    public void ConstructorWithOneUnusedParameter_ReportsOnlyThatParameter()
    {
        const string code = """
class UiStackPanel {
}

class UiWindow {
    private val title: string

    init(content: UiStackPanel, title: string) {
        Content = content
    }

    val Content: UiStackPanel
    val Title: string => title
}
""";

        var diagnostic = Assert.Single(AnalyzeAfterCompilerDiagnostics(code));

        Assert.Equal("Parameter 'title' is never used.", diagnostic.GetMessage());
    }

    [Fact]
    public void ConstructorWithOneUnusedParameter_WorkspacePath_ReportsOnlyThatParameter()
    {
        const string code = """
class UiStackPanel {
}

class UiWindow {
    private val title: string

    init(content: UiStackPanel, title: string) {
        Content = content
    }

    val Content: UiStackPanel
    val Title: string => title
}
""";

        var verifier = CreateAnalyzerVerifier<UnusedParameterAnalyzer>(
            code,
            expectedDiagnostics: [
                new DiagnosticResult(UnusedParameterAnalyzer.DiagnosticId)
                    .WithSpan(7, 33, 7, 38)
                    .WithArguments("title")
            ],
            disabledDiagnostics: [CompilerDiagnostics.ConsoleApplicationRequiresEntryPoint.Id]);

        verifier.Verify();
    }

    [Fact]
    public void ConstructorWithUnresolvedBodyExpression_StillReportsDefinitelyUnusedParameter()
    {
        const string code = """
class UiStackPanel {
}

class UiWindow {
    private val title: string

    init(content: UiStackPanel, title: string) {
        Content = content
        MissingCall()
    }

    val Content: UiStackPanel
    val Title: string => title
}
""";

        var diagnostic = Assert.Single(AnalyzeAfterCompilerDiagnostics(code));
        Assert.Equal("Parameter 'title' is never used.", diagnostic.GetMessage());
    }

    [Fact]
    public void ConstructorWithIncompleteSemanticUsage_DoesNotReportOtherParametersAsUnused()
    {
        const string code = """
class UiControl {
}

class List<T> {
}

class Orientation {
}

class UiStackPanel : UiControl {
    private val children: List<UiControl>
    private val orientation: Orientation
    private val spacing: double

    init(children: List<UiControl>, orientation: Orientation, spacing: double) {
        self.children = children
        self.orientation = orientation
        self.spacing = spacing
        MissingCall()
    }
}
""";

        Assert.Empty(AnalyzeAfterCompilerDiagnostics(code));
    }

    [Fact]
    public void ParameterUsedAsForSource_DoesNotReportDiagnostic()
    {
        const string code = """
class C {
    public func M(items: int[]) -> int {
        var total = 0

        for item in items {
            total += item
        }

        return total
    }
}
""";

        Assert.Empty(Analyze(code));
    }

    [Fact]
    public void ParameterForwardedToInvocation_DoesNotReportDiagnostic()
    {
        const string code = """
class C {
    public func M(items: int[]) -> int {
        return Count(items)
    }

    private func Count(items: int[]) -> int {
        return items.Length
    }
}
""";

        Assert.Empty(Analyze(code));
    }

    [Fact]
    public void BuilderParameterUsedAsForSourceAndForwarded_DoNotReportDiagnostic()
    {
        const string code = """
class UiNode {
    func AddRange(node: UiNode) -> unit {
        node.ToString()
    }
}

class UiBuilder {
    static func BuildBlock(items: UiNode[]) -> UiNode {
        val result = UiNode()

        for item in items {
            result.AddRange(item)
        }

        return result
    }

    static func BuildArray(items: UiNode[]) -> UiNode {
        return BuildBlock(items)
    }
}
""";

        Assert.Empty(Analyze(code));
    }

    [Fact]
    public void BuilderParameterUsedAsForSource_AfterCompilerDiagnostics_DoesNotReportDiagnostic()
    {
        const string code = """
class UiNode {
    func AddRange(node: UiNode) -> unit {
        node.ToString()
    }
}

class UiBuilder {
    static func BuildBlock(items: UiNode[]) -> UiNode {
        val result = UiNode()

        for item in items {
            result.AddRange(item)
        }

        return result
    }
}
""";

        Assert.Empty(AnalyzeAfterCompilerDiagnostics(code));
    }

    private static Diagnostic[] Analyze(string code)
        => AnalyzeCore(code, runCompilerDiagnosticsFirst: false);

    private static Diagnostic[] AnalyzeAfterCompilerDiagnostics(string code)
        => AnalyzeCore(code, runCompilerDiagnosticsFirst: true);

    private static Diagnostic[] AnalyzeCore(string code, bool runCompilerDiagnosticsFirst)
    {
        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "lib",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        if (runCompilerDiagnosticsFirst)
            _ = compilation.GetDiagnostics();

        return new UnusedParameterAnalyzer()
            .Analyze(compilation)
            .Where(d => d.Id == UnusedParameterAnalyzer.DiagnosticId)
            .ToArray();
    }
}
