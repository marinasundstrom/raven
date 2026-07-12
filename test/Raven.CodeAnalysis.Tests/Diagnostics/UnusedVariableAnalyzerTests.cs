using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Diagnostics;

public class UnusedVariableAnalyzerTests : AnalyzerTestBase
{
    [Fact]
    public void Analyzer_RegistersGlobalAndOwnerScopedSyntaxActions()
    {
        var analyzer = new UnusedVariableAnalyzer();

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

        var diagnostics = Analyze(code);

        var diagnostic = Assert.Single(diagnostics);
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
    public void UnusedMethodParameter_ReportsDiagnostic()
    {
        const string code = """
class C {
    public func M(value: int) -> unit {
    }
}
""";

        var diagnostics = AnalyzeParameters(code);

        var diagnostic = Assert.Single(diagnostics);
        Assert.Equal("Parameter 'value' is never used.", diagnostic.GetMessage());
        Assert.Equal("value", diagnostic.GetMessageArgs().FirstOrDefault()?.ToString());
    }

    [Fact]
    public void ConstructorParameterForwardedToBaseInitializer_DoesNotReportDiagnostic()
    {
        const string code = """
open class Base {
    private val stored: object

    init(value: object) {
        stored = value
    }
}

class Derived : Base {
    init(value: object) : base(value) {
    }
}
""";

        Assert.Empty(AnalyzeParameters(code));
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

        Assert.Empty(AnalyzeParameters(code));
    }

    [Fact]
    public void OverrideMethodParameter_DoesNotReportDiagnostic()
    {
        const string code = """
open class Base {
    public virtual func Transform(value: int) -> int {
        value
    }
}

class Derived : Base {
    public override func Transform(value: int) -> int {
        0
    }
}
""";

        Assert.Empty(AnalyzeParameters(code));
    }

    [Fact]
    public void InterfaceImplementationParameter_DoesNotReportDiagnostic()
    {
        const string code = """
interface ITransformer {
    func Transform(value: int) -> int
}

class Transformer : ITransformer {
    func Transform(value: int) -> int {
        0
    }
}
""";

        Assert.Empty(AnalyzeParameters(code));
    }

    [Fact]
    public void ReadMethodParameter_DoesNotReportDiagnostic()
    {
        const string code = """
class C {
    public func M(value: int) -> int {
        value
    }
}
""";

        Assert.Empty(AnalyzeParameters(code));
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

        Assert.Empty(AnalyzeParameters(code));
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

        Assert.Empty(AnalyzeParameters(code));
    }

    [Fact]
    public void CapturedMethodParameter_DoesNotReportDiagnostic()
    {
        const string code = """
class C {
    public func M(value: int) -> int {
        val read = func () => value
        read()
    }
}
""";

        Assert.Empty(AnalyzeParameters(code));
    }

    [Fact]
    public void UnusedFunctionStatementParameter_ReportsDiagnostic()
    {
        const string code = """
class C {
    public func M() -> unit {
        func Local(value: int) -> unit {
        }

        Local(1)
    }
}
""";

        var diagnostics = AnalyzeParameters(code);

        var diagnostic = Assert.Single(diagnostics);
        Assert.Equal("value", diagnostic.GetMessageArgs().FirstOrDefault()?.ToString());
    }

    [Fact]
    public void UnusedFunctionExpressionParameter_ReportsDiagnostic()
    {
        const string code = """
class C {
    public func M() -> unit {
        val callback = func (value: int) -> unit {
        }

        callback(1)
    }
}
""";

        var diagnostics = AnalyzeParameters(code);

        var diagnostic = Assert.Single(diagnostics);
        Assert.Equal("value", diagnostic.GetMessageArgs().FirstOrDefault()?.ToString());
    }

    [Fact]
    public void UnusedFunctionExpressionDiscardParameter_DoesNotReportDiagnostic()
    {
        const string code = """
class C {
    public func M() -> unit {
        val callback: (int, int) -> int = func (value, _) => value

        callback(1, 2)
    }
}
""";

        Assert.Empty(AnalyzeParameters(code));
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
    public void IfPatternBinding_UsedAsAssignmentReceiver_DoesNotReportDiagnostic()
    {
        const string code = """
class Label {
    var Content: string = ""
}

class C {
    private var native: object? = null

    public func M(content: string) -> unit {
        if native is Label label {
            label.Content = content
        }
    }
}
""";

        Assert.Empty(Analyze(code));
    }

    [Fact]
    public void FunctionExpressionPatternBinding_UsedAsAssignmentReceiver_DoesNotReportDiagnostic()
    {
        const string code = """
class Button {
    var Content: string = ""
}

class C {
    public func M(sender: object) -> unit {
        val handler = func () -> unit {
            if sender is Button clickedButton {
                clickedButton.Content = "Clicked"
            }
        }

        handler()
    }
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

        val result = match actual {
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

    [Fact]
    public void InterpolatedStringShorthandReference_DoesNotReportDiagnostic()
    {
        const string code = """
class C {
    public func M() -> string {
        val content = "hello"
        return "submitted: $content"
    }
}
""";

        Assert.Empty(Analyze(code));
    }

    [Fact]
    public void InterpolatedStringBracedReference_DoesNotReportDiagnostic()
    {
        const string code = """
class C {
    public func M() -> string {
        val content = "hello"
        return "submitted: ${content}"
    }
}
""";

        Assert.Empty(Analyze(code));
    }

    [Fact]
    public void InterpolatedStringShorthandReference_InsideAsyncLambda_DoesNotReportDiagnostic()
    {
        const string code = """
import System.*

class RequestContext {
    public func ReadAsync() -> Task<string> { throw NotImplementedException() }
}

class C {
    public func M() -> unit {
        val app = 0

        MapPost(app, "/submit", async func (context: RequestContext) => {
            val content = await context.ReadAsync()
            return "submitted: $content"
        })
    }

    private func MapPost(app: int, path: string, handler: func (RequestContext) -> Task<string>) -> unit { }
}
""";

        Assert.Empty(Analyze(code));
    }

    [Fact]
    public void AnalyzeSingleTree_DoesNotRequestDocumentDiagnosticsForErrorGuard()
    {
        var instrumentation = new PerformanceInstrumentation();
        var targetTree = SyntaxTree.ParseText(
            """
class C {
    public func M() -> unit {
        val count = 0
    }
}
""",
            path: "/tmp/target.rav");
        var otherTree = SyntaxTree.ParseText(
            """
func Broken() -> unit {
    val value: MissingType = 0
}
""",
            path: "/tmp/other.rav");

        var compilation = Compilation.Create(
                "lib",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                    .WithPerformanceInstrumentation(instrumentation))
            .AddSyntaxTrees(targetTree, otherTree)
            .AddReferences(TestMetadataReferences.Default);

        var before = instrumentation.DiagnosticBinding.CaptureSnapshot();
        var diagnostics = new UnusedVariableAnalyzer()
            .Analyze(compilation, targetTree)
            .Where(d => d.Id == UnusedVariableAnalyzer.DiagnosticId)
            .ToArray();
        var delta = DiagnosticBindingInstrumentation.Subtract(
            instrumentation.DiagnosticBinding.CaptureSnapshot(),
            before);

        Assert.Single(diagnostics);
        Assert.Equal(0, delta.CompleteCalls);
        Assert.Equal(0, delta.DocumentCalls);
    }

    [Fact]
    public void SyntaxError_SuppressesUnusedVariableDiagnosticWithoutDocumentDiagnostics()
    {
        var instrumentation = new PerformanceInstrumentation();
        var tree = SyntaxTree.ParseText(
            """
class C {
    public func M() -> unit {
        val count =
    }
}
""");
        var compilation = Compilation.Create(
                "lib",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                    .WithPerformanceInstrumentation(instrumentation))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        var before = instrumentation.DiagnosticBinding.CaptureSnapshot();
        var diagnostics = new UnusedVariableAnalyzer()
            .Analyze(compilation, tree)
            .Where(d => d.Id == UnusedVariableAnalyzer.DiagnosticId)
            .ToArray();
        var delta = DiagnosticBindingInstrumentation.Subtract(
            instrumentation.DiagnosticBinding.CaptureSnapshot(),
            before);

        Assert.Empty(diagnostics);
        Assert.Equal(0, delta.CompleteCalls);
        Assert.Equal(0, delta.DocumentCalls);
    }

    [Fact]
    public void GenericExtensionLambdaChain_DoesNotRecurseDuringUsageAnalysis()
    {
        const string code = """
import System.*
import System.Linq.*
import System.Collections.Generic.*
import System.Linq.Expressions.*

class User(var Name: string, var IsActive: bool)

class C {
    public func Run(users: IQueryable<User>) -> unit {
        val query = users
            |> Where(user => user.IsActive)
            |> Select(user => user.Name)

        Consume(query)
    }

    private func Consume(value: object) -> unit { }
}
""";

        Assert.Empty(Analyze(code));
    }

    private static Diagnostic[] Analyze(string code)
        => Analyze(code, UnusedVariableAnalyzer.DiagnosticId);

    private static Diagnostic[] AnalyzeParameters(string code)
        => Analyze(code, UnusedVariableAnalyzer.UnusedParameterDiagnosticId);

    private static Diagnostic[] Analyze(string code, string diagnosticId)
    {
        var tree = SyntaxTree.ParseText(code);
        var compilation = Compilation.Create(
                "lib",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(tree)
            .AddReferences(TestMetadataReferences.Default);

        return new UnusedVariableAnalyzer()
            .Analyze(compilation)
            .Where(d => d.Id == diagnosticId)
            .ToArray();
    }
}
