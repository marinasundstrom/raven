using System.Linq;

using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class SemanticModelDiagnosticCachingTests : CompilationTestBase
{
    [Fact]
    public void GetDocumentDiagnostics_StoresBinderDiagnosticsUnderExecutableOwner()
    {
        var tree = SyntaxTree.ParseText("""
class C {
    func Test() {
        Missing()
    }
}
""");
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();

        var diagnostics = model.GetDocumentDiagnostics();

        Assert.Contains(diagnostics, diagnostic => diagnostic.Id == "RAV0103");

        Assert.True(model.TryGetCachedBoundDiagnostics(method, out var ownerDiagnostics));
        var diagnostic = Assert.Single(ownerDiagnostics.Where(diagnostic => diagnostic.Id == "RAV0103"));
        Assert.True(method.Span.IntersectsWith(diagnostic.Location.SourceSpan));
    }

    [Fact]
    public void SpeculativeInitializerBinding_DoesNotLeakLambdaParametersIntoDocumentDiagnostics()
    {
        var tree = SyntaxTree.ParseText("""
class Box {
    func Select(selector: int -> int) -> int {
        return selector(1)
    }
}

class C {
    static func Main(box: Box) -> () {
        val vehicle = box.Select(vehicle => vehicle)
        vehicle.ToString()
    }
}
""");
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();

        _ = model.GetDeclaredSymbol(declarator);

        var diagnostics = model.GetDocumentDiagnostics();

        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Id == "RAV0168");
    }

    [Fact]
    public void SpeculativeInitializerBinding_DoesNotSplitDeclaredAndReferencedLocalSymbols()
    {
        var tree = SyntaxTree.ParseText("""
class Box {
    func Select(selector: int -> int) -> int {
        return selector(1)
    }
}

class C {
    static func Main(box: Box) -> () {
        val vehicle = box.Select(vehicle => vehicle)
        vehicle.ToString()
    }
}
""");
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().Single();

        _ = model.GetDeclaredSymbol(declarator);

        var diagnostics = new UnusedVariableAnalyzer()
            .Analyze(compilation, tree)
            .Where(diagnostic => diagnostic.Id == UnusedVariableAnalyzer.DiagnosticId)
            .ToArray();

        Assert.Empty(diagnostics);
    }

    [Fact]
    public void GetSymbolInfo_ForStatement_DoesNotTriggerDiagnosticBinding()
    {
        var instrumentation = new PerformanceInstrumentation();
        var options = new CompilationOptions(
            OutputKind.DynamicallyLinkedLibrary,
            performanceInstrumentation: instrumentation);
        var tree = SyntaxTree.ParseText("""
func Main() {
label:
    goto label
    return
}
""");
        var compilation = CreateCompilation(tree, options: options);
        var model = compilation.GetSemanticModel(tree);
        var gotoStatement = tree.GetRoot().DescendantNodes().OfType<GotoStatementSyntax>().Single();

        var before = instrumentation.DiagnosticBinding.CaptureSnapshot();

        var info = model.GetSymbolInfo(gotoStatement);

        var delta = DiagnosticBindingInstrumentation.Subtract(
            instrumentation.DiagnosticBinding.CaptureSnapshot(),
            before);
        var symbol = Assert.IsAssignableFrom<ILabelSymbol>(info.Symbol);
        Assert.Equal("label", symbol.Name);
        Assert.Equal(0, delta.Calls);
    }

    [Fact]
    public void AnalyzeControlFlow_DoesNotTriggerDiagnosticBinding()
    {
        var instrumentation = new PerformanceInstrumentation();
        var options = new CompilationOptions(
            OutputKind.DynamicallyLinkedLibrary,
            performanceInstrumentation: instrumentation);
        var tree = SyntaxTree.ParseText("""
func Main() {
    goto target
target:
    return
}
""");
        var compilation = CreateCompilation(tree, options: options);
        var model = compilation.GetSemanticModel(tree);
        var labeled = tree.GetRoot().DescendantNodes().OfType<LabeledStatementSyntax>().Single();

        var before = instrumentation.DiagnosticBinding.CaptureSnapshot();

        var analysis = model.AnalyzeControlFlow(labeled);

        var delta = DiagnosticBindingInstrumentation.Subtract(
            instrumentation.DiagnosticBinding.CaptureSnapshot(),
            before);
        var entry = Assert.Single(analysis.EntryPoints);
        Assert.Same(labeled, entry);
        Assert.Equal(0, delta.Calls);
    }

    [Fact]
    public void GetLabelTarget_DoesNotTriggerDiagnosticBinding()
    {
        var instrumentation = new PerformanceInstrumentation();
        var options = new CompilationOptions(
            OutputKind.DynamicallyLinkedLibrary,
            performanceInstrumentation: instrumentation);
        var tree = SyntaxTree.ParseText("""
func Main() {
    goto target
target:
    return
}
""");
        var compilation = CreateCompilation(tree, options: options);
        var model = compilation.GetSemanticModel(tree);
        var gotoStatement = tree.GetRoot().DescendantNodes().OfType<GotoStatementSyntax>().Single();
        var labeled = tree.GetRoot().DescendantNodes().OfType<LabeledStatementSyntax>().Single();

        var before = instrumentation.DiagnosticBinding.CaptureSnapshot();

        var target = model.GetLabelTarget(gotoStatement);

        var delta = DiagnosticBindingInstrumentation.Subtract(
            instrumentation.DiagnosticBinding.CaptureSnapshot(),
            before);
        Assert.NotNull(target);
        Assert.Equal(labeled.Span, target.Span);
        Assert.Equal(0, delta.Calls);
    }

    [Fact]
    public void HasExternalGotoToLabel_DoesNotTriggerDiagnosticBinding()
    {
        var instrumentation = new PerformanceInstrumentation();
        var options = new CompilationOptions(
            OutputKind.DynamicallyLinkedLibrary,
            performanceInstrumentation: instrumentation);
        var tree = SyntaxTree.ParseText("""
func Main() {
    goto target
target:
    return
}
""");
        var compilation = CreateCompilation(tree, options: options);
        var model = compilation.GetSemanticModel(tree);
        var labeled = tree.GetRoot().DescendantNodes().OfType<LabeledStatementSyntax>().Single();
        var region = new ControlFlowRegion(labeled);

        var before = instrumentation.DiagnosticBinding.CaptureSnapshot();

        var hasExternalGoto = model.HasExternalGotoToLabel(labeled, region);

        var delta = DiagnosticBindingInstrumentation.Subtract(
            instrumentation.DiagnosticBinding.CaptureSnapshot(),
            before);
        Assert.True(hasExternalGoto);
        Assert.Equal(0, delta.Calls);
    }

    [Fact]
    public void GetSymbolInfo_ForAttribute_DoesNotTriggerDiagnosticBinding()
    {
        var instrumentation = new PerformanceInstrumentation();
        var options = new CompilationOptions(
            OutputKind.DynamicallyLinkedLibrary,
            performanceInstrumentation: instrumentation);
        var tree = SyntaxTree.ParseText("""
class InfoAttribute : System.Attribute {
    public init(name: string) {}
}

[Info("Widget")]
class Widget {}
""");
        var compilation = CreateCompilation(tree, options: options);
        var model = compilation.GetSemanticModel(tree);
        var attribute = tree.GetRoot().DescendantNodes().OfType<AttributeSyntax>().Single();

        var before = instrumentation.DiagnosticBinding.CaptureSnapshot();

        var info = model.GetSymbolInfo(attribute);

        var delta = DiagnosticBindingInstrumentation.Subtract(
            instrumentation.DiagnosticBinding.CaptureSnapshot(),
            before);
        Assert.IsAssignableFrom<IMethodSymbol>(info.Symbol);
        Assert.Equal(0, delta.Calls);
    }

    [Fact]
    public void GetConstantValue_ForBoundConstReference_DoesNotTriggerDiagnosticBinding()
    {
        var instrumentation = new PerformanceInstrumentation();
        var options = new CompilationOptions(
            OutputKind.DynamicallyLinkedLibrary,
            performanceInstrumentation: instrumentation);
        var tree = SyntaxTree.ParseText("""
class C {
    const Answer = 41
    const Next = Answer
}
""");
        var compilation = CreateCompilation(tree, options: options);
        var model = compilation.GetSemanticModel(tree);
        var answerReference = tree.GetRoot()
            .DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(identifier => identifier.Identifier.ValueText == "Answer");

        var before = instrumentation.DiagnosticBinding.CaptureSnapshot();

        var constant = model.GetConstantValue(answerReference);

        var delta = DiagnosticBindingInstrumentation.Subtract(
            instrumentation.DiagnosticBinding.CaptureSnapshot(),
            before);
        Assert.Equal(TypedConstantKind.Primitive, constant.Kind);
        Assert.Equal(41, constant.Value);
        Assert.Equal(0, delta.Calls);
    }

    [Fact]
    public void GetOperation_ForExpressionStatement_DoesNotTriggerDiagnosticBinding()
    {
        var instrumentation = new PerformanceInstrumentation();
        var options = new CompilationOptions(
            OutputKind.DynamicallyLinkedLibrary,
            performanceInstrumentation: instrumentation);
        var tree = SyntaxTree.ParseText("""
class C {
    static func Main() -> () {
        System.Console.WriteLine("hello")
    }
}
""");
        var compilation = CreateCompilation(tree, options: options);
        var model = compilation.GetSemanticModel(tree);
        var statement = tree.GetRoot().DescendantNodes().OfType<ExpressionStatementSyntax>().Single();

        var before = instrumentation.DiagnosticBinding.CaptureSnapshot();

        var operation = model.GetOperation(statement);

        var delta = DiagnosticBindingInstrumentation.Subtract(
            instrumentation.DiagnosticBinding.CaptureSnapshot(),
            before);
        Assert.NotNull(operation);
        Assert.Equal(0, delta.Calls);
    }

    [Fact]
    public void GetMatchExhaustiveness_DoesNotTriggerDiagnosticBinding()
    {
        var instrumentation = new PerformanceInstrumentation();
        var options = new CompilationOptions(
            OutputKind.DynamicallyLinkedLibrary,
            performanceInstrumentation: instrumentation);
        var tree = SyntaxTree.ParseText("""
val result: Result<int, string> = .Ok(42)

val text = result match {
    .Ok(val value) => value.ToString()
}

union Result<T, E> {
    case Ok(value: T)
    case Error(message: E)
}
""");
        var compilation = CreateCompilation(tree, options: options);
        var model = compilation.GetSemanticModel(tree);
        var match = tree.GetRoot().DescendantNodes().OfType<MatchExpressionSyntax>().Single();

        var before = instrumentation.DiagnosticBinding.CaptureSnapshot();

        var info = model.GetMatchExhaustiveness(match);

        var delta = DiagnosticBindingInstrumentation.Subtract(
            instrumentation.DiagnosticBinding.CaptureSnapshot(),
            before);
        Assert.False(info.IsExhaustive);
        Assert.Contains("Error", info.MissingCases);
        Assert.Equal(0, delta.Calls);
    }

    [Fact]
    public void GetCapturedVariables_ForFunctionExpression_DoesNotTriggerDiagnosticBinding()
    {
        var instrumentation = new PerformanceInstrumentation();
        var options = new CompilationOptions(
            OutputKind.DynamicallyLinkedLibrary,
            performanceInstrumentation: instrumentation);
        var tree = SyntaxTree.ParseText("""
class C {
    static func Main() -> int {
        val offset = 1
        val read = func () -> int {
            offset
        }

        return read()
    }
}
""");
        var compilation = CreateCompilation(tree, options: options);
        var model = compilation.GetSemanticModel(tree);
        var lambda = tree.GetRoot().DescendantNodes().OfType<ParenthesizedFunctionExpressionSyntax>().Single();

        var before = instrumentation.DiagnosticBinding.CaptureSnapshot();

        var captures = model.GetCapturedVariables(lambda);

        var delta = DiagnosticBindingInstrumentation.Subtract(
            instrumentation.DiagnosticBinding.CaptureSnapshot(),
            before);
        Assert.Contains(captures, static symbol => symbol is ILocalSymbol { Name: "offset" });
        Assert.Equal(0, delta.Calls);
    }

    [Fact]
    public void IsCapturedVariable_DoesNotTriggerDiagnosticBinding()
    {
        var instrumentation = new PerformanceInstrumentation();
        var options = new CompilationOptions(
            OutputKind.DynamicallyLinkedLibrary,
            performanceInstrumentation: instrumentation);
        var tree = SyntaxTree.ParseText("""
class C {
    static func Main() -> int {
        val offset = 1
        val read = func () -> int {
            offset
        }

        return read()
    }
}
""");
        var compilation = CreateCompilation(tree, options: options);
        var model = compilation.GetSemanticModel(tree);
        var declarator = tree.GetRoot()
            .DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .Single(declarator => declarator.Identifier.ValueText == "offset");
        var local = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarator));

        var before = instrumentation.DiagnosticBinding.CaptureSnapshot();

        var isCaptured = model.IsCapturedVariable(local);

        var delta = DiagnosticBindingInstrumentation.Subtract(
            instrumentation.DiagnosticBinding.CaptureSnapshot(),
            before);
        Assert.True(isCaptured);
        Assert.Equal(0, delta.Calls);
    }
}
