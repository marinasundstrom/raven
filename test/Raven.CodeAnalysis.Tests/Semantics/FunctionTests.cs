using System.Linq;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Symbols;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class FunctionTests : CompilationTestBase
{
    [Fact]
    public void Function_WithoutReturnType_DefaultsToVoid()
    {
        var source = """
func outer() {
    func inner() { }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var inner = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single(l => l.Identifier.Text == "inner");
        var symbol = (IMethodSymbol)model.GetDeclaredSymbol(inner)!;
        Assert.Equal(SpecialType.System_Unit, symbol.ReturnType.SpecialType);
    }

    [Fact]
    public void AsyncFunction_WithoutReturnType_DefaultsToTask()
    {
        var source = """
func outer() {
    async func inner() { }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var inner = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single(l => l.Identifier.Text == "inner");
        var symbol = (IMethodSymbol)model.GetDeclaredSymbol(inner)!;
        Assert.True(symbol.IsAsync);
        Assert.Equal(SpecialType.System_Threading_Tasks_Task, symbol.ReturnType.SpecialType);
    }

    [Fact]
    public void AsyncFunction_WithoutReturnType_WithReturnExpression_InfersTaskOfResult()
    {
        var source = """
func outer() {
    async func inner() {
        return 1
    }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var inner = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single(l => l.Identifier.Text == "inner");
        var symbol = (IMethodSymbol)model.GetDeclaredSymbol(inner)!;

        Assert.True(symbol.IsAsync);
        Assert.Equal(
            "System.Threading.Tasks.Task<System.Int32>",
            symbol.ReturnType.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void GenericFunction_CanInferTypeArgumentsForInvocation()
    {
        var source = """
import System.Threading.Tasks.*

async func Test<T>(value: T) -> Task<T> {
    await Task.Delay(10)
    return value
}

async func Outer() -> Task<int> {
    let result = await Test(42)
    return result
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);

        var functions = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().ToArray();
        var testFunction = functions.Single(f => f.Identifier.Text == "Test");
        var testSymbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(testFunction));
        Assert.True(testSymbol.IsGenericMethod);
        Assert.Single(testSymbol.TypeParameters);
        Assert.Equal("T", testSymbol.TypeParameters[0].Name);
        Assert.Equal(
            "System.Threading.Tasks.Task<T>",
            testSymbol.ReturnType.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));

        var invocation = tree
            .GetRoot()
            .DescendantNodes()
            .OfType<InvocationExpressionSyntax>()
            .Single(i => i.Expression is IdentifierNameSyntax id && id.Identifier.Text == "Test");
        var symbolInfo = model.GetSymbolInfo(invocation);
        var invokedSymbol = Assert.IsAssignableFrom<IMethodSymbol>(symbolInfo.Symbol);
        Assert.Equal(testSymbol, invokedSymbol.OriginalDefinition);
        Assert.Single(invokedSymbol.TypeArguments);
        Assert.Equal(
            compilation.GetSpecialType(SpecialType.System_Int32),
            invokedSymbol.TypeArguments[0]);

        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void DuplicateFunction_DiagnosticReported()
    {
        var source = """
func test() {}
func test() {}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var funcs = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().ToArray();
        _ = model.GetDeclaredSymbol(funcs[0]);
        _ = model.GetDeclaredSymbol(funcs[1]);
        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.FunctionAlreadyDefined, diagnostic.Descriptor);
    }
}
