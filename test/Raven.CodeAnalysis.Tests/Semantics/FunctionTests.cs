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
