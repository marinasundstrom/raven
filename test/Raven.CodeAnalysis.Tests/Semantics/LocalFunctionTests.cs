using System.Linq;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Tests;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class LocalFunctionTests
{
    [Fact]
    public void LocalFunction_WithoutReturnType_DefaultsToVoid()
    {
        var source = """
func outer() {
    func inner() { }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(tree);
        var inner = tree.GetRoot().DescendantNodes().OfType<LocalFunctionStatementSyntax>().Single(l => l.Identifier.Text == "inner");
        var symbol = (IMethodSymbol)model.GetDeclaredSymbol(inner)!;
        Assert.Equal(SpecialType.System_Unit, symbol.ReturnType.SpecialType);
    }

    [Fact]
    public void DuplicateLocalFunction_DiagnosticReported()
    {
        var source = """
func test() {}
func test() {}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("test", [tree], TestMetadataReferences.Default, new CompilationOptions(OutputKind.ConsoleApplication));
        var model = compilation.GetSemanticModel(tree);
        var funcs = tree.GetRoot().DescendantNodes().OfType<LocalFunctionStatementSyntax>().ToArray();
        _ = model.GetDeclaredSymbol(funcs[0]);
        _ = model.GetDeclaredSymbol(funcs[1]);
        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.LocalFunctionAlreadyDefined, diagnostic.Descriptor);
    }
}
