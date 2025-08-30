using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ByRefParameterTests : CompilationTestBase
{
    [Fact]
    public void Parameter_WithAmpersand_HasRefKindRef()
    {
        var source = """
class C {
    test(x: &int) -> unit { }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var methodSymbol = (IMethodSymbol)model.GetDeclaredSymbol(method)!;
        var symbol = methodSymbol.Parameters.Single();
        Assert.Equal(RefKind.Ref, symbol.RefKind);
    }

    [Fact]
    public void OutParameter_HasRefKindOut()
    {
        var source = """
class C {
    test(out x: &int) -> unit { x = 1 }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var methodSymbol = (IMethodSymbol)model.GetDeclaredSymbol(method)!;
        var symbol = methodSymbol.Parameters.Single();
        Assert.Equal(RefKind.Out, symbol.RefKind);
    }

    [Fact]
    public void ConstructorParameter_WithAmpersand_HasRefKindRef()
    {
        var source = """
class C {
    init(x: &int) { }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var ctor = tree.GetRoot().DescendantNodes().OfType<ConstructorDeclarationSyntax>().Single();
        var ctorSymbol = (IMethodSymbol)model.GetDeclaredSymbol(ctor)!;
        var parameter = ctorSymbol.Parameters.Single();
        Assert.Equal(RefKind.Ref, parameter.RefKind);
    }

    [Fact]
    public void Constructor_OutParameter_HasRefKindOut()
    {
        var source = """
class C {
    init(out x: &int) { x = 0 }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var ctor = tree.GetRoot().DescendantNodes().OfType<ConstructorDeclarationSyntax>().Single();
        var ctorSymbol = (IMethodSymbol)model.GetDeclaredSymbol(ctor)!;
        var parameter = ctorSymbol.Parameters.Single();
        Assert.Equal(RefKind.Out, parameter.RefKind);
    }

    [Fact]
    public void FunctionParameter_WithAmpersand_HasRefKindRef()
    {
        var source = """
func outer() {
    func inner(x: &int) { }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var inner = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single(l => l.Identifier.Text == "inner");
        var symbol = (IMethodSymbol)model.GetDeclaredSymbol(inner)!;
        var parameter = symbol.Parameters.Single();
        Assert.Equal(RefKind.Ref, parameter.RefKind);
    }

    [Fact]
    public void Function_OutParameter_HasRefKindOut()
    {
        var source = """
func outer() {
    func inner(out x: &int) { x = 1 }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var inner = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single(l => l.Identifier.Text == "inner");
        var symbol = (IMethodSymbol)model.GetDeclaredSymbol(inner)!;
        var parameter = symbol.Parameters.Single();
        Assert.Equal(RefKind.Out, parameter.RefKind);
    }

    [Fact]
    public void Method_ByRefParameter_Passed_To_Other_ByRefMethod_Compiles()
    {
        var source = """
class C {
    static Set(x: &int) { x = 1 }
    static Pass(x: &int) { Set(&x) }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, d => d.Severity == DiagnosticSeverity.Error);
    }
}
