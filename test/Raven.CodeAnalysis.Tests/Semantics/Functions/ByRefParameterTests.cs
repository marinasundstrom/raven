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
        Assert.Equal(SpecialType.System_Int32, symbol.Type.SpecialType);
    }

    [Fact]
    public void OutParameter_HasRefKindOut()
    {
        var source = """
class C {
    test(out var x: &int) -> unit { x = 1 }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();
        var methodSymbol = (IMethodSymbol)model.GetDeclaredSymbol(method)!;
        var symbol = methodSymbol.Parameters.Single();
        Assert.Equal(RefKind.Out, symbol.RefKind);
        Assert.Equal(SpecialType.System_Int32, symbol.Type.SpecialType);
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
        Assert.Equal(SpecialType.System_Int32, parameter.Type.SpecialType);
    }

    [Fact]
    public void Constructor_OutParameter_HasRefKindOut()
    {
        var source = """
class C {
    init(out var x: &int) { x = 0 }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var ctor = tree.GetRoot().DescendantNodes().OfType<ConstructorDeclarationSyntax>().Single();
        var ctorSymbol = (IMethodSymbol)model.GetDeclaredSymbol(ctor)!;
        var parameter = ctorSymbol.Parameters.Single();
        Assert.Equal(RefKind.Out, parameter.RefKind);
        Assert.Equal(SpecialType.System_Int32, parameter.Type.SpecialType);
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
        Assert.Equal(SpecialType.System_Int32, parameter.Type.SpecialType);
    }

    [Fact]
    public void Function_OutParameter_HasRefKindOut()
    {
        var source = """
func outer() {
    func inner(out var x: &int) { x = 1 }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var model = compilation.GetSemanticModel(tree);
        var inner = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single(l => l.Identifier.Text == "inner");
        var symbol = (IMethodSymbol)model.GetDeclaredSymbol(inner)!;
        var parameter = symbol.Parameters.Single();
        Assert.Equal(RefKind.Out, parameter.RefKind);
        Assert.Equal(SpecialType.System_Int32, parameter.Type.SpecialType);
    }

    [Fact]
    public void Method_ByRefParameter_Passed_To_Other_ByRefMethod_Compiles()
    {
        var source = """
class C {
    static Set(var x: &int) { x = 1 }
    static Pass(x: &int) { Set(&x) }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, d => d.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void Method_ByRefParameter_WithAddressOfLocal_Compiles()
    {
        var source = """
class C {
    static Set(var value: &int) -> unit { value = 42 }

    static Run() -> unit {
        var data = 0
        Set(&data)
    }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, d => d.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void Method_OutParameter_WithAddressOfLocal_Compiles()
    {
        var source = """
class C {
    static TryParse(text: string, out var result: &int) -> bool {
        result = 1
        return true
    }

    static Consume(arg: string) -> bool {
        var total = 0
        if !TryParse(arg, &total) {
            return false
        }

        total = total + 1
        return true
    }
}
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, d => d.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void Method_OutParameter_WithoutVar_ReportsDiagnostic()
    {
        var source = """
class C {
    static TryParse(text: string, out result: &int) -> bool {
        result = 0
        return true
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();
        var diagnostic = Assert.Single(diagnostics.Where(d => d.Severity == DiagnosticSeverity.Error));
        Assert.Equal(CompilerDiagnostics.ThisValueIsNotMutable.Id, diagnostic.Id);
    }

    [Fact]
    public void Method_ValueParameter_WithVar_AllowsAssignment()
    {
        var source = """
class C {
    static Increment(var value: int) -> int {
        value = value + 1
        return value
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, d => d.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void Method_ByRefReturn_AddressOfLocal_ReportsDiagnostic()
    {
        var source = """
class C {
    static MakeRef() -> &int {
        val x = 2
        return &x
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();
        var diagnostic = Assert.Single(diagnostics.Where(d => d.Severity == DiagnosticSeverity.Error));
        Assert.Equal(CompilerDiagnostics.ByRefReturnCannotReferenceLocal.Id, diagnostic.Id);
    }

    [Fact]
    public void Method_ByRefReturn_AddressOfValueParameter_ReportsDiagnostic()
    {
        var source = """
class C {
    static Forward(x: int) -> &int {
        return &x
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();
        var diagnostic = Assert.Single(diagnostics.Where(d => d.Severity == DiagnosticSeverity.Error));
        Assert.Equal(CompilerDiagnostics.ByRefReturnCannotReferenceValueParameter.Id, diagnostic.Id);
    }

}
