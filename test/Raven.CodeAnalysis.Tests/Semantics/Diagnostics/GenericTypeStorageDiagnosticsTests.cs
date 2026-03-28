using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class GenericTypeStorageDiagnosticsTests : CompilationTestBase
{
    [Fact]
    public void FunctionParameter_WithBareGenericInterface_ReportsTypeRequiresTypeArguments()
    {
        const string source = """
sealed interface Expr<T> {
    record NumericalExpr(Value: float) : Expr<float>
}

func Evaluate(expr: Expr) {
}
""";

        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.ConsoleApplication));
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.TypeRequiresTypeArguments);
    }

    [Fact]
    public void PropertyType_WithBareGenericInterface_ReportsTypeRequiresTypeArguments()
    {
        const string source = """
sealed interface Expr<T> {
    record NumericalExpr(Value: float) : Expr<float>
}

class Box {
    val Value: Expr
}
""";

        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.ConsoleApplication));
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.TypeRequiresTypeArguments);
    }

    [Fact]
    public void LocalDeclaration_WithBareGenericInterface_ReportsTypeRequiresTypeArguments()
    {
        const string source = """
sealed interface Expr<T> {
    record NumericalExpr(Value: float) : Expr<float>
}

func Main() {
    val value: Expr = Expr.NumericalExpr(1)
}
""";

        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree, new CompilationOptions(OutputKind.ConsoleApplication));
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.TypeRequiresTypeArguments);
    }
}
