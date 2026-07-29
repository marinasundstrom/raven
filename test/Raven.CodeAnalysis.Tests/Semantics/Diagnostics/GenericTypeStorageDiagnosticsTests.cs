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
        var compilation = CreateCompilation(tree);
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
        var compilation = CreateCompilation(tree);
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
    let value: Expr = Expr.NumericalExpr(1)
}
""";

        var tree = SyntaxTree.ParseText(source, path: "file.rvn");
        var compilation = CreateCompilation(tree);
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.TypeRequiresTypeArguments);
    }

    [Fact]
    public void GenericType_WithoutAllowByRefLike_RejectsSpanArgument()
    {
        const string source = """
func Main() {
    Reject<System.Span<int>>()
}

func Reject<T>() {}
""";

        var (compilation, _) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.TypeArgumentDoesNotSatisfyConstraint);
    }

    [Fact]
    public void GenericType_WithAllowByRefLike_AcceptsSpanArgument()
    {
        const string source = """
func Main() {
    let type = typeof(System.Action<System.Span<int>>)
}
""";

        var (compilation, _) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(diagnostics, d => d.Descriptor == CompilerDiagnostics.TypeArgumentDoesNotSatisfyConstraint);
    }

    [Fact]
    public void SourceGenericMethod_WithAllowByRefLike_AcceptsSpanArgument()
    {
        const string source = """
func Main() {
    Accept<System.Span<int>>()
}

func Accept<T>() where T: allows ref struct {}
""";

        var (compilation, tree) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(diagnostics, d => d.Descriptor == CompilerDiagnostics.TypeArgumentDoesNotSatisfyConstraint);
        var declaration = tree.GetRoot()
            .DescendantNodes()
            .OfType<FunctionStatementSyntax>()
            .Single(function => function.Identifier.ValueText == "Accept");
        var method = Assert.IsAssignableFrom<IMethodSymbol>(
            compilation.GetSemanticModel(tree).GetDeclaredSymbol(declaration));
        Assert.Equal(
            TypeParameterConstraintKind.AllowByRefLike,
            method.TypeParameters[0].ConstraintKind & TypeParameterConstraintKind.AllowByRefLike);
    }
}
