using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class OperatorBindingTests : CompilationTestBase
{
    [Fact]
    public void OperatorDeclaration_BindsUserDefinedOperatorSymbol()
    {
        var source = """
class Number
{
    public static operator +(left: Number, right: Number) -> Number { return left }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        compilation.EnsureSetup();
        var model = compilation.GetSemanticModel(tree);
        var declaration = tree.GetRoot().DescendantNodes().OfType<OperatorDeclarationSyntax>().Single();

        var symbol = Assert.IsType<SourceMethodSymbol>(model.GetDeclaredSymbol(declaration));

        Assert.Equal(MethodKind.UserDefinedOperator, symbol.MethodKind);
        Assert.Equal("op_Addition", symbol.Name);
        Assert.True(symbol.IsStatic);
        Assert.Equal(2, symbol.Parameters.Length);
        Assert.Same(symbol.ContainingType, symbol.ReturnType);

        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void OperatorDeclaration_MissingStatic_ReportsDiagnostic()
    {
        var source = """
class Number
{
    public operator +(left: Number, right: Number) -> Number { return left }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        compilation.EnsureSetup();

        Assert.Contains(
            compilation.GetDiagnostics(),
            d => d.Descriptor == CompilerDiagnostics.OperatorMustBeStatic);
    }

    [Fact]
    public void OperatorDeclaration_InvalidParameterCount_ReportsDiagnostic()
    {
        var source = """
class Counter
{
    public static operator ++(left: Counter, right: Counter) -> Counter { return left }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        compilation.EnsureSetup();

        Assert.Contains(
            compilation.GetDiagnostics(),
            d => d.Descriptor == CompilerDiagnostics.OperatorParameterCountInvalid);
    }

    [Fact]
    public void ExtensionOperator_NotSupported()
    {
        var source = """
extension IntOps for int
{
    public static operator +(left: int, right: int) -> int => left + right
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(tree);
        compilation.EnsureSetup();

        Assert.Contains(
            compilation.GetDiagnostics(),
            d => d.Descriptor == CompilerDiagnostics.OperatorNotSupportedInExtensions);
    }
}
