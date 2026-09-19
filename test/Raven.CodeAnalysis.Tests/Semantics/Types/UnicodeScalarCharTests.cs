using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;
namespace Raven.CodeAnalysis.Semantics.Tests;

public class UnicodeScalarCharTests : CompilationTestBase
{
    [Theory]
    [InlineData("'🌍'")]
    [InlineData("'\\U0001F600'")]
    public void SupplementaryLiteralRequiresScalarContract(string literal)
    {
        var source = "func Value() -> char { return " + literal + " }";
        var (ordinary, _) = CreateCompilation(source);
        Assert.Contains(ordinary.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var (scalar, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithUnicodeScalarChar(true));
        Assert.DoesNotContain(scalar.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var expression = tree.GetRoot().DescendantNodes().OfType<LiteralExpressionSyntax>().Single();
        Assert.Equal(SpecialType.System_Char, scalar.GetSemanticModel(tree).GetTypeInfo(expression).Type!.SpecialType);
    }
    [Fact]
    public void ScalarContractRejectsSurrogateLiteral()
    {
        var (compilation, _) = CreateCompilation("func Value() -> char { return '\\uD800' }", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithUnicodeScalarChar(true));
        Assert.Contains(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }
    [Theory]
    [InlineData("""'\U00110000'""")]
    [InlineData("""'\u123'""")]
    public void InvalidEscapeIsDiagnosed(string literal)
    {
        var (compilation, _) = CreateCompilation("func Value() -> char { return " + literal + " }", new CompilationOptions().WithUnicodeScalarChar(true));
        Assert.Contains(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void CloningOptionsPreservesExplicitContract()
    {
        var options = new CompilationOptions().WithUnicodeScalarChar(true)
            .WithOutputKind(OutputKind.DynamicallyLinkedLibrary).WithTargetCoreAssemblyName("TestCore")
            .WithRuntimeTypeOfContract(null).WithRuntimeUnitContract(null);
        Assert.True(options.UseUnicodeScalarChar);
        Assert.False(new CompilationOptions().UseUnicodeScalarChar);
    }
}
