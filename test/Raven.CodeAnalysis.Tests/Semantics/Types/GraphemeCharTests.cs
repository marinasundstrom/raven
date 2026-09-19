using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class GraphemeCharTests : CompilationTestBase
{
    [Theory]
    [InlineData("'A'")]
    [InlineData("'🌍'")]
    [InlineData("'é'")]
    [InlineData("'👨‍👩‍👧‍👦'")]
    [InlineData("'🇸🇪'")]
    [InlineData("""'e\u0301'""")]
    public void CharacterLiteralHasCharType(string literal)
    {
        var (compilation, tree) = CreateCompilation("func Value() -> char { return " + literal + " }",
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithGraphemeChar(true));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var expression = tree.GetRoot().DescendantNodes().OfType<LiteralExpressionSyntax>().Single();
        Assert.Equal(SpecialType.System_Char, compilation.GetSemanticModel(tree).GetTypeInfo(expression).Type!.SpecialType);
    }

    [Theory]
    [InlineData("'ab'")]
    [InlineData("""'\uD800'""")]
    public void InvalidCharacterIsDiagnosed(string literal)
    {
        var (compilation, _) = CreateCompilation("func Value() -> char { return " + literal + " }",
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithGraphemeChar(true));
        Assert.Contains(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    [Theory]
    [InlineData("func Value(value: char) -> int { return value }")]
    [InlineData("func Value() -> int { return 'A' }")]
    [InlineData("func Value(value: int) -> char { return (char)value }")]
    [InlineData("func Value(value: char) -> char { return value + value }")]
    public void CharacterIsNotNumeric(string source)
    {
        var (compilation, _) = CreateCompilation(source, new CompilationOptions().WithGraphemeChar(true));
        Assert.Contains(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void OrdinaryDotNetTargetRejectsGraphemeLiteral()
    {
        var (compilation, _) = CreateCompilation("func Value() -> char { return 'é' }");
        Assert.Contains(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void CloningOptionsPreservesContract()
    {
        var options = new CompilationOptions().WithGraphemeChar(true).WithUnicodeScalarChar(false)
            .WithOutputKind(OutputKind.DynamicallyLinkedLibrary).WithTargetCoreAssemblyName("TestCore")
            .WithRuntimeTypeOfContract(null).WithRuntimeUnitContract(null);
        Assert.True(options.UseGraphemeChar);
        Assert.False(new CompilationOptions().UseGraphemeChar);
    }
}
