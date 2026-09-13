using System.Linq;
using Raven.CodeAnalysis.Tests;
using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ArrayVariancePolicyTests
{
    [Theory]
    [InlineData(true, "return values")]
    [InlineData(false, "return values")]
    [InlineData(true, "return (object[])values")]
    [InlineData(false, "return (object[])values")]
    [InlineData(true, "let result: object[] = values; return result")]
    [InlineData(false, "let result: object[] = values; return result")]
    [InlineData(true, "return Accept(values)")]
    [InlineData(false, "return Accept(values)")]
    public void ArrayConversions_RespectTargetPolicy(bool enabled, string body)
    {
        var source = $$"""
func Accept(values: object[]) -> object[] { return values }
func Convert(values: string[]) -> object[] { {{body}} }
""";
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary, allowArrayCovariance: enabled)
            .WithRunAnalyzers(false).WithOutputKind(OutputKind.DynamicallyLinkedLibrary);
        var compilation = Compilation.Create("array_policy", options)
            .AddSyntaxTrees(SyntaxTree.ParseText(source)).AddReferences(TestMetadataReferences.Default);
        var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        if (enabled)
            Assert.Empty(errors);
        else
            Assert.NotEmpty(errors);
        var strings = compilation.CreateArrayTypeSymbol(compilation.GetSpecialType(SpecialType.System_String));
        var objects = compilation.CreateArrayTypeSymbol(compilation.GetSpecialType(SpecialType.System_Object));
        Assert.Equal(enabled, compilation.ClassifyConversion(strings, objects).Exists);
        Assert.True(compilation.ClassifyConversion(strings, strings).IsIdentity);
        Assert.Equal(enabled, compilation.ClassifyConversion(
            compilation.CreateArrayTypeSymbol(strings), compilation.CreateArrayTypeSymbol(objects)).Exists);
    }

    [Fact]
    public void ArrayCovariance_DefaultsToClrBehavior()
    {
        Assert.True(new CompilationOptions().AllowArrayCovariance);
        var options = new CompilationOptions().WithAllowArrayCovariance(false)
            .WithRuntimeIterationContract(null).WithRuntimePropagationContract(null)
            .WithMetadataImportOptions(null);
        Assert.False(options.AllowArrayCovariance);
    }
}
