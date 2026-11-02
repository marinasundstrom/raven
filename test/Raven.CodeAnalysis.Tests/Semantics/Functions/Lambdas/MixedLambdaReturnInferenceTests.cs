using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class MixedLambdaReturnInferenceTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Select_MixedReferenceReturnsInferNullableObject(bool diagnosticsFirst)
    {
        var tree = SyntaxTree.ParseText("""
import System.Collections.Generic.*
import System.Linq.*
func Project(values: object?[]) -> IEnumerable<object?> {
    return values.Select(value => {
        if value is string text { return text }
        return value
    })
}
""");
        var compilation = Compilation.Create("MixedLambdaReturns", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        if (diagnosticsFirst)
            _ = compilation.GetDiagnostics();
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var method = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);
        var resultType = method.TypeArguments.Last();
        Assert.True(resultType.IsNullable);
        Assert.Equal(SpecialType.System_Object, resultType.GetNonNullableType().SpecialType);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }
}

[Trait("Target", "NeoCLR")]
public sealed class NeoClrMixedLambdaReturnInferenceTests
{
    [Fact]
    public void NeoClrSemanticBindingWithHostReferences_PreservesNullableCommonReturn()
    {
        var tree = SyntaxTree.ParseText("""
import System.Collections.Generic.*
import System.Linq.*
func Project(values: object?[]) -> IEnumerable<object?> {
    return values.Select(value => {
        if value is string text { return text }
        return value
    })
}
""");
        var compilation = Compilation.Create("NeoClrMixedLambdaReturns", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(TargetPlatform.NeoCLR));
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var method = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);
        Assert.True(method.TypeArguments.Last().IsNullable);
        Assert.Equal(SpecialType.System_Object, method.TypeArguments.Last().GetNonNullableType().SpecialType);
        Assert.DoesNotContain(model.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }
}
