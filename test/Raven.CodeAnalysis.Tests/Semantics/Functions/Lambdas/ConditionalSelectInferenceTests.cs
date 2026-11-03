using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class ConditionalSelectInferenceTests
{
    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void ConditionalSelect_PreservesSingleParameterLambda(bool coalesce, bool diagnosticsFirst)
    {
        var tree = SyntaxTree.ParseText($$"""
import System.Collections.Generic.*
import System.Linq.*
func Project(values: object?[]?) -> IEnumerable<object?>{{(coalesce ? "" : "?")}} {
    return values?.Select(value => {
        if value is string text { return text }
        return value
    }){{(coalesce ? " ?? []" : "")}}
}
""");
        var compilation = Compilation.Create("ConditionalSelect", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        if (diagnosticsFirst)
            _ = compilation.GetDiagnostics();
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var method = Assert.IsAssignableFrom<MethodSymbol>(model.GetSymbolInfo(invocation).Symbol);
        Assert.Equal("Select", method.Name);
        Assert.Single(Assert.IsAssignableFrom<NamedTypeSymbol>(method.Parameters.Last().Type).GetDelegateInvokeMethod()!.Parameters);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }
    [Theory]
    [InlineData(0, false)]
    [InlineData(1, false)]
    [InlineData(2, false)]
    [InlineData(0, true)]
    [InlineData(1, true)]
    [InlineData(2, true)]
    public void ConditionalOverloadHint_RespectsLambdaArityIndependentOfOrder(int arity, bool reverse)
    {
        string[] members = [
            "func Map(selector: System.Func<int>) -> int => selector()",
            "func Map(selector: System.Func<int, int>) -> int => selector(7)",
            "func Map(selector: System.Func<int, int, int>) -> int => selector(7, 3)"
        ];
        var lambda = arity switch { 0 => "() => 4", 1 => "value => value", _ => "(value, index) => value + index" };
        var tree = SyntaxTree.ParseText($$"""
class Source {
{{string.Join(Environment.NewLine, reverse ? members.Reverse() : members)}}
}
func Pick(source: Source?) -> int? => source?.Map({{lambda}})
""");
        var compilation = Compilation.Create("ConditionalDelegateArity", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Last();
        var method = Assert.IsAssignableFrom<MethodSymbol>(compilation.GetSemanticModel(tree).GetSymbolInfo(invocation).Symbol);
        Assert.Equal(arity, Assert.IsAssignableFrom<NamedTypeSymbol>(method.Parameters.Single().Type).GetDelegateInvokeMethod()!.Parameters.Length);
    }

}

[Trait("Target", "NeoCLR")]
public sealed class NeoClrConditionalSelectInferenceTests
{
    [Fact]
    public void NeoClrSemanticBindingWithHostReferences_UsesCompatibleLambdaHint()
    {
        var tree = SyntaxTree.ParseText("""
import System.Collections.Generic.*
import System.Linq.*
func Project(values: object?[]?) -> IEnumerable<object?> => values?.Select(value => value) ?? []
""");
        var compilation = Compilation.Create("NeoClrConditionalSelect", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(TargetPlatform.NeoCLR));
        var model = compilation.GetSemanticModel(tree);
        Assert.DoesNotContain(model.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var method = Assert.IsAssignableFrom<MethodSymbol>(model.GetSymbolInfo(invocation).Symbol);
        Assert.Single(Assert.IsAssignableFrom<NamedTypeSymbol>(method.Parameters.Last().Type).GetDelegateInvokeMethod()!.Parameters);
    }
}
