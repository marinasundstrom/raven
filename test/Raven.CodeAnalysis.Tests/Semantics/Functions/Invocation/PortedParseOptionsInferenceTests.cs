using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class PortedParseOptionsInferenceTests
{
    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void DictionaryCopyAndStringPipeline_PreserveNonNullableElements(bool diagnosticsFirst, bool reverseQueries)
    {
        var tree = SyntaxTree.ParseText("""
            import System.*
            import System.Collections.Generic.*
            import System.Collections.Immutable.*
            import System.Linq.*
            func Snapshot(features: IReadOnlyDictionary<string, string>) -> ImmutableDictionary<string, string>
                => features.ToImmutableDictionary(StringComparer.Ordinal)
            func Normalize(names: IEnumerable<string>) -> ImmutableArray<string>
                => names.Where(name => !string.IsNullOrWhiteSpace(name)).Select(name => name.Trim())
                    .Distinct(StringComparer.Ordinal).OrderBy(name => name, StringComparer.Ordinal).ToImmutableArray()
            """);
        var compilation = Compilation.Create("ParseOptionsInference", [tree], TestMetadataReferences.Default,
            CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        if (diagnosticsFirst)
            Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var model = compilation.GetSemanticModel(tree);
        var invocations = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>();
        foreach (var invocation in reverseQueries ? invocations.Reverse() : invocations)
        {
            var info = model.GetSymbolInfo(invocation);
            var symbol = info.Symbol;
            Assert.True(symbol is MethodSymbol, $"No method for: {invocation}; {info}");
            var method = (MethodSymbol)symbol!;
            Assert.True(SymbolEqualityComparer.Default.Equals(method, model.GetSymbolInfo(invocation).Symbol));
            foreach (var argument in method.TypeArguments.Where(a => a.GetNonNullableType().SpecialType == SpecialType.System_String))
                Assert.False(argument.IsNullable, method.ToDisplayString());
        }
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }
}
