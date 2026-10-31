using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Symbols;

public sealed class SourceStructuralUnionTests
{
    [Theory]
    [InlineData("A", "A", "A | B", "first")]
    [InlineData("A | B", "A | B", "A | B | C", "first")]
    [InlineData("A | B", "A | B | C", "A | B | C | D", "first")]
    [InlineData("A", "A | B", "A | C", null)]
    [InlineData("A | B", "A | B | C", "A | B | D", null)]
    public void NativeUnionOverloads_UseSharedRanking(
        string argumentType, string firstType, string secondType, string? expectedParameter)
    {
        foreach (var reverse in new[] { false, true })
        {
            var first = $"func Pick(first: {firstType}) -> int {{ 1 }}";
            var second = $"func Pick(second: {secondType}) -> int {{ 2 }}";
            var tree = SyntaxTree.ParseText($$"""
                class A {}
                class B {}
                class C {}
                class D {}
                {{(reverse ? second : first)}}
                {{(reverse ? first : second)}}
                func Test(value: {{argumentType}}) -> int { Pick(value) }
                """);
            var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                .WithTargetPlatform(TargetPlatform.NeoCLR)
                .WithMetadataImportOptions(new MetadataImportOptions());
            var compilation = Compilation.Create("native-overloads", [tree], TestMetadataReferences.DefaultWithRavenCore, options);
            var model = compilation.GetSemanticModel(tree);
            var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
            var info = model.GetSymbolInfo(invocation);
            if (expectedParameter is null)
            {
                Assert.Null(info.Symbol);
                Assert.Equal(CandidateReason.Ambiguous, info.CandidateReason);
                Assert.Equal(2, info.CandidateSymbols.Length);
                Assert.Contains(model.GetDiagnostics(), d => d.Descriptor == CompilerDiagnostics.CallIsAmbiguous);
            }
            else
            {
                var method = Assert.IsAssignableFrom<IMethodSymbol>(info.Symbol);
                Assert.Equal(expectedParameter, method.Parameters[0].Name);
                Assert.DoesNotContain(model.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
            }
        }
    }

    [Theory]
    [InlineData("(Response)value", false)]
    [InlineData("value", true)]
    public void ReferenceExtraction_RequiresExplicitCast(string expression, bool hasError)
    {
        var tree = SyntaxTree.ParseText($$"""
            class Response {}
            func Extract(value: Response | int) -> Response { return {{expression}} }
            """);
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
            .WithTargetPlatform(TargetPlatform.NeoCLR)
            .WithMetadataImportOptions(new MetadataImportOptions());
        var compilation = Compilation.Create("union-extraction", [tree], TestMetadataReferences.DefaultWithRavenCore, options);
        var errors = compilation.GetSemanticModel(tree).GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error);
        if (hasError)
            Assert.NotEmpty(errors);
        else
            Assert.Empty(errors);
    }

    [Theory]
    [InlineData(TargetPlatform.NeoCLR, true)]
    [InlineData(TargetPlatform.DotNet, false)]
    public void UnionSyntax_UsesTargetContractWithoutOptIn(TargetPlatform target, bool structural)
    {
        var tree = SyntaxTree.ParseText("""
            interface A {}
            interface B {}
            class Box<T> {}
            func Echo(value: A | B) -> A | B { return value }
            func Wrap(value: Box<A | B>) -> Box<A | B> { return value }
            """);
        var options = new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
            .WithTargetPlatform(target)
            .WithMetadataImportOptions(new MetadataImportOptions());
        var compilation = Compilation.Create("source-unions", [tree], TestMetadataReferences.DefaultWithRavenCore, options);
        var model = compilation.GetSemanticModel(tree);
        foreach (var syntax in tree.GetRoot().DescendantNodes().OfType<UnionTypeSyntax>())
        {
            var type = model.GetTypeInfo(syntax).Type;
            if (structural)
            {
                var union = Assert.IsAssignableFrom<IStructuralUnionTypeSymbol>(type);
                Assert.Equal(2, union.ConstituentTypes.Length);
                Assert.Empty(union.MetadataName);
                Assert.True(SymbolEqualityComparer.Default.Equals(union, model.GetTypeInfo(syntax).Type));
            }
            else
            {
                var carrier = Assert.IsAssignableFrom<INamedTypeSymbol>(type);
                Assert.Equal("Union", carrier.Name);
            }
        }
    }
}
