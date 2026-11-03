using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class NullableComparerInferenceTests
{
    [Fact]
    public void StringComparer_ImportsNullableEqualityInterfaceArgument()
    {
        var compilation = Compilation.Create("ComparerInterface", [], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var comparer = compilation.GetTypeByMetadataName("System.StringComparer")!;
        var equality = Assert.Single(comparer.Interfaces.Where(type => type.Name == "IEqualityComparer" && type.Arity == 1));
        Assert.True(equality.TypeArguments.Single().IsNullable);
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void HashCodeAdd_AcceptsNullableReferenceValueAndComparer(bool nullableValue, bool explicitType)
    {
        var tree = SyntaxTree.ParseText($$"""
import System.*
func Add(value: string{{(nullableValue ? "?" : "")}}) -> unit {
    var hash = HashCode()
    hash.Add{{(explicitType ? "<string?>" : "")}}(value, StringComparer.Ordinal)
}
""");
        var compilation = Compilation.Create("NullableComparer", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var diagnostics = compilation.GetDiagnostics();
        Assert.DoesNotContain(diagnostics, d => d.Severity == DiagnosticSeverity.Error);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Last();
        var method = Assert.IsAssignableFrom<MethodSymbol>(compilation.GetSemanticModel(tree).GetSymbolInfo(invocation).Symbol);
        Assert.Equal(SpecialType.System_String, method.TypeArguments.Single().GetNonNullableType().SpecialType);
        if (nullableValue || explicitType)
            Assert.True(method.TypeArguments.Single().IsNullable);
    }
}

[Trait("Target", "NeoCLR")]
public sealed class NeoClrNullableComparerInferenceTests
{
    [Fact]
    public void NeoClrWithHostReferences_AcceptsNullableComparer()
    {
        var tree = SyntaxTree.ParseText("""
            import System.*
            func Add(value: string?) -> unit {
                var hash = HashCode()
                hash.Add<string?>(value, StringComparer.Ordinal)
            }
            """);
        var compilation = Compilation.Create("NeoClrNullableComparer", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(TargetPlatform.NeoCLR));
        Assert.DoesNotContain(compilation.GetSemanticModel(tree).GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }
}
