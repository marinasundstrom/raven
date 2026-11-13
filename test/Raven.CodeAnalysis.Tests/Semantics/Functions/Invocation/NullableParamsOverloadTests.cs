using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class NullableParamsOverloadTests
{
    [Theory]
    [Trait("Target", "DotNet")]
    [InlineData(false)]
    [InlineData(true)]
    public void DotNet_NullableParamsArrayAcceptsExpandedArguments(bool imported)
        => Verify(TargetPlatform.DotNet, imported);

    [Theory]
    [Trait("Target", "NeoCLR")]
    [InlineData(false)]
    [InlineData(true)]
    public void NeoClr_NullableParamsArrayAcceptsExpandedArguments(bool imported)
        => Verify(TargetPlatform.NeoCLR, imported);

    [Theory]
    [Trait("Target", "DotNet")]
    [InlineData(false)]
    [InlineData(true)]
    public void DotNet_DerivedArrayPrefersNormalParamsArrayOverEnumerable(bool named)
        => VerifyDerivedArray(TargetPlatform.DotNet, named);

    [Theory]
    [Trait("Target", "NeoCLR")]
    [InlineData(false)]
    [InlineData(true)]
    public void NeoClr_DerivedArrayPrefersNormalParamsArrayOverEnumerable(bool named)
        => VerifyDerivedArray(TargetPlatform.NeoCLR, named);

    private static void VerifyDerivedArray(TargetPlatform target, bool named)
    {
        var argument = named ? "arguments: arguments" : "arguments";
        var tree = SyntaxTree.ParseText($$"""
            import System.Linq.Expressions.*
            import System.Reflection.*
            class Probe {
                static func Run(instance: Expression?, method: MethodInfo, arguments: UnaryExpression[]) -> MethodCallExpression {
                    return Expression.Call(instance, method, {{argument}})
                }
            }
            """);
        var compilation = Compilation.Create("DerivedArrayOverloads", [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));
        var model = compilation.GetSemanticModel(tree);
        var overloads = compilation.GetTypeByMetadataName("System.Linq.Expressions.Expression")!
            .GetMembers("Call").OfType<MethodSymbol>()
            .Where(m => m.Parameters.Length == 3 && m.Parameters[1].Type.Name == "MethodInfo").ToArray();
        var array = overloads.Single(m => m.Parameters[^1].IsVarParams).Parameters[^1].Type;
        var enumerable = overloads.Single(m => !m.Parameters[^1].IsVarParams).Parameters[^1].Type;
        Assert.True(compilation.ClassifyConversion(array, enumerable).IsImplicit,
            $"Expected implicit {array} -> {enumerable}");
        Assert.Empty(model.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        var method = Assert.IsAssignableFrom<MethodSymbol>(model.GetSymbolInfo(invocation).Symbol);
        Assert.True(method.Parameters[^1].IsVarParams);
    }

    private static void Verify(TargetPlatform target, bool imported)
    {
        var receiver = imported ? typeof(NullableParamsFixture).FullName! : "Collector";
        var tree = SyntaxTree.ParseText($$"""
            class Collector {
                static func Collect(marker: string, params values: object?[]?) {}
                static func Collect(marker: string, mode: int, params values: object?[]?) {}
            }
            class Probe {
                static func Run(value: object?) {
                    {{receiver}}.Collect("empty")
                    {{receiver}}.Collect("normal", null)
                    {{receiver}}.Collect("single", value)
                    {{receiver}}.Collect("multiple", value, value)
                }
            }
            """);
        var compilation = Compilation.Create("NullableParamsOverloads", [tree],
            [.. TestMetadataReferences.Default, MetadataReference.CreateFromFile(typeof(NullableParamsFixture).Assembly.Location)],
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(target));
        Assert.Empty(compilation.GetSemanticModel(tree).GetDiagnostics()
            .Where(diagnostic => diagnostic.Severity == DiagnosticSeverity.Error));
    }
}

public static class NullableParamsFixture
{
    public static int Collect(string marker, params object?[]? values) => values?.Length ?? -100;

    public static int Collect(string marker, int mode, params object?[]? values) => 1000;
}
