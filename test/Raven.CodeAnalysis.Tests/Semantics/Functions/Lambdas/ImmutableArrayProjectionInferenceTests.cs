using Raven.CodeAnalysis.Semantics.Tests;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Semantics.Functions.Lambdas;

[Trait("Target", "DotNet")]
public sealed class ImmutableArrayProjectionInferenceTests : CompilationTestBase
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ProjectionInfersInputFromImmutableArrayAndOutputFromLambda(bool staticLambda)
    {
        var code = """
            import System.Collections.Immutable.*

            class Program {
                static func Convert(values: ImmutableArray<int>) -> ImmutableArray<string> {
                    return ImmutableArray.CreateRange(values, LAMBDA(value) => value.ToString())
                }
            }
            """.Replace("LAMBDA", staticLambda ? "static func" : "func");
        var (compilation, tree) = CreateCompilation(code);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var model = compilation.GetSemanticModel(tree);
        var syntax = Assert.Single(tree.GetRoot().DescendantNodes().OfType<ParenthesizedFunctionExpressionSyntax>());
        var lambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(syntax));
        Assert.Equal(SpecialType.System_Int32, Assert.Single(lambda.Parameters).Type.SpecialType);
        Assert.Equal(SpecialType.System_String, lambda.ReturnType.SpecialType);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>()
            .First(n => n.Expression.ToString().EndsWith(".CreateRange", StringComparison.Ordinal));
        var type = Assert.IsAssignableFrom<NamedTypeSymbol>(model.GetTypeInfo(invocation).Type);
        Assert.Equal(SpecialType.System_String, Assert.Single(type.TypeArguments).SpecialType);
    }

    [Fact]
    public void StaticContainerMarkerDoesNotMakeOrdinaryMethodsExtensions()
    {
        var (compilation, _) = CreateCompilation("class Program {}");
        var type = compilation.GetTypeByMetadataName("System.Collections.Immutable.ImmutableArray")!;
        var methods = type.GetMembers("CreateRange").OfType<MethodSymbol>().ToArray();
        Assert.NotEmpty(methods);
        Assert.All(methods, method => Assert.False(method.IsExtensionMethod));
        var enumerable = compilation.GetTypeByMetadataName("System.Linq.Enumerable")!;
        Assert.All(enumerable.GetMembers("Select").OfType<MethodSymbol>(), method => Assert.True(method.IsExtensionMethod));
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(true, false)]
    [InlineData(false, true)]
    [InlineData(true, true)]
    public void RealExtensionProjectionPreservesStaticAndInstanceCallInference(bool staticLambda, bool extensionSyntax)
    {
        var code = """
            import System.Collections.Generic.*
            import System.Linq.*

            class Program {
                static func Convert(values: int[]) -> IEnumerable<string> {
                    return CALL(LAMBDA(value) => value.ToString())
                }
            }
            """.Replace("LAMBDA", staticLambda ? "static func" : "func")
            .Replace("CALL(", extensionSyntax ? "values.Select(" : "Enumerable.Select(values, ");
        var (compilation, tree) = CreateCompilation(code);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var model = compilation.GetSemanticModel(tree);
        var syntax = Assert.Single(tree.GetRoot().DescendantNodes().OfType<ParenthesizedFunctionExpressionSyntax>());
        var lambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(syntax));
        Assert.Equal(SpecialType.System_Int32, Assert.Single(lambda.Parameters).Type.SpecialType);
        Assert.Equal(SpecialType.System_String, lambda.ReturnType.SpecialType);
    }

    [Theory]
    [InlineData("Projection.Project(values, func(value) => value.ToString())")]
    [InlineData("values.Project(func(value) => value.ToString())")]
    [InlineData("values |> Project(func(value) => value.ToString())")]
    public void ExtensionOverloadsWithDifferentVisibleArityUseCorrectCallback(string call)
    {
        const string metadata = """
            import System.Runtime.CompilerServices.*
            namespace ProjectionFixture {
                public static class Projection {
                    [ExtensionAttribute]
                    static func Project<TResult>(values: int[], selector: int -> TResult) -> TResult {
                        return selector(values[0])
                    }
                    [ExtensionAttribute]
                    static func Project<TState, TResult>(values: int[], selector: (int, TState) -> TResult, state: TState) -> TResult {
                        return selector(values[0], state)
                    }
                }
            }
            """;
        var reference = TestMetadataFactory.CreateFileReferenceFromSource(metadata, assemblyName: "projection-arity-fixture");
        var code = """
            import ProjectionFixture.*
            class Program {
                static func Convert(values: int[]) -> string {
                    return CALL
                }
            }
            """.Replace("CALL", call);
        var (compilation, tree) = CreateCompilation(code, references: TestMetadataReferences.Default.Append(reference).ToArray());
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var model = compilation.GetSemanticModel(tree);
        var syntax = Assert.Single(tree.GetRoot().DescendantNodes().OfType<ParenthesizedFunctionExpressionSyntax>());
        var lambda = Assert.IsType<BoundFunctionExpression>(model.GetBoundNode(syntax));
        Assert.Equal(SpecialType.System_Int32, Assert.Single(lambda.Parameters).Type.SpecialType);
        Assert.Equal(SpecialType.System_String, lambda.ReturnType.SpecialType);
    }

}
