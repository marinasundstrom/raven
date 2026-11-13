using Raven.CodeAnalysis.Semantics.Tests;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class NullableParamsOverloadExecutionTests
{
    [Fact]
    public void DerivedArrayUsesNormalParamsArrayWhenBuildingExpressionCall()
    {
        var tree = SyntaxTree.ParseText("""
            import System.*
            import System.Linq.Expressions.*
            class Probe {
                static func Run() -> int {
                    let method = typeof(Math).GetMethod("Abs", [typeof(int)])!
                    let arguments: UnaryExpression[] = [Expression.Convert(Expression.Constant(-7), typeof(int))]
                    let call = Expression.Call(null, method, arguments)
                    return Expression.Lambda<Func<int>>(call).Compile()()
                }
            }
            """);
        var compilation = Compilation.Create("DerivedArrayExecution", [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(7, loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ExpandedNullableElementsRemainDistinctFromNullArrayArguments(bool imported)
    {
        var receiver = imported ? typeof(NullableParamsFixture).FullName! : "Collector";
        var tree = SyntaxTree.ParseText($$"""
            class Collector {
                static func Collect(marker: string, params values: object?[]?) -> int => values?.Length ?? -100
                static func Collect(marker: string, mode: int, params values: object?[]?) -> int => 1000
            }
            class Probe {
                static func Run(value: object?) -> int {
                    return {{receiver}}.Collect("empty") + {{receiver}}.Collect("normal", null) +
                        {{receiver}}.Collect("single", value) + {{receiver}}.Collect("multiple", value, value)
                }
            }
            """);
        var compilation = Compilation.Create("NullableParamsExecution", [tree],
            [.. TestMetadataReferences.Default, MetadataReference.CreateFromFile(typeof(NullableParamsFixture).Assembly.Location)],
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(-97, loaded.Assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, [null]));
    }
}
