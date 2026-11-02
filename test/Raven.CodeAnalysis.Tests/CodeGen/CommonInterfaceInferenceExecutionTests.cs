using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class CommonInterfaceInferenceExecutionTests
{
    [Theory]
    [InlineData("IReadOnlyList<int>", "IEnumerable<int>")]
    [InlineData("int[]", "IEnumerable<int>")]
    [InlineData("IEnumerable<int>", "int[]")]
    public void Coalescing_PreservesGenericValuesAndShortCircuitsFallback(string leftType, string fallbackType)
    {
        var tree = SyntaxTree.ParseText($$"""
            import System.Collections.Generic.*
            class Probe {
                var Calls: int
                func Fallback() -> {{fallbackType}} {
                    Calls++
                    return [7, 8]
                }
                func Pick(left: {{leftType}}?) -> IEnumerable<int> => left ?? Fallback()
                func Choose(flag: bool, left: IList<int>, right: IReadOnlyList<int>) -> IEnumerable<int> {
                    let result = if flag { left } else { right }
                    return result
                }
            }
            """);
        var compilation = Compilation.Create("CommonInterfaceExecution", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join("; ", emitted.Diagnostics));
        var type = Assembly.Load(image.ToArray()).GetType("Probe")!;
        var instance = Activator.CreateInstance(type);
        var pick = type.GetMethod("Pick")!;
        var original = new[] { 1, 2, 3 };
        Assert.Same(original, pick.Invoke(instance, [original]));
        Assert.Equal(0, type.GetProperty("Calls")!.GetValue(instance));
        var fallback = Assert.IsAssignableFrom<IEnumerable<int>>(pick.Invoke(instance, [null]));
        Assert.Equal(new[] { 7, 8 }, fallback);
        Assert.Equal(1, type.GetProperty("Calls")!.GetValue(instance));
        var choose = type.GetMethod("Choose")!;
        var left = new List<int> { 4, 5 };
        Assert.Same(left, choose.Invoke(instance, [true, left, original]));
        Assert.Same(original, choose.Invoke(instance, [false, left, original]));
    }
}
