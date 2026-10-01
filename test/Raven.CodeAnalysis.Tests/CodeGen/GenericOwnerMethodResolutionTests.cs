using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class GenericOwnerMethodResolutionTests
{
    [Theory]
    [InlineData(OptimizationLevel.Release)]
    [InlineData(OptimizationLevel.Debug)]
    public void StaticGenericOwnersKeepTypeAndMethodScopesIndependent(OptimizationLevel optimization)
    {
        var tree = SyntaxTree.ParseText("""
            static class PairHelpers<Left, Right> {
                static func Second(first: Left, second: Right) -> Right => second
                static func Flip(first: Left, second: Right) -> Left => PairHelpers<Right, Left>.Second(second, first)
                static func Cross<Other>(value: Left, other: Other) -> Left => PairHelpers<Other, Left>.Second(other, value)
            }
            static class Helpers<Element> {
                static func SelectValue<Result>(ignored: Element, value: Result) -> Result => value
                static func Forward<Other>(ignored: Element, value: Other) -> Other => SelectValue<Other>(ignored, value)
                static func Empty() -> Element => default(Element)
                static func First(values: Element[]) -> Element => values[0]
            }
            func Main() -> int {
                if PairHelpers<int, long>.Flip(42, 5000000000L) != 42 { return 15 }
                if PairHelpers<int, long>.Cross<long>(42, 5000000000L) != 42 { return 16 }
                let values: int[] = [42]
                if Helpers<int>.Forward<long>(1, 5000000000L) != 5000000000L { return 1 }
                if Helpers<int>.Empty() != 0 { return 2 }
                return Helpers<int>.First(values)
            }
            """);
        var compilation = Compilation.Create("GenericOwners", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication).WithOptimizationLevel(optimization));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        Assert.Equal(42, Assembly.Load(image.ToArray()).EntryPoint!.Invoke(null, null));
    }

}
