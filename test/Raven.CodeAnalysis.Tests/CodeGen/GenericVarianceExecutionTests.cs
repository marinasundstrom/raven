using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class GenericVarianceExecutionTests
{
    [Fact]
    public void GenericTypeArguments_MatchCSharpInBothParameterOrders()
    {
        var tree = SyntaxTree.ParseText("""
            import System.*
            import System.Collections.Generic.*
            class Probe {
                static func First<T>(value: T, comparer: IComparer<T>) -> string => typeof(T).Name
                static func Last<T>(comparer: IComparer<T>, value: T) -> string => typeof(T).Name
                static func Run() -> string => First("x", Comparer<object>.Default) + "|" +
                    Last(Comparer<object>.Default, "x") + "|" +
                    First("x", StringComparer.Ordinal) + "|" + Last(StringComparer.Ordinal, "x")
            }
            """);
        var compilation = Compilation.Create("GenericVarianceExecution", [tree], TestMetadataReferences.Default,
            CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join("; ", emitted.Diagnostics));
        var method = Assembly.Load(image.ToArray()).GetType("Probe")!.GetMethod("Run")!;
        static string First<T>(T value, IComparer<T> comparer) => typeof(T).Name;
        static string Last<T>(IComparer<T> comparer, T value) => typeof(T).Name;
        var expected = First("x", Comparer<object>.Default) + "|" + Last(Comparer<object>.Default, "x") + "|" +
            First("x", StringComparer.Ordinal) + "|" + Last(StringComparer.Ordinal, "x");
        Assert.Equal(expected, method.Invoke(null, null));
    }
}
