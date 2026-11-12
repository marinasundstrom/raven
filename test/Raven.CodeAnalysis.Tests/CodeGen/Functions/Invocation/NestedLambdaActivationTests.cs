using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class NestedLambdaActivationTests
{
    [Theory]
    [InlineData("value == candidate")]
    [InlineData("Equal(value, candidate)")]
    public void NestedQueryCapturesOuterLambdaParameter(string predicate)
    {
        Assert.Equal(1, Execute($$"""
import System.*
import System.Linq.*
class Runner {
    static func Run() -> int {
        let candidates = [1, 2]
        let values = [2, 3]
        return candidates.Where(candidate => values.Any(value => {{predicate}})).Count()
        func Equal(value: int, expected: int) -> bool {
            return value == expected
        }
    }
}
"""));
    }

    [Fact]
    public void ReturnedClosuresKeepEachInvocationParameterAndEnclosingCapture()
    {
        Assert.Equal(1213, Execute("""
import System.*
class Runner {
    static func Run() -> int {
        let offset = 10
        let make: Func<int, Func<int>> = value => () => value + offset
        let first = make(2)
        let second = make(3)
        return first() * 100 + second()
    }
}
"""));
    }

    [Fact]
    public void ReturnedClosuresKeepGenericParameterValues()
    {
        Assert.Equal(23, Execute("""
import System.*
class Runner {
    static func Make<T>(value: T) -> Func<T, Func<T>> {
        return other => () => other
    }
    static func Run() -> int {
        let make = Make<int>(0)
        let first = make(2)
        let second = make(3)
        return first() * 10 + second()
    }
}
"""));
    }

    [Fact]
    public void ReturnedClosuresMutateIndependentInvocationLocals()
    {
        Assert.Equal(414, Execute("""
import System.*
class Runner {
    static func Run() -> int {
        let make: Func<int, Func<int>> = value => {
            var count = value
            return () => {
                count += 1
                return count
            }
        }
        let first = make(2)
        let second = make(10)
        return first() * 100 + second() * 10 + first()
    }
}
"""));
    }

    private static int Execute(string source)
    {
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("NestedLambdaActivation", [SyntaxTree.ParseText(source)], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        var method = loaded.Assembly.GetType("Runner", true)!.GetMethod("Run", BindingFlags.Public | BindingFlags.Static)!;
        return (int)method.Invoke(null, null)!;
    }
}
