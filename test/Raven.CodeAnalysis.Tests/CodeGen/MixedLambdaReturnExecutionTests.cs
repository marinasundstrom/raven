using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class MixedLambdaReturnExecutionTests
{
    [Fact]
    public void Select_PreservesNullsAndObjectIdentityAcrossMixedReturns()
    {
        var tree = SyntaxTree.ParseText("""
import System.Collections.Generic.*
import System.Linq.*
class Probe {
    static func Project(values: object?[]) -> IEnumerable<object?> {
        return values.Select(value => {
            if value is string text { return text.ToUpperInvariant() }
            return value
        })
    }
}
""");
        var compilation = Compilation.Create("MixedLambdaExecution", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join("; ", emitted.Diagnostics));
        var type = Assembly.Load(image.ToArray()).GetType("Probe")!;
        object?[] input = ["raven", new object(), null, 42];
        var output = Assert.IsAssignableFrom<IEnumerable<object?>>(type.GetMethod("Project")!.Invoke(null, [input])).ToArray();
        Assert.Equal(input.Select(value => value is string text ? text.ToUpperInvariant() : value), output);
        Assert.Same(input[1], output[1]);
        Assert.Same(input[3], output[3]);
    }
}
