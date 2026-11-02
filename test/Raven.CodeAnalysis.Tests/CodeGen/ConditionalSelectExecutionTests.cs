using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class ConditionalSelectExecutionTests
{
    [Fact]
    public void ConditionalSelect_PreservesNullShortCircuitAndIndexedOverload()
    {
        var tree = SyntaxTree.ParseText("""
import System.Collections.Generic.*
import System.Linq.*
class Probe {
    var ReceiverCalls: int
    var ProjectCalls: int
    func Values(values: object?[]?) -> object?[]? { ReceiverCalls++; return values }
    func Project(values: object?[]?) -> IEnumerable<object?> {
        return Values(values)?.Select(value => {
            ProjectCalls++
            if value is string text { return text.ToUpperInvariant() }
            return value
        }) ?? []
    }
    func Indexed(values: int[]?) -> IEnumerable<int> => values?.Select((value, index) => value + index) ?? []
}
""");
        var compilation = Compilation.Create("ConditionalSelectExecution", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join("; ", emitted.Diagnostics));
        var type = Assembly.Load(image.ToArray()).GetType("Probe")!;
        var instance = Activator.CreateInstance(type);
        var project = type.GetMethod("Project")!;
        Assert.Empty(Assert.IsAssignableFrom<IEnumerable<object?>>(project.Invoke(instance, [null])));
        Assert.Equal(1, type.GetProperty("ReceiverCalls")!.GetValue(instance));
        Assert.Equal(0, type.GetProperty("ProjectCalls")!.GetValue(instance));
        object?[] values = ["raven", new object(), null];
        var projected = Assert.IsAssignableFrom<IEnumerable<object?>>(project.Invoke(instance, [values])).ToArray();
        Assert.Equal("RAVEN", projected[0]);
        Assert.Same(values[1], projected[1]);
        Assert.Null(projected[2]);
        Assert.Equal(2, type.GetProperty("ReceiverCalls")!.GetValue(instance));
        Assert.Equal(3, type.GetProperty("ProjectCalls")!.GetValue(instance));
        var indexed = type.GetMethod("Indexed")!;
        Assert.Empty(Assert.IsAssignableFrom<IEnumerable<int>>(indexed.Invoke(instance, [null])));
        Assert.Equal(new[] { 4, 6 }, Assert.IsAssignableFrom<IEnumerable<int>>(indexed.Invoke(instance, [new[] { 4, 5 }])));
    }
}
