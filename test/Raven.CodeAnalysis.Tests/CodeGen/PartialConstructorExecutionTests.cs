using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

[Trait("Target", "DotNet")]
public sealed class PartialConstructorExecutionTests
{
    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void PartialConstructor_InitializesBaseWithoutAddingDefault(bool reverseTrees, bool primaryConstructor)
    {
        var sources = new[]
        {
            "open class Parent { val Value: int; init(value: int) { Value = value } }",
            primaryConstructor
                ? "partial class Child(value: int) : Parent(value) { }"
                : "partial class Child : Parent { init(value: int) : base(value) { } }",
            "partial class Child : Parent { }"
        };
        var trees = sources.Select(source => SyntaxTree.ParseText(source)).ToArray();
        var compilation = Compilation.Create("PartialConstructorExecution", reverseTrees ? trees.Reverse().ToArray() : trees,
            TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        var assembly = Assembly.Load(image.ToArray());
        var child = assembly.GetType("Child")!;
        var constructor = Assert.Single(child.GetConstructors());
        Assert.Single(constructor.GetParameters());
        var instance = constructor.Invoke([42]);
        Assert.Equal(42, child.GetProperty("Value")!.GetValue(instance));
    }
}
