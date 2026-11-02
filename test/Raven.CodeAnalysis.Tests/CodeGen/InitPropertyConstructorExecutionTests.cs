using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

[Trait("Target", "DotNet")]
public sealed class InitPropertyConstructorExecutionTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Constructor_InvokesInitSetterForAssignmentAndUpdates(bool inherited)
    {
        const string property = """
            private field _value: int
            private field _assignments: int
            val Value: int { get => _value; init { _value = value * 2; _assignments++ } }
            val Assignments: int => _assignments
            """;
        const string constructor = "init(value: int) { Value = value; Value += 1; self.Value++ }";
        var source = inherited
            ? $"open class Parent {{\n{property}\n}}\nclass Child : Parent {{ {constructor} }}"
            : $"class Child {{\n{property}\n{constructor}\n}}";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("InitConstructorExecution", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        var type = Assembly.Load(image.ToArray()).GetType("Child")!;
        var instance = Activator.CreateInstance(type, [1]);
        Assert.Equal(14, type.GetProperty("Value")!.GetValue(instance));
        Assert.Equal(3, type.GetProperty("Assignments")!.GetValue(instance));
    }

    [Fact]
    public void DerivedConstructor_AssignsMetadataInitProperty()
    {
        var tree = SyntaxTree.ParseText("""
            class Child : Raven.CodeAnalysis.Tests.CodeGen.InitConstructorMetadataBase {
                init(value: int) { self.Value = value }
            }
            """);
        var references = TestMetadataReferences.Default.Append(
            MetadataReference.CreateFromFile(typeof(InitConstructorMetadataBase).Assembly.Location)).ToArray();
        var compilation = Compilation.Create("MetadataInitConstructorExecution", [tree], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join("; ", result.Diagnostics));
        var type = Assembly.Load(image.ToArray()).GetType("Child")!;
        var instance = Assert.IsAssignableFrom<InitConstructorMetadataBase>(Activator.CreateInstance(type, [42]));
        Assert.Equal(42, instance.Value);
    }
}

public class InitConstructorMetadataBase
{
    public int Value { get; init; }
}
