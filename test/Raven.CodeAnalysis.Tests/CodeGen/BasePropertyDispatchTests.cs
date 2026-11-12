using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class BasePropertyDispatchTests
{
    [Theory]
    [InlineData(false, "return base.Value", 7)]
    [InlineData(false, "return self.Value", 99)]
    [InlineData(false, "base.Value = 11\nreturn ReadStored()", 11)]
    [InlineData(false, "base.Value += 2\nreturn ReadStored()", 9)]
    [InlineData(true, "return base[0]", 7)]
    [InlineData(true, "return self[0]", 99)]
    [InlineData(true, "base[0] = 11\nreturn ReadStored()", 11)]
    [InlineData(true, "base[0] += 2\nreturn ReadStored()", 9)]
    [InlineData(false, "let previous = base.Value++\nreturn previous * 10 + ReadStored()", 78)]
    [InlineData(false, "let next = ++base.Value\nreturn next * 10 + ReadStored()", 88)]
    [InlineData(true, "let previous = base[0]++\nreturn previous * 10 + ReadStored()", 78)]
    [InlineData(true, "let next = ++base[0]\nreturn next * 10 + ReadStored()", 88)]
    public void BaseAccessorBypassesOverrideWhileOrdinaryAccessRemainsVirtual(bool indexer, string body, int expected)
    {
        var member = indexer ? "self[index: int]" : "Value";
        var source = $$"""
open class Parent {
    private var saved: int = 7

    virtual var {{member}}: int {
        get => saved
        set { saved = value }
    }

    func ReadStored() -> int => saved
}

class Child : Parent {
    private var marker: int = 0

    override var {{member}}: int {
        get => 99
        set { marker = value }
    }

    func Run() -> int {
        {{body}}
    }
}
""";
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("base-accessors", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(SyntaxTree.ParseText(source)).AddReferences(references);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        var type = loaded.Assembly.GetType("Child", true)!;
        Assert.Equal(expected, type.GetMethod("Run")!.Invoke(Activator.CreateInstance(type), null));
    }
}
