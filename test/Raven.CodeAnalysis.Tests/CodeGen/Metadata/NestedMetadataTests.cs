using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public class NestedMetadataTests
{
    [Theory]
    [InlineData("let builder = System.Collections.Immutable.ImmutableArray.CreateBuilder<int>(); builder.Add(42); return builder.ToImmutable()[0]")]
    [InlineData("let values = System.Collections.Generic.Dictionary<string, int>(); values.Add(\"answer\", 42); let items = values.Values; return items.Count + 41")]
    [InlineData("let nested = Raven.CodeAnalysis.Tests.NestedMetadataFixture.Create(); return nested.First + nested.Second.Length + nested.Third")]
    public void DotNet_ImportedNestedMembersRetainEnclosingTypeArguments(string body)
    {
        var tree = SyntaxTree.ParseText($"class Probe {{ static func Run() -> int {{ {body} }} }}");
        var references = TestMetadataReferences.Default.Append(MetadataReference.CreateFromFile(typeof(NestedMetadataFixture).Assembly.Location)).ToArray();
        var compilation = Compilation.Create("NestedOwner", [tree], references,
            CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(42, loaded.Assembly.GetType("Probe", true)!.GetMethod("Run")!.Invoke(null, null));
    }
}

public static class NestedMetadataFixture
{
    public static Outer<int>.Middle<string>.Inner<byte> Create() => new(37, "abc", 2);

    public class Outer<T>
    {
        public class Middle<U>
        {
            public sealed record Inner<V>(T First, U Second, V Third);
        }
    }
}
