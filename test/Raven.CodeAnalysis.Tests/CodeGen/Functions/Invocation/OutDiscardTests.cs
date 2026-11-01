using Raven.CodeAnalysis.Semantics.Tests;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class OutDiscardTests : CompilationTestBase
{
    [Theory]
    [InlineData("return int.TryParse(\"42\", out _)", "", true)]
    [InlineData("let items = System.Collections.Generic.Dictionary<string, int>()\nitems.Add(\"key\", 42)\nreturn items.TryGetValue(\"key\", out _)", "", true)]
    [InlineData("return Copy(42, out _)", "static func Copy<T>(value: T, out result: T) -> bool { result = value; return true }", true)]
    [InlineData("return Choose(42, out _)", "static func Choose(value: int, out result: int) -> bool { result = value; return true }\nstatic func Choose(value: string, out result: string) -> bool { result = value; return false }", true)]
    [InlineData("return Write(out _, out _)", "static func Write(out first: int, out second: int) -> bool { first = 1; second = 2; return first == 1 }", true)]
    public void OutDiscardUsesSelectedParameterStorage(string body, string members, bool expected)
    {
        var source = $$"""
        class Runner {
            {{members}}
            static func Run() -> bool { {{body}} }
        }
        """;
        var (compilation, _) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
            .WithFrameworkProjectionMode(FrameworkProjectionMode.None));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(image, compilation.References);
        Assert.Equal(expected, loaded.Assembly.GetType("Runner")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Theory]
    [InlineData("ref")]
    [InlineData("in")]
    public void InputReferenceDiscardRemainsInvalid(string modifier)
    {
        var (compilation, _) = CreateCompilation($$"""
        func Consume({{modifier}} value: int) {}
        Consume({{modifier}} _)
        """);
        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAV0134");
    }

    [Fact]
    public void DiscardDoesNotChooseBetweenDifferentOutputTypes()
    {
        var (compilation, _) = CreateCompilation("""
        func Choose(out value: int) { value = 1 }
        func Choose(out value: string) { value = "one" }
        Choose(out _)
        """);
        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == CompilerDiagnostics.CallIsAmbiguous.Id);
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAV0134");
    }
}
