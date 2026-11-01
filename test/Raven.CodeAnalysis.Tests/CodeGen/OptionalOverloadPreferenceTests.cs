using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class OptionalOverloadPreferenceTests
{
    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void DotNet_EquivalentConversionsPreferNoOmittedArguments(bool reverse, bool named)
    {
        var shortOverload = "static func Pick(value: int) -> int => 1";
        var longOverload = "static func Pick(value: int, extra: int = 0) -> int => 2";
        var source = $$"""
            public class Cases {
                {{(reverse ? longOverload : shortOverload)}}
                {{(reverse ? shortOverload : longOverload)}}
                static func Run() -> int => Pick({{(named ? "value: " : "")}}42)
            }
            """;
        var compilation = Compile(source);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, compilation.References);
        Assert.Equal(1, loaded.Assembly.GetType("Cases")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Fact]
    public void DotNet_OmittedArgumentPreferenceDoesNotOverrideBetterConversion()
    {
        var compilation = Compile("""
            public class Cases {
                static func Pick(value: object) -> int => 1
                static func Pick(value: string, extra: int = 0) -> int => 2
                static func Run() -> int => Pick("text")
            }
            """);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, compilation.References);
        Assert.Equal(2, loaded.Assembly.GetType("Cases")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Fact]
    public void DotNet_TwoCandidatesRequiringDefaultsRemainAmbiguous()
    {
        var compilation = Compile("""
            class Cases {
                static func Pick(value: int, first: int = 0) -> int => 1
                static func Pick(value: int, first: int = 0, second: int = 0) -> int => 2
                static func Run() -> int => Pick(42)
            }
            """);
        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAV0121");
    }

    [Fact]
    public void DotNet_StreamReaderConstructorSelectsExactArity()
    {
        var compilation = Compile("""
            import System.IO.*
            import System.Text.*
            public class Cases {
                static func Open(stream: Stream, encoding: Encoding) -> StreamReader => StreamReader(stream, encoding)
            }
            """);
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

    private static Compilation Compile(string source) => Compilation.Create(
        "OptionalOverload" + Guid.NewGuid().ToString("N"), [SyntaxTree.ParseText(source)],
        TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
}
