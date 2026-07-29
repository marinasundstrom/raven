using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class Sha256DigestMacroTests
{
    [Theory]
    [InlineData("\"hello\"", "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824")]
    [InlineData("42", "73475cb40a568e8da8a045ced110137e159f890ac4da883b6b17dc651b3a8049")]
    public void Sha256Digest_EmbedsExpectedDigest(string literal, string expected)
    {
        var result = InvokeRun(
            $$"""
            import Raven.Macros.*

            class Harness {
                public static func Run() -> string {
                    return sha256Digest!({{literal}})
                }
            }
            """);

        Assert.Equal(expected, result);
    }

    [Fact]
    public void Sha256Digest_NonLiteralReportsDiagnostic()
    {
        var syntaxTree = SyntaxTree.ParseText(
            """
            import Raven.Macros.*

            func Main(value: string) -> string =>
                sha256Digest!(value)
            """);
        var compilation = CreateCompilation(syntaxTree);

        var diagnostic = Assert.Single(
            compilation.GetDiagnostics()
                .Where(static diagnostic =>
                    diagnostic.Id == "RAVM021" &&
                    diagnostic.GetMessage().Contains("SHA256001", StringComparison.Ordinal)));

        Assert.Equal(
            "value",
            syntaxTree.GetText().ToString(diagnostic.Location.SourceSpan));
    }

    private static object? InvokeRun(string source)
    {
        var syntaxTree = SyntaxTree.ParseText(source);
        var compilation = CreateCompilation(syntaxTree);

        using var peStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream);
        Assert.True(
            emitResult.Success,
            string.Join(Environment.NewLine, emitResult.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(
            peStream,
            TestMetadataReferences.Default);
        var method = loaded.Assembly
            .GetType("Harness", throwOnError: true)!
            .GetMethod("Run", BindingFlags.Public | BindingFlags.Static);
        return method!.Invoke(null, null);
    }

    private static Compilation CreateCompilation(SyntaxTree syntaxTree)
        => Compilation.Create(
                "test",
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(TestMetadataReferences.DefaultWithRavenMacros);
}
