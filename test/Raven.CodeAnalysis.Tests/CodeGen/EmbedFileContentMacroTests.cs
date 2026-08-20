using System.Reflection;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class EmbedFileContentMacroTests
{
    [Fact]
    public void EmbedFileContent_EmbedsTextWithoutRuntimeFileAccess()
    {
        var tempRoot = CreateTempRoot();
        try
        {
            var sourcePath = Path.Combine(tempRoot, "Program.rvn");
            var embeddedPath = Path.Combine(tempRoot, "assets", "template.txt");
            Directory.CreateDirectory(Path.GetDirectoryName(embeddedPath)!);
            const string expected = "first line\n\"quoted\" \\\\ path";
            File.WriteAllText(embeddedPath, expected);

            var result = InvokeRun(
                """
                import Raven.Macros.*

                class Harness {
                    public static func Run() -> string {
                        return embedFileContent!("assets/template.txt")
                    }
                }
                """,
                sourcePath);

            Assert.Equal(expected, result);
        }
        finally
        {
            Directory.Delete(tempRoot, recursive: true);
        }
    }

    [Fact]
    public void EmbedFileContent_MissingFileReportsDiagnosticAtPathArgument()
    {
        var tempRoot = CreateTempRoot();
        try
        {
            var syntaxTree = SyntaxTree.ParseText(
                """
                import Raven.Macros.*

                func Main() -> string =>
                    embedFileContent!("missing.txt")
                """,
                path: Path.Combine(tempRoot, "Program.rvn"));
            var compilation = CreateCompilation(syntaxTree);

            var diagnostic = Assert.Single(
                compilation.GetDiagnostics()
                    .Where(static diagnostic =>
                        diagnostic.Id == "RAVM021" &&
                        diagnostic.GetMessage().Contains("EMBEDFILE001", StringComparison.Ordinal)));

            Assert.Equal(
                "\"missing.txt\"",
                syntaxTree.GetText().ToString(diagnostic.Location.SourceSpan));
        }
        finally
        {
            Directory.Delete(tempRoot, recursive: true);
        }
    }

    [Fact]
    public void EmbedFileContent_CachedExpansionRefreshesAfterChangeDeleteAndRecreate()
    {
        var tempRoot = CreateTempRoot();
        try
        {
            var embeddedPath = Path.Combine(tempRoot, "content.txt");
            File.WriteAllText(embeddedPath, "first");
            var syntaxTree = SyntaxTree.ParseText(
                """
                import Raven.Macros.*

                func Main() -> string =>
                    embedFileContent!("content.txt")
                """,
                path: Path.Combine(tempRoot, "Program.rvn"));
            var compilation = CreateCompilation(syntaxTree);
            var model = compilation.GetSemanticModel(syntaxTree);
            var invocation = syntaxTree.GetRoot()
                .DescendantNodes()
                .OfType<FreestandingMacroExpressionSyntax>()
                .Single();

            var initial = model.GetMacroExpansion(invocation);
            Assert.Equal("first", GetEmbeddedValue(initial));
            Assert.Single(initial!.FileDependencies);

            File.WriteAllText(embeddedPath, "second");
            File.SetLastWriteTimeUtc(embeddedPath, DateTime.UtcNow.AddSeconds(2));
            Assert.Equal("second", GetEmbeddedValue(model.GetMacroExpansion(invocation)));

            File.Delete(embeddedPath);
            var missing = model.GetMacroExpansion(invocation);
            Assert.Null(missing?.Expression);
            Assert.Contains(
                missing!.MacroDiagnostics,
                static diagnostic => diagnostic.Code == "EMBEDFILE001");

            File.WriteAllText(embeddedPath, "third");
            Assert.Equal("third", GetEmbeddedValue(model.GetMacroExpansion(invocation)));
            Assert.DoesNotContain(
                model.GetDiagnostics(),
                static diagnostic => diagnostic.GetMessage().Contains("EMBEDFILE001", StringComparison.Ordinal));
        }
        finally
        {
            Directory.Delete(tempRoot, recursive: true);
        }
    }

    private static string? GetEmbeddedValue(FreestandingMacroExpansionResult? expansion)
        => (expansion?.Expression as LiteralExpressionSyntax)?.Token.ValueText;

    private static object? InvokeRun(string source, string sourcePath)
    {
        var syntaxTree = SyntaxTree.ParseText(source, path: sourcePath);
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

    private static string CreateTempRoot()
    {
        var path = Path.Combine(
            Path.GetTempPath(),
            $"raven-embed-file-content-{Guid.NewGuid():N}");
        Directory.CreateDirectory(path);
        return path;
    }
}
