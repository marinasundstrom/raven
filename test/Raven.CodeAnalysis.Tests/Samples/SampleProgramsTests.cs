using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public class SampleProgramsTests
{
    public static IEnumerable<object[]> SamplePrograms =>
    [
        ["arrays.rav"],
        ["enums.rav"],
        ["general.rav"],
        ["generics.rav"],
        ["io.rav"],
        ["test2.rav"],
        ["type-unions.rav"],
        ["tuples.rav"],
        ["main.rav"],
        ["classes.rav"],
    ];

    [Theory]
    [MemberData(nameof(SamplePrograms))]
    public void Sample_should_load_into_compilation(string fileName)
    {
        var projectDir = Path.GetFullPath(Path.Combine("..", "..", "..", "..", "..", "src", "Raven.Compiler"));
        var samplePath = Path.Combine(projectDir, "samples", fileName);
        var text = File.ReadAllText(samplePath);
        var tree = SyntaxTree.ParseText(text, path: samplePath);

        var outputDir = Path.Combine(Path.GetTempPath(), "raven_samples", Guid.NewGuid().ToString());
        Directory.CreateDirectory(outputDir);

        var testDepOutputPath = Path.Combine(outputDir, "TestDep.dll");

        var testDep = Path.Combine(projectDir, "TestDep.dll");
        if (File.Exists(testDep))
            File.Copy(testDep, testDepOutputPath, overwrite: true);

        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        var refAssembliesPath = TargetFrameworkResolver.GetDirectoryPath(version);

        var compilation = Compilation.Create("samples", [tree], new CompilationOptions(OutputKind.ConsoleApplication))
                .AddReferences([
                    MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.dll")),
                    MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Collections.dll")),
                    MetadataReference.CreateFromFile(typeof(Console).Assembly.Location),
                    MetadataReference.CreateFromFile(testDepOutputPath)]);

        var diagnostics = compilation.GetDiagnostics();
        Assert.Empty(diagnostics.Where(d => d.Descriptor != CompilerDiagnostics.FileScopedCodeOutOfOrder));
    }

    [Fact]
    public void Test()
    {
        var source = """
        //import System.*

        System.Console.WriteLine("Test")
        """;

        var tree = SyntaxTree.ParseText(source);

        var version = TargetFrameworkResolver.ResolveVersion(TestTargetFramework.Default);
        var refAssembliesPath = TargetFrameworkResolver.GetDirectoryPath(version);

        var compilation = Compilation.Create("samples", [tree], new CompilationOptions(OutputKind.ConsoleApplication))
                .AddReferences([
                    MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Runtime.dll")),
                    MetadataReference.CreateFromFile(Path.Combine(refAssembliesPath!, "System.Collections.dll")),
                    MetadataReference.CreateFromFile(typeof(Console).Assembly.Location)]);

        var diagnostics = compilation.GetDiagnostics();
        Assert.Empty(diagnostics);
    }
}
