using System.Diagnostics;
using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public class SampleProgramsTests
{
    public static IEnumerable<object[]> SamplePrograms =>
    [
        new object[] {"arrays.rav", Array.Empty<string>()},
        ["enums.rav", Array.Empty<string>()],
        ["general.rav", Array.Empty<string>()],
        ["generics.rav", Array.Empty<string>()],
        ["io.rav", new[] {"."}],
        ["test2.rav", Array.Empty<string>()],
        ["type-unions.rav", Array.Empty<string>()],
        ["tuples.rav", Array.Empty<string>()],
        ["main.rav", Array.Empty<string>()],
        ["classes.rav", Array.Empty<string>()],
    ];

    [Theory]
    [MemberData(nameof(SamplePrograms))]
    public async Task Sample_should_compile_and_run(string fileName, string[] args)
    {
        var projectDir = Path.GetFullPath(Path.Combine("..", "..", "..", "..", "..", "src", "Raven.Compiler"));
        var samplePath = Path.Combine(projectDir, "samples", fileName);

        var outputDir = Path.Combine(Path.GetTempPath(), "raven_samples", Guid.NewGuid().ToString());
        Directory.CreateDirectory(outputDir);
        var outputDll = Path.Combine(outputDir, Path.ChangeExtension(fileName, ".dll"));

        var build = Process.Start(new ProcessStartInfo
        {
            FileName = "dotnet",
            Arguments = $"run --project \"{projectDir}\" -- \"{samplePath}\" -o \"{outputDll}\"",
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
            WorkingDirectory = projectDir,
        })!;

        build.WaitForExit(TimeSpan.FromSeconds(3));
        _ = await build.StandardOutput.ReadToEndAsync();
        build.WaitForExit();

        Assert.Equal(0, build.ExitCode);

        var testDep = Path.Combine(projectDir, "TestDep.dll");
        if (File.Exists(testDep))
            File.Copy(testDep, Path.Combine(outputDir, "TestDep.dll"), overwrite: true);

        Assert.True(File.Exists(Path.Combine(outputDir, Path.ChangeExtension(fileName, ".runtimeconfig.json"))));

        var run = Process.Start(new ProcessStartInfo
        {
            FileName = "dotnet",
            Arguments = $"\"{outputDll}\" {string.Join(' ', args)}",
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
            WorkingDirectory = outputDir,
        })!;

        run.WaitForExit(TimeSpan.FromSeconds(2));
        _ = await run.StandardOutput.ReadToEndAsync();
        run.WaitForExit();

        Assert.Equal(0, run.ExitCode);
    }

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
    public async Task NoEmit_flag_should_skip_emission()
    {
        var projectDir = Path.GetFullPath(Path.Combine("..", "..", "..", "..", "..", "src", "Raven.Compiler"));
        var samplePath = Path.Combine(projectDir, "samples", "enums.rav");

        var outputDir = Path.Combine(Path.GetTempPath(), "raven_samples", Guid.NewGuid().ToString());
        Directory.CreateDirectory(outputDir);
        var outputDll = Path.Combine(outputDir, "enums.dll");

        var build = Process.Start(new ProcessStartInfo
        {
            FileName = "dotnet",
            Arguments = $"run --project \\\"{projectDir}\\\" -- \\\"{samplePath}\\\" -o \\\"{outputDll}\\\" --no-emit",
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
            WorkingDirectory = projectDir,
        })!;

        build.WaitForExit(TimeSpan.FromSeconds(3));
        _ = await build.StandardOutput.ReadToEndAsync();
        build.WaitForExit();

        Assert.Equal(0, build.ExitCode);
        Assert.False(File.Exists(outputDll));
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
