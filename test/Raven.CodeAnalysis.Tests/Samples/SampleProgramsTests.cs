using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests;

public class SampleProgramsTests
{
    public static IEnumerable<object[]> SamplePrograms =>
    [
        ["arrays.rav"],
        ["async/async-inference-regression.rav"],
        ["classes.rav"],
        ["enums.rav"],
        ["general.rav"],
        ["generics/generics.rav"],
        ["io.rav"],
        ["main.rav"],
        ["discriminated-unions-generics.rav"],
        ["discriminated-unions2.rav"],
        ["test2.rav"],
        ["tuples.rav"],
        ["type-unions.rav"],
    ];

    public static IEnumerable<object[]> SampleProgramsWithExpectedOutputs =>
    [
        ["async/async-inference-regression.rav"],
        ["discriminated-unions-generics.rav"],
        ["discriminated-unions2.rav"],
    ];

    [Theory]
    [MemberData(nameof(SamplePrograms))]
    public void Sample_should_load_into_compilation(string fileName)
    {
        var repoRoot = Path.GetFullPath(Path.Combine("..", "..", "..", "..", ".."));
        var compilerDir = Path.Combine(repoRoot, "src", "Raven.Compiler");
        var samplePath = Path.Combine(repoRoot, "samples", fileName);
        var text = File.ReadAllText(samplePath);
        var tree = SyntaxTree.ParseText(text, path: samplePath);

        var outputDir = Path.Combine(Path.GetTempPath(), "raven_samples", Guid.NewGuid().ToString());
        Directory.CreateDirectory(outputDir);

        var testDepOutputPath = Path.Combine(outputDir, "TestDep.dll");

        var testDep = Path.Combine(compilerDir, "TestDep.dll");
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

        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        var errors = diagnostics.Where(d => d.Severity == DiagnosticSeverity.Error);
        Assert.Empty(errors);
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

    [Theory]
    [MemberData(nameof(SampleProgramsWithExpectedOutputs))]
    public void Sample_should_match_expected_output(string fileName)
    {
        var (samplePath, code) = ReadSample(fileName);
        var expectedPath = Path.ChangeExtension(samplePath, ".out");
        var expected = File.ReadAllText(expectedPath).ReplaceLineEndings("\n").TrimEnd('\n');

        var output = EmitAndRun(code, Path.GetFileNameWithoutExtension(fileName));
        Assert.Equal(expected, output);
    }

    private static (string Path, string Code) ReadSample(string fileName)
    {
        var repoRoot = GetRepositoryRoot();
        var samplePath = Path.Combine(repoRoot, "samples", fileName);
        var code = File.ReadAllText(samplePath);
        return (samplePath, code);
    }

    private static string GetRepositoryRoot()
    {
        return Path.GetFullPath(Path.Combine("..", "..", "..", "..", ".."));
    }

    private static string EmitAndRun(string code, string assemblyName)
    {
        var syntaxTree = Syntax.SyntaxTree.ParseText(code);
        var references = RuntimeMetadataReferences;

        var compilation = Compilation.Create(assemblyName, new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(syntaxTree)
            .AddReferences(references);

        compilation.EnsureSetup();

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        var assemblyBytes = peStream.ToArray();
        var assembly = Assembly.Load(assemblyBytes);
        var entryPoint = assembly.EntryPoint;
        Assert.NotNull(entryPoint);

        var originalOut = Console.Out;
        using var writer = new StringWriter();
        Console.SetOut(writer);

        try
        {
            var parameters = entryPoint!.GetParameters();

            object?[]? arguments = parameters.Length switch
            {
                0 => null,
                1 => new object?[] { Array.Empty<string>() },
                _ => throw new InvalidOperationException("Unexpected entry point signature."),
            };

            entryPoint.Invoke(null, arguments);
        }
        finally
        {
            Console.SetOut(originalOut);
        }

        var output = writer.ToString();
        return output.ReplaceLineEndings("\n").TrimEnd('\n');
    }

    private static readonly MetadataReference[] RuntimeMetadataReferences = GetRuntimeMetadataReferences();

    private static MetadataReference[] GetRuntimeMetadataReferences()
    {
        var tpa = AppContext.GetData("TRUSTED_PLATFORM_ASSEMBLIES") as string;
        if (string.IsNullOrEmpty(tpa))
            return TestMetadataReferences.Default;

        var references = new List<MetadataReference>();
        var seen = new HashSet<string>(StringComparer.OrdinalIgnoreCase);

        foreach (var path in tpa.Split(Path.PathSeparator))
        {
            if (string.IsNullOrEmpty(path))
                continue;

            var name = Path.GetFileNameWithoutExtension(path);
            if (!seen.Add(name))
                continue;

            references.Add(MetadataReference.CreateFromFile(path));
        }

        return references.ToArray();
    }
}
