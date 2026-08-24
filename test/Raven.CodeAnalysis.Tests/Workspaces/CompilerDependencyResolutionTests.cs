using System.Diagnostics;
using System.Text;

using Raven;

namespace Raven.CodeAnalysis.Tests.Workspaces;

public sealed class CompilerDependencyResolutionTests
{
    [Fact]
    public void StandaloneCompile_ReferencesCodeAnalysisWithoutMacroLibrary()
    {
        var repoRoot = Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, "..", "..", "..", "..", ".."));
        var testOutput = new DirectoryInfo(AppContext.BaseDirectory);
        var configuration = testOutput.Parent?.Name ?? "Debug";
        var targetFramework = testOutput.Name;
        var compilerOutput = Path.Combine(repoRoot, "src", "Raven.Compiler", "bin", configuration, targetFramework);
        var corePath = Path.Combine(repoRoot, "src", "Raven.Core", "bin", configuration, targetFramework, "Raven.Core.dll");
        var tempRoot = Path.Combine(Path.GetTempPath(), "raven-compiler-dependency-tests", Guid.NewGuid().ToString("N"));

        try
        {
            var isolatedCompiler = Path.Combine(tempRoot, "compiler");
            CopyDirectory(compilerOutput, isolatedCompiler);

            var sourcePath = Path.Combine(tempRoot, "main.rvn");
            File.WriteAllText(sourcePath, """
                                          import System.*
                                          import Raven.CodeAnalysis.*
                                          import Raven.CodeAnalysis.Syntax.*

                                          let syntaxTree = SyntaxTree.ParseText("let x = 42")
                                          Console.WriteLine(syntaxTree.GetRoot())
                                          """);

            var compilerPath = Path.Combine(isolatedCompiler, "rvnc.dll");
            var outputPath = Path.Combine(tempRoot, "out.dll");
            var result = RunCompiler(compilerPath, sourcePath, outputPath, corePath, targetFramework);

            Assert.True(
                result.ExitCode == 0,
                $"Expected isolated compiler to resolve Raven.CodeAnalysis without Raven.Macros.\nstdout:\n{result.StdOut}\nstderr:\n{result.StdErr}");
        }
        finally
        {
            if (Directory.Exists(tempRoot))
                Directory.Delete(tempRoot, recursive: true);
        }
    }

    private static void CopyDirectory(string sourceDirectory, string destinationDirectory)
    {
        Directory.CreateDirectory(destinationDirectory);

        foreach (var file in Directory.EnumerateFiles(sourceDirectory))
        {
            if (Path.GetFileName(file).StartsWith("Raven.Macros.", StringComparison.OrdinalIgnoreCase))
                continue;

            File.Copy(file, Path.Combine(destinationDirectory, Path.GetFileName(file)));
        }

        foreach (var directory in Directory.EnumerateDirectories(sourceDirectory))
        {
            CopyDirectory(directory, Path.Combine(destinationDirectory, Path.GetFileName(directory)));
        }
    }

    private static (int ExitCode, string StdOut, string StdErr) RunCompiler(
        string compilerPath,
        string sourcePath,
        string outputPath,
        string corePath,
        string targetFramework)
    {
        var startInfo = new ProcessStartInfo(
            "dotnet",
            $"\"{compilerPath}\" \"{sourcePath}\" -o \"{outputPath}\" --framework {targetFramework} --raven-core \"{corePath}\"")
        {
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
            WorkingDirectory = Path.GetDirectoryName(sourcePath)!
        };

        using var process = Process.Start(startInfo) ?? throw new InvalidOperationException("Failed to start rvnc.");
        var stdout = new StringBuilder();
        var stderr = new StringBuilder();
        process.OutputDataReceived += (_, args) =>
        {
            if (args.Data is not null)
                stdout.AppendLine(args.Data);
        };
        process.ErrorDataReceived += (_, args) =>
        {
            if (args.Data is not null)
                stderr.AppendLine(args.Data);
        };
        process.BeginOutputReadLine();
        process.BeginErrorReadLine();

        const int timeoutMilliseconds = 30_000;
        if (!process.WaitForExit(timeoutMilliseconds))
        {
            process.Kill(entireProcessTree: true);
            _ = process.WaitForExit(5_000);
            return (-1, stdout.ToString(), $"{stderr}\nTimed out after {timeoutMilliseconds}ms.");
        }

        _ = process.WaitForExit(5_000);
        return (process.ExitCode, stdout.ToString(), stderr.ToString());
    }
}
