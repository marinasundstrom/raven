using System.Diagnostics;
using System.Text;

namespace Raven.CodeAnalysis.Tests.Workspaces;

public sealed class RavenProjectOutputDeterminismTests
{
    [Fact]
    public void ProjectCompile_WithDotSegmentOutputDirectory_ProducesDeterministicAssemblyPath()
    {
        var tempRoot = CreateTempDirectory();
        try
        {
            var projectName = "DeterministicProject";
            var projectPath = Path.Combine(tempRoot, $"{projectName}.ravenproj");
            var sourcePath = Path.Combine(tempRoot, "main.rvn");
            File.WriteAllText(projectPath, $$"""
                                          <Project Name="{{projectName}}" TargetFramework="net10.0" Output="{{projectName}}" OutputKind="ConsoleApplication">
                                          </Project>
                                          """);
            File.WriteAllText(sourcePath, """
                                          import System.*
                                          Console.WriteLine("hello")
                                          """);

            var outputDirectory = Path.Combine(tempRoot, "bin", "Debug", "net10.0");
            var expectedAssemblyPath = Path.Combine(outputDirectory, $"{projectName}.dll");

            var firstRun = RunCompiler(projectPath, outputDirectory);
            Assert.Equal(0, firstRun.ExitCode);
            Assert.True(File.Exists(expectedAssemblyPath), $"Expected assembly at '{expectedAssemblyPath}'.\nstdout:\n{firstRun.StdOut}\nstderr:\n{firstRun.StdErr}");

            var secondRun = RunCompiler(projectPath, outputDirectory);
            Assert.Equal(0, secondRun.ExitCode);
            Assert.True(File.Exists(expectedAssemblyPath), $"Expected assembly at '{expectedAssemblyPath}'.\nstdout:\n{secondRun.StdOut}\nstderr:\n{secondRun.StdErr}");
        }
        finally
        {
            DeleteDirectoryIfExists(tempRoot);
        }
    }

    [Fact]
    public void ProjectCompile_WithFilePathOutput_IsRejected()
    {
        var tempRoot = CreateTempDirectory();
        try
        {
            var projectName = "DeterministicProject";
            var projectPath = Path.Combine(tempRoot, $"{projectName}.ravenproj");
            var sourcePath = Path.Combine(tempRoot, "main.rvn");
            File.WriteAllText(projectPath, $$"""
                                          <Project Name="{{projectName}}" TargetFramework="net10.0" Output="{{projectName}}" OutputKind="ConsoleApplication">
                                          </Project>
                                          """);
            File.WriteAllText(sourcePath, """
                                          import System.*
                                          Console.WriteLine("hello")
                                          """);

            var outputFilePath = Path.Combine(tempRoot, "bin", "Debug", "out.dll");
            var result = RunCompiler(projectPath, outputFilePath);

            Assert.NotEqual(0, result.ExitCode);
            Assert.Contains("For project-file inputs, -o/--output must be a directory path, not a file path.", result.StdOut + result.StdErr);
        }
        finally
        {
            DeleteDirectoryIfExists(tempRoot);
        }
    }

    private static (int ExitCode, string StdOut, string StdErr) RunCompiler(string projectPath, string outputPath)
    {
        var repoRoot = Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, "..", "..", "..", "..", ".."));
        var compilerDllPath = EnsureCompilerBuilt(repoRoot);
        var args = $"\"{compilerDllPath}\" \"{projectPath}\" -o \"{outputPath}\" --framework net10.0";

        var startInfo = new ProcessStartInfo("dotnet", args)
        {
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
            WorkingDirectory = repoRoot
        };

        using var process = Process.Start(startInfo) ?? throw new InvalidOperationException("Failed to start dotnet process.");
        var stdoutBuilder = new StringBuilder();
        var stderrBuilder = new StringBuilder();
        process.OutputDataReceived += (_, e) =>
        {
            if (e.Data is not null)
                stdoutBuilder.AppendLine(e.Data);
        };
        process.ErrorDataReceived += (_, e) =>
        {
            if (e.Data is not null)
                stderrBuilder.AppendLine(e.Data);
        };
        process.BeginOutputReadLine();
        process.BeginErrorReadLine();

        const int timeoutMilliseconds = 120_000;
        if (!process.WaitForExit(timeoutMilliseconds))
        {
            try
            {
                process.Kill(entireProcessTree: true);
            }
            catch
            {
                // Ignore kill failures in test teardown paths.
            }

            _ = process.WaitForExit(5_000);
            var timedOutStdOut = stdoutBuilder.ToString();
            var timedOutStdErr = stderrBuilder.ToString();
            return (-1, timedOutStdOut, $"{timedOutStdErr}{Environment.NewLine}Timed out after {timeoutMilliseconds}ms.");
        }

        _ = process.WaitForExit(5_000);
        var stdout = stdoutBuilder.ToString();
        var stderr = stderrBuilder.ToString();
        return (process.ExitCode, stdout, stderr);
    }

    private static string EnsureCompilerBuilt(string repoRoot)
    {
        var compilerProjectPath = Path.Combine(repoRoot, "src", "Raven.Compiler", "Raven.Compiler.csproj");
        var buildArgs = $"build \"{compilerProjectPath}\" --framework net10.0 /property:WarningLevel=0";
        var startInfo = new ProcessStartInfo("dotnet", buildArgs)
        {
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
            WorkingDirectory = repoRoot
        };

        using var process = Process.Start(startInfo) ?? throw new InvalidOperationException("Failed to start dotnet process.");
        var stdout = process.StandardOutput.ReadToEnd();
        var stderr = process.StandardError.ReadToEnd();
        process.WaitForExit();
        Assert.True(process.ExitCode == 0, $"Failed to build rvn CLI.\nstdout:\n{stdout}\nstderr:\n{stderr}");

        var compilerDllPath = Path.Combine(repoRoot, "src", "Raven.Compiler", "bin", "Debug", "net10.0", "rvn.dll");
        Assert.True(File.Exists(compilerDllPath), $"Expected compiler output at '{compilerDllPath}'.");
        return compilerDllPath;
    }

    private static string CreateTempDirectory()
    {
        var directory = Path.Combine(Path.GetTempPath(), "raven-project-output-tests", Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        return directory;
    }

    private static void DeleteDirectoryIfExists(string path)
    {
        if (!Directory.Exists(path))
            return;

        Directory.Delete(path, recursive: true);
    }
}
