using System.Diagnostics;
using System.Text;

using Xunit.Abstractions;

namespace Raven.CodeAnalysis.Tests.Workspaces;

public sealed class MsBuildSampleProjectCompilationTests(ITestOutputHelper output)
{
    [Fact]
    public void SampleProjects_CompileThroughRvnCli()
    {
        var repoRoot = GetRepositoryRoot();
        var projectsRoot = Path.Combine(repoRoot, "samples", "projects");
        var compilerDllPath = EnsureCompilerBuilt(repoRoot);
        var projectPaths = Directory
            .EnumerateFiles(projectsRoot, "*.rvnproj", SearchOption.AllDirectories)
            .OrderBy(static path => path, StringComparer.OrdinalIgnoreCase)
            .ToArray();

        Assert.NotEmpty(projectPaths);

        var outputRoot = CreateTempDirectory();
        try
        {
            var failures = new List<string>();
            foreach (var projectPath in projectPaths)
            {
                var relativeProjectPath = Path.GetRelativePath(repoRoot, projectPath);
                var projectOutputDirectory = Path.Combine(
                    outputRoot,
                    Path.ChangeExtension(relativeProjectPath, null) ?? Path.GetFileNameWithoutExtension(projectPath));

                Directory.CreateDirectory(projectOutputDirectory);

                var result = RunCompiler(repoRoot, compilerDllPath, projectPath, projectOutputDirectory);
                output.WriteLine($"[{relativeProjectPath}] exit={result.ExitCode}");
                if (!string.IsNullOrWhiteSpace(result.StdOut))
                    output.WriteLine(result.StdOut);
                if (!string.IsNullOrWhiteSpace(result.StdErr))
                    output.WriteLine(result.StdErr);

                if (result.ExitCode != 0)
                    failures.Add($"{relativeProjectPath}\nstdout:\n{result.StdOut}\nstderr:\n{result.StdErr}");
            }

            Assert.True(failures.Count == 0, string.Join("\n\n", failures));
        }
        finally
        {
            DeleteDirectoryIfExists(outputRoot);
        }
    }

    private static string EnsureCompilerBuilt(string repoRoot)
    {
        var compilerDllPath = Path.Combine(repoRoot, "src", "Raven.Compiler", "bin", "Debug", "net11.0", "rvn.dll");
        if (!File.Exists(compilerDllPath))
        {
            var compilerProjectPath = Path.Combine(repoRoot, "src", "Raven.Compiler", "Raven.Compiler.csproj");
            var buildArgs = $"build \"{compilerProjectPath}\" --framework net11.0 /property:WarningLevel=0";
            var buildResult = RunProcess("dotnet", buildArgs, repoRoot, timeoutMilliseconds: 300_000);
            Assert.True(
                buildResult.ExitCode == 0,
                $"Failed to build rvn CLI for sample-project tests.\nstdout:\n{buildResult.StdOut}\nstderr:\n{buildResult.StdErr}");
        }

        Assert.True(File.Exists(compilerDllPath), $"Expected compiler output at '{compilerDllPath}'.");
        return compilerDllPath;
    }

    private static (int ExitCode, string StdOut, string StdErr) RunCompiler(
        string repoRoot,
        string compilerDllPath,
        string projectPath,
        string outputDirectory)
    {
        var args = $"\"{compilerDllPath}\" \"{projectPath}\" -o \"{outputDirectory}\"";
        return RunProcess("dotnet", args, repoRoot, timeoutMilliseconds: 300_000);
    }

    private static (int ExitCode, string StdOut, string StdErr) RunProcess(
        string fileName,
        string arguments,
        string workingDirectory,
        int timeoutMilliseconds)
    {
        var startInfo = new ProcessStartInfo(fileName, arguments)
        {
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
            WorkingDirectory = workingDirectory
        };

        using var process = Process.Start(startInfo) ?? throw new InvalidOperationException($"Failed to start {fileName} process.");
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

        if (!process.WaitForExit(timeoutMilliseconds))
        {
            try
            {
                process.Kill(entireProcessTree: true);
            }
            catch
            {
                // Ignore kill failures in teardown paths.
            }

            _ = process.WaitForExit(5_000);
            return (-1, stdoutBuilder.ToString(), $"{stderrBuilder}{Environment.NewLine}Timed out after {timeoutMilliseconds}ms.");
        }

        _ = process.WaitForExit(5_000);
        return (process.ExitCode, stdoutBuilder.ToString(), stderrBuilder.ToString());
    }

    private static string GetRepositoryRoot()
        => Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, "..", "..", "..", "..", ".."));

    private static string CreateTempDirectory()
    {
        var directory = Path.Combine(Path.GetTempPath(), "raven-msbuild-sample-project-tests", Guid.NewGuid().ToString("N"));
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
