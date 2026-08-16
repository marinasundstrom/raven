using System.Diagnostics;
using System.Text;

using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Tests.Workspaces;

public sealed class RavenCliFileRunTests
{
    [Fact]
    public void RunSourceFile_ForwardsArgumentsAndLeavesSourceDirectoryClean()
    {
        var repoRoot = GetRepositoryRoot();
        var frontendPath = EnsureFrontendBuilt(repoRoot);
        var samplePath = Path.Combine(repoRoot, "samples", "scripts", "hello.rvn");
        var sampleDirectory = Path.GetDirectoryName(samplePath)!;
        var existingFiles = Directory.GetFiles(sampleDirectory).Order().ToArray();

        var result = RunProcess(
            "dotnet",
            [frontendPath, "run", samplePath, "--", "first", "second"],
            repoRoot);

        Assert.Equal(0, result.ExitCode);
        Assert.Contains("Hello from a single Raven file!", result.StdOut);
        Assert.Contains("Argument: first", result.StdOut);
        Assert.Contains("Argument: second", result.StdOut);
        Assert.Equal(existingFiles, Directory.GetFiles(sampleDirectory).Order().ToArray());
    }

    [Fact]
    public void SourcePathShorthand_ForwardsArgumentsWithoutSeparator()
    {
        var repoRoot = GetRepositoryRoot();
        var frontendPath = EnsureFrontendBuilt(repoRoot);
        var samplePath = Path.Combine(repoRoot, "samples", "scripts", "hello.rvn");

        var result = RunProcess(
            "dotnet",
            [frontendPath, samplePath, "from-shorthand"],
            repoRoot);

        Assert.Equal(0, result.ExitCode);
        Assert.Contains("Argument: from-shorthand", result.StdOut);
    }

    [Fact]
    public void ExecutableRavenFile_RunsThroughShebang()
    {
        if (OperatingSystem.IsWindows())
            return;

        var repoRoot = GetRepositoryRoot();
        _ = EnsureFrontendBuilt(repoRoot);
        var samplePath = Path.Combine(repoRoot, "samples", "scripts", "hello.rvn");
        var developmentBin = Path.Combine(repoRoot, "eng", "development");

        var result = RunProcess(
            samplePath,
            ["from-shebang"],
            repoRoot,
            configure: startInfo =>
                startInfo.Environment["PATH"] = $"{developmentBin}{Path.PathSeparator}{Environment.GetEnvironmentVariable("PATH")}");

        Assert.Equal(0, result.ExitCode);
        Assert.Contains("Argument: from-shebang", result.StdOut);
    }

    [Fact]
    public void RunSourceFile_ReturnsApplicationExitCode()
    {
        var repoRoot = GetRepositoryRoot();
        var frontendPath = EnsureFrontendBuilt(repoRoot);
        var tempDirectory = Path.Combine(Path.GetTempPath(), $"raven-cli-test-{Guid.NewGuid():N}");
        Directory.CreateDirectory(tempDirectory);

        try
        {
            var sourcePath = Path.Combine(tempDirectory, "exit-code.rvn");
            File.WriteAllText(sourcePath, "func Main() -> int { return 7 }");

            var result = RunProcess(
                "dotnet",
                [frontendPath, "run", sourcePath],
                repoRoot);

            Assert.Equal(7, result.ExitCode);
            Assert.Equal([sourcePath], Directory.GetFiles(tempDirectory));
        }
        finally
        {
            Directory.Delete(tempDirectory, recursive: true);
        }
    }

    private static string EnsureFrontendBuilt(string repoRoot)
    {
        const string targetFramework = TestTargetFramework.Default;
        var frontendPath = Path.Combine(repoRoot, "src", "Raven", "bin", "Debug", targetFramework, "rvn.dll");
        if (!File.Exists(frontendPath))
        {
            var projectPath = Path.Combine(repoRoot, "src", "Raven", "Raven.csproj");
            var result = RunProcess(
                "dotnet",
                ["build", projectPath, "--framework", targetFramework, "--disable-build-servers", "--property", "WarningLevel=0"],
                repoRoot);
            Assert.True(
                result.ExitCode == 0,
                $"Failed to build rvn.\nstdout:\n{result.StdOut}\nstderr:\n{result.StdErr}");
        }

        Assert.True(File.Exists(frontendPath), $"Expected Raven frontend at '{frontendPath}'.");
        return frontendPath;
    }

    private static ProcessResult RunProcess(
        string fileName,
        IReadOnlyList<string> arguments,
        string workingDirectory,
        Action<ProcessStartInfo>? configure = null)
    {
        var startInfo = new ProcessStartInfo(fileName)
        {
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
            WorkingDirectory = workingDirectory
        };
        foreach (var argument in arguments)
            startInfo.ArgumentList.Add(argument);
        configure?.Invoke(startInfo);

        using var process = Process.Start(startInfo) ?? throw new InvalidOperationException($"Failed to start '{fileName}'.");
        var stdout = new StringBuilder();
        var stderr = new StringBuilder();
        process.OutputDataReceived += (_, e) =>
        {
            if (e.Data is not null)
                stdout.AppendLine(e.Data);
        };
        process.ErrorDataReceived += (_, e) =>
        {
            if (e.Data is not null)
                stderr.AppendLine(e.Data);
        };
        process.BeginOutputReadLine();
        process.BeginErrorReadLine();

        if (!process.WaitForExit(120_000))
        {
            process.Kill(entireProcessTree: true);
            throw new TimeoutException($"'{fileName}' did not exit within 120 seconds.");
        }

        process.WaitForExit();
        return new ProcessResult(process.ExitCode, stdout.ToString(), stderr.ToString());
    }

    private static string GetRepositoryRoot()
        => Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, "../../../../../"));

    private sealed record ProcessResult(int ExitCode, string StdOut, string StdErr);
}
