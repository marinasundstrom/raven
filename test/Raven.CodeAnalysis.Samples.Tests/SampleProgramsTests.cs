using System.Diagnostics;

namespace Raven.CodeAnalysis.Tests;

public class SampleProgramsTests
{
    static SampleProgramsTests()
    {
        Environment.SetEnvironmentVariable("MSBUILDTERMINALLOGGER", "false");
    }

    public static IEnumerable<object[]> SamplePrograms =>
    [
        ["arrays.rav", Array.Empty<string>()],
        ["enums.rav", Array.Empty<string>()],
        ["general.rav", Array.Empty<string>()],
        ["function-types.rav", Array.Empty<string>()],
        ["generics.rav", Array.Empty<string>()],
        ["io.rav", new[] {"."}],
        ["test2.rav", Array.Empty<string>()],
        ["type-unions.rav", Array.Empty<string>()],
        ["tuples.rav", Array.Empty<string>()],
        ["main.rav", Array.Empty<string>()],
        ["classes.rav", Array.Empty<string>()],
        ["async-await.rav", Array.Empty<string>()],
        ["async-task-return.rav", Array.Empty<string>()],
        ["async-accessors.rav", Array.Empty<string>()],
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

        var buildInfo = CreateProcessStartInfo(
            fileName: "dotnet",
            arguments: $"run --project \"{projectDir}\" -- \"{samplePath}\" -o \"{outputDll}\"",
            workingDirectory: projectDir);
        var buildResult = await RunProcessAsync(buildInfo, TimeSpan.FromSeconds(30));

        Assert.True(buildResult.Exited,
            $"Build process did not exit. StdOut: {buildResult.StandardOutput}{Environment.NewLine}StdErr: {buildResult.StandardError}");
        Assert.True(buildResult.ExitCode == 0,
            $"Build failed with exit code {buildResult.ExitCode}.{Environment.NewLine}StdOut: {buildResult.StandardOutput}{Environment.NewLine}StdErr: {buildResult.StandardError}");

        var testDep = Path.Combine(projectDir, "TestDep.dll");
        if (File.Exists(testDep))
            File.Copy(testDep, Path.Combine(outputDir, "TestDep.dll"), overwrite: true);

        Assert.True(File.Exists(Path.Combine(outputDir, Path.ChangeExtension(fileName, ".runtimeconfig.json"))));

        var runArguments = args.Length > 0
            ? $"\"{outputDll}\" {string.Join(' ', args)}"
            : $"\"{outputDll}\"";
        var runInfo = CreateProcessStartInfo("dotnet", runArguments, outputDir);
        var runResult = await RunProcessAsync(runInfo, TimeSpan.FromSeconds(15));

        Assert.True(runResult.Exited,
            $"Execution did not exit. StdOut: {runResult.StandardOutput}{Environment.NewLine}StdErr: {runResult.StandardError}");
        Assert.True(runResult.ExitCode == 0,
            $"Execution failed with exit code {runResult.ExitCode}.{Environment.NewLine}StdOut: {runResult.StandardOutput}{Environment.NewLine}StdErr: {runResult.StandardError}");
        Assert.DoesNotContain("InvalidProgramException", runResult.StandardError, StringComparison.Ordinal);
    }

    [Fact]
    public async Task Linq_sample_should_execute_extension_lambdas()
    {
        var projectDir = Path.GetFullPath(Path.Combine("..", "..", "..", "..", "..", "src", "Raven.Compiler"));
        var samplePath = Path.Combine(projectDir, "samples", "linq.rav");

        var fixtureAssembly = Path.Combine(AppContext.BaseDirectory, "Raven.ExtensionMethodsFixture.dll");
        Assert.True(File.Exists(fixtureAssembly), $"Fixture assembly missing at '{fixtureAssembly}'.");

        var outputDir = Path.Combine(Path.GetTempPath(), "raven_samples", Guid.NewGuid().ToString());
        Directory.CreateDirectory(outputDir);
        var outputDll = Path.Combine(outputDir, "linq.dll");

        var buildInfo = CreateProcessStartInfo(
            fileName: "dotnet",
            arguments: $"run --project \"{projectDir}\" -- \"{samplePath}\" -o \"{outputDll}\" --refs \"{fixtureAssembly}\"",
            workingDirectory: projectDir);
        var buildResult = await RunProcessAsync(buildInfo, TimeSpan.FromSeconds(30));

        Assert.True(buildResult.Exited,
            $"Build process did not exit. StdOut: {buildResult.StandardOutput}{Environment.NewLine}StdErr: {buildResult.StandardError}");
        Assert.True(buildResult.ExitCode == 0,
            $"Build failed with exit code {buildResult.ExitCode}.{Environment.NewLine}StdOut: {buildResult.StandardOutput}{Environment.NewLine}StdErr: {buildResult.StandardError}");

        var fixtureCopy = Path.Combine(outputDir, Path.GetFileName(fixtureAssembly));
        File.Copy(fixtureAssembly, fixtureCopy, overwrite: true);

        Assert.True(File.Exists(Path.Combine(outputDir, "linq.runtimeconfig.json")));

        var runInfo = CreateProcessStartInfo("dotnet", $"\"{outputDll}\"", outputDir);
        var runResult = await RunProcessAsync(runInfo, TimeSpan.FromSeconds(15));

        Assert.True(runResult.Exited,
            $"Execution did not exit. StdOut: {runResult.StandardOutput}{Environment.NewLine}StdErr: {runResult.StandardError}");
        Assert.True(runResult.ExitCode == 0,
            $"Execution failed with exit code {runResult.ExitCode}.{Environment.NewLine}StdOut: {runResult.StandardOutput}{Environment.NewLine}StdErr: {runResult.StandardError}");

        var outputLines = runResult.StandardOutput.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries);
        Assert.Equal(new[] { "2", "4", "6" }, outputLines);
    }

    [Fact]
    public async Task AsyncAwait_sample_executes_expected_flow()
    {
        var projectDir = Path.GetFullPath(Path.Combine("..", "..", "..", "..", "..", "src", "Raven.Compiler"));
        var samplePath = Path.Combine(projectDir, "samples", "async-await.rav");

        var outputDir = Path.Combine(Path.GetTempPath(), "raven_samples", Guid.NewGuid().ToString());
        Directory.CreateDirectory(outputDir);
        var outputDll = Path.Combine(outputDir, "async-await.dll");

        var buildInfo = CreateProcessStartInfo(
            fileName: "dotnet",
            arguments: $"run --project \"{projectDir}\" -- \"{samplePath}\" -o \"{outputDll}\"",
            workingDirectory: projectDir);
        var buildResult = await RunProcessAsync(buildInfo, TimeSpan.FromSeconds(30));

        Assert.True(buildResult.Exited,
            $"Build process did not exit. StdOut: {buildResult.StandardOutput}{Environment.NewLine}StdErr: {buildResult.StandardError}");
        Assert.True(buildResult.ExitCode == 0,
            $"Build failed with exit code {buildResult.ExitCode}.{Environment.NewLine}StdOut: {buildResult.StandardOutput}{Environment.NewLine}StdErr: {buildResult.StandardError}");

        Assert.True(File.Exists(Path.Combine(outputDir, "async-await.runtimeconfig.json")));

        var runInfo = CreateProcessStartInfo("dotnet", $"\"{outputDll}\"", outputDir);
        var runResult = await RunProcessAsync(runInfo, TimeSpan.FromSeconds(15));

        Assert.True(runResult.Exited,
            $"Execution did not exit. StdOut: {runResult.StandardOutput}{Environment.NewLine}StdErr: {runResult.StandardError}");
        Assert.True(runResult.ExitCode == 0,
            $"Execution failed with exit code {runResult.ExitCode}.{Environment.NewLine}StdOut: {runResult.StandardOutput}{Environment.NewLine}StdErr: {runResult.StandardError}");
        Assert.DoesNotContain("InvalidProgramException", runResult.StandardError, StringComparison.Ordinal);

        var outputLines = runResult.StandardOutput.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries);
        Assert.Equal(new[] { "first:1", "sum:5", "done" }, outputLines);
    }

    [Fact]
    public async Task AsyncTaskReturn_sample_executes_expected_flow()
    {
        var projectDir = Path.GetFullPath(Path.Combine("..", "..", "..", "..", "..", "src", "Raven.Compiler"));
        var samplePath = Path.Combine(projectDir, "samples", "async-task-return.rav");

        var outputDir = Path.Combine(Path.GetTempPath(), "raven_samples", Guid.NewGuid().ToString());
        Directory.CreateDirectory(outputDir);
        var outputDll = Path.Combine(outputDir, "async-task-return.dll");

        var buildInfo = CreateProcessStartInfo(
            fileName: "dotnet",
            arguments: $"run --project \"{projectDir}\" -- \"{samplePath}\" -o \"{outputDll}\"",
            workingDirectory: projectDir);
        var buildResult = await RunProcessAsync(buildInfo, TimeSpan.FromSeconds(30));

        Assert.True(buildResult.Exited,
            $"Build process did not exit. StdOut: {buildResult.StandardOutput}{Environment.NewLine}StdErr: {buildResult.StandardError}");
        Assert.True(buildResult.ExitCode == 0,
            $"Build failed with exit code {buildResult.ExitCode}.{Environment.NewLine}StdOut: {buildResult.StandardOutput}{Environment.NewLine}StdErr: {buildResult.StandardError}");

        Assert.True(File.Exists(Path.Combine(outputDir, "async-task-return.runtimeconfig.json")));

        var runInfo = CreateProcessStartInfo("dotnet", $"\"{outputDll}\"", outputDir);
        var runResult = await RunProcessAsync(runInfo, TimeSpan.FromSeconds(15));

        Assert.True(runResult.Exited,
            $"Execution did not exit. StdOut: {runResult.StandardOutput}{Environment.NewLine}StdErr: {runResult.StandardError}");
        Assert.True(runResult.ExitCode == 0,
            $"Execution failed with exit code {runResult.ExitCode}.{Environment.NewLine}StdOut: {runResult.StandardOutput}{Environment.NewLine}StdErr: {runResult.StandardError}");
        Assert.DoesNotContain("InvalidProgramException", runResult.StandardError, StringComparison.Ordinal);

        var outputLines = runResult.StandardOutput.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries);
        Assert.Equal(new[] { "start", "sum:5", "report:5", "done" }, outputLines);
    }

    [Fact]
    public async Task AsyncAccessor_sample_executes_expected_flow()
    {
        var projectDir = Path.GetFullPath(Path.Combine("..", "..", "..", "..", "..", "src", "Raven.Compiler"));
        var samplePath = Path.Combine(projectDir, "samples", "async-accessors.rav");

        var outputDir = Path.Combine(Path.GetTempPath(), "raven_samples", Guid.NewGuid().ToString());
        Directory.CreateDirectory(outputDir);
        var outputDll = Path.Combine(outputDir, "async-accessors.dll");

        var buildInfo = CreateProcessStartInfo(
            fileName: "dotnet",
            arguments: $"run --project \"{projectDir}\" -- \"{samplePath}\" -o \"{outputDll}\"",
            workingDirectory: projectDir);
        var buildResult = await RunProcessAsync(buildInfo, TimeSpan.FromSeconds(30));

        Assert.True(buildResult.Exited,
            $"Build process did not exit. StdOut: {buildResult.StandardOutput}{Environment.NewLine}StdErr: {buildResult.StandardError}");
        Assert.True(buildResult.ExitCode == 0,
            $"Build failed with exit code {buildResult.ExitCode}.{Environment.NewLine}StdOut: {buildResult.StandardOutput}{Environment.NewLine}StdErr: {buildResult.StandardError}");

        Assert.True(File.Exists(Path.Combine(outputDir, "async-accessors.runtimeconfig.json")));

        var runInfo = CreateProcessStartInfo("dotnet", $"\"{outputDll}\"", outputDir);
        var runResult = await RunProcessAsync(runInfo, TimeSpan.FromSeconds(15));

        Assert.True(runResult.Exited,
            $"Execution did not exit. StdOut: {runResult.StandardOutput}{Environment.NewLine}StdErr: {runResult.StandardError}");
        Assert.True(runResult.ExitCode == 0,
            $"Execution failed with exit code {runResult.ExitCode}.{Environment.NewLine}StdOut: {runResult.StandardOutput}{Environment.NewLine}StdErr: {runResult.StandardError}");
        Assert.DoesNotContain("InvalidProgramException", runResult.StandardError, StringComparison.Ordinal);

        var outputLines = runResult.StandardOutput.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries);
        Assert.Equal(new[] { "initial:0", "after-first:2", "after-second:5" }, outputLines);
    }

    [Fact]
    public async Task NoEmit_flag_should_skip_emission()
    {
        var projectDir = Path.GetFullPath(Path.Combine("..", "..", "..", "..", "..", "src", "Raven.Compiler"));
        var samplePath = Path.Combine(projectDir, "samples", "enums.rav");

        var outputDir = Path.Combine(Path.GetTempPath(), "raven_samples", Guid.NewGuid().ToString());
        Directory.CreateDirectory(outputDir);
        var outputDll = Path.Combine(outputDir, "enums.dll");

        var buildInfo = new ProcessStartInfo
        {
            FileName = "dotnet",
            Arguments = $"run --project \\\"{projectDir}\\\" -- \\\"{samplePath}\\\" -o \\\"{outputDll}\\\" --no-emit",
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
            WorkingDirectory = projectDir,
        };
        buildInfo.Environment["MSBUILDTERMINALLOGGER"] = "false";
        buildInfo.EnvironmentVariables["MSBUILDTERMINALLOGGER"] = "false";
        var build = Process.Start(buildInfo)!;

        build.WaitForExit(TimeSpan.FromSeconds(3));
        _ = await build.StandardOutput.ReadToEndAsync();
        build.WaitForExit();

        Assert.Equal(0, build.ExitCode);
        Assert.False(File.Exists(outputDll));
    }

    private static ProcessStartInfo CreateProcessStartInfo(string fileName, string arguments, string workingDirectory)
    {
        var startInfo = new ProcessStartInfo
        {
            FileName = fileName,
            Arguments = arguments,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
            WorkingDirectory = workingDirectory,
        };

        startInfo.Environment["MSBUILDTERMINALLOGGER"] = "false";
        startInfo.EnvironmentVariables["MSBUILDTERMINALLOGGER"] = "false";

        return startInfo;
    }

    private static async Task<ProcessResult> RunProcessAsync(ProcessStartInfo startInfo, TimeSpan timeout)
    {
        using var process = Process.Start(startInfo) ?? throw new InvalidOperationException("Process failed to start.");

        var stdOutTask = process.StandardOutput.ReadToEndAsync();
        var stdErrTask = process.StandardError.ReadToEndAsync();

        var exited = process.WaitForExit((int)timeout.TotalMilliseconds);
        if (!exited)
        {
            try
            {
                process.Kill(entireProcessTree: true);
            }
            catch
            {
            }
        }

        process.WaitForExit();

        var standardOutput = await stdOutTask;
        var standardError = await stdErrTask;

        return new ProcessResult(exited, process.ExitCode, standardOutput, standardError);
    }

    private readonly record struct ProcessResult(bool Exited, int ExitCode, string StandardOutput, string StandardError);
}
