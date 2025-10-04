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

        var buildInfo = new ProcessStartInfo
        {
            FileName = "dotnet",
            Arguments = $"run --project \"{projectDir}\" -- \"{samplePath}\" -o \"{outputDll}\"",
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

        var testDep = Path.Combine(projectDir, "TestDep.dll");
        if (File.Exists(testDep))
            File.Copy(testDep, Path.Combine(outputDir, "TestDep.dll"), overwrite: true);

        Assert.True(File.Exists(Path.Combine(outputDir, Path.ChangeExtension(fileName, ".runtimeconfig.json"))));

        var runInfo = new ProcessStartInfo
        {
            FileName = "dotnet",
            Arguments = $"\"{outputDll}\" {string.Join(' ', args)}",
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
            WorkingDirectory = outputDir,
        };
        runInfo.Environment["MSBUILDTERMINALLOGGER"] = "false";
        runInfo.EnvironmentVariables["MSBUILDTERMINALLOGGER"] = "false";
        var run = Process.Start(runInfo)!;

        run.WaitForExit(TimeSpan.FromSeconds(2));
        _ = await run.StandardOutput.ReadToEndAsync();
        run.WaitForExit();

        Assert.Equal(0, run.ExitCode);
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

        var buildInfo = new ProcessStartInfo
        {
            FileName = "dotnet",
            Arguments = $"run --project \"{projectDir}\" -- \"{samplePath}\" -o \"{outputDll}\" --refs \"{fixtureAssembly}\"",
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
            WorkingDirectory = projectDir,
        };
        buildInfo.Environment["MSBUILDTERMINALLOGGER"] = "false";
        buildInfo.EnvironmentVariables["MSBUILDTERMINALLOGGER"] = "false";
        using var build = Process.Start(buildInfo)!;

        var buildStdOutTask = build.StandardOutput.ReadToEndAsync();
        var buildStdErrTask = build.StandardError.ReadToEndAsync();
        build.WaitForExit(TimeSpan.FromSeconds(10));
        var buildStdOut = await buildStdOutTask;
        var buildStdErr = await buildStdErrTask;

        Assert.True(build.HasExited, $"Build process did not exit. StdOut: {buildStdOut}{Environment.NewLine}StdErr: {buildStdErr}");
        Assert.Equal(0, build.ExitCode);

        var fixtureCopy = Path.Combine(outputDir, Path.GetFileName(fixtureAssembly));
        File.Copy(fixtureAssembly, fixtureCopy, overwrite: true);

        Assert.True(File.Exists(Path.Combine(outputDir, "linq.runtimeconfig.json")));

        var runInfo = new ProcessStartInfo
        {
            FileName = "dotnet",
            Arguments = $"\"{outputDll}\"",
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
            WorkingDirectory = outputDir,
        };
        runInfo.Environment["MSBUILDTERMINALLOGGER"] = "false";
        runInfo.EnvironmentVariables["MSBUILDTERMINALLOGGER"] = "false";
        using var run = Process.Start(runInfo)!;

        var runStdOutTask = run.StandardOutput.ReadToEndAsync();
        var runStdErrTask = run.StandardError.ReadToEndAsync();
        run.WaitForExit(TimeSpan.FromSeconds(5));
        var runStdOut = await runStdOutTask;
        var runStdErr = await runStdErrTask;

        Assert.True(run.HasExited, $"Execution did not exit. StdOut: {runStdOut}{Environment.NewLine}StdErr: {runStdErr}");
        Assert.Equal(0, run.ExitCode);

        var outputLines = runStdOut.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries);
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

        var buildInfo = new ProcessStartInfo
        {
            FileName = "dotnet",
            Arguments = $"run --project \"{projectDir}\" -- \"{samplePath}\" -o \"{outputDll}\"",
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
            WorkingDirectory = projectDir,
        };
        buildInfo.Environment["MSBUILDTERMINALLOGGER"] = "false";
        buildInfo.EnvironmentVariables["MSBUILDTERMINALLOGGER"] = "false";

        using var build = Process.Start(buildInfo)!;

        var buildStdOutTask = build.StandardOutput.ReadToEndAsync();
        var buildStdErrTask = build.StandardError.ReadToEndAsync();
        build.WaitForExit(TimeSpan.FromSeconds(10));
        var buildStdOut = await buildStdOutTask;
        var buildStdErr = await buildStdErrTask;

        Assert.True(build.HasExited, $"Build process did not exit. StdOut: {buildStdOut}{Environment.NewLine}StdErr: {buildStdErr}");
        Assert.Equal(0, build.ExitCode);

        Assert.True(File.Exists(Path.Combine(outputDir, "async-await.runtimeconfig.json")));

        var runInfo = new ProcessStartInfo
        {
            FileName = "dotnet",
            Arguments = $"\"{outputDll}\"",
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
            WorkingDirectory = outputDir,
        };
        runInfo.Environment["MSBUILDTERMINALLOGGER"] = "false";
        runInfo.EnvironmentVariables["MSBUILDTERMINALLOGGER"] = "false";

        using var run = Process.Start(runInfo)!;

        var runStdOutTask = run.StandardOutput.ReadToEndAsync();
        var runStdErrTask = run.StandardError.ReadToEndAsync();
        run.WaitForExit(TimeSpan.FromSeconds(5));
        var runStdOut = await runStdOutTask;
        var runStdErr = await runStdErrTask;

        Assert.True(run.HasExited, $"Execution did not exit. StdOut: {runStdOut}{Environment.NewLine}StdErr: {runStdErr}");
        Assert.Equal(0, run.ExitCode);

        var outputLines = runStdOut.Split(Environment.NewLine, StringSplitOptions.RemoveEmptyEntries);
        Assert.Equal(new[] { "first:1", "sum:5", "done" }, outputLines);
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
}
