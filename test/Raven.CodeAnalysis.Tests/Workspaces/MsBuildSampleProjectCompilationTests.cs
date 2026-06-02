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

    [Fact]
    public void RavenProject_BuildsThroughDotnetBuild()
    {
        var repoRoot = GetRepositoryRoot();
        var compilerDllPath = EnsureCompilerBuilt(repoRoot, "net10.0");
        var projectRoot = CreateTempDirectory();
        try
        {
            var languageTargetsPath = Path.Combine(repoRoot, "build", "Raven.Language.targets");
            var sourceDirectory = Path.Combine(projectRoot, "src");
            Directory.CreateDirectory(sourceDirectory);

            var projectPath = Path.Combine(projectRoot, "Library.rvnproj");
            File.WriteAllText(projectPath, $$"""
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <LanguageTargets>{{languageTargetsPath}}</LanguageTargets>
                    <RavenCompilerHost>{{compilerDllPath}}</RavenCompilerHost>
                    <TargetFramework>net10.0</TargetFramework>
                    <AssemblyName>RavenBuildOutput</AssemblyName>
                    <OutputType>Library</OutputType>
                  </PropertyGroup>
                  <ItemGroup>
                    <RavenCompile Include="src/**/*.rvn" />
                  </ItemGroup>
                </Project>
                """);

            File.WriteAllText(Path.Combine(sourceDirectory, "main.rvn"), """
                class Greeter {
                    static func Message() -> string {
                        "Hello from dotnet build"
                    }
                }
                """);

            var result = RunProcess("dotnet", $"build \"{projectPath}\" --property WarningLevel=0", projectRoot, timeoutMilliseconds: 300_000);
            output.WriteLine(result.StdOut);
            output.WriteLine(result.StdErr);

            Assert.True(result.ExitCode == 0, $"dotnet build failed.\nstdout:\n{result.StdOut}\nstderr:\n{result.StdErr}");
            Assert.True(
                File.Exists(Path.Combine(projectRoot, "bin", "Debug", "net10.0", "RavenBuildOutput.dll")),
                "Expected Raven project build output in the SDK target directory.");
        }
        finally
        {
            DeleteDirectoryIfExists(projectRoot);
        }
    }

    [Fact]
    public void RavenProject_BuildsThroughDotnetBuild_WithRavenCoreRuntimeDependency()
    {
        var repoRoot = GetRepositoryRoot();
        var compilerDllPath = EnsureCompilerBuilt(repoRoot, "net10.0");
        EnsureRavenCoreBuilt(repoRoot, "net10.0");
        var projectRoot = CreateTempDirectory();
        try
        {
            var languageTargetsPath = Path.Combine(repoRoot, "build", "Raven.Language.targets");
            var sourceDirectory = Path.Combine(projectRoot, "src");
            Directory.CreateDirectory(sourceDirectory);

            var projectPath = Path.Combine(projectRoot, "App.rvnproj");
            File.WriteAllText(projectPath, $$"""
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <LanguageTargets>{{languageTargetsPath}}</LanguageTargets>
                    <RavenCompilerHost>{{compilerDllPath}}</RavenCompilerHost>
                    <TargetFramework>net10.0</TargetFramework>
                    <AssemblyName>RavenCoreRuntimeDependency</AssemblyName>
                    <OutputType>Exe</OutputType>
                  </PropertyGroup>
                  <ItemGroup>
                    <RavenCompile Include="src/**/*.rvn" />
                  </ItemGroup>
                </Project>
                """);

            File.WriteAllText(Path.Combine(sourceDirectory, "main.rvn"), """
                import System.*

                func Main() {
                    Console.WriteLine("Raven.Core dependency")
                }
                """);

            var result = RunProcess("dotnet", $"build \"{projectPath}\" --property WarningLevel=0", projectRoot, timeoutMilliseconds: 300_000);
            output.WriteLine(result.StdOut);
            output.WriteLine(result.StdErr);

            Assert.True(result.ExitCode == 0, $"dotnet build failed.\nstdout:\n{result.StdOut}\nstderr:\n{result.StdErr}");

            var outputDirectory = Path.Combine(projectRoot, "bin", "Debug", "net10.0");
            var depsPath = Path.Combine(outputDirectory, "RavenCoreRuntimeDependency.deps.json");
            var corePath = Path.Combine(outputDirectory, "Raven.Core.dll");

            Assert.True(File.Exists(corePath), $"Expected Raven.Core copy-local output at '{corePath}'.");
            Assert.True(File.Exists(depsPath), $"Expected deps file at '{depsPath}'.");

            var depsJson = File.ReadAllText(depsPath);
            Assert.Contains("Raven.Core", depsJson);
        }
        finally
        {
            DeleteDirectoryIfExists(projectRoot);
        }
    }

    [Fact]
    public void CSharpProject_CanReferenceRavenProjectThroughProjectReference()
    {
        var repoRoot = GetRepositoryRoot();
        var compilerDllPath = EnsureCompilerBuilt(repoRoot, "net10.0");
        var root = CreateTempDirectory();
        try
        {
            var languageTargetsPath = Path.Combine(repoRoot, "build", "Raven.Language.targets");
            var ravenDirectory = Path.Combine(root, "raven");
            var csharpDirectory = Path.Combine(root, "csharp");
            Directory.CreateDirectory(ravenDirectory);
            Directory.CreateDirectory(csharpDirectory);

            File.WriteAllText(Path.Combine(ravenDirectory, "Greeter.rvnproj"), $$"""
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <LanguageTargets>{{languageTargetsPath}}</LanguageTargets>
                    <RavenCompilerHost>{{compilerDllPath}}</RavenCompilerHost>
                    <TargetFramework>net10.0</TargetFramework>
                    <AssemblyName>GreeterLib</AssemblyName>
                    <OutputType>Library</OutputType>
                  </PropertyGroup>
                  <ItemGroup>
                    <RavenCompile Include="main.rvn" />
                  </ItemGroup>
                </Project>
                """);

            File.WriteAllText(Path.Combine(ravenDirectory, "main.rvn"), """
                class Greeter {
                    static func Message() -> string {
                        "Hello from Raven reference"
                    }
                }
                """);

            File.WriteAllText(Path.Combine(csharpDirectory, "App.csproj"), """
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <OutputType>Exe</OutputType>
                    <TargetFramework>net10.0</TargetFramework>
                  </PropertyGroup>
                  <ItemGroup>
                    <ProjectReference Include="../raven/Greeter.rvnproj" />
                  </ItemGroup>
                </Project>
                """);

            File.WriteAllText(Path.Combine(csharpDirectory, "Program.cs"), """
                using System;

                Console.WriteLine(Greeter.Message());
                """);

            var appProjectPath = Path.Combine(csharpDirectory, "App.csproj");
            var result = RunProcess("dotnet", $"run --project \"{appProjectPath}\" --property WarningLevel=0", root, timeoutMilliseconds: 300_000);
            output.WriteLine(result.StdOut);
            output.WriteLine(result.StdErr);

            Assert.True(result.ExitCode == 0, $"dotnet run failed.\nstdout:\n{result.StdOut}\nstderr:\n{result.StdErr}");
            Assert.Contains("Hello from Raven reference", result.StdOut);
        }
        finally
        {
            DeleteDirectoryIfExists(root);
        }
    }

    private static string EnsureCompilerBuilt(string repoRoot, string targetFramework = "net11.0")
    {
        var compilerDllPath = Path.Combine(repoRoot, "src", "Raven.Compiler", "bin", "Debug", targetFramework, "rvnc.dll");
        if (!File.Exists(compilerDllPath))
        {
            var compilerProjectPath = Path.Combine(repoRoot, "src", "Raven.Compiler", "Raven.Compiler.csproj");
            var buildArgs = $"build \"{compilerProjectPath}\" --framework {targetFramework} /property:WarningLevel=0 /property:UseRavenCoreReference=false";
            var buildResult = RunProcess("dotnet", buildArgs, repoRoot, timeoutMilliseconds: 300_000);
            Assert.True(
                buildResult.ExitCode == 0,
                $"Failed to build rvnc compiler for sample-project tests.\nstdout:\n{buildResult.StdOut}\nstderr:\n{buildResult.StdErr}");
        }

        Assert.True(File.Exists(compilerDllPath), $"Expected compiler output at '{compilerDllPath}'.");
        return compilerDllPath;
    }

    private static void EnsureRavenCoreBuilt(string repoRoot, string targetFramework)
    {
        var ravenCoreDllPath = Path.Combine(repoRoot, "src", "Raven.Core", "bin", "Debug", targetFramework, "Raven.Core.dll");
        if (File.Exists(ravenCoreDllPath))
            return;

        var ravenCoreProjectPath = Path.Combine(repoRoot, "src", "Raven.Core", "Raven.Core.rvnproj");
        var buildArgs = $"build \"{ravenCoreProjectPath}\" --framework {targetFramework} /property:WarningLevel=0";
        var buildResult = RunProcess("dotnet", buildArgs, repoRoot, timeoutMilliseconds: 300_000);
        Assert.True(
            buildResult.ExitCode == 0,
            $"Failed to build Raven.Core for sample-project tests.\nstdout:\n{buildResult.StdOut}\nstderr:\n{buildResult.StdErr}");

        Assert.True(File.Exists(ravenCoreDllPath), $"Expected Raven.Core output at '{ravenCoreDllPath}'.");
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
