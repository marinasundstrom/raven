using System.Diagnostics;
using System.Text;

using Xunit.Abstractions;

namespace Raven.CodeAnalysis.Tests.Workspaces;

public sealed class ProjectDocumentationEmissionTests(ITestOutputHelper output)
{
    [Fact]
    public void MsBuildProject_EmitsMarkdownDocumentation_WhenConfigured()
    {
        var root = CreateTempDirectory();
        try
        {
            var sourcePath = Path.Combine(root, "main.rvn");
            File.WriteAllText(sourcePath, """
/// Creates a documented widget.
public class Widget {
    /// Returns the current title.
    public func GetTitle() -> string {
        return "Hello"
    }
}
""");

            var projectPath = Path.Combine(root, "Docs.rvnproj");
            File.WriteAllText(projectPath, """
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <TargetFramework>net10.0</TargetFramework>
                    <AssemblyName>DocsProject</AssemblyName>
                    <OutputType>Library</OutputType>
                    <GenerateMarkdownDocumentationFile>true</GenerateMarkdownDocumentationFile>
                    <MarkdownDocumentationOutputPath>artifacts/docs/DocsProject.docs</MarkdownDocumentationOutputPath>
                  </PropertyGroup>
                  <ItemGroup>
                    <RavenCompile Include="main.rvn" />
                  </ItemGroup>
                </Project>
                """);

            var compilerDllPath = EnsureCompilerBuilt();
            var outputDirectory = Path.Combine(root, "bin");
            Directory.CreateDirectory(outputDirectory);

            var result = RunProcess(
                "dotnet",
                $"\"{compilerDllPath}\" \"{projectPath}\" -o \"{outputDirectory}\"",
                root,
                300_000);

            output.WriteLine(result.StdOut);
            output.WriteLine(result.StdErr);
            Assert.Equal(0, result.ExitCode);

            var docsRoot = Path.Combine(outputDirectory, "artifacts", "docs", "DocsProject.docs");
            Assert.True(File.Exists(Path.Combine(docsRoot, "manifest.json")));
            Assert.True(Directory.Exists(Path.Combine(docsRoot, "invariant", "symbols", "T")));
            Assert.True(Directory.Exists(Path.Combine(docsRoot, "invariant", "symbols", "M")));
        }
        finally
        {
            DeleteDirectoryIfExists(root);
        }
    }

    [Fact]
    public void MsBuildProject_EmitsXmlDocumentation_WhenConfigured()
    {
        var root = CreateTempDirectory();
        try
        {
            var sourcePath = Path.Combine(root, "main.rvn");
            File.WriteAllText(sourcePath, """
/// <summary>Creates a documented widget.</summary>
public class Widget {}
""");

            var projectPath = Path.Combine(root, "Docs.rvnproj");
            File.WriteAllText(projectPath, """
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <TargetFramework>net10.0</TargetFramework>
                    <AssemblyName>DocsProject</AssemblyName>
                    <OutputType>Library</OutputType>
                    <GenerateDocumentationFile>true</GenerateDocumentationFile>
                    <DocumentationFile>artifacts/docs/DocsProject.xml</DocumentationFile>
                  </PropertyGroup>
                  <ItemGroup>
                    <RavenCompile Include="main.rvn" />
                  </ItemGroup>
                </Project>
                """);

            var compilerDllPath = EnsureCompilerBuilt();
            var outputDirectory = Path.Combine(root, "bin");
            Directory.CreateDirectory(outputDirectory);

            var result = RunProcess(
                "dotnet",
                $"\"{compilerDllPath}\" \"{projectPath}\" -o \"{outputDirectory}\"",
                root,
                300_000);

            output.WriteLine(result.StdOut);
            output.WriteLine(result.StdErr);
            Assert.Equal(0, result.ExitCode);

            var xmlPath = Path.Combine(outputDirectory, "artifacts", "docs", "DocsProject.xml");
            Assert.True(File.Exists(xmlPath));
            var xml = File.ReadAllText(xmlPath);
            Assert.Contains("<member name=\"T:Widget\">", xml, StringComparison.Ordinal);
        }
        finally
        {
            DeleteDirectoryIfExists(root);
        }
    }

    [Fact]
    public void MsBuildProject_EmitsMarkdownAndXmlDocumentation_WhenBothAreConfigured()
    {
        var root = CreateTempDirectory();
        try
        {
            var sourcePath = Path.Combine(root, "main.rvn");
            File.WriteAllText(sourcePath, """
/// Creates a documented widget.
class Widget {
    func GetTitle() -> string => "Hello"
}
""");

            var projectPath = Path.Combine(root, "Docs.rvnproj");
            File.WriteAllText(projectPath, """
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <TargetFramework>net10.0</TargetFramework>
                    <AssemblyName>DocsProject</AssemblyName>
                    <OutputType>Library</OutputType>
                    <GenerateDocumentationFile>true</GenerateDocumentationFile>
                    <GenerateMarkdownDocumentationFile>true</GenerateMarkdownDocumentationFile>
                    <DocumentationFile>artifacts/docs/DocsProject.xml</DocumentationFile>
                    <MarkdownDocumentationOutputPath>artifacts/docs/DocsProject.docs</MarkdownDocumentationOutputPath>
                  </PropertyGroup>
                  <ItemGroup>
                    <RavenCompile Include="main.rvn" />
                  </ItemGroup>
                </Project>
                """);

            var compilerDllPath = EnsureCompilerBuilt();
            var outputDirectory = Path.Combine(root, "bin");
            Directory.CreateDirectory(outputDirectory);

            var result = RunProcess(
                "dotnet",
                $"\"{compilerDllPath}\" \"{projectPath}\" -o \"{outputDirectory}\"",
                root,
                300_000);

            output.WriteLine(result.StdOut);
            output.WriteLine(result.StdErr);
            Assert.Equal(0, result.ExitCode);

            var docsRoot = Path.Combine(outputDirectory, "artifacts", "docs", "DocsProject.docs");
            var xmlPath = Path.Combine(outputDirectory, "artifacts", "docs", "DocsProject.xml");

            Assert.True(File.Exists(Path.Combine(docsRoot, "manifest.json")));
            Assert.True(File.Exists(xmlPath));
        }
        finally
        {
            DeleteDirectoryIfExists(root);
        }
    }

    private static string EnsureCompilerBuilt()
    {
        var repoRoot = GetRepositoryRoot();
        var compilerProjectPath = Path.Combine(repoRoot, "src", "Raven.Compiler", "Raven.Compiler.csproj");
        var buildResult = RunProcess(
            "dotnet",
            $"build \"{compilerProjectPath}\" --framework net10.0 /property:WarningLevel=0",
            repoRoot,
            300_000);
        Assert.True(buildResult.ExitCode == 0, buildResult.StdOut + Environment.NewLine + buildResult.StdErr);

        var compilerDllPath = Path.Combine(repoRoot, "src", "Raven.Compiler", "bin", "Debug", "net10.0", "rvn.dll");
        Assert.True(File.Exists(compilerDllPath), $"Expected compiler at '{compilerDllPath}'.");
        return compilerDllPath;
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

        using var process = Process.Start(startInfo) ?? throw new InvalidOperationException($"Failed to start {fileName}.");
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
        var directory = Path.Combine(Path.GetTempPath(), "raven-project-doc-emission", Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        return directory;
    }

    private static void DeleteDirectoryIfExists(string path)
    {
        if (Directory.Exists(path))
            Directory.Delete(path, recursive: true);
    }
}
