using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public sealed class ProjectFileNuGetReferenceTests
{
    [Fact]
    public void OpenProject_PackageReference_ResolvesFromGlobalCache()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        var globalPackages = Path.Combine(root, "packages");
        var projectDir = Path.Combine(root, "project");
        var sourceDir = Path.Combine(projectDir, "src");

        Directory.CreateDirectory(globalPackages);
        Directory.CreateDirectory(projectDir);
        Directory.CreateDirectory(sourceDir);

        var packageAssemblyPath = Path.Combine(
            globalPackages,
            "fake.package",
            "1.0.0",
            "ref",
            TestMetadataReferences.TargetFramework,
            "Fake.Package.dll");
        Directory.CreateDirectory(Path.GetDirectoryName(packageAssemblyPath)!);
        File.Copy(typeof(object).Assembly.Location, packageAssemblyPath, overwrite: true);

        var sourcePath = Path.Combine(sourceDir, "main.rav");
        File.WriteAllText(sourcePath, "System.Console.WriteLine(\"hi\")");

        var projectPath = Path.Combine(projectDir, "App.ravenproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Name="App" TargetFramework="net9.0" Output="App">
              <PackageReference Include="Fake.Package" Version="1.0.0" />
            </Project>
            """);

        var originalPackages = Environment.GetEnvironmentVariable("NUGET_PACKAGES");
        Environment.SetEnvironmentVariable("NUGET_PACKAGES", globalPackages);
        try
        {
            var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
            var projectId = workspace.OpenProject(projectPath);
            var project = workspace.CurrentSolution.GetProject(projectId)!;
            Assert.Contains(project.Documents, static d => string.Equals(d.Name, "main.rav", StringComparison.OrdinalIgnoreCase));

            Assert.Contains(
                project.MetadataReferences.OfType<PortableExecutableReference>(),
                reference => string.Equals(reference.FilePath, packageAssemblyPath, StringComparison.OrdinalIgnoreCase));
        }
        finally
        {
            Environment.SetEnvironmentVariable("NUGET_PACKAGES", originalPackages);
        }
    }

    [Fact]
    public void OpenProject_FrameworkReference_ResolvesFromInstalledPacks()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        var projectDir = Path.Combine(root, "project");
        var sourceDir = Path.Combine(projectDir, "src");

        Directory.CreateDirectory(projectDir);
        Directory.CreateDirectory(sourceDir);

        var sourcePath = Path.Combine(sourceDir, "main.rav");
        File.WriteAllText(sourcePath, "System.Console.WriteLine(\"hi\")");

        var projectPath = Path.Combine(projectDir, "App.ravenproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Name="App" TargetFramework="net9.0" Output="App">
              <FrameworkReference Include="Microsoft.AspNetCore.App" />
            </Project>
            """);

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        Assert.Contains(
            project.MetadataReferences.OfType<PortableExecutableReference>(),
            reference => reference.FilePath.Contains("Microsoft.AspNetCore", StringComparison.OrdinalIgnoreCase));
    }

    [Fact]
    public void OpenProject_FrameworkReference_AllowsMapGetWithParameterlessLambda()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        var projectDir = Path.Combine(root, "project");
        var sourceDir = Path.Combine(projectDir, "src");

        Directory.CreateDirectory(projectDir);
        Directory.CreateDirectory(sourceDir);

        var sourcePath = Path.Combine(sourceDir, "main.rav");
        File.WriteAllText(
            sourcePath,
            """
            val builder = Microsoft.AspNetCore.Builder.WebApplication.CreateBuilder(args)
            val app = builder.Build()
            app.MapGet("/", () => "Hello from Raven Minimal API")
            app.Run()
            """);

        var projectPath = Path.Combine(projectDir, "App.ravenproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Name="App" TargetFramework="net9.0" Output="App">
              <FrameworkReference Include="Microsoft.AspNetCore.App" />
            </Project>
            """);

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var diagnostics = workspace.GetDiagnostics(projectId);

        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Id == "RAV1501");
    }


    [Fact]
    public void OpenProject_FrameworkReference_AllowsMapGetWithLambdaWithParameters()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        var projectDir = Path.Combine(root, "project");
        var sourceDir = Path.Combine(projectDir, "src");

        Directory.CreateDirectory(projectDir);
        Directory.CreateDirectory(sourceDir);

        var sourcePath = Path.Combine(sourceDir, "main.rav");
        File.WriteAllText(
            sourcePath,
            """
            val builder = Microsoft.AspNetCore.Builder.WebApplication.CreateBuilder(args)
            val app = builder.Build()
            app.MapGet("/", (name: string) => "Hello ${name} from Raven Minimal API")
            app.Run()
            """);

        var projectPath = Path.Combine(projectDir, "App.ravenproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Name="App" TargetFramework="net9.0" Output="App">
              <FrameworkReference Include="Microsoft.AspNetCore.App" />
            </Project>
            """);

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var diagnostics = workspace.GetDiagnostics(projectId);

        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Id == "RAV1503");
    }

    [Fact]
    public void OpenProject_FrameworkReference_AllowsMapGetAndMapPostWithAsyncLambdas()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        var projectDir = Path.Combine(root, "project");
        var sourceDir = Path.Combine(projectDir, "src");

        Directory.CreateDirectory(projectDir);
        Directory.CreateDirectory(sourceDir);

        var sourcePath = Path.Combine(sourceDir, "main.rav");
        File.WriteAllText(
            sourcePath,
            """
            import System.Threading.Tasks.*

            val builder = Microsoft.AspNetCore.Builder.WebApplication.CreateBuilder(args)
            val app = builder.Build()

            app.MapGet("/", () => "sync")
            app.MapGet("/async", async () => {
                await Task.Delay(1)
                return "async-get"
            })

            app.MapPost("/submit", () => "sync-post")
            app.MapPost("/submit-async", async () => {
                await Task.Delay(1)
                return "async-post"
            })

            app.Run()
            """);

        var projectPath = Path.Combine(projectDir, "App.ravenproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Name="App" TargetFramework="net9.0" Output="App">
              <FrameworkReference Include="Microsoft.AspNetCore.App" />
            </Project>
            """);

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var diagnostics = workspace.GetDiagnostics(projectId);

        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Id == "RAV1501");
        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Id == "RAV0121");
    }

}
