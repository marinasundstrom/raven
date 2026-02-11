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
            Assert.Single(project.Documents);

            Assert.Contains(
                project.MetadataReferences.OfType<PortableExecutableReference>(),
                reference => string.Equals(reference.FilePath, packageAssemblyPath, StringComparison.OrdinalIgnoreCase));
        }
        finally
        {
            Environment.SetEnvironmentVariable("NUGET_PACKAGES", originalPackages);
        }
    }
}
