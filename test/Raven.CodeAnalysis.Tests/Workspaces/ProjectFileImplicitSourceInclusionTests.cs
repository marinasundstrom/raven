using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Tests.Workspaces;

public sealed class ProjectFileImplicitSourceInclusionTests
{
    [Fact]
    public void OpenProject_WithoutDocumentElements_ImplicitlyIncludesRavenSources()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(root);

        try
        {
            var srcDir = Path.Combine(root, "src");
            Directory.CreateDirectory(srcDir);

            var includedMain = Path.Combine(srcDir, "main.rav");
            var includedNested = Path.Combine(srcDir, "nested", "feature.rav");
            var excludedObj = Path.Combine(root, "obj", "generated.rav");
            var excludedBin = Path.Combine(root, "bin", "artifact.rav");

            Directory.CreateDirectory(Path.GetDirectoryName(includedNested)!);
            Directory.CreateDirectory(Path.GetDirectoryName(excludedObj)!);
            Directory.CreateDirectory(Path.GetDirectoryName(excludedBin)!);

            File.WriteAllText(includedMain, "func Main() { }\n");
            File.WriteAllText(includedNested, "func Feature() { }\n");
            File.WriteAllText(excludedObj, "func Generated() { }\n");
            File.WriteAllText(excludedBin, "func Artifact() { }\n");

            var projectPath = Path.Combine(root, "App.ravenproj");
            File.WriteAllText(projectPath, "<Project Name=\"App\" Output=\"App\" />\n");

            var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
            var projectId = workspace.OpenProject(projectPath);
            var project = workspace.CurrentSolution.GetProject(projectId)!;

            var documentPaths = project.Documents
                .Select(d => d.FilePath)
                .Where(static p => p is not null)
                .Select(static p => Path.GetFullPath(p!))
                .ToArray();

            Assert.Contains(Path.GetFullPath(includedMain), documentPaths);
            Assert.Contains(Path.GetFullPath(includedNested), documentPaths);
            Assert.DoesNotContain(Path.GetFullPath(excludedObj), documentPaths);
            Assert.DoesNotContain(Path.GetFullPath(excludedBin), documentPaths);
        }
        finally
        {
            Directory.Delete(root, recursive: true);
        }
    }

    [Fact]
    public void OpenProject_EnableDefaultRavItemsFalse_DoesNotImplicitlyIncludeSources()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(root);

        try
        {
            var sourcePath = Path.Combine(root, "main.rav");
            File.WriteAllText(sourcePath, "func Main() { }\n");

            var projectPath = Path.Combine(root, "App.ravenproj");
            File.WriteAllText(
                projectPath,
                "<Project Name=\"App\" Output=\"App\" EnableDefaultRavItems=\"false\" />\n");

            var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
            var projectId = workspace.OpenProject(projectPath);
            var project = workspace.CurrentSolution.GetProject(projectId)!;

            Assert.Empty(project.Documents);
        }
        finally
        {
            Directory.Delete(root, recursive: true);
        }
    }

    [Fact]
    public void OpenProject_WithExplicitDocumentElements_DoesNotAddImplicitSources()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(root);

        try
        {
            var includedPath = Path.Combine(root, "main.rav");
            var extraPath = Path.Combine(root, "extra.rav");
            File.WriteAllText(includedPath, "func Main() { }\n");
            File.WriteAllText(extraPath, "func Extra() { }\n");

            var projectPath = Path.Combine(root, "App.ravenproj");
            File.WriteAllText(
                projectPath,
                """
<Project Name="App" Output="App" EnableDefaultRavItems="true">
  <Document Path="main.rav" />
</Project>
""");

            var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
            var projectId = workspace.OpenProject(projectPath);
            var project = workspace.CurrentSolution.GetProject(projectId)!;

            var documentPaths = project.Documents
                .Select(d => d.FilePath)
                .Where(static p => p is not null)
                .Select(static p => Path.GetFullPath(p!))
                .ToArray();

            Assert.Single(documentPaths);
            Assert.Contains(Path.GetFullPath(includedPath), documentPaths);
            Assert.DoesNotContain(Path.GetFullPath(extraPath), documentPaths);
        }
        finally
        {
            Directory.Delete(root, recursive: true);
        }
    }
}
