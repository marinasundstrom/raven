using System;
using System.IO;
using System.Xml.Linq;

using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Tests;

public sealed class ProjectFileMemberAccessibilityOptionTests
{
    [Fact]
    public void OpenProject_ReadsMembersPublicByDefaultAttribute()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        var projectDir = Path.Combine(root, "project");
        Directory.CreateDirectory(projectDir);
        File.WriteAllText(Path.Combine(projectDir, "main.rvn"), "class C { M() -> unit { return; } }");

        var projectPath = Path.Combine(projectDir, "App.ravenproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Name="App" TargetFramework="net10.0" Output="App" OutputKind="DynamicallyLinkedLibrary" MembersPublicByDefault="true" />
            """);

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var project = workspace.CurrentSolution.GetProject(projectId)!;
        var options = project.CompilationOptions!;

        Assert.True(options.MembersPublicByDefaultConfigured);
        Assert.True(options.MembersPublicByDefault);
    }

    [Fact]
    public void SaveProject_WritesMembersPublicByDefaultAttribute_WhenConfigured()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(root);
        var projectPath = Path.Combine(root, "App.ravenproj");

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject(
            "App",
            filePath: projectPath,
            compilationOptions: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                .WithMembersPublicByDefault(true));

        workspace.SaveProject(projectId, projectPath);

        var document = XDocument.Load(projectPath);
        var value = (string?)document.Root?.Attribute("MembersPublicByDefault");
        Assert.Equal("true", value);
    }

    [Fact]
    public void SaveProject_OmitsMembersPublicByDefaultAttribute_WhenNotConfigured()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(root);
        var projectPath = Path.Combine(root, "App.ravenproj");

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject(
            "App",
            filePath: projectPath,
            compilationOptions: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        workspace.SaveProject(projectId, projectPath);

        var document = XDocument.Load(projectPath);
        Assert.Null((string?)document.Root?.Attribute("MembersPublicByDefault"));
    }
}
