using System;
using System.IO;
using System.Xml.Linq;

using Raven.CodeAnalysis;
namespace Raven.CodeAnalysis.Tests;

public sealed class ProjectFileCompilationOptionTests
{
    [Fact]
    public void OpenProject_ReadsRunAnalyzersAttribute()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        var projectDir = Path.Combine(root, "project");
        Directory.CreateDirectory(projectDir);
        File.WriteAllText(Path.Combine(projectDir, "main.rvn"), "class C { M() -> unit { return; } }");

        var projectPath = Path.Combine(projectDir, "App.rvnproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Sdk="Microsoft.NET.Sdk">
              <PropertyGroup>
                <TargetFramework>net10.0</TargetFramework>
                <OutputType>Library</OutputType>
                <RavenRunAnalyzers>false</RavenRunAnalyzers>
              </PropertyGroup>
            </Project>
            """);

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        Assert.False(project.CompilationOptions!.RunAnalyzers);
    }

    [Fact]
    public void OpenProject_ReadsDisabledAnalyzersAttribute()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        var projectDir = Path.Combine(root, "project");
        Directory.CreateDirectory(projectDir);
        File.WriteAllText(Path.Combine(projectDir, "main.rvn"), "class C { M() -> unit { return; } }");

        var projectPath = Path.Combine(projectDir, "App.rvnproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Sdk="Microsoft.NET.Sdk">
              <PropertyGroup>
                <TargetFramework>net10.0</TargetFramework>
                <OutputType>Library</OutputType>
                <RavenDisabledAnalyzers>UnusedVariableAnalyzer;VarCanBeLetAnalyzer</RavenDisabledAnalyzers>
              </PropertyGroup>
            </Project>
            """);

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        Assert.Contains("UnusedVariableAnalyzer", project.CompilationOptions!.DisabledAnalyzers);
        Assert.Contains("VarCanBeLetAnalyzer", project.CompilationOptions.DisabledAnalyzers);
    }

    [Fact]
    public void OpenProject_ReadsReturnedValueHandlingAttribute()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        var projectDir = Path.Combine(root, "project");
        Directory.CreateDirectory(projectDir);
        File.WriteAllText(Path.Combine(projectDir, "main.rvn"), "class C { M() -> unit { return; } }");

        var projectPath = Path.Combine(projectDir, "App.rvnproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Sdk="Microsoft.NET.Sdk">
              <PropertyGroup>
                <TargetFramework>net10.0</TargetFramework>
                <OutputType>Library</OutputType>
                <RavenReturnedValueHandlingMode>full</RavenReturnedValueHandlingMode>
              </PropertyGroup>
            </Project>
            """);

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        Assert.True(project.CompilationOptions!.ReturnedValueHandlingModeConfigured);
        Assert.Equal(ReturnedValueHandlingMode.Full, project.CompilationOptions.ReturnedValueHandlingMode);
    }

    [Fact]
    public void OpenProject_ReadsEnableReturnedValueAnalyzerAttribute()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        var projectDir = Path.Combine(root, "project");
        Directory.CreateDirectory(projectDir);
        File.WriteAllText(Path.Combine(projectDir, "main.rvn"), "class C { M() -> unit { return; } }");

        var projectPath = Path.Combine(projectDir, "App.rvnproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Sdk="Microsoft.NET.Sdk">
              <PropertyGroup>
                <TargetFramework>net10.0</TargetFramework>
                <OutputType>Library</OutputType>
                <RavenEnableReturnedValueAnalyzer>true</RavenEnableReturnedValueAnalyzer>
              </PropertyGroup>
            </Project>
            """);

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        Assert.True(project.CompilationOptions!.ReturnedValueHandlingModeConfigured);
        Assert.Equal(ReturnedValueHandlingMode.Full, project.CompilationOptions.ReturnedValueHandlingMode);
    }

    [Fact]
    public void SaveProject_WritesRunAnalyzersAttribute()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(root);
        var projectPath = Path.Combine(root, "App.rvnproj");
        File.WriteAllText(projectPath, "<Project Sdk=\"Microsoft.NET.Sdk\" />");

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject(
            "App",
            filePath: projectPath,
            compilationOptions: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                .WithRunAnalyzers(false));

        workspace.SaveProject(projectId, projectPath);

        var document = XDocument.Load(projectPath);
        var value = (string?)document.Descendants("RavenRunAnalyzers").SingleOrDefault();
        Assert.Equal("false", value);
    }

    [Fact]
    public void SaveProject_WritesDisabledAnalyzersAttribute()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(root);
        var projectPath = Path.Combine(root, "App.rvnproj");
        File.WriteAllText(projectPath, "<Project Sdk=\"Microsoft.NET.Sdk\" />");

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject(
            "App",
            filePath: projectPath,
            compilationOptions: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                .WithDisabledAnalyzers(["UnusedVariableAnalyzer"]));

        workspace.SaveProject(projectId, projectPath);

        var document = XDocument.Load(projectPath);
        var value = (string?)document.Descendants("RavenDisabledAnalyzers").SingleOrDefault();
        Assert.Equal("UnusedVariableAnalyzer", value);
    }

    [Fact]
    public void SaveProject_WritesReturnedValueHandlingAttribute()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(root);
        var projectPath = Path.Combine(root, "App.rvnproj");
        File.WriteAllText(projectPath, "<Project Sdk=\"Microsoft.NET.Sdk\" />");

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject(
            "App",
            filePath: projectPath,
            compilationOptions: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                .WithReturnedValueHandlingMode(ReturnedValueHandlingMode.Full));

        workspace.SaveProject(projectId, projectPath);

        var document = XDocument.Load(projectPath);
        var value = (string?)document.Descendants("RavenReturnedValueHandlingMode").SingleOrDefault();
        Assert.Equal("full", value);
    }

}
