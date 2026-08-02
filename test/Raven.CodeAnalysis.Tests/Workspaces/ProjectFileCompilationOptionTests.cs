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

        var projectPath = Path.Combine(projectDir, "App.ravenproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Name="App" TargetFramework="net10.0" Output="App" OutputKind="DynamicallyLinkedLibrary" RunAnalyzers="false" />
            """);

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        Assert.False(project.CompilationOptions!.RunAnalyzers);
    }

    [Fact]
    public void OpenProject_ReadsNullFlowAnalysisAttribute()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        var projectDir = Path.Combine(root, "project");
        Directory.CreateDirectory(projectDir);
        File.WriteAllText(Path.Combine(projectDir, "main.rvn"), "class C { M() -> unit { return; } }");

        var projectPath = Path.Combine(projectDir, "App.ravenproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Name="App" TargetFramework="net10.0" Output="App" OutputKind="DynamicallyLinkedLibrary" EnableNullFlowAnalysis="false" />
            """);

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        Assert.False(project.CompilationOptions!.EnableNullFlowAnalysis);
    }

    [Fact]
    public void OpenProject_ReadsDisabledAnalyzersAttribute()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        var projectDir = Path.Combine(root, "project");
        Directory.CreateDirectory(projectDir);
        File.WriteAllText(Path.Combine(projectDir, "main.rvn"), "class C { M() -> unit { return; } }");

        var projectPath = Path.Combine(projectDir, "App.ravenproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Name="App" TargetFramework="net10.0" Output="App" OutputKind="DynamicallyLinkedLibrary" DisabledAnalyzers="UnusedVariableAnalyzer;VarCanBeLetAnalyzer" />
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

        var projectPath = Path.Combine(projectDir, "App.ravenproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Name="App" TargetFramework="net10.0" Output="App" OutputKind="DynamicallyLinkedLibrary" ReturnedValueHandlingMode="full" />
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

        var projectPath = Path.Combine(projectDir, "App.ravenproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Name="App" TargetFramework="net10.0" Output="App" OutputKind="DynamicallyLinkedLibrary" EnableReturnedValueAnalyzer="true" />
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
        var projectPath = Path.Combine(root, "App.ravenproj");

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject(
            "App",
            filePath: projectPath,
            compilationOptions: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                .WithRunAnalyzers(false));

        workspace.SaveProject(projectId, projectPath);

        var document = XDocument.Load(projectPath);
        var value = (string?)document.Root?.Attribute("RunAnalyzers");
        Assert.Equal("false", value);
    }

    [Fact]
    public void SaveProject_WritesNullFlowAnalysisAttribute()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(root);
        var projectPath = Path.Combine(root, "App.ravenproj");

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject(
            "App",
            filePath: projectPath,
            compilationOptions: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                .WithEnableNullFlowAnalysis(false));

        workspace.SaveProject(projectId, projectPath);

        var document = XDocument.Load(projectPath);
        var value = (string?)document.Root?.Attribute("EnableNullFlowAnalysis");
        Assert.Equal("false", value);
    }

    [Fact]
    public void SaveProject_WritesDisabledAnalyzersAttribute()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(root);
        var projectPath = Path.Combine(root, "App.ravenproj");

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject(
            "App",
            filePath: projectPath,
            compilationOptions: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                .WithDisabledAnalyzers(["UnusedVariableAnalyzer"]));

        workspace.SaveProject(projectId, projectPath);

        var document = XDocument.Load(projectPath);
        var value = (string?)document.Root?.Attribute("DisabledAnalyzers");
        Assert.Equal("UnusedVariableAnalyzer", value);
    }

    [Fact]
    public void SaveProject_WritesReturnedValueHandlingAttribute()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(root);
        var projectPath = Path.Combine(root, "App.ravenproj");

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.AddProject(
            "App",
            filePath: projectPath,
            compilationOptions: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                .WithReturnedValueHandlingMode(ReturnedValueHandlingMode.Full));

        workspace.SaveProject(projectId, projectPath);

        var document = XDocument.Load(projectPath);
        var value = (string?)document.Root?.Attribute("ReturnedValueHandlingMode");
        Assert.Equal("full", value);
    }

}
