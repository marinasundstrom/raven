using System;
using System.IO;
using System.Linq;
using System.Reflection;

namespace Raven.CodeAnalysis.Tests;

public sealed class ProjectFileTargetFrameworkAttributeTests
{
    [Fact]
    public void OpenProject_GeneratesTargetFrameworkAttributeSource_WhenMissing()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        var projectDir = Path.Combine(root, "project");
        var sourceDir = Path.Combine(projectDir, "src");
        Directory.CreateDirectory(sourceDir);

        var sourcePath = Path.Combine(sourceDir, "main.rav");
        File.WriteAllText(sourcePath, "class C { }");

        var projectPath = Path.Combine(projectDir, "App.ravenproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Name="App" TargetFramework="net9.0" Output="App" OutputKind="DynamicallyLinkedLibrary" />
            """);

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        var generated = Assert.Single(
            project.Documents,
            static d => d.Name.EndsWith("TargetFrameworkAttribute.g.rav", StringComparison.OrdinalIgnoreCase));
        Assert.NotNull(generated.FilePath);
        Assert.Contains(
            $"{Path.DirectorySeparatorChar}obj{Path.DirectorySeparatorChar}Debug{Path.DirectorySeparatorChar}raven{Path.DirectorySeparatorChar}generated{Path.DirectorySeparatorChar}",
            generated.FilePath!,
            StringComparison.OrdinalIgnoreCase);
        Assert.Equal(
            """
            import System.Runtime.Versioning.*

            [assembly: TargetFramework(".NETCoreApp,Version=v9.0")]
            """,
            generated.Text.ToString().TrimEnd());

        var compilation = workspace.GetCompilation(projectId);
        using var peStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));

        var assembly = Assembly.Load(peStream.ToArray());
        var targetFrameworkAttribute = Assert.Single(
            assembly.GetCustomAttributesData(),
            static a => a.AttributeType.FullName == "System.Runtime.Versioning.TargetFrameworkAttribute");
        Assert.Equal(".NETCoreApp,Version=v9.0", targetFrameworkAttribute.ConstructorArguments.Single().Value);
    }

    [Fact]
    public void OpenProject_DoesNotGenerateTargetFrameworkAttribute_WhenAlreadyDeclared()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        var projectDir = Path.Combine(root, "project");
        Directory.CreateDirectory(projectDir);

        var sourcePath = Path.Combine(projectDir, "main.rav");
        File.WriteAllText(
            sourcePath,
            """
            import System.Runtime.Versioning.*

            [assembly: TargetFramework(".NETCoreApp,Version=v9.0")]

            class C { }
            """);

        var projectPath = Path.Combine(projectDir, "App.ravenproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Name="App" TargetFramework="net9.0" Output="App" OutputKind="DynamicallyLinkedLibrary" />
            """);

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        Assert.DoesNotContain(
            project.Documents,
            static d => d.Name.EndsWith("TargetFrameworkAttribute.g.rav", StringComparison.OrdinalIgnoreCase));

        var compilation = workspace.GetCompilation(projectId);
        using var peStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));

        var assembly = Assembly.Load(peStream.ToArray());
        var targetFrameworkAttributes = assembly
            .GetCustomAttributesData()
            .Where(static a => a.AttributeType.FullName == "System.Runtime.Versioning.TargetFrameworkAttribute")
            .ToArray();

        Assert.Single(targetFrameworkAttributes);
        Assert.Equal(".NETCoreApp,Version=v9.0", targetFrameworkAttributes[0].ConstructorArguments.Single().Value);
    }

    [Fact]
    public void OpenProject_UsesProjectConfigurationForGeneratedObjPath()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        var projectDir = Path.Combine(root, "project");
        Directory.CreateDirectory(projectDir);
        File.WriteAllText(Path.Combine(projectDir, "main.rav"), "class C { }");

        var projectPath = Path.Combine(projectDir, "App.ravenproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Name="App" TargetFramework="net9.0" Output="App" OutputKind="DynamicallyLinkedLibrary" Configuration="Release" />
            """);

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        var generated = Assert.Single(
            project.Documents,
            static d => d.Name.EndsWith("TargetFrameworkAttribute.g.rav", StringComparison.OrdinalIgnoreCase));
        Assert.NotNull(generated.FilePath);

        Assert.Contains(
            $"{Path.DirectorySeparatorChar}obj{Path.DirectorySeparatorChar}Release{Path.DirectorySeparatorChar}raven{Path.DirectorySeparatorChar}generated{Path.DirectorySeparatorChar}",
            generated.FilePath!,
            StringComparison.OrdinalIgnoreCase);
    }

    [Fact]
    public void OpenProject_WithTopLevelStatements_AndGeneratedTargetFrameworkAttribute_HasSingleEntryPoint()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        var projectDir = Path.Combine(root, "project");
        Directory.CreateDirectory(projectDir);
        File.WriteAllText(Path.Combine(projectDir, "main.rav"), "val x = 1");

        var projectPath = Path.Combine(projectDir, "App.ravenproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Name="App" TargetFramework="net9.0" Output="App" OutputKind="ConsoleApplication" />
            """);

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var compilation = workspace.GetCompilation(projectId);

        using var peStream = new MemoryStream();
        var emitResult = compilation.Emit(peStream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));
        Assert.DoesNotContain(
            emitResult.Diagnostics,
            static d => d.Descriptor == CompilerDiagnostics.EntryPointIsAmbiguous);
    }
}
