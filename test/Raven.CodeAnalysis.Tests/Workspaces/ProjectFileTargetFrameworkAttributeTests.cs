using System;
using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

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

        var sourcePath = Path.Combine(sourceDir, "main.rvn");
        File.WriteAllText(sourcePath, "class C { }");

        var projectPath = Path.Combine(projectDir, "App.ravenproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Name="App" TargetFramework="net10.0" Output="App" OutputKind="DynamicallyLinkedLibrary" />
            """);

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        var generated = Assert.Single(
            project.Documents,
            static d => d.Name.EndsWith("TargetFrameworkAttribute.g.rvn", StringComparison.OrdinalIgnoreCase));
        Assert.NotNull(generated.FilePath);
        Assert.Contains(
            $"{Path.DirectorySeparatorChar}obj{Path.DirectorySeparatorChar}Debug{Path.DirectorySeparatorChar}raven{Path.DirectorySeparatorChar}generated{Path.DirectorySeparatorChar}",
            generated.FilePath!,
            StringComparison.OrdinalIgnoreCase);
        Assert.Equal(
            """
            import System.Runtime.Versioning.*

            [assembly: TargetFramework(".NETCoreApp,Version=v10.0")]
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
        Assert.Equal(".NETCoreApp,Version=v10.0", targetFrameworkAttribute.ConstructorArguments.Single().Value);
    }

    [Fact]
    public void OpenProject_DoesNotGenerateTargetFrameworkAttribute_WhenAlreadyDeclared()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        var projectDir = Path.Combine(root, "project");
        Directory.CreateDirectory(projectDir);

        var sourcePath = Path.Combine(projectDir, "main.rvn");
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
            <Project Name="App" TargetFramework="net10.0" Output="App" OutputKind="DynamicallyLinkedLibrary" />
            """);

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        Assert.DoesNotContain(
            project.Documents,
            static d => d.Name.EndsWith("TargetFrameworkAttribute.g.rvn", StringComparison.OrdinalIgnoreCase));

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
        File.WriteAllText(Path.Combine(projectDir, "main.rvn"), "class C { }");

        var projectPath = Path.Combine(projectDir, "App.ravenproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Name="App" TargetFramework="net10.0" Output="App" OutputKind="DynamicallyLinkedLibrary" Configuration="Release" />
            """);

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        var generated = Assert.Single(
            project.Documents,
            static d => d.Name.EndsWith("TargetFrameworkAttribute.g.rvn", StringComparison.OrdinalIgnoreCase));
        Assert.NotNull(generated.FilePath);

        Assert.Contains(
            $"{Path.DirectorySeparatorChar}obj{Path.DirectorySeparatorChar}Release{Path.DirectorySeparatorChar}raven{Path.DirectorySeparatorChar}generated{Path.DirectorySeparatorChar}",
            generated.FilePath!,
            StringComparison.OrdinalIgnoreCase);
    }

    [Fact]
    public void OpenProject_GeneratesPreludeSource()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        var projectDir = Path.Combine(root, "project");
        Directory.CreateDirectory(projectDir);
        File.WriteAllText(Path.Combine(projectDir, "main.rvn"), "class C { }");

        var projectPath = Path.Combine(projectDir, "App.ravenproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Name="App" TargetFramework="net10.0" Output="App" OutputKind="DynamicallyLinkedLibrary" />
            """);

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        var generated = Assert.Single(
            project.Documents,
            static d => d.Name.EndsWith("Prelude.g.rvn", StringComparison.OrdinalIgnoreCase));

        Assert.Equal(
            """
            global {
                import System.*
                import System.Collections.*
                import System.Collections.Generic.*
                import System.IO.*
                import System.Linq.*
                import System.Net.Http.*
                import System.Threading.*
                import System.Threading.Tasks.*
                import System.Result.*
                import System.Option.*
            }
            """,
            generated.Text.ToString().TrimEnd());

        var tree = generated.GetSyntaxTreeAsync().GetAwaiter().GetResult()!;
        var block = Assert.Single(tree.GetRoot().Members.OfType<GlobalImportBlockSyntax>());
        Assert.Collection(
            block.Imports,
            import => Assert.Equal("System.*", import.Name.ToString()),
            import => Assert.Equal("System.Collections.*", import.Name.ToString()),
            import => Assert.Equal("System.Collections.Generic.*", import.Name.ToString()),
            import => Assert.Equal("System.IO.*", import.Name.ToString()),
            import => Assert.Equal("System.Linq.*", import.Name.ToString()),
            import => Assert.Equal("System.Net.Http.*", import.Name.ToString()),
            import => Assert.Equal("System.Threading.*", import.Name.ToString()),
            import => Assert.Equal("System.Threading.Tasks.*", import.Name.ToString()),
            import => Assert.Equal("System.Result.*", import.Name.ToString()),
            import => Assert.Equal("System.Option.*", import.Name.ToString()));
    }

    [Fact]
    public void OpenProject_GeneratesPreludeSource_WithProjectImportsAndAliases()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        var projectDir = Path.Combine(root, "project");
        Directory.CreateDirectory(projectDir);
        File.WriteAllText(Path.Combine(projectDir, "main.rvn"), "class C { }");

        var projectPath = Path.Combine(projectDir, "App.ravenproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Name="App" TargetFramework="net10.0" Output="App" OutputKind="DynamicallyLinkedLibrary">
              <Import Include="SuperheroApp.Models" />
              <Import Include="System.Console" Static="True" />
              <Import Include="System.DateTime" Alias="DT" />
            </Project>
            """);

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        var generated = Assert.Single(
            project.Documents,
            static d => d.Name.EndsWith("Prelude.g.rvn", StringComparison.OrdinalIgnoreCase));

        var source = generated.Text.ToString();
        Assert.Contains("import SuperheroApp.Models.*", source, StringComparison.Ordinal);
        Assert.Contains("import System.Console.*", source, StringComparison.Ordinal);
        Assert.Contains("alias DT = System.DateTime", source, StringComparison.Ordinal);
    }

    [Fact]
    public void OpenProject_DoesNotGeneratePreludeSource_WhenPreludeImportsDisabled()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        var projectDir = Path.Combine(root, "project");
        Directory.CreateDirectory(projectDir);
        File.WriteAllText(Path.Combine(projectDir, "main.rvn"), "class C { }");

        var projectPath = Path.Combine(projectDir, "App.ravenproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Name="App" TargetFramework="net10.0" Output="App" OutputKind="DynamicallyLinkedLibrary" GeneratePreludeImports="false" />
            """);

        var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
        var projectId = workspace.OpenProject(projectPath);
        var project = workspace.CurrentSolution.GetProject(projectId)!;

        Assert.DoesNotContain(
            project.Documents,
            static d => d.Name.EndsWith("Prelude.g.rvn", StringComparison.OrdinalIgnoreCase));
    }

    [Fact]
    public void OpenProject_WithTopLevelStatements_AndGeneratedTargetFrameworkAttribute_HasSingleEntryPoint()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        var projectDir = Path.Combine(root, "project");
        Directory.CreateDirectory(projectDir);
        File.WriteAllText(Path.Combine(projectDir, "main.rvn"), "val x = 1");

        var projectPath = Path.Combine(projectDir, "App.ravenproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Name="App" TargetFramework="net10.0" Output="App" OutputKind="ConsoleApplication" />
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

    [Fact]
    public void OpenProject_WithExplicitMainFunction_AndGeneratedTargetFrameworkAttribute_HasSingleEntryPoint()
    {
        var root = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N"));
        var projectDir = Path.Combine(root, "project");
        Directory.CreateDirectory(projectDir);
        File.WriteAllText(
            Path.Combine(projectDir, "main.rvn"),
            """
            import System.ComponentModel.*

            func Main() -> unit {
                val viewModel = MyViewModel()
            }

            class MyViewModel {
            }
            """);

        var projectPath = Path.Combine(projectDir, "App.ravenproj");
        File.WriteAllText(
            projectPath,
            """
            <Project Name="App" TargetFramework="net10.0" Output="App" OutputKind="ConsoleApplication" />
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

        var entryPoint = compilation.GetEntryPoint();
        Assert.NotNull(entryPoint);
        Assert.Equal("Main", entryPoint!.Name);
    }
}
