using System;
using System.IO;
using System.Linq;

namespace Raven.CodeAnalysis.Tests.Workspaces;

public sealed class MsBuildProjectSystemServiceTests
{
    [Fact]
    public void OpenProject_MsBuildProject_LoadsRavenCompileItemsAndOptions()
    {
        var root = CreateTempDirectory();
        try
        {
            var sourceDirectory = Path.Combine(root, "src");
            Directory.CreateDirectory(sourceDirectory);

            var mainPath = Path.Combine(sourceDirectory, "main.rvn");
            var helperPath = Path.Combine(sourceDirectory, "helper.rvn");
            File.WriteAllText(mainPath, "import System.*\nConsole.WriteLine(\"Hello\")");
            File.WriteAllText(helperPath, "func answer() -> int => 42");

            var projectPath = Path.Combine(root, "App.rvnproj");
            File.WriteAllText(projectPath, """
                                          <Project Sdk="Microsoft.NET.Sdk">
                                            <PropertyGroup>
                                              <TargetFramework>net10.0</TargetFramework>
                                              <AssemblyName>App.Assembly</AssemblyName>
                                              <OutputType>Library</OutputType>
                                              <AllowUnsafeBlocks>true</AllowUnsafeBlocks>
                                              <RavenAllowGlobalStatements>false</RavenAllowGlobalStatements>
                                              <MembersPublicByDefault>false</MembersPublicByDefault>
                                            </PropertyGroup>
                                            <ItemGroup>
                                              <RavenCompile Include="src/**/*.rvn" />
                                            </ItemGroup>
                                          </Project>
                                          """);

            var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
            var projectId = workspace.OpenProject(projectPath);
            var project = workspace.CurrentSolution.GetProject(projectId)!;

            Assert.Equal("App.Assembly", project.AssemblyName);
            Assert.Equal("net10.0", project.TargetFramework);
            Assert.NotNull(project.CompilationOptions);
            Assert.Equal(OutputKind.DynamicallyLinkedLibrary, project.CompilationOptions!.OutputKind);
            Assert.True(project.CompilationOptions.AllowUnsafe);
            Assert.False(project.CompilationOptions.AllowGlobalStatements);
            Assert.True(project.CompilationOptions.MembersPublicByDefaultConfigured);
            Assert.False(project.CompilationOptions.MembersPublicByDefault);
            Assert.Contains(project.Documents, document => string.Equals(document.FilePath, mainPath, StringComparison.OrdinalIgnoreCase));
            Assert.Contains(project.Documents, document => string.Equals(document.FilePath, helperPath, StringComparison.OrdinalIgnoreCase));
            Assert.Contains(
                project.Documents,
                document => document.Name.EndsWith("TargetFrameworkAttribute.g.rvn", StringComparison.OrdinalIgnoreCase));
        }
        finally
        {
            DeleteDirectoryIfExists(root);
        }
    }

    [Fact]
    public void OpenProject_MsBuildProjectReference_AddsWorkspaceProjectReference()
    {
        var root = CreateTempDirectory();
        try
        {
            var libDirectory = Path.Combine(root, "Lib");
            var appDirectory = Path.Combine(root, "App");
            Directory.CreateDirectory(libDirectory);
            Directory.CreateDirectory(appDirectory);

            File.WriteAllText(Path.Combine(libDirectory, "lib.rvn"), "public func libValue() -> int => 42");
            File.WriteAllText(Path.Combine(appDirectory, "app.rvn"), "val x = 42");

            var libProjectPath = Path.Combine(libDirectory, "Lib.rvnproj");
            var appProjectPath = Path.Combine(appDirectory, "App.rvnproj");

            File.WriteAllText(libProjectPath, """
                                             <Project Sdk="Microsoft.NET.Sdk">
                                               <PropertyGroup>
                                                 <TargetFramework>net10.0</TargetFramework>
                                               </PropertyGroup>
                                               <ItemGroup>
                                                 <RavenCompile Include="lib.rvn" />
                                               </ItemGroup>
                                             </Project>
                                             """);

            File.WriteAllText(appProjectPath, $$"""
                                             <Project Sdk="Microsoft.NET.Sdk">
                                               <PropertyGroup>
                                                 <TargetFramework>net10.0</TargetFramework>
                                               </PropertyGroup>
                                               <ItemGroup>
                                                 <RavenCompile Include="app.rvn" />
                                                 <ProjectReference Include="{{Path.GetRelativePath(appDirectory, libProjectPath)}}" />
                                               </ItemGroup>
                                             </Project>
                                             """);

            var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
            var libProjectId = workspace.OpenProject(libProjectPath);
            var appProjectId = workspace.OpenProject(appProjectPath);

            var appProject = workspace.CurrentSolution.GetProject(appProjectId)!;
            var projectReference = Assert.Single(appProject.ProjectReferences);
            Assert.Equal(libProjectId, projectReference.ProjectId);
        }
        finally
        {
            DeleteDirectoryIfExists(root);
        }
    }

    [Fact]
    public void SaveProject_MsBuildProject_RewritesRavenStateAndPreservesUnrelatedItems()
    {
        var root = CreateTempDirectory();
        try
        {
            var libDirectory = Path.Combine(root, "Lib");
            var appDirectory = Path.Combine(root, "App");
            var csDirectory = Path.Combine(root, "CsSupport");
            Directory.CreateDirectory(libDirectory);
            Directory.CreateDirectory(appDirectory);
            Directory.CreateDirectory(csDirectory);

            var libProjectPath = Path.Combine(libDirectory, "Lib.rvnproj");
            var appProjectPath = Path.Combine(appDirectory, "App.rvnproj");
            var csProjectPath = Path.Combine(csDirectory, "CsSupport.csproj");

            File.WriteAllText(Path.Combine(libDirectory, "lib.rvn"), "public func libValue() -> int => 42");
            File.WriteAllText(Path.Combine(appDirectory, "main.rvn"), "val x = 1");

            File.WriteAllText(libProjectPath, """
                                             <Project Sdk="Microsoft.NET.Sdk">
                                               <PropertyGroup>
                                                 <TargetFramework>net10.0</TargetFramework>
                                               </PropertyGroup>
                                               <ItemGroup>
                                                 <RavenCompile Include="lib.rvn" />
                                               </ItemGroup>
                                             </Project>
                                             """);

            File.WriteAllText(csProjectPath, """
                                            <Project Sdk="Microsoft.NET.Sdk">
                                              <PropertyGroup>
                                                <TargetFramework>net10.0</TargetFramework>
                                              </PropertyGroup>
                                            </Project>
                                            """);

            File.WriteAllText(appProjectPath, $$"""
                                             <Project Sdk="Microsoft.NET.Sdk">
                                               <PropertyGroup>
                                                 <TargetFramework>net10.0</TargetFramework>
                                                 <OutputType>Exe</OutputType>
                                                 <RavenAllowGlobalStatements>true</RavenAllowGlobalStatements>
                                               </PropertyGroup>
                                               <ItemGroup>
                                                 <PackageReference Include="Newtonsoft.Json" Version="13.0.3" />
                                                 <ProjectReference Include="{{Path.GetRelativePath(appDirectory, csProjectPath)}}" />
                                                 <RavenCompile Include="main.rvn" />
                                               </ItemGroup>
                                             </Project>
                                             """);

            var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
            var libProjectId = workspace.OpenProject(libProjectPath);
            var appProjectId = workspace.OpenProject(appProjectPath);
            var appProject = workspace.CurrentSolution.GetProject(appProjectId)!;

            var extraDocument = appProject.AddDocument("extra.rvn", Raven.CodeAnalysis.Text.SourceText.From("func extra() -> int => 42"), Path.Combine(appDirectory, "extra.rvn"));
            workspace.TryApplyChanges(extraDocument.Project.Solution);

            appProject = workspace.CurrentSolution.GetProject(appProjectId)!;
            var updatedProject = appProject.WithCompilationOptions(
                new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                    .WithAllowUnsafe(true)
                    .WithAllowGlobalStatements(false)
                    .WithMembersPublicByDefault(true));
            workspace.TryApplyChanges(updatedProject.Solution);

            workspace.SaveProject(appProjectId, appProjectPath);

            var savedDocument = System.Xml.Linq.XDocument.Load(appProjectPath);
            var rootElement = savedDocument.Root!;

            Assert.Contains(rootElement.Descendants(), e => e.Name.LocalName == "PackageReference" && (string?)e.Attribute("Include") == "Newtonsoft.Json");
            Assert.Contains(rootElement.Descendants(), e => e.Name.LocalName == "ProjectReference" && PathsEqual((string?)e.Attribute("Include"), Path.GetRelativePath(appDirectory, csProjectPath)));
            var ravenCompileIncludes = rootElement.Descendants()
                .Where(e => e.Name.LocalName == "RavenCompile")
                .Select(e => (string?)e.Attribute("Include"))
                .Where(static value => !string.IsNullOrWhiteSpace(value))
                .ToArray();

            Assert.Contains("main.rvn", ravenCompileIncludes);
            Assert.Contains("extra.rvn", ravenCompileIncludes);
            Assert.DoesNotContain(ravenCompileIncludes, include => include!.EndsWith("TargetFrameworkAttribute.g.rvn", StringComparison.OrdinalIgnoreCase));

            Assert.Equal("Library", rootElement.Descendants().First(e => e.Name.LocalName == "OutputType").Value);
            Assert.Equal("true", rootElement.Descendants().First(e => e.Name.LocalName == "AllowUnsafeBlocks").Value);
            Assert.Equal("false", rootElement.Descendants().First(e => e.Name.LocalName == "RavenAllowGlobalStatements").Value);
            Assert.Equal("true", rootElement.Descendants().First(e => e.Name.LocalName == "MembersPublicByDefault").Value);

            Assert.True(File.Exists(Path.Combine(appDirectory, "extra.rvn")));
            Assert.Contains("extra", File.ReadAllText(Path.Combine(appDirectory, "extra.rvn")));
        }
        finally
        {
            DeleteDirectoryIfExists(root);
        }
    }

    private static string CreateTempDirectory()
    {
        var directory = Path.Combine(Path.GetTempPath(), "raven-msbuild-project-system-tests", Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        return directory;
    }

    private static void DeleteDirectoryIfExists(string path)
    {
        if (!Directory.Exists(path))
            return;

        Directory.Delete(path, recursive: true);
    }

    private static bool PathsEqual(string? left, string? right)
    {
        if (string.IsNullOrWhiteSpace(left) || string.IsNullOrWhiteSpace(right))
            return false;

        return string.Equals(
            left.Replace('\\', '/'),
            right.Replace('\\', '/'),
            StringComparison.OrdinalIgnoreCase);
    }
}
