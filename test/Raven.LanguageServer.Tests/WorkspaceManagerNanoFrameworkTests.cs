using Microsoft.Extensions.Logging.Abstractions;

using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;

namespace Raven.LanguageServer.Tests;

public sealed class WorkspaceManagerNanoFrameworkTests : IDisposable
{
    private readonly string _tempRoot = Path.Combine(
        Path.GetTempPath(),
        $"raven-ls-nanoframework-{Guid.NewGuid():N}");

    [Fact]
    public void Initialize_NanoFrameworkProject_DoesNotAddDesktopRavenReferences()
    {
        Directory.CreateDirectory(_tempRoot);
        File.WriteAllText(Path.Combine(_tempRoot, "Program.rvn"), "func Main() -> unit { }");

        var targetCoreLibraryPath = typeof(object).Assembly.Location;
        var targetReferenceDirectory = Path.GetDirectoryName(targetCoreLibraryPath)!;
        File.WriteAllText(Path.Combine(_tempRoot, "App.rvnproj"), $$"""
            <Project Sdk="Microsoft.NET.Sdk">
              <PropertyGroup>
                <TargetFramework>netnano1.0</TargetFramework>
                <TargetFrameworkIdentifier>.NETnanoFramework</TargetFrameworkIdentifier>
                <TargetFrameworkVersion>v1.0</TargetFrameworkVersion>
                <DisableImplicitFrameworkReferences>true</DisableImplicitFrameworkReferences>
                <RavenUseHostFrameworkReferences>false</RavenUseHostFrameworkReferences>
                <RavenEmitCoreTypesOnly>true</RavenEmitCoreTypesOnly>
                <GeneratePreludeImports>false</GeneratePreludeImports>
                <_TargetFrameworkDirectories>{{targetReferenceDirectory}}</_TargetFrameworkDirectories>
                <_FullFrameworkReferenceAssemblyPaths>{{targetReferenceDirectory}}</_FullFrameworkReferenceAssemblyPaths>
              </PropertyGroup>
              <ItemGroup>
                <Reference Include="TargetCoreLibrary">
                  <HintPath>{{targetCoreLibraryPath}}</HintPath>
                </Reference>
              </ItemGroup>
            </Project>
            """);

        var workspace = RavenWorkspace.Create(targetFramework: "net10.0");
        var manager = new WorkspaceManager(workspace, NullLogger<WorkspaceManager>.Instance);
        manager.Initialize(new InitializeParams
        {
            WorkspaceFolders = new Container<WorkspaceFolder>(new WorkspaceFolder
            {
                Name = "nanoFramework",
                Uri = DocumentUri.FromFileSystemPath(_tempRoot)
            })
        });

        var project = Assert.Single(workspace.CurrentSolution.Projects);
        Assert.Equal("netnano1.0", project.TargetFramework);
        Assert.True(project.CompilationOptions!.EmbedCoreTypes);
        Assert.DoesNotContain(
            project.MetadataReferences.OfType<PortableExecutableReference>(),
            static reference => string.Equals(
                Path.GetFileName(reference.FilePath),
                "Raven.Core.dll",
                StringComparison.OrdinalIgnoreCase));
        Assert.DoesNotContain(
            project.MetadataReferences.OfType<PortableExecutableReference>(),
            static reference => string.Equals(
                Path.GetFileName(reference.FilePath),
                "Raven.Macros.dll",
                StringComparison.OrdinalIgnoreCase));
    }

    public void Dispose()
    {
        if (Directory.Exists(_tempRoot))
            Directory.Delete(_tempRoot, recursive: true);
    }
}
