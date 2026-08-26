using System;
using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Workspaces;

public sealed class MsBuildProjectSystemServiceTests
{
    [Fact]
    public void Evaluate_DoesNotExposeSdkImplicitCoreFrameworkReference()
    {
        var root = CreateTempDirectory();
        try
        {
            var projectPath = Path.Combine(root, "App.rvnproj");
            File.WriteAllText(projectPath, """
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <TargetFramework>net10.0</TargetFramework>
                  </PropertyGroup>
                </Project>
                """);

            MsBuildLocatorRegistration.EnsureRegistered();
            var evaluation = MsBuildProjectEvaluator.Evaluate(
                projectPath,
                RavenProjectConventions.Default);

            Assert.DoesNotContain(
                evaluation.FrameworkReferences,
                reference => string.Equals(
                    reference.Name,
                    "Microsoft.NETCore.App",
                    StringComparison.OrdinalIgnoreCase));
        }
        finally
        {
            DeleteDirectoryIfExists(root);
        }
    }

    [Fact]
    public void Evaluate_UsesRequestedConfigurationAndTargetFramework()
    {
        var root = CreateTempDirectory();
        try
        {
            var releasePath = Path.Combine(root, "release.rvn");
            var debugPath = Path.Combine(root, "debug.rvn");
            File.WriteAllText(releasePath, "class ReleaseSource { }");
            File.WriteAllText(debugPath, "class DebugSource { }");

            var projectPath = Path.Combine(root, "App.rvnproj");
            File.WriteAllText(projectPath, """
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <TargetFrameworks>net9.0;net10.0</TargetFrameworks>
                    <EnableDefaultCompileItems>false</EnableDefaultCompileItems>
                  </PropertyGroup>
                  <ItemGroup>
                    <Compile Include="release.rvn" Condition="'$(Configuration)' == 'Release' and '$(TargetFramework)' == 'net10.0'" />
                    <Compile Include="debug.rvn" Condition="'$(Configuration)' == 'Debug'" />
                  </ItemGroup>
                </Project>
                """);

            MsBuildLocatorRegistration.EnsureRegistered();
            var evaluation = MsBuildProjectEvaluator.Evaluate(
                projectPath,
                RavenProjectConventions.Default,
                requestedTargetFramework: "net10.0",
                requestedConfiguration: "Release");

            Assert.Equal("Release", evaluation.Configuration);
            Assert.Equal("net10.0", evaluation.TargetFramework);
            Assert.Contains(evaluation.Documents, document => PathsEqual(document.FilePath, releasePath));
            Assert.DoesNotContain(evaluation.Documents, document => PathsEqual(document.FilePath, debugPath));
            Assert.Equal(
                Path.Combine(root, "obj", "Release", "net10.0", "raven", "generated"),
                evaluation.GeneratedSourceDirectory);
        }
        finally
        {
            DeleteDirectoryIfExists(root);
        }
    }

    [Fact]
    public void Evaluate_ProjectsMacroOptionsIntoParseOptions()
    {
        var root = CreateTempDirectory();
        try
        {
            var projectPath = Path.Combine(root, "App.rvnproj");
            File.WriteAllText(projectPath, """
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <TargetFramework>net10.0</TargetFramework>
                  </PropertyGroup>
                  <ItemGroup>
                    <MacroOption Include="sample.theme" Value="light" />
                    <MacroOption Include="sample.theme" Value="dark" />
                    <MacroOption Include="sample.scope" Value="b-sample" />
                  </ItemGroup>
                </Project>
                """);

            MsBuildLocatorRegistration.EnsureRegistered();
            var evaluation = MsBuildProjectEvaluator.Evaluate(
                projectPath,
                RavenProjectConventions.Default);

            Assert.Equal("dark", evaluation.ParseOptions.Features["sample.theme"]);
            Assert.Equal("b-sample", evaluation.ParseOptions.Features["sample.scope"]);
        }
        finally
        {
            DeleteDirectoryIfExists(root);
        }
    }

    [Fact]
    public void Evaluate_RavenMacroProjectItem_ReportsProjectReferenceMigration()
    {
        var root = CreateTempDirectory();
        try
        {
            MsBuildLocatorRegistration.EnsureRegistered();
            var projectPath = Path.Combine(root, "App.rvnproj");
            File.WriteAllText(projectPath, """
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <TargetFramework>net10.0</TargetFramework>
                  </PropertyGroup>
                  <ItemGroup>
                    <RavenMacro Include="Macros.rvnproj" />
                  </ItemGroup>
                </Project>
                """);

            var exception = Assert.Throws<InvalidDataException>(
                () => MsBuildProjectEvaluator.Evaluate(projectPath, RavenProjectConventions.Default));

            Assert.Contains("no longer supported", exception.Message, StringComparison.Ordinal);
            Assert.Contains("ProjectReference", exception.Message, StringComparison.Ordinal);
            Assert.Contains("RavenCompilerPlugin", exception.Message, StringComparison.Ordinal);
        }
        finally
        {
            DeleteDirectoryIfExists(root);
        }
    }

    [Fact]
    public void OpenProject_MsBuildProject_LoadsCompileItemsAndOptions()
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
                                            <RavenRunAnalyzers>false</RavenRunAnalyzers>
                                            <EnableIsNotNullNarrowing>true</EnableIsNotNullNarrowing>
                                            <RavenDisabledAnalyzers>UnusedVariableAnalyzer;VarCanBeLetAnalyzer</RavenDisabledAnalyzers>
                                            <RavenReturnedValueHandlingMode>full</RavenReturnedValueHandlingMode>
                                            <RavenFrameworkProjections>None</RavenFrameworkProjections>
                                            <DefineConstants>DEBUG;WINDOWS</DefineConstants>
                                            <GenerateDocumentationFile>true</GenerateDocumentationFile>
                                            <GenerateMarkdownDocumentationFile>true</GenerateMarkdownDocumentationFile>
                                            <DocumentationFile>artifacts/App.xml</DocumentationFile>
                                            <MarkdownDocumentationOutputPath>artifacts/App.docs</MarkdownDocumentationOutputPath>
                                          </PropertyGroup>
                                            <ItemGroup>
                                              <Compile Include="src/**/*.rvn" />
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
            Assert.False(project.CompilationOptions.RunAnalyzers);
            Assert.True(project.CompilationOptions.EnableIsNotNullNarrowing);
            Assert.Contains("UnusedVariableAnalyzer", project.CompilationOptions.DisabledAnalyzers);
            Assert.Contains("VarCanBeLetAnalyzer", project.CompilationOptions.DisabledAnalyzers);
            Assert.True(project.CompilationOptions.ReturnedValueHandlingModeConfigured);
            Assert.Equal(ReturnedValueHandlingMode.Full, project.CompilationOptions.ReturnedValueHandlingMode);
            Assert.Equal(FrameworkProjectionMode.None, project.CompilationOptions.FrameworkProjectionMode);
            Assert.NotNull(project.ParseOptions);
            Assert.Equal(
                ["DEBUG", "WINDOWS"],
                project.ParseOptions!.PreprocessorSymbolNames.OrderBy(static symbol => symbol, StringComparer.Ordinal));
            Assert.NotNull(project.DocumentationOptions);
            Assert.True(project.DocumentationOptions!.GenerateXmlDocumentation);
            Assert.True(project.DocumentationOptions.GenerateMarkdownDocumentation);
            Assert.Equal("artifacts/App.xml", project.DocumentationOptions.XmlDocumentationFile);
            Assert.Equal("artifacts/App.docs", project.DocumentationOptions.MarkdownDocumentationOutputPath);
            Assert.Contains(project.Documents, document => string.Equals(document.FilePath, mainPath, StringComparison.OrdinalIgnoreCase));
            Assert.Contains(project.Documents, document => string.Equals(document.FilePath, helperPath, StringComparison.OrdinalIgnoreCase));
            Assert.Contains(
                project.Documents,
                document => document.Name.EndsWith("TargetFrameworkAttribute.g.rvn", StringComparison.OrdinalIgnoreCase));
            Assert.Contains(
                project.Documents,
                document => document.Name.EndsWith("Prelude.g.rvn", StringComparison.OrdinalIgnoreCase));
        }
        finally
        {
            DeleteDirectoryIfExists(root);
        }
    }

    [Fact]
    public void OpenProject_MsBuildProject_LoadsExplicitCompileItems_WhenDefaultItemsAreDisabled()
    {
        var root = CreateTempDirectory();
        try
        {
            var sourceDirectory = Path.Combine(root, "src");
            Directory.CreateDirectory(sourceDirectory);

            var includedPath = Path.Combine(sourceDirectory, "included.rvn");
            var excludedPath = Path.Combine(sourceDirectory, "excluded.rvn");
            File.WriteAllText(includedPath, "class Included { }");
            File.WriteAllText(excludedPath, "class Excluded { }");

            var projectPath = Path.Combine(root, "App.rvnproj");
            File.WriteAllText(projectPath, """
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <TargetFramework>net10.0</TargetFramework>
                    <EnableDefaultCompileItems>false</EnableDefaultCompileItems>
                  </PropertyGroup>
                  <ItemGroup>
                    <Compile Include="src/included.rvn" />
                  </ItemGroup>
                </Project>
                """);

            var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
            var projectId = workspace.OpenProject(projectPath);
            var project = workspace.CurrentSolution.GetProject(projectId)!;

            Assert.Contains(project.Documents, document => PathsEqual(document.FilePath, includedPath));
            Assert.DoesNotContain(project.Documents, document => PathsEqual(document.FilePath, excludedPath));
        }
        finally
        {
            DeleteDirectoryIfExists(root);
        }
    }

    [Fact]
    public void OpenProject_CoreTypesProject_DisablesFrameworkProjections()
    {
        var root = CreateTempDirectory();
        try
        {
            var sourcePath = Path.Combine(root, "Core.rav");
            File.WriteAllText(sourcePath, "public class CoreType { }");

            var projectPath = Path.Combine(root, "Core.rvnproj");
            File.WriteAllText(projectPath, """
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <TargetFramework>net10.0</TargetFramework>
                    <OutputType>Library</OutputType>
                    <RavenEmitCoreTypesOnly>true</RavenEmitCoreTypesOnly>
                  </PropertyGroup>
                  <ItemGroup>
                    <Compile Include="Core.rav" />
                  </ItemGroup>
                </Project>
                """);

            MsBuildLocatorRegistration.EnsureRegistered();
            var evaluation = MsBuildProjectEvaluator.Evaluate(projectPath, RavenProjectConventions.Default);
            Assert.Equal(
                Path.Combine(root, "obj", "Debug", "net10.0", "raven", "generated"),
                evaluation.GeneratedSourceDirectory);

            var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
            var projectId = workspace.OpenProject(projectPath);
            var options = workspace.CurrentSolution.GetProject(projectId)!.CompilationOptions!;

            Assert.True(options.EmbedCoreTypes);
            Assert.Equal(FrameworkProjectionMode.None, options.FrameworkProjectionMode);
        }
        finally
        {
            DeleteDirectoryIfExists(root);
        }
    }

    [Fact]
    public void OpenProject_NanoFrameworkProject_UsesOnlyExplicitTargetReferences()
    {
        var root = CreateTempDirectory();
        var originalPackages = Environment.GetEnvironmentVariable("NUGET_PACKAGES");
        try
        {
            var sourcePath = Path.Combine(root, "Program.rvn");
            File.WriteAllText(sourcePath, "func Main() -> unit { }");

            var globalPackages = Path.Combine(root, "packages");
            var targetCoreLibraryPath = Path.Combine(
                globalPackages,
                "fake.nanoframework.corelibrary",
                "1.0.0",
                "lib",
                "netnano1.0",
                "mscorlib.dll");
            var targetReferenceDirectory = Path.GetDirectoryName(targetCoreLibraryPath)!;
            Directory.CreateDirectory(targetReferenceDirectory);
            File.Copy(typeof(object).Assembly.Location, targetCoreLibraryPath);
            var projectPath = Path.Combine(root, "App.rvnproj");
            File.WriteAllText(projectPath, $$"""
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
                    <PackageReference Include="Fake.nanoFramework.CoreLibrary" Version="1.0.0" />
                  </ItemGroup>
                </Project>
                """);

            Environment.SetEnvironmentVariable("NUGET_PACKAGES", globalPackages);
            var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
            var projectId = workspace.OpenProject(projectPath);
            var project = workspace.CurrentSolution.GetProject(projectId)!;

            Assert.Equal("netnano1.0", project.TargetFramework);
            Assert.True(project.CompilationOptions!.EmbedCoreTypes);
            Assert.Equal(FrameworkProjectionMode.None, project.CompilationOptions.FrameworkProjectionMode);
            var reference = Assert.Single(project.MetadataReferences.OfType<PortableExecutableReference>());
            Assert.True(PathsEqual(targetCoreLibraryPath, reference.FilePath));
            Assert.DoesNotContain(
                project.Documents,
                static document => document.Name.EndsWith("Prelude.g.rvn", StringComparison.OrdinalIgnoreCase));
        }
        finally
        {
            Environment.SetEnvironmentVariable("NUGET_PACKAGES", originalPackages);
            DeleteDirectoryIfExists(root);
        }
    }

    [Fact]
    public void OpenProject_MsBuildProject_LoadsAnalyzerAndSourceGeneratorItems()
    {
        var root = CreateTempDirectory();
        try
        {
            var dependencyDirectory = Path.Combine(root, "Dependency");
            Directory.CreateDirectory(dependencyDirectory);
            File.WriteAllText(Path.Combine(dependencyDirectory, "dependency.rvn"), "class Dependency { }");
            var dependencyProjectPath = Path.Combine(dependencyDirectory, "Dependency.rvnproj");
            File.WriteAllText(dependencyProjectPath, """
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <TargetFramework>net10.0</TargetFramework>
                  </PropertyGroup>
                  <ItemGroup>
                    <Compile Include="dependency.rvn" />
                  </ItemGroup>
                </Project>
                """);

            File.WriteAllText(Path.Combine(root, "main.rvn"), "class C { }");
            var extensionAssemblyPath = typeof(MsBuildProjectSystemServiceTests).Assembly.Location;
            var projectPath = Path.Combine(root, "App.rvnproj");
            File.WriteAllText(projectPath, $$"""
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <TargetFramework>net10.0</TargetFramework>
                  </PropertyGroup>
                  <ItemGroup>
                    <Compile Include="main.rvn" />
                    <Analyzer Include="{{extensionAssemblyPath}}" />
                    <SourceGenerator Include="{{extensionAssemblyPath}}" />
                    <ProjectReference Include="{{Path.GetRelativePath(root, dependencyProjectPath)}}" />
                  </ItemGroup>
                </Project>
                """);

            MsBuildLocatorRegistration.EnsureRegistered();
            var evaluation = MsBuildProjectEvaluator.Evaluate(projectPath, RavenProjectConventions.Default);
            Assert.Contains(extensionAssemblyPath, evaluation.AnalyzerReferencePaths);
            Assert.Contains(extensionAssemblyPath, evaluation.GeneratorReferencePaths);

            var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
            var projectId = workspace.OpenProject(projectPath);
            var project = workspace.CurrentSolution.GetProject(projectId)!;

            Assert.NotEmpty(project.AnalyzerReferences);
            Assert.Single(project.GeneratorReferences);
            Assert.Single(project.ProjectReferences);
        }
        finally
        {
            DeleteDirectoryIfExists(root);
        }
    }

    [Fact]
    public void OpenProject_MsBuildProject_GeneratesPreludeFromImportItems()
    {
        var root = CreateTempDirectory();
        try
        {
            File.WriteAllText(Path.Combine(root, "main.rvn"), "class C { }");

            var projectPath = Path.Combine(root, "App.rvnproj");
            File.WriteAllText(projectPath, """
                                          <Project Sdk="Microsoft.NET.Sdk">
                                            <PropertyGroup>
                                              <TargetFramework>net10.0</TargetFramework>
                                            </PropertyGroup>
                                            <ItemGroup>
                                              <Compile Include="main.rvn" />
                                              <Import Include="SuperheroApp.Models" />
                                              <Import Include="System.Console" Static="True" />
                                              <Import Include="System.DateTime" Alias="DT" />
                                            </ItemGroup>
                                          </Project>
                                          """);

            var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
            var projectId = workspace.OpenProject(projectPath);
            var project = workspace.CurrentSolution.GetProject(projectId)!;

            var generated = Assert.Single(
                project.Documents,
                static document => document.Name.EndsWith("Prelude.g.rvn", StringComparison.OrdinalIgnoreCase));
            var source = generated.Text.ToString();
            Assert.Contains("import SuperheroApp.Models.*", source, StringComparison.Ordinal);
            Assert.Contains("import System.Console.*", source, StringComparison.Ordinal);
            Assert.Contains("alias DT = System.DateTime", source, StringComparison.Ordinal);
        }
        finally
        {
            DeleteDirectoryIfExists(root);
        }
    }

    [Fact]
    public void OpenProject_MsBuildProject_GeneratesPreludeFromSdkImportItems()
    {
        var root = CreateTempDirectory();
        try
        {
            File.WriteAllText(Path.Combine(root, "main.rvn"), "class C { }");

            var projectPath = Path.Combine(root, "App.rvnproj");
            File.WriteAllText(projectPath, """
                                          <Project Sdk="Microsoft.NET.Sdk">
                                            <PropertyGroup>
                                              <TargetFramework>net10.0</TargetFramework>
                                              <ImplicitImports>enable</ImplicitImports>
                                              <_RavenSdkProvidesImplicitImports>true</_RavenSdkProvidesImplicitImports>
                                            </PropertyGroup>
                                            <ItemGroup>
                                              <Compile Include="main.rvn" />
                                              <Import Include="System" IsImplicitlyDefined="true" />
                                              <Import Include="System.Linq" IsImplicitlyDefined="true" />
                                              <Import Remove="System.Linq" />
                                              <Import Include="Microsoft.AspNetCore.Builder" />
                                              <Import Include="System.Console" Static="true" />
                                              <Import Include="System.DateTime" Alias="DT" />
                                            </ItemGroup>
                                          </Project>
                                          """);

            var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
            var projectId = workspace.OpenProject(projectPath);
            var project = workspace.CurrentSolution.GetProject(projectId)!;

            var generated = Assert.Single(
                project.Documents,
                static document => document.Name.EndsWith("Prelude.g.rvn", StringComparison.OrdinalIgnoreCase));
            var source = generated.Text.ToString();
            Assert.Contains("import System.*", source, StringComparison.Ordinal);
            Assert.DoesNotContain("import System.Linq.*", source, StringComparison.Ordinal);
            Assert.Contains("import Microsoft.AspNetCore.Builder.*", source, StringComparison.Ordinal);
            Assert.Contains("import System.Console.*", source, StringComparison.Ordinal);
            Assert.Contains("alias DT = System.DateTime", source, StringComparison.Ordinal);
            Assert.DoesNotContain("import System.Collections.*", source, StringComparison.Ordinal);
        }
        finally
        {
            DeleteDirectoryIfExists(root);
        }
    }

    [Fact]
    public void OpenProject_MsBuildProject_DoesNotGeneratePreludeWhenImplicitImportsDisabled()
    {
        var root = CreateTempDirectory();
        try
        {
            File.WriteAllText(Path.Combine(root, "main.rvn"), "class C { }");

            var projectPath = Path.Combine(root, "App.rvnproj");
            File.WriteAllText(projectPath, """
                                          <Project Sdk="Microsoft.NET.Sdk">
                                            <PropertyGroup>
                                              <TargetFramework>net10.0</TargetFramework>
                                              <ImplicitImports>disable</ImplicitImports>
                                            </PropertyGroup>
                                            <ItemGroup>
                                              <Compile Include="main.rvn" />
                                            </ItemGroup>
                                          </Project>
                                          """);

            var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
            var projectId = workspace.OpenProject(projectPath);
            var project = workspace.CurrentSolution.GetProject(projectId)!;

            Assert.DoesNotContain(
                project.Documents,
                static document => document.Name.EndsWith("Prelude.g.rvn", StringComparison.OrdinalIgnoreCase));
        }
        finally
        {
            DeleteDirectoryIfExists(root);
        }
    }

    [Fact]
    public void OpenProject_MsBuildProject_GeneratesExplicitImportsWhenImplicitImportsDisabled()
    {
        var root = CreateTempDirectory();
        try
        {
            File.WriteAllText(Path.Combine(root, "main.rvn"), "class C { }");

            var projectPath = Path.Combine(root, "App.rvnproj");
            File.WriteAllText(projectPath, """
                                          <Project Sdk="Microsoft.NET.Sdk">
                                            <PropertyGroup>
                                              <TargetFramework>net10.0</TargetFramework>
                                              <ImplicitImports>disable</ImplicitImports>
                                            </PropertyGroup>
                                            <ItemGroup>
                                              <Compile Include="main.rvn" />
                                              <Import Include="System.Result" Static="true" />
                                            </ItemGroup>
                                          </Project>
                                          """);

            var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
            var projectId = workspace.OpenProject(projectPath);
            var project = workspace.CurrentSolution.GetProject(projectId)!;

            var generated = Assert.Single(
                project.Documents,
                static document => document.Name.EndsWith("Prelude.g.rvn", StringComparison.OrdinalIgnoreCase));
            var source = generated.Text.ToString();
            Assert.Contains("import System.Result.*", source, StringComparison.Ordinal);
            Assert.DoesNotContain("import System.Collections.*", source, StringComparison.Ordinal);
        }
        finally
        {
            DeleteDirectoryIfExists(root);
        }
    }

    [Fact]
    public void OpenProject_MsBuildProject_DoesNotGeneratePreludeWhenDisabled()
    {
        var root = CreateTempDirectory();
        try
        {
            File.WriteAllText(Path.Combine(root, "main.rvn"), "class C { }");

            var projectPath = Path.Combine(root, "App.rvnproj");
            File.WriteAllText(projectPath, """
                                          <Project Sdk="Microsoft.NET.Sdk">
                                            <PropertyGroup>
                                              <TargetFramework>net10.0</TargetFramework>
                                              <GeneratePreludeImports>false</GeneratePreludeImports>
                                            </PropertyGroup>
                                            <ItemGroup>
                                              <Compile Include="main.rvn" />
                                            </ItemGroup>
                                          </Project>
                                          """);

            var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
            var projectId = workspace.OpenProject(projectPath);
            var project = workspace.CurrentSolution.GetProject(projectId)!;

            Assert.DoesNotContain(
                project.Documents,
                static document => document.Name.EndsWith("Prelude.g.rvn", StringComparison.OrdinalIgnoreCase));
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
            File.WriteAllText(Path.Combine(appDirectory, "app.rvn"), "let x = 42");

            var libProjectPath = Path.Combine(libDirectory, "Lib.rvnproj");
            var appProjectPath = Path.Combine(appDirectory, "App.rvnproj");

            File.WriteAllText(libProjectPath, """
                                             <Project Sdk="Microsoft.NET.Sdk">
                                               <PropertyGroup>
                                                 <TargetFramework>net10.0</TargetFramework>
                                               </PropertyGroup>
                                               <ItemGroup>
                                                 <Compile Include="lib.rvn" />
                                               </ItemGroup>
                                             </Project>
                                             """);

            File.WriteAllText(appProjectPath, $$"""
                                             <Project Sdk="Microsoft.NET.Sdk">
                                               <PropertyGroup>
                                                 <TargetFramework>net10.0</TargetFramework>
                                               </PropertyGroup>
                                               <ItemGroup>
                                                 <Compile Include="app.rvn" />
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
    public void OpenProject_MsBuildProjectReference_RecursivelyLoadsReferencedRavenProject()
    {
        var root = CreateTempDirectory();
        try
        {
            var libDirectory = Path.Combine(root, "Lib");
            var appDirectory = Path.Combine(root, "App");
            Directory.CreateDirectory(libDirectory);
            Directory.CreateDirectory(appDirectory);

            File.WriteAllText(Path.Combine(libDirectory, "lib.rvn"), """
namespace Samples.Docs

public class WidgetFactory {
    public static func CreateDefault() -> int => 42
}
""");

            File.WriteAllText(Path.Combine(appDirectory, "app.rvn"), """
import Samples.Docs.*

func Main() {
    System.Console.WriteLine(WidgetFactory.CreateDefault())
}
""");

            var libProjectPath = Path.Combine(libDirectory, "Lib.rvnproj");
            var appProjectPath = Path.Combine(appDirectory, "App.rvnproj");

            File.WriteAllText(libProjectPath, """
                                             <Project Sdk="Microsoft.NET.Sdk">
                                               <PropertyGroup>
                                                 <TargetFramework>net10.0</TargetFramework>
                                               </PropertyGroup>
                                               <ItemGroup>
                                                 <Compile Include="lib.rvn" />
                                               </ItemGroup>
                                             </Project>
                                             """);

            File.WriteAllText(appProjectPath, $$"""
                                             <Project Sdk="Microsoft.NET.Sdk">
                                               <PropertyGroup>
                                                 <TargetFramework>net10.0</TargetFramework>
                                               </PropertyGroup>
                                               <ItemGroup>
                                                 <Compile Include="app.rvn" />
                                                 <ProjectReference Include="{{Path.GetRelativePath(appDirectory, libProjectPath)}}" />
                                               </ItemGroup>
                                             </Project>
                                             """);

            var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
            var appProjectId = workspace.OpenProject(appProjectPath);

            var appProject = workspace.CurrentSolution.GetProject(appProjectId)!;
            var projectReference = Assert.Single(appProject.ProjectReferences);
            var libProject = workspace.CurrentSolution.GetProject(projectReference.ProjectId);

            Assert.NotNull(libProject);
            Assert.True(string.Equals(libProject!.FilePath, libProjectPath, StringComparison.OrdinalIgnoreCase));
            Assert.Contains(appProject.Documents, document => document.Name == "app.rvn");

            var compilation = workspace.GetCompilation(appProjectId);
            Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAV0103");
            Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAV1014");
            Assert.NotNull(compilation.GetEntryPoint());
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
            File.WriteAllText(Path.Combine(appDirectory, "main.rvn"), "let x = 1");

            File.WriteAllText(libProjectPath, """
                                             <Project Sdk="Microsoft.NET.Sdk">
                                               <PropertyGroup>
                                                 <TargetFramework>net10.0</TargetFramework>
                                               </PropertyGroup>
                                               <ItemGroup>
                                                 <Compile Include="lib.rvn" />
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
                                               <EnableDefaultCompileItems>false</EnableDefaultCompileItems>
                                               <RavenAllowGlobalStatements>true</RavenAllowGlobalStatements>
                                               <GenerateDocumentationFile>false</GenerateDocumentationFile>
                                             </PropertyGroup>
                                               <ItemGroup>
                                                 <PackageReference Include="Newtonsoft.Json" Version="13.0.3" />
                                                 <ProjectReference Include="{{Path.GetRelativePath(appDirectory, csProjectPath)}}" />
                                                 <Compile Include="main.rvn" />
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
                    .WithFrameworkProjectionMode(FrameworkProjectionMode.None));
            updatedProject = updatedProject.WithDocumentationOptions(
                new ProjectDocumentationOptions(
                    GenerateXmlDocumentation: true,
                    GenerateMarkdownDocumentation: true,
                    XmlDocumentationFile: "artifacts/App.xml",
                    MarkdownDocumentationOutputPath: "artifacts/App.docs"));
            workspace.TryApplyChanges(updatedProject.Solution);

            workspace.SaveProject(appProjectId, appProjectPath);

            var savedDocument = System.Xml.Linq.XDocument.Load(appProjectPath);
            var rootElement = savedDocument.Root!;

            Assert.Contains(rootElement.Descendants(), e => e.Name.LocalName == "PackageReference" && (string?)e.Attribute("Include") == "Newtonsoft.Json");
            Assert.Contains(rootElement.Descendants(), e => e.Name.LocalName == "ProjectReference" && PathsEqual((string?)e.Attribute("Include"), Path.GetRelativePath(appDirectory, csProjectPath)));
            var compileIncludes = rootElement.Descendants()
                .Where(e => e.Name.LocalName == "Compile")
                .Select(e => (string?)e.Attribute("Include"))
                .Where(static value => !string.IsNullOrWhiteSpace(value))
                .ToArray();

            Assert.Contains("main.rvn", compileIncludes);
            Assert.Contains("extra.rvn", compileIncludes);
            Assert.DoesNotContain(compileIncludes, include => include!.EndsWith("TargetFrameworkAttribute.g.rvn", StringComparison.OrdinalIgnoreCase));

            Assert.Equal("Library", rootElement.Descendants().First(e => e.Name.LocalName == "OutputType").Value);
            Assert.Equal("true", rootElement.Descendants().First(e => e.Name.LocalName == "AllowUnsafeBlocks").Value);
            Assert.Equal("false", rootElement.Descendants().First(e => e.Name.LocalName == "RavenAllowGlobalStatements").Value);
            Assert.Equal("false", rootElement.Descendants().First(e => e.Name.LocalName == "EnableIsNotNullNarrowing").Value);
            Assert.DoesNotContain(rootElement.Descendants(), e => e.Name.LocalName == "EnableNullFlowAnalysis");
            Assert.DoesNotContain(rootElement.Descendants(), e => e.Name.LocalName is "MembersPublicByDefault" or "RavenMembersPublicByDefault");
            Assert.Equal("None", rootElement.Descendants().First(e => e.Name.LocalName == "RavenFrameworkProjections").Value);
            Assert.Equal("true", rootElement.Descendants().First(e => e.Name.LocalName == "GenerateDocumentationFile").Value);
            Assert.Equal("true", rootElement.Descendants().First(e => e.Name.LocalName == "GenerateMarkdownDocumentationFile").Value);
            Assert.Equal("artifacts/App.xml", rootElement.Descendants().First(e => e.Name.LocalName == "DocumentationFile").Value);
            Assert.Equal("artifacts/App.docs", rootElement.Descendants().First(e => e.Name.LocalName == "MarkdownDocumentationOutputPath").Value);

            Assert.True(File.Exists(Path.Combine(appDirectory, "extra.rvn")));
            Assert.Contains("extra", File.ReadAllText(Path.Combine(appDirectory, "extra.rvn")));
        }
        finally
        {
            DeleteDirectoryIfExists(root);
        }
    }

    [Fact]
    public void SaveProject_MsBuildProject_DoesNotMaterializeImplicitCompileItems()
    {
        var root = CreateTempDirectory();
        try
        {
            var projectPath = Path.Combine(root, "App.rvnproj");
            File.WriteAllText(projectPath, """
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <TargetFramework>net10.0</TargetFramework>
                  </PropertyGroup>
                </Project>
                """);

            var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
            var projectId = workspace.OpenProject(projectPath);
            var project = workspace.CurrentSolution.GetProject(projectId)!;
            var sourcePath = Path.Combine(root, "main.rvn");
            var document = project.AddDocument(
                "main.rvn",
                Raven.CodeAnalysis.Text.SourceText.From("class App { }"),
                sourcePath);
            workspace.TryApplyChanges(document.Project.Solution);

            workspace.SaveProject(projectId, projectPath);

            var rootElement = System.Xml.Linq.XDocument.Load(projectPath).Root!;
            Assert.DoesNotContain(
                rootElement.Descendants(),
                element => element.Name.LocalName == "Compile");
            Assert.True(File.Exists(sourcePath));
        }
        finally
        {
            DeleteDirectoryIfExists(root);
        }
    }

    [Fact]
    public void OpenProject_MarkedCompilerPluginProjectReference_BuildsAndLoadsDirectMacro()
    {
        var root = CreateTempDirectory();
        try
        {
            var macrosDirectory = Path.Combine(root, "macros");
            var appDirectory = Path.Combine(root, "app");
            Directory.CreateDirectory(macrosDirectory);
            Directory.CreateDirectory(appDirectory);

            var macroSourcePath = Path.Combine(macrosDirectory, "main.rvn");
            File.WriteAllText(macroSourcePath, """"
                import System.Collections.Generic.*
                import Raven.CodeAnalysis.Macros.*
                import Raven.CodeAnalysis.Syntax.*
                import Raven.CodeAnalysis.Syntax.SyntaxFactory.*

                [assembly: RavenCompilerPlugin(typeof(ObservableMacro))]

                class ObservableMacro : IMacroDefinition {
                    val Name: string => "Observable"
                    val Kind: MacroKind => MacroKind.AttachedDeclaration
                    val Targets: MacroTarget => MacroTarget.Property

                    func Expand(context: AttachedMacroContext) -> MacroExpansionResult {
                        let property: PropertyDeclarationSyntax = context.TargetDeclaration else {
                            return MacroExpansionResult.Empty
                        }

                        let tree = SyntaxFactory.ParseSyntaxTree("""
                            class __GeneratedContainer {
                                private var _Title: string

                                var Title: string {
                                    get => _Title
                                    set {
                                        _Title = value
                                    }
                                }
                            }
                            """)

                        let container: ClassDeclarationSyntax = tree.GetRoot().Members[0] else {
                            return MacroExpansionResult.Empty
                        }

                        let backingStorage: PropertyDeclarationSyntax = container.Members[0] else {
                            return MacroExpansionResult.Empty
                        }
                        let replacement: PropertyDeclarationSyntax = container.Members[1] else {
                            return MacroExpansionResult.Empty
                        }

                        MacroExpansionResult {
                            ReplacementDeclaration = replacement
                            IntroducedMembers = [container.Members[0]]
                        }
                    }
                }

                public static class MacroRuntime {
                    static func Answer() -> int => 42
                }
                """");

            var macroProjectPath = Path.Combine(macrosDirectory, "ObservableMacros.rvnproj");
            var ravenCodeAnalysisPath = typeof(Compilation).Assembly.Location;
            File.WriteAllText(macroProjectPath, $$"""
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <TargetFramework>net10.0</TargetFramework>
                    <AssemblyName>ObservableMacros</AssemblyName>
                    <OutputType>Library</OutputType>
                  </PropertyGroup>
                  <ItemGroup>
                    <Compile Include="main.rvn" />
                    <Reference Include="Raven.CodeAnalysis">
                      <HintPath>{{ravenCodeAnalysisPath}}</HintPath>
                    </Reference>
                  </ItemGroup>
                </Project>
                """);

            var appSourcePath = Path.Combine(appDirectory, "main.rvn");
            File.WriteAllText(appSourcePath, """
                class MyViewModel {
                    #[Observable]
                    var Title: string
                }

                func ReadMacroRuntime() -> int => MacroRuntime.Answer()
                """);

            var appProjectPath = Path.Combine(appDirectory, "App.rvnproj");
            File.WriteAllText(appProjectPath, $$"""
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <TargetFramework>net10.0</TargetFramework>
                  </PropertyGroup>
                  <ItemGroup>
                    <Compile Include="main.rvn" />
                    <ProjectReference Include="{{Path.GetRelativePath(appDirectory, macroProjectPath)}}" />
                  </ItemGroup>
                </Project>
                """);

            var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
            var projectId = workspace.OpenProject(appProjectPath);
            var compilation = workspace.GetCompilation(projectId);
            var project = workspace.CurrentSolution.GetProject(projectId)!;
            var document = project.Documents.Single(doc => doc.FilePath == appSourcePath);
            var syntaxTree = document.GetSyntaxTreeAsync().GetAwaiter().GetResult()!;
            var attribute = syntaxTree.GetRoot().DescendantNodes().OfType<AttributeSyntax>().Single();

            var expansion = compilation.GetSemanticModel(syntaxTree).GetMacroExpansion(attribute);

            Assert.NotNull(expansion);
            Assert.IsType<PropertyDeclarationSyntax>(expansion!.ReplacementDeclaration);
            Assert.Single(expansion.IntroducedMembers);
            Assert.DoesNotContain(
                compilation.GetDiagnostics(),
                static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
            Assert.Single(project.MacroReferences);
            Assert.Empty(project.ProjectReferences);
            var macroOutputPath = Path.Combine(macrosDirectory, "bin", "Debug", "net10.0", "ObservableMacros.dll");
            Assert.True(File.Exists(macroOutputPath));
            Assert.Contains(
                project.MetadataReferences.OfType<PortableExecutableReference>(),
                reference => PathsEqual(reference.FilePath, macroOutputPath));

            workspace.SaveProject(projectId, appProjectPath);
            var savedProject = System.Xml.Linq.XDocument.Load(appProjectPath);
            Assert.Contains(
                savedProject.Descendants(),
                element =>
                    element.Name.LocalName == "ProjectReference" &&
                    PathsEqual(
                        (string?)element.Attribute("Include"),
                        Path.GetRelativePath(appDirectory, macroProjectPath)));
            Assert.DoesNotContain(
                savedProject.Descendants(),
                static element => element.Name.LocalName == "RavenMacro");
        }
        finally
        {
            DeleteDirectoryIfExists(root);
        }
    }

    [Fact]
    public void OpenProject_MarkedCSharpCompilerPluginProjectReference_BuildsAndLoadsDirectMacro()
    {
        var root = CreateTempDirectory();
        try
        {
            var macrosDirectory = Path.Combine(root, "macros");
            var appDirectory = Path.Combine(root, "app");
            Directory.CreateDirectory(macrosDirectory);
            Directory.CreateDirectory(appDirectory);

            var macroSourcePath = Path.Combine(macrosDirectory, "AnswerMacro.cs");
            File.WriteAllText(macroSourcePath, """
                using Raven.CodeAnalysis.Macros;
                using Raven.CodeAnalysis.Syntax;

                [assembly: RavenCompilerPlugin(typeof(AnswerMacro))]

                public sealed class AnswerMacro : IMacroDefinition
                {
                    public string Name => "answer";
                    public MacroKind Kind => MacroKind.Freestanding;

                    public FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context)
                        => FreestandingMacroExpansionResult.FromExpression(
                            SyntaxFactory.ParseExpression("42"));
                }
                """);

            var macroProjectPath = Path.Combine(macrosDirectory, "AnswerMacros.csproj");
            var ravenCodeAnalysisPath = typeof(Compilation).Assembly.Location;
            File.WriteAllText(macroProjectPath, $$"""
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <TargetFramework>{{TestTargetFramework.Default}}</TargetFramework>
                    <AssemblyName>AnswerMacros</AssemblyName>
                    <Nullable>enable</Nullable>
                  </PropertyGroup>
                  <ItemGroup>
                    <Reference Include="Raven.CodeAnalysis">
                      <HintPath>{{ravenCodeAnalysisPath}}</HintPath>
                    </Reference>
                  </ItemGroup>
                </Project>
                """);

            var appSourcePath = Path.Combine(appDirectory, "main.rvn");
            File.WriteAllText(appSourcePath, "func Main() -> int => answer!{ }");

            var appProjectPath = Path.Combine(appDirectory, "App.rvnproj");
            File.WriteAllText(appProjectPath, $$"""
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <TargetFramework>{{TestTargetFramework.Default}}</TargetFramework>
                    <OutputType>Library</OutputType>
                  </PropertyGroup>
                  <ItemGroup>
                    <Compile Include="main.rvn" />
                    <ProjectReference Include="{{Path.GetRelativePath(appDirectory, macroProjectPath)}}" />
                  </ItemGroup>
                </Project>
                """);

            var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
            var projectId = workspace.OpenProject(appProjectPath);
            var project = workspace.CurrentSolution.GetProject(projectId)!;
            var compilation = workspace.GetCompilation(projectId);

            Assert.DoesNotContain(
                compilation.GetDiagnostics(),
                static diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
            Assert.Single(project.MacroReferences);
            Assert.Empty(project.ProjectReferences);
            Assert.True(File.Exists(Path.Combine(
                macrosDirectory,
                "bin",
                "Debug",
                TestTargetFramework.Default,
                "AnswerMacros.dll")));
        }
        finally
        {
            DeleteDirectoryIfExists(root);
        }
    }

    [Fact]
    public void OpenProject_CompilerPluginProjectReference_WithObservableReplacement_EmitsExpandedSetter()
    {
        var root = CreateTempDirectory();
        try
        {
            var macrosDirectory = Path.Combine(root, "macros");
            var appDirectory = Path.Combine(root, "app");
            Directory.CreateDirectory(macrosDirectory);
            Directory.CreateDirectory(appDirectory);

            var macroSourcePath = Path.Combine(macrosDirectory, "main.rvn");
            File.WriteAllText(macroSourcePath, """"
                import Raven.CodeAnalysis.Macros.*
                import Raven.CodeAnalysis.Syntax.*

                [assembly: RavenCompilerPlugin(typeof(ObservableMacro))]

                class ObservableMacro : IMacroDefinition {
                    val Name: string => "Observable"
                    val Kind: MacroKind => MacroKind.AttachedDeclaration
                    val Targets: MacroTarget => MacroTarget.Property

                    func Expand(context: AttachedMacroContext) -> MacroExpansionResult {
                        let property: PropertyDeclarationSyntax = context.TargetDeclaration else {
                            return MacroExpansionResult.Empty
                        }

                        let propertyName = property.Identifier.ValueText
                        let propertyType = property.Type.Type.ToString()
                        let backingFieldName = "_${propertyName}"

                        let tree = SyntaxFactory.ParseSyntaxTree("""
                            class __GeneratedContainer {
                                private var ${backingFieldName}: ${propertyType}

                                var ${propertyName}: ${propertyType} {
                                    get => ${backingFieldName}
                                    set {
                                        let oldValue = ${backingFieldName}
                                        ${backingFieldName} = value
                                        RaisePropertyChanged(nameof(${propertyName}), oldValue, value)
                                    }
                                }
                            }
                            """)

                        let container: ClassDeclarationSyntax = tree.GetRoot().Members[0] else {
                            return MacroExpansionResult.Empty
                        }

                        let backingStorage: PropertyDeclarationSyntax = container.Members[0] else {
                            return MacroExpansionResult.Empty
                        }
                        let replacement: PropertyDeclarationSyntax = container.Members[1] else {
                            return MacroExpansionResult.Empty
                        }

                        MacroExpansionResult {
                            ReplacementDeclaration = replacement
                            IntroducedMembers = [container.Members[0]]
                        }
                    }
                }
                """");

            var macroProjectPath = Path.Combine(macrosDirectory, "ObservableMacros.rvnproj");
            var ravenCodeAnalysisPath = typeof(Compilation).Assembly.Location;
            File.WriteAllText(macroProjectPath, $$"""
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <TargetFramework>net10.0</TargetFramework>
                    <AssemblyName>ObservableMacros</AssemblyName>
                    <OutputType>Library</OutputType>
                  </PropertyGroup>
                  <ItemGroup>
                    <Compile Include="main.rvn" />
                    <Reference Include="Raven.CodeAnalysis">
                      <HintPath>{{ravenCodeAnalysisPath}}</HintPath>
                    </Reference>
                  </ItemGroup>
                </Project>
                """);

            var appSourcePath = Path.Combine(appDirectory, "main.rvn");
            File.WriteAllText(appSourcePath, """
                open class ObservableBase {
                    var Count: int

                    protected func RaisePropertyChanged(propertyName: string, oldValue: object?, newValue: object?) -> unit {
                        Count = Count + 1
                    }
                }

                class MyViewModel : ObservableBase {
                    #[Observable]
                    var Title: string = ""
                }

                class Harness {
                    static func Run() -> int {
                        let model = MyViewModel()
                        model.Title = "Hello"
                        return model.Count
                    }
                }
                """);

            var appProjectPath = Path.Combine(appDirectory, "App.rvnproj");
            File.WriteAllText(appProjectPath, $$"""
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <TargetFramework>net10.0</TargetFramework>
                  </PropertyGroup>
                  <ItemGroup>
                    <Compile Include="main.rvn" />
                    <ProjectReference Include="{{Path.GetRelativePath(appDirectory, macroProjectPath)}}" />
                  </ItemGroup>
                </Project>
                """);

            var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
            var projectId = workspace.OpenProject(appProjectPath);
            var compilation = workspace.GetCompilation(projectId);
            var project = workspace.CurrentSolution.GetProject(projectId)!;
            var document = project.Documents.Single(doc => doc.FilePath == appSourcePath);
            var syntaxTree = document.GetSyntaxTreeAsync().GetAwaiter().GetResult()!;

            using var peStream = new MemoryStream();
            var emitResult = compilation.Emit(peStream);
            Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));

            using var loaded = TestAssemblyLoader.LoadFromStream(peStream, TestMetadataReferences.Default);
            var method = loaded.Assembly.GetType("Harness", throwOnError: true)!
                .GetMethod("Run", BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic);

            Assert.Equal(1, method!.Invoke(null, null));
        }
        finally
        {
            DeleteDirectoryIfExists(root);
        }
    }

    [Fact]
    public void OpenProject_CompilerPluginProjectReference_WithListComprehensionOverSyntaxList_ExpandsMacro()
    {
        var root = CreateTempDirectory();
        try
        {
            var macrosDirectory = Path.Combine(root, "macros");
            var appDirectory = Path.Combine(root, "app");
            Directory.CreateDirectory(macrosDirectory);
            Directory.CreateDirectory(appDirectory);

            var macroSourcePath = Path.Combine(macrosDirectory, "main.rvn");
            File.WriteAllText(macroSourcePath, """"
                import Raven.CodeAnalysis.Macros.*
                import Raven.CodeAnalysis.Syntax.*
                import Raven.CodeAnalysis.Syntax.SyntaxFactory.*

                [assembly: RavenCompilerPlugin(typeof(ObservableMacro))]

                class ObservableMacro : IMacroDefinition {
                    val Name: string => "Observable"
                    val Kind: MacroKind => MacroKind.AttachedDeclaration
                    val Targets: MacroTarget => MacroTarget.Property

                    func Expand(context: AttachedMacroContext) -> MacroExpansionResult {
                        let property: PropertyDeclarationSyntax = context.CurrentDeclaration else {
                            return MacroExpansionResult.Empty
                        }

                        MacroExpansionResult {
                            ReplacementDeclaration = property.WithAttributeLists(FilterNonMacroAttributeLists(property.AttributeLists))
                        }
                    }

                    func FilterNonMacroAttributeLists(attributeLists: SyntaxList<AttributeListSyntax>) -> SyntaxList<AttributeListSyntax> {
                        List<AttributeListSyntax>([
                            for attributeList in attributeLists
                            if attributeList.Attributes.Count > 0 && !attributeList.Attributes[0].IsMacroAttribute()
                                => attributeList
                        ])
                    }
                }
                """");

            var macroProjectPath = Path.Combine(macrosDirectory, "ObservableMacros.rvnproj");
            var ravenCodeAnalysisPath = typeof(Compilation).Assembly.Location;
            File.WriteAllText(macroProjectPath, $$"""
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <TargetFramework>net10.0</TargetFramework>
                    <AssemblyName>ObservableMacros</AssemblyName>
                    <OutputType>Library</OutputType>
                  </PropertyGroup>
                  <ItemGroup>
                    <Compile Include="main.rvn" />
                    <Reference Include="Raven.CodeAnalysis">
                      <HintPath>{{ravenCodeAnalysisPath}}</HintPath>
                    </Reference>
                  </ItemGroup>
                </Project>
                """);

            var appSourcePath = Path.Combine(appDirectory, "main.rvn");
            File.WriteAllText(appSourcePath, """
                import System.*

                class MyViewModel {
                    #[Observable]
                    [Obsolete]
                    var Title: string = ""
                }
                """);

            var appProjectPath = Path.Combine(appDirectory, "App.rvnproj");
            File.WriteAllText(appProjectPath, $$"""
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <TargetFramework>net10.0</TargetFramework>
                  </PropertyGroup>
                  <ItemGroup>
                    <Compile Include="main.rvn" />
                    <ProjectReference Include="{{Path.GetRelativePath(appDirectory, macroProjectPath)}}" />
                  </ItemGroup>
                </Project>
                """);

            var workspace = RavenWorkspace.Create(targetFramework: TestMetadataReferences.TargetFramework);
            var projectId = workspace.OpenProject(appProjectPath);
            var compilation = workspace.GetCompilation(projectId);
            var project = workspace.CurrentSolution.GetProject(projectId)!;
            var document = project.Documents.Single(doc => doc.FilePath == appSourcePath);
            var syntaxTree = document.GetSyntaxTreeAsync().GetAwaiter().GetResult()!;
            var model = compilation.GetSemanticModel(syntaxTree);
            var attribute = syntaxTree.GetRoot().DescendantNodes().OfType<AttributeSyntax>().Single(a => a.Name.ToString() == "Observable");

            var expansion = model.GetMacroExpansion(attribute);

            Assert.NotNull(expansion);
            Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAVM020");
            var replacement = Assert.IsType<PropertyDeclarationSyntax>(expansion!.ReplacementDeclaration);
            Assert.Single(replacement.AttributeLists);
            Assert.Equal("Obsolete", replacement.AttributeLists[0].Attributes[0].Name.ToString());
        }
        finally
        {
            DeleteDirectoryIfExists(root);
        }
    }

    [Fact]
    public void GetCompilerPluginOutputPath_IncludesTargetFrameworkSegment()
    {
        var root = CreateTempDirectory();
        try
        {
            var macrosDirectory = Path.Combine(root, "macros");
            Directory.CreateDirectory(macrosDirectory);

            var projectPath = Path.Combine(macrosDirectory, "ObservableMacros.rvnproj");
            File.WriteAllText(projectPath, """
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <TargetFramework>net11.0</TargetFramework>
                    <AssemblyName>ObservableMacros</AssemblyName>
                    <OutputType>Library</OutputType>
                  </PropertyGroup>
                </Project>
                """);

            var outputPath = MsBuildProjectSystemService.GetCompilerPluginOutputPath(
                projectPath,
                configuration: "Debug",
                targetFramework: "net11.0",
                assemblyName: "ObservableMacros");

            Assert.Equal(
                Path.Combine(macrosDirectory, "bin", "Debug", "net11.0", "ObservableMacros.dll"),
                outputPath);
        }
        finally
        {
            DeleteDirectoryIfExists(root);
        }
    }

    [Fact]
    public void GetCompilerPluginRebuildInputs_IncludeReferencedProjectOutputs()
    {
        var root = CreateTempDirectory();
        try
        {
            var helperDirectory = Path.Combine(root, "helper");
            var macrosDirectory = Path.Combine(root, "macros");
            Directory.CreateDirectory(helperDirectory);
            Directory.CreateDirectory(macrosDirectory);

            File.WriteAllText(Path.Combine(helperDirectory, "Helper.csproj"), """
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <TargetFramework>net10.0</TargetFramework>
                  </PropertyGroup>
                </Project>
                """);

            File.WriteAllText(Path.Combine(macrosDirectory, "main.rvn"), "class ObservableMacroPlugin {}");

            var macroProjectPath = Path.Combine(macrosDirectory, "ObservableMacros.rvnproj");
            File.WriteAllText(macroProjectPath, $$"""
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <TargetFramework>net10.0</TargetFramework>
                    <AssemblyName>ObservableMacros</AssemblyName>
                  </PropertyGroup>
                  <ItemGroup>
                    <Compile Include="main.rvn" />
                    <ProjectReference Include="{{Path.GetRelativePath(macrosDirectory, Path.Combine(helperDirectory, "Helper.csproj"))}}" />
                  </ItemGroup>
                </Project>
                """);

            var evaluation = MsBuildProjectEvaluator.Evaluate(macroProjectPath, RavenProjectConventions.Default);
            var rebuildInputs = MsBuildProjectSystemService.GetCompilerPluginRebuildInputs(evaluation).ToArray();
            var helperOutputPath = MsBuildProjectEvaluator.TryResolveReferencedProjectOutputPath(
                Path.Combine(helperDirectory, "Helper.csproj"),
                evaluation.Configuration,
                evaluation.TargetFramework);

            Assert.Contains(Path.Combine(helperDirectory, "Helper.csproj"), rebuildInputs);
            Assert.Contains(helperOutputPath, rebuildInputs);
        }
        finally
        {
            DeleteDirectoryIfExists(root);
        }
    }

    [Fact]
    public void NeedsRebuild_ZeroLengthMacroOutput_ReturnsTrue()
    {
        var root = CreateTempDirectory();
        try
        {
            var projectPath = Path.Combine(root, "ReactiveMacros.rvnproj");
            var sourcePath = Path.Combine(root, "main.rvn");
            var outputDirectory = Path.Combine(root, "bin", "Debug", "net10.0");
            var outputPath = Path.Combine(outputDirectory, "ReactiveMacros.dll");

            File.WriteAllText(projectPath, """
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <TargetFramework>net10.0</TargetFramework>
                  </PropertyGroup>
                </Project>
                """);
            File.WriteAllText(sourcePath, "class ReactiveMacroPlugin {}");
            Directory.CreateDirectory(outputDirectory);
            File.WriteAllBytes(outputPath, []);

            Assert.True(MsBuildProjectSystemService.NeedsRebuild(projectPath, outputPath, [sourcePath]));
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
