using System;
using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

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

    [Fact]
    public void OpenProject_RavenMacroProjectReference_BuildsAndLoadsCurrentMacroAssembly()
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
                import System.Collections.Immutable.*
                import Raven.CodeAnalysis.Macros.*
                import Raven.CodeAnalysis.Syntax.*

                class ObservableMacroPlugin : IRavenMacroPlugin {
                    val Name: string => "Tests.Observable"

                    func GetMacros() -> ImmutableArray<IMacroDefinition> {
                        ImmutableArray.Create<IMacroDefinition>(ObservableMacro())
                    }
                }

                class ObservableMacro : IAttachedDeclarationMacro {
                    val Name: string => "Observable"
                    val Kind: MacroKind => MacroKind.AttachedDeclaration
                    val Targets: MacroTarget => MacroTarget.Property

                    func Expand(context: AttachedMacroContext) -> MacroExpansionResult {
                        val property = context.TargetDeclaration as PropertyDeclarationSyntax
                        if property is null {
                            return MacroExpansionResult.Empty
                        }

                        val tree = SyntaxFactory.ParseSyntaxTree("""
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

                        val container = tree.GetRoot().Members[0] as ClassDeclarationSyntax
                        if container is null {
                            return MacroExpansionResult.Empty
                        }

                        val backingStorage = container.Members[0] as PropertyDeclarationSyntax
                        val replacement = container.Members[1] as PropertyDeclarationSyntax
                        if backingStorage is null || replacement is null {
                            return MacroExpansionResult.Empty
                        }

                        MacroExpansionResult {
                            ReplacementDeclaration = replacement
                            IntroducedMembers = ImmutableArray.Create<MemberDeclarationSyntax>(backingStorage)
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
                    <RavenCompile Include="main.rvn" />
                    <Reference Include="Raven.CodeAnalysis">
                      <HintPath>{{ravenCodeAnalysisPath}}</HintPath>
                    </Reference>
                  </ItemGroup>
                </Project>
                """);

            var appSourcePath = Path.Combine(appDirectory, "main.rvn");
            File.WriteAllText(appSourcePath, """
                class MyViewModel {
                    [@Observable]
                    var Title: string
                }
                """);

            var appProjectPath = Path.Combine(appDirectory, "App.rvnproj");
            File.WriteAllText(appProjectPath, $$"""
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <TargetFramework>net10.0</TargetFramework>
                  </PropertyGroup>
                  <ItemGroup>
                    <RavenCompile Include="main.rvn" />
                    <RavenMacro Include="{{Path.GetRelativePath(appDirectory, macroProjectPath)}}" />
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
            Assert.True(File.Exists(Path.Combine(macrosDirectory, "bin", "Debug", "net10.0", "ObservableMacros.dll")));
        }
        finally
        {
            DeleteDirectoryIfExists(root);
        }
    }

    [Fact]
    public void OpenProject_RavenMacroProjectReference_WithObservableReplacement_EmitsExpandedSetter()
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
                import System.Collections.Immutable.*
                import Raven.CodeAnalysis.Macros.*
                import Raven.CodeAnalysis.Syntax.*

                class ObservableMacroPlugin : IRavenMacroPlugin {
                    val Name: string => "Tests.Observable"

                    func GetMacros() -> ImmutableArray<IMacroDefinition> {
                        ImmutableArray.Create<IMacroDefinition>(ObservableMacro())
                    }
                }

                class ObservableMacro : IAttachedDeclarationMacro {
                    val Name: string => "Observable"
                    val Kind: MacroKind => MacroKind.AttachedDeclaration
                    val Targets: MacroTarget => MacroTarget.Property

                    func Expand(context: AttachedMacroContext) -> MacroExpansionResult {
                        val property = context.TargetDeclaration as PropertyDeclarationSyntax
                        if property is null {
                            return MacroExpansionResult.Empty
                        }

                        val propertyName = property.Identifier.ValueText
                        val propertyType = property.Type.Type.ToString()
                        val backingFieldName = "_${propertyName}"

                        val tree = SyntaxFactory.ParseSyntaxTree("""
                            class __GeneratedContainer {
                                private var ${backingFieldName}: ${propertyType}

                                var ${propertyName}: ${propertyType} {
                                    get => ${backingFieldName}
                                    set {
                                        val oldValue = ${backingFieldName}
                                        ${backingFieldName} = value
                                        RaisePropertyChanged(nameof(${propertyName}), oldValue, value)
                                    }
                                }
                            }
                            """)

                        val container = tree.GetRoot().Members[0] as ClassDeclarationSyntax
                        if container is null {
                            return MacroExpansionResult.Empty
                        }

                        val backingStorage = container.Members[0] as PropertyDeclarationSyntax
                        val replacement = container.Members[1] as PropertyDeclarationSyntax
                        if backingStorage is null || replacement is null {
                            return MacroExpansionResult.Empty
                        }

                        MacroExpansionResult {
                            ReplacementDeclaration = replacement
                            IntroducedMembers = ImmutableArray.Create<MemberDeclarationSyntax>(backingStorage)
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
                    <RavenCompile Include="main.rvn" />
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
                    [@Observable]
                    var Title: string = ""
                }

                class Harness {
                    static func Run() -> int {
                        val model = MyViewModel()
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
                    <RavenCompile Include="main.rvn" />
                    <RavenMacro Include="{{Path.GetRelativePath(appDirectory, macroProjectPath)}}" />
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
    public void GetRavenMacroOutputPath_IncludesTargetFrameworkSegment()
    {
        var projectPath = Path.Combine(Path.GetTempPath(), "macros", "ObservableMacros.rvnproj");

        var outputPath = MsBuildProjectSystemService.GetRavenMacroOutputPath(
            projectPath,
            configuration: "Debug",
            targetFramework: "net11.0",
            assemblyName: "ObservableMacros");

        Assert.Equal(
            Path.Combine(Path.GetTempPath(), "macros", "bin", "Debug", "net11.0", "ObservableMacros.dll"),
            outputPath);
    }

    [Fact]
    public void GetRavenMacroRebuildInputs_IncludeReferencedProjectOutputs()
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
                    <RavenCompile Include="main.rvn" />
                    <ProjectReference Include="{{Path.GetRelativePath(macrosDirectory, Path.Combine(helperDirectory, "Helper.csproj"))}}" />
                  </ItemGroup>
                </Project>
                """);

            var evaluation = MsBuildProjectEvaluator.Evaluate(macroProjectPath, RavenProjectConventions.Default);
            var rebuildInputs = MsBuildProjectSystemService.GetRavenMacroRebuildInputs(evaluation).ToArray();
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
