using System.Diagnostics;
using System.Text;

using Xunit.Abstractions;

namespace Raven.CodeAnalysis.Tests.Workspaces;

public sealed class MsBuildSampleProjectCompilationTests(ITestOutputHelper output)
{
    [Fact]
    public void SampleProjects_CompileThroughRvnCli()
    {
        var repoRoot = GetRepositoryRoot();
        var projectsRoot = Path.Combine(repoRoot, "samples", "projects");
        var compilerDllPath = EnsureCompilerBuilt(repoRoot);
        var projectPaths = Directory
            .EnumerateFiles(projectsRoot, "*.rvnproj", SearchOption.AllDirectories)
            .OrderBy(static path => path, StringComparer.OrdinalIgnoreCase)
            .ToArray();

        Assert.NotEmpty(projectPaths);

        var outputRoot = CreateTempDirectory();
        try
        {
            var failures = new List<string>();
            foreach (var projectPath in projectPaths)
            {
                var relativeProjectPath = Path.GetRelativePath(repoRoot, projectPath);
                var projectOutputDirectory = Path.Combine(
                    outputRoot,
                    Path.ChangeExtension(relativeProjectPath, null) ?? Path.GetFileNameWithoutExtension(projectPath));

                Directory.CreateDirectory(projectOutputDirectory);

                var result = RunCompiler(repoRoot, compilerDllPath, projectPath, projectOutputDirectory);
                output.WriteLine($"[{relativeProjectPath}] exit={result.ExitCode}");
                if (!string.IsNullOrWhiteSpace(result.StdOut))
                    output.WriteLine(result.StdOut);
                if (!string.IsNullOrWhiteSpace(result.StdErr))
                    output.WriteLine(result.StdErr);

                if (result.ExitCode != 0)
                    failures.Add($"{relativeProjectPath}\nstdout:\n{result.StdOut}\nstderr:\n{result.StdErr}");
            }

            Assert.True(failures.Count == 0, string.Join("\n\n", failures));
        }
        finally
        {
            DeleteDirectoryIfExists(outputRoot);
        }
    }

    [Fact]
    public void MacroDeclarationsSample_RunsThroughDotnetBuild()
    {
        var repoRoot = GetRepositoryRoot();
        var projectPath = Path.Combine(
            repoRoot,
            "samples",
            "projects",
            "macro-declarations",
            "MacroDeclarations.rvnproj");
        var result = RunProcess(
            "dotnet",
            $"run --project \"{projectPath}\" --property WarningLevel=0",
            Path.GetDirectoryName(projectPath)!,
            timeoutMilliseconds: 300_000);
        output.WriteLine(result.StdOut);
        output.WriteLine(result.StdErr);

        Assert.True(
            result.ExitCode == 0,
            $"dotnet run failed.\nstdout:\n{result.StdOut}\nstderr:\n{result.StdErr}");
        Assert.Contains(
            $"42{Environment.NewLine}42{Environment.NewLine}6",
            result.StdOut,
            StringComparison.Ordinal);
    }

    [Fact]
    public void RavenProject_BuildsThroughDotnetBuild()
    {
        var repoRoot = GetRepositoryRoot();
        var compilerDllPath = EnsureCompilerBuilt(repoRoot, "net10.0");
        var projectRoot = CreateTempDirectory();
        try
        {
            var languageTargetsPath = Path.Combine(repoRoot, "build", "Raven.Language.targets");
            var sourceDirectory = Path.Combine(projectRoot, "src");
            Directory.CreateDirectory(sourceDirectory);

            var projectPath = Path.Combine(projectRoot, "Library.rvnproj");
            File.WriteAllText(projectPath, $$"""
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <LanguageTargets>{{languageTargetsPath}}</LanguageTargets>
                    <RavenCompilerHost>{{compilerDllPath}}</RavenCompilerHost>
                    <TargetFramework>net10.0</TargetFramework>
                    <AssemblyName>RavenBuildOutput</AssemblyName>
                    <OutputType>Library</OutputType>
                  </PropertyGroup>
                </Project>
                """);

            File.WriteAllText(Path.Combine(sourceDirectory, "main.rvn"), """
                /// Greets a caller.
                class Greeter {
                    /// Gets the greeting.
                    ///
                    /// @result A greeting from the SDK build.
                    static func Message() -> string {
                        "Hello from dotnet build"
                    }
                }
                """);

            var result = RunProcess("dotnet", $"build \"{projectPath}\" --property WarningLevel=0", projectRoot, timeoutMilliseconds: 300_000);
            output.WriteLine(result.StdOut);
            output.WriteLine(result.StdErr);

            Assert.True(result.ExitCode == 0, $"dotnet build failed.\nstdout:\n{result.StdOut}\nstderr:\n{result.StdErr}");
            Assert.True(
                File.Exists(Path.Combine(projectRoot, "bin", "Debug", "net10.0", "RavenBuildOutput.dll")),
                "Expected Raven project build output in the SDK target directory.");
            var xmlDocumentationPath = Path.Combine(projectRoot, "bin", "Debug", "net10.0", "RavenBuildOutput.xml");
            Assert.True(
                File.Exists(xmlDocumentationPath),
                "Expected default XML documentation beside the Raven library.");
            Assert.True(
                File.Exists(Path.Combine(projectRoot, "bin", "Debug", "net10.0", "RavenBuildOutput.docs", "manifest.json")),
                "Expected default Markdown documentation beside the Raven library.");
            Assert.False(
                File.Exists(Path.Combine(projectRoot, "bin", "Debug", "net10.0", "Raven.CodeAnalysis.dll")),
                "Ordinary Raven projects should not copy Raven.CodeAnalysis.");
            Assert.Contains(
                "<returns>A greeting from the SDK build.</returns>",
                File.ReadAllText(xmlDocumentationPath),
                StringComparison.Ordinal);

            var rebuildResult = RunProcess(
                "dotnet",
                $"build \"{projectPath}\" --no-restore --property WarningLevel=0",
                projectRoot,
                timeoutMilliseconds: 300_000);
            output.WriteLine(rebuildResult.StdOut);
            output.WriteLine(rebuildResult.StdErr);
            Assert.True(
                rebuildResult.ExitCode == 0,
                $"Second dotnet build failed.\nstdout:\n{rebuildResult.StdOut}\nstderr:\n{rebuildResult.StdErr}");
            Assert.DoesNotContain("Raven CoreCompile:", rebuildResult.StdOut, StringComparison.Ordinal);

            var cleanResult = RunProcess(
                "dotnet",
                $"clean \"{projectPath}\" --property WarningLevel=0",
                projectRoot,
                timeoutMilliseconds: 300_000);
            output.WriteLine(cleanResult.StdOut);
            output.WriteLine(cleanResult.StdErr);
            Assert.True(
                cleanResult.ExitCode == 0,
                $"dotnet clean failed.\nstdout:\n{cleanResult.StdOut}\nstderr:\n{cleanResult.StdErr}");
            Assert.False(File.Exists(Path.Combine(projectRoot, "bin", "Debug", "net10.0", "RavenBuildOutput.dll")));
            Assert.False(File.Exists(xmlDocumentationPath));
            Assert.False(Directory.Exists(Path.Combine(projectRoot, "bin", "Debug", "net10.0", "RavenBuildOutput.docs")));
            Assert.False(Directory.EnumerateFiles(
                Path.Combine(projectRoot, "obj", "Debug", "net10.0"),
                "*.rvn",
                SearchOption.AllDirectories).Any());
        }
        finally
        {
            DeleteDirectoryIfExists(projectRoot);
        }
    }

    [Fact]
    public void RavenProject_BuildsExplicitCompileItems_WhenDefaultItemsAreDisabled()
    {
        var repoRoot = GetRepositoryRoot();
        var compilerDllPath = EnsureCompilerBuilt(repoRoot, "net10.0");
        var projectRoot = CreateTempDirectory();
        try
        {
            var languageTargetsPath = Path.Combine(repoRoot, "build", "Raven.Language.targets");
            var sourceDirectory = Path.Combine(projectRoot, "src");
            Directory.CreateDirectory(sourceDirectory);

            var projectPath = Path.Combine(projectRoot, "Library.rvnproj");
            File.WriteAllText(projectPath, $$"""
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <LanguageTargets>{{languageTargetsPath}}</LanguageTargets>
                    <RavenCompilerHost>{{compilerDllPath}}</RavenCompilerHost>
                    <TargetFramework>net10.0</TargetFramework>
                    <OutputType>Library</OutputType>
                    <EnableDefaultCompileItems>false</EnableDefaultCompileItems>
                  </PropertyGroup>
                  <ItemGroup>
                    <Compile Include="src/main.rvn" />
                  </ItemGroup>
                </Project>
                """);

            File.WriteAllText(Path.Combine(sourceDirectory, "main.rvn"), "class Included { }");
            File.WriteAllText(Path.Combine(sourceDirectory, "excluded.rvn"), "func broken(");

            var result = RunProcess(
                "dotnet",
                $"build \"{projectPath}\" --property WarningLevel=0",
                projectRoot,
                timeoutMilliseconds: 300_000);
            output.WriteLine(result.StdOut);
            output.WriteLine(result.StdErr);

            Assert.True(
                result.ExitCode == 0,
                $"dotnet build failed.\nstdout:\n{result.StdOut}\nstderr:\n{result.StdErr}");
            Assert.True(
                File.Exists(Path.Combine(projectRoot, "bin", "Debug", "net10.0", "Library.dll")),
                "Expected the explicitly included Raven source to build without the excluded source.");
        }
        finally
        {
            DeleteDirectoryIfExists(projectRoot);
        }
    }

    [Fact]
    public void RavenProject_UsesActiveConfigurationAndInnerTargetFramework()
    {
        var repoRoot = GetRepositoryRoot();
        var compilerDllPath = EnsureCompilerBuilt(repoRoot, "net10.0");
        var projectRoot = CreateTempDirectory();
        try
        {
            var languageTargetsPath = Path.Combine(repoRoot, "build", "Raven.Language.targets");
            var projectPath = Path.Combine(projectRoot, "ConfiguredLibrary.rvnproj");
            File.WriteAllText(projectPath, $$"""
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <LanguageTargets>{{languageTargetsPath}}</LanguageTargets>
                    <RavenCompilerHost>{{compilerDllPath}}</RavenCompilerHost>
                    <TargetFrameworks>net9.0;net10.0</TargetFrameworks>
                    <OutputType>Library</OutputType>
                    <EnableDefaultCompileItems>false</EnableDefaultCompileItems>
                  </PropertyGroup>
                  <ItemGroup>
                    <Compile Include="release-net10.rvn" Condition="'$(Configuration)' == 'Release' and '$(TargetFramework)' == 'net10.0'" />
                    <Compile Include="wrong-context.rvn" Condition="'$(Configuration)' != 'Release' or '$(TargetFramework)' != 'net10.0'" />
                  </ItemGroup>
                </Project>
                """);

            File.WriteAllText(Path.Combine(projectRoot, "release-net10.rvn"), "class ConfiguredLibrary { }");
            File.WriteAllText(Path.Combine(projectRoot, "wrong-context.rvn"), "func broken(");

            var result = RunProcess(
                "dotnet",
                $"build \"{projectPath}\" --configuration Release --framework net10.0 --property WarningLevel=0",
                projectRoot,
                timeoutMilliseconds: 300_000);
            output.WriteLine(result.StdOut);
            output.WriteLine(result.StdErr);

            Assert.True(
                result.ExitCode == 0,
                $"dotnet build failed.\nstdout:\n{result.StdOut}\nstderr:\n{result.StdErr}");
            Assert.True(
                File.Exists(Path.Combine(projectRoot, "bin", "Release", "net10.0", "ConfiguredLibrary.dll")),
                "Expected the active Release/net10.0 inner build output.");
            Assert.True(
                Directory.EnumerateFiles(
                    Path.Combine(projectRoot, "obj", "Release", "net10.0", "raven", "generated"),
                    "*.TargetFrameworkAttribute.g.rvn").Any(),
                "Expected generated Raven sources under the active inner-build intermediate directory.");
            Assert.False(
                Directory.Exists(Path.Combine(projectRoot, "obj", "Debug", "raven", "generated")),
                "The compiler must not fall back to Debug project evaluation.");
        }
        finally
        {
            DeleteDirectoryIfExists(projectRoot);
        }
    }

    [Fact]
    public void RavenProject_CompileMacro_DiscoversRuntimeDependencyClosureFromOutput()
    {
        var repoRoot = GetRepositoryRoot();
        var compilerDllPath = EnsureCompilerBuilt(repoRoot, "net10.0");
        var projectRoot = CreateTempDirectory();
        try
        {
            var languageTargetsPath = Path.Combine(repoRoot, "build", "Raven.Language.targets");
            var sourceDirectory = Path.Combine(projectRoot, "src");
            Directory.CreateDirectory(sourceDirectory);

            var projectPath = Path.Combine(projectRoot, "App.rvnproj");
            File.WriteAllText(projectPath, $$"""
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <LanguageTargets>{{languageTargetsPath}}</LanguageTargets>
                    <RavenCompilerHost>{{compilerDllPath}}</RavenCompilerHost>
                    <TargetFramework>net10.0</TargetFramework>
                    <AssemblyName>CompileMacroRuntimeDependency</AssemblyName>
                    <OutputType>Exe</OutputType>
                  </PropertyGroup>
                  <ItemGroup>
                    <Compile Include="src/**/*.rvn" />
                  </ItemGroup>
                </Project>
                """);

            File.WriteAllText(Path.Combine(sourceDirectory, "main.rvn"), """
                import System.*
                import Raven.Macros.*

                func Main() {
                    let increment = compile<System.Func<int, int>>! {
                        value => value + 1
                    }

                    Console.WriteLine(increment(41))
                }
                """);

            var buildResult = RunProcess(
                "dotnet",
                $"build \"{projectPath}\" --property WarningLevel=0",
                projectRoot,
                timeoutMilliseconds: 300_000);
            output.WriteLine(buildResult.StdOut);
            output.WriteLine(buildResult.StdErr);

            Assert.True(
                buildResult.ExitCode == 0,
                $"dotnet build failed.\nstdout:\n{buildResult.StdOut}\nstderr:\n{buildResult.StdErr}");

            var outputDirectory = Path.Combine(projectRoot, "bin", "Debug", "net10.0");
            var codeAnalysisPath = Path.Combine(outputDirectory, "Raven.CodeAnalysis.dll");
            var depsPath = Path.Combine(outputDirectory, "CompileMacroRuntimeDependency.deps.json");
            Assert.True(
                File.Exists(codeAnalysisPath),
                $"Expected the macro runtime dependency at '{codeAnalysisPath}'.");
            Assert.Contains("Raven.CodeAnalysis", File.ReadAllText(depsPath), StringComparison.Ordinal);

            var runResult = RunProcess(
                "dotnet",
                $"run --project \"{projectPath}\" --no-build",
                projectRoot,
                timeoutMilliseconds: 300_000);
            output.WriteLine(runResult.StdOut);
            output.WriteLine(runResult.StdErr);

            Assert.True(
                runResult.ExitCode == 0,
                $"dotnet run failed.\nstdout:\n{runResult.StdOut}\nstderr:\n{runResult.StdErr}");
            Assert.Contains("42", runResult.StdOut, StringComparison.Ordinal);

            File.WriteAllText(Path.Combine(sourceDirectory, "main.rvn"), """
                import System.*

                func Main() {
                    Console.WriteLine(42)
                }
                """);

            var rebuildResult = RunProcess(
                "dotnet",
                $"build \"{projectPath}\" --property WarningLevel=0",
                projectRoot,
                timeoutMilliseconds: 300_000);
            output.WriteLine(rebuildResult.StdOut);
            output.WriteLine(rebuildResult.StdErr);

            Assert.True(
                rebuildResult.ExitCode == 0,
                $"dotnet rebuild failed.\nstdout:\n{rebuildResult.StdOut}\nstderr:\n{rebuildResult.StdErr}");
            Assert.False(
                File.Exists(Path.Combine(
                    projectRoot,
                    "obj",
                    "Debug",
                    "net10.0",
                    ".raven-runtime-dependencies")));
            Assert.DoesNotContain(
                "Raven.CodeAnalysis",
                File.ReadAllText(depsPath),
                StringComparison.Ordinal);
        }
        finally
        {
            DeleteDirectoryIfExists(projectRoot);
        }
    }

    [Fact]
    public void RavenProject_QuoteMacro_UsesExplicitCodeAnalysisReferenceWithGeneralDependencyClosure()
    {
        var repoRoot = GetRepositoryRoot();
        var compilerDllPath = EnsureCompilerBuilt(repoRoot, "net10.0");
        var codeAnalysisPath = Path.Combine(
            Path.GetDirectoryName(compilerDllPath)!,
            "Raven.CodeAnalysis.dll");
        var projectRoot = CreateTempDirectory();
        try
        {
            var languageTargetsPath = Path.Combine(repoRoot, "build", "Raven.Language.targets");
            var sourceDirectory = Path.Combine(projectRoot, "src");
            Directory.CreateDirectory(sourceDirectory);

            var projectPath = Path.Combine(projectRoot, "App.rvnproj");
            File.WriteAllText(projectPath, $$"""
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <LanguageTargets>{{languageTargetsPath}}</LanguageTargets>
                    <RavenCompilerHost>{{compilerDllPath}}</RavenCompilerHost>
                    <TargetFramework>net10.0</TargetFramework>
                    <AssemblyName>ExplicitCodeAnalysisReference</AssemblyName>
                    <OutputType>Exe</OutputType>
                  </PropertyGroup>
                  <ItemGroup>
                    <Compile Include="src/**/*.rvn" />
                    <Reference Include="Raven.CodeAnalysis">
                      <HintPath>{{codeAnalysisPath}}</HintPath>
                      <Private>true</Private>
                    </Reference>
                  </ItemGroup>
                </Project>
                """);

            File.WriteAllText(Path.Combine(sourceDirectory, "main.rvn"), """
                import System.*
                import Raven.Macros.*

                func Main() {
                    let syntax = quote! { 40 + 2 }
                    Console.WriteLine(syntax.ToString())
                }
                """);

            var buildResult = RunProcess(
                "dotnet",
                $"build \"{projectPath}\" --property WarningLevel=0",
                projectRoot,
                timeoutMilliseconds: 300_000);
            output.WriteLine(buildResult.StdOut);
            output.WriteLine(buildResult.StdErr);

            Assert.True(
                buildResult.ExitCode == 0,
                $"dotnet build failed.\nstdout:\n{buildResult.StdOut}\nstderr:\n{buildResult.StdErr}");

            var outputDirectory = Path.Combine(projectRoot, "bin", "Debug", "net10.0");
            Assert.True(File.Exists(Path.Combine(outputDirectory, "Raven.CodeAnalysis.dll")));
            var manifestPath = Path.Combine(
                projectRoot,
                "obj",
                "Debug",
                "net10.0",
                ".raven-runtime-dependencies");
            Assert.True(File.Exists(manifestPath));
            Assert.Contains(
                "Raven.CodeAnalysis.dll",
                File.ReadAllText(manifestPath),
                StringComparison.Ordinal);
        }
        finally
        {
            DeleteDirectoryIfExists(projectRoot);
        }
    }

    [Fact]
    public void RavenProject_BuildsSameProjectMacroWithoutMacroProjectItem()
    {
        var repoRoot = GetRepositoryRoot();
        var compilerDllPath = EnsureCompilerBuilt(repoRoot, "net10.0");
        var projectRoot = CreateTempDirectory();
        try
        {
            var languageTargetsPath = Path.Combine(repoRoot, "build", "Raven.Language.targets");
            var sourceDirectory = Path.Combine(projectRoot, "src");
            Directory.CreateDirectory(sourceDirectory);

            var projectPath = Path.Combine(projectRoot, "App.rvnproj");
            File.WriteAllText(projectPath, $$"""
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <LanguageTargets>{{languageTargetsPath}}</LanguageTargets>
                    <RavenCompilerHost>{{compilerDllPath}}</RavenCompilerHost>
                    <TargetFramework>net10.0</TargetFramework>
                    <AssemblyName>SameProjectMacro</AssemblyName>
                    <OutputType>Library</OutputType>
                  </PropertyGroup>
                  <ItemGroup>
                    <Compile Include="src/**/*.rvn" />
                  </ItemGroup>
                </Project>
                """);

            File.WriteAllText(Path.Combine(sourceDirectory, "macros.rvn"), """
                import Raven.CodeAnalysis.Macros.*

                class LocalAnswerMacro : ITokenTreeExpressionMacro {
                    val Name: string => "localAnswer"

                    func Expand(context: TokenTreeMacroContext) -> FreestandingMacroExpansionResult {
                        FreestandingMacroExpansionResult {
                            Expression = quote!{ 42 }
                        }
                    }
                }
                """);
            File.WriteAllText(Path.Combine(sourceDirectory, "main.rvn"), """
                class Harness {
                    public static func Value() -> int => localAnswer!{ }
                }
                """);

            var result = RunProcess(
                "dotnet",
                $"build \"{projectPath}\" --property WarningLevel=0",
                projectRoot,
                timeoutMilliseconds: 300_000);
            output.WriteLine(result.StdOut);
            output.WriteLine(result.StdErr);

            Assert.True(
                result.ExitCode == 0,
                $"dotnet build failed.\nstdout:\n{result.StdOut}\nstderr:\n{result.StdErr}");
            Assert.True(
                File.Exists(Path.Combine(projectRoot, "bin", "Debug", "net10.0", "SameProjectMacro.dll")),
                "Expected the consumer assembly to be emitted.");
        }
        finally
        {
            DeleteDirectoryIfExists(projectRoot);
        }
    }

    [Fact]
    public void RavenProject_BuildsThroughDotnetBuild_WithRavenCoreRuntimeDependency()
    {
        var repoRoot = GetRepositoryRoot();
        var compilerDllPath = EnsureCompilerBuilt(repoRoot, "net10.0");
        EnsureRavenCoreBuilt(repoRoot, "net10.0");
        var projectRoot = CreateTempDirectory();
        try
        {
            var languageTargetsPath = Path.Combine(repoRoot, "build", "Raven.Language.targets");
            var sourceDirectory = Path.Combine(projectRoot, "src");
            Directory.CreateDirectory(sourceDirectory);

            var projectPath = Path.Combine(projectRoot, "App.rvnproj");
            File.WriteAllText(projectPath, $$"""
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <LanguageTargets>{{languageTargetsPath}}</LanguageTargets>
                    <RavenCompilerHost>{{compilerDllPath}}</RavenCompilerHost>
                    <TargetFramework>net10.0</TargetFramework>
                    <AssemblyName>RavenCoreRuntimeDependency</AssemblyName>
                    <OutputType>Exe</OutputType>
                  </PropertyGroup>
                  <ItemGroup>
                    <Compile Include="src/**/*.rvn" />
                  </ItemGroup>
                </Project>
                """);

            File.WriteAllText(Path.Combine(sourceDirectory, "main.rvn"), """
                import System.*

                func Main() {
                    Console.WriteLine("Raven.Core dependency")
                }
                """);

            var result = RunProcess("dotnet", $"build \"{projectPath}\" --property WarningLevel=0", projectRoot, timeoutMilliseconds: 300_000);
            output.WriteLine(result.StdOut);
            output.WriteLine(result.StdErr);

            Assert.True(result.ExitCode == 0, $"dotnet build failed.\nstdout:\n{result.StdOut}\nstderr:\n{result.StdErr}");

            var outputDirectory = Path.Combine(projectRoot, "bin", "Debug", "net10.0");
            var depsPath = Path.Combine(outputDirectory, "RavenCoreRuntimeDependency.deps.json");
            var corePath = Path.Combine(outputDirectory, "Raven.Core.dll");

            Assert.True(File.Exists(corePath), $"Expected Raven.Core copy-local output at '{corePath}'.");
            Assert.True(File.Exists(depsPath), $"Expected deps file at '{depsPath}'.");

            var depsJson = File.ReadAllText(depsPath);
            Assert.Contains("Raven.Core", depsJson);
        }
        finally
        {
            DeleteDirectoryIfExists(projectRoot);
        }
    }

    [Fact]
    public void CSharpProject_CanReferenceRavenProjectThroughProjectReference()
    {
        var repoRoot = GetRepositoryRoot();
        var compilerDllPath = EnsureCompilerBuilt(repoRoot, "net10.0");
        var root = CreateTempDirectory();
        try
        {
            var languageTargetsPath = Path.Combine(repoRoot, "build", "Raven.Language.targets");
            var ravenDirectory = Path.Combine(root, "raven");
            var csharpDirectory = Path.Combine(root, "csharp");
            Directory.CreateDirectory(ravenDirectory);
            Directory.CreateDirectory(csharpDirectory);

            File.WriteAllText(Path.Combine(ravenDirectory, "Greeter.rvnproj"), $$"""
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <LanguageTargets>{{languageTargetsPath}}</LanguageTargets>
                    <RavenCompilerHost>{{compilerDllPath}}</RavenCompilerHost>
                    <TargetFramework>net10.0</TargetFramework>
                    <AssemblyName>GreeterLib</AssemblyName>
                    <OutputType>Library</OutputType>
                  </PropertyGroup>
                  <ItemGroup>
                    <Compile Include="main.rvn" />
                  </ItemGroup>
                </Project>
                """);

            File.WriteAllText(Path.Combine(ravenDirectory, "main.rvn"), """
                class Greeter {
                    static func Message() -> string {
                        "Hello from Raven reference"
                    }
                }
                """);

            File.WriteAllText(Path.Combine(csharpDirectory, "App.csproj"), """
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <OutputType>Exe</OutputType>
                    <TargetFramework>net10.0</TargetFramework>
                  </PropertyGroup>
                  <ItemGroup>
                    <ProjectReference Include="../raven/Greeter.rvnproj" />
                  </ItemGroup>
                </Project>
                """);

            File.WriteAllText(Path.Combine(csharpDirectory, "Program.cs"), """
                using System;

                Console.WriteLine(Greeter.Message());
                """);

            var appProjectPath = Path.Combine(csharpDirectory, "App.csproj");
            var result = RunProcess("dotnet", $"run --project \"{appProjectPath}\" --property WarningLevel=0", root, timeoutMilliseconds: 300_000);
            output.WriteLine(result.StdOut);
            output.WriteLine(result.StdErr);

            Assert.True(result.ExitCode == 0, $"dotnet run failed.\nstdout:\n{result.StdOut}\nstderr:\n{result.StdErr}");
            Assert.Contains("Hello from Raven reference", result.StdOut);
        }
        finally
        {
            DeleteDirectoryIfExists(root);
        }
    }

    private static string EnsureCompilerBuilt(string repoRoot, string targetFramework = "net11.0")
    {
        var compilerDllPath = Path.Combine(repoRoot, "src", "Raven.Compiler", "bin", "Debug", targetFramework, "rvnc.dll");
        if (!File.Exists(compilerDllPath))
        {
            var compilerProjectPath = Path.Combine(repoRoot, "src", "Raven.Compiler", "Raven.Compiler.csproj");
            var buildArgs = $"build \"{compilerProjectPath}\" --framework {targetFramework} /property:WarningLevel=0 /property:UseRavenCoreReference=false";
            var buildResult = RunProcess("dotnet", buildArgs, repoRoot, timeoutMilliseconds: 300_000);
            Assert.True(
                buildResult.ExitCode == 0,
                $"Failed to build rvnc compiler for sample-project tests.\nstdout:\n{buildResult.StdOut}\nstderr:\n{buildResult.StdErr}");
        }

        Assert.True(File.Exists(compilerDllPath), $"Expected compiler output at '{compilerDllPath}'.");
        return compilerDllPath;
    }

    private static void EnsureRavenCoreBuilt(string repoRoot, string targetFramework)
    {
        var ravenCoreDllPath = Path.Combine(repoRoot, "src", "Raven.Core", "bin", "Debug", targetFramework, "Raven.Core.dll");
        if (File.Exists(ravenCoreDllPath))
            return;

        var ravenCoreProjectPath = Path.Combine(repoRoot, "src", "Raven.Core", "Raven.Core.rvnproj");
        var buildArgs = $"build \"{ravenCoreProjectPath}\" --framework {targetFramework} /property:WarningLevel=0";
        var buildResult = RunProcess("dotnet", buildArgs, repoRoot, timeoutMilliseconds: 300_000);
        Assert.True(
            buildResult.ExitCode == 0,
            $"Failed to build Raven.Core for sample-project tests.\nstdout:\n{buildResult.StdOut}\nstderr:\n{buildResult.StdErr}");

        Assert.True(File.Exists(ravenCoreDllPath), $"Expected Raven.Core output at '{ravenCoreDllPath}'.");
    }

    private static (int ExitCode, string StdOut, string StdErr) RunCompiler(
        string repoRoot,
        string compilerDllPath,
        string projectPath,
        string outputDirectory)
    {
        var args = $"\"{compilerDllPath}\" \"{projectPath}\" -o \"{outputDirectory}\"";
        return RunProcess("dotnet", args, repoRoot, timeoutMilliseconds: 300_000);
    }

    private static (int ExitCode, string StdOut, string StdErr) RunProcess(
        string fileName,
        string arguments,
        string workingDirectory,
        int timeoutMilliseconds)
    {
        var startInfo = new ProcessStartInfo(fileName, arguments)
        {
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
            WorkingDirectory = workingDirectory
        };

        using var process = Process.Start(startInfo) ?? throw new InvalidOperationException($"Failed to start {fileName} process.");
        var stdoutBuilder = new StringBuilder();
        var stderrBuilder = new StringBuilder();
        process.OutputDataReceived += (_, e) =>
        {
            if (e.Data is not null)
                stdoutBuilder.AppendLine(e.Data);
        };
        process.ErrorDataReceived += (_, e) =>
        {
            if (e.Data is not null)
                stderrBuilder.AppendLine(e.Data);
        };
        process.BeginOutputReadLine();
        process.BeginErrorReadLine();

        if (!process.WaitForExit(timeoutMilliseconds))
        {
            try
            {
                process.Kill(entireProcessTree: true);
            }
            catch
            {
                // Ignore kill failures in teardown paths.
            }

            _ = process.WaitForExit(5_000);
            return (-1, stdoutBuilder.ToString(), $"{stderrBuilder}{Environment.NewLine}Timed out after {timeoutMilliseconds}ms.");
        }

        _ = process.WaitForExit(5_000);
        return (process.ExitCode, stdoutBuilder.ToString(), stderrBuilder.ToString());
    }

    private static string GetRepositoryRoot()
        => Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, "..", "..", "..", "..", ".."));

    private static string CreateTempDirectory()
    {
        var directory = Path.Combine(Path.GetTempPath(), "raven-msbuild-sample-project-tests", Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        return directory;
    }

    private static void DeleteDirectoryIfExists(string path)
    {
        if (!Directory.Exists(path))
            return;

        Directory.Delete(path, recursive: true);
    }
}
