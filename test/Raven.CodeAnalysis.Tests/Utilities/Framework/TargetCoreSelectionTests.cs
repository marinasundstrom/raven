using System.Reflection;

using Mono.Cecil;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class TargetCoreSelectionTests
{
    private static Compilation Create(CompilationOptions options)
    {
        var references = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
        return Compilation.Create("SelectedCore", [SyntaxTree.ParseText("""
            class Example {
                static func Echo(value: string) -> string { return value }
            }
            """)], references.Select(MetadataReference.CreateFromFile).ToArray(), options);
    }

    private static CompilationOptions Options => new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
        .WithMetadataImportOptions(new MetadataImportOptions("System.Runtime"))
        .WithTargetCoreAssemblyName("System.Runtime");

    [Fact]
    public async Task CompilerDriverKeepsProjectTargetReferencesIsolated()
    {
        var repository = new DirectoryInfo(AppContext.BaseDirectory);
        while (repository is not null && !File.Exists(Path.Combine(repository.FullName, "Raven.sln")))
            repository = repository.Parent;
        Assert.NotNull(repository);
        var configuration = new DirectoryInfo(AppContext.BaseDirectory).Parent!.Name;
        var compiler = Path.Combine(repository!.FullName, "src/Raven.Compiler/bin", configuration, "net11.0/rvnc.dll");
        Assert.True(File.Exists(compiler), "Build the repository compiler before running driver tests.");
        var directory = Path.Combine(Path.GetTempPath(), "raven-target-driver", Guid.NewGuid().ToString("N"));
        Directory.CreateDirectory(directory);
        try
        {
            var core = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"))
                .Single(path => Path.GetFileName(path) == "System.Runtime.dll");
            var project = Path.Combine(directory, "Probe.rvnproj");
            File.WriteAllText(project, $$"""
                <Project Sdk="Microsoft.NET.Sdk">
                  <PropertyGroup>
                    <TargetFramework>net11.0</TargetFramework>
                    <OutputType>Library</OutputType>
                    <RavenMetadataCoreAssemblyName>System.Runtime</RavenMetadataCoreAssemblyName>
                    <RavenTargetCoreAssemblyName>System.Runtime</RavenTargetCoreAssemblyName>
                    <ImplicitImports>disable</ImplicitImports>
                    <RavenFrameworkProjections>None</RavenFrameworkProjections>
                    <EnableDefaultCompileItems>false</EnableDefaultCompileItems>
                  </PropertyGroup>
                  <ItemGroup>
                    <Compile Include="Main.rvn" />
                    <Reference Include="System.Runtime"><HintPath>{{System.Security.SecurityElement.Escape(core)}}</HintPath></Reference>
                  </ItemGroup>
                </Project>
                """);
            async Task<(int ExitCode, string Output)> Compile(string outputDirectory)
            {
                var info = new System.Diagnostics.ProcessStartInfo("dotnet")
                {
                    RedirectStandardOutput = true,
                    RedirectStandardError = true,
                    UseShellExecute = false,
                    WorkingDirectory = directory
                };
                foreach (var argument in new[] { compiler, project, "--framework", "net11.0", "--no-project-restore", "-o", outputDirectory })
                    info.ArgumentList.Add(argument);
                using var process = System.Diagnostics.Process.Start(info)!;
                var stdout = process.StandardOutput.ReadToEndAsync();
                var stderr = process.StandardError.ReadToEndAsync();
                using var timeout = new CancellationTokenSource(TimeSpan.FromSeconds(60));
                try { await process.WaitForExitAsync(timeout.Token); }
                catch (OperationCanceledException) { process.Kill(entireProcessTree: true); throw; }
                return (process.ExitCode, await stdout + await stderr);
            }
            File.WriteAllText(Path.Combine(directory, "Main.rvn"), "class Example { static func Echo(value: string) -> string { return value } }");
            var validDirectory = Path.Combine(directory, "valid");
            var valid = await Compile(validDirectory);
            Assert.True(valid.ExitCode == 0, valid.Output);
            using (var image = AssemblyDefinition.ReadAssembly(Path.Combine(validDirectory, "Probe.dll")))
                Assert.All(image.MainModule.AssemblyReferences, reference => Assert.Equal("System.Runtime", reference.Name));

            File.WriteAllText(Path.Combine(directory, "Main.rvn"), "class Example { static func Print() { System.Console.WriteLine(42) } }");
            var invalidDirectory = Path.Combine(directory, "invalid");
            var invalid = await Compile(invalidDirectory);
            Assert.NotEqual(0, invalid.ExitCode);
            Assert.False(File.Exists(Path.Combine(invalidDirectory, "Probe.dll")), invalid.Output);
        }
        finally { Directory.Delete(directory, true); }
    }

    [Fact]
    public void NormalEmitUsesTheSelectedTargetCoreAndOptionCopiesPreserveIt()
    {
        var options = Options.WithRunAnalyzers(false).WithAllowArrayCovariance(false)
            .WithOutputKind(OutputKind.DynamicallyLinkedLibrary).WithOptimizationLevel(OptimizationLevel.Release);
        Assert.Equal("System.Runtime", options.TargetCoreAssemblyName);
        var compilation = Create(options);
        using var output = new MemoryStream();
        var result = compilation.Emit(output);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        output.Position = 0;
        using var image = AssemblyDefinition.ReadAssembly(output);
        var method = image.MainModule.GetType("Example").Methods.Single(m => m.Name == "Echo");
        Assert.Equal("System.Runtime", method.ReturnType.Scope.Name);
        Assert.Equal("System.Runtime", method.Parameters[0].ParameterType.Scope.Name);
        Assert.DoesNotContain(image.MainModule.AssemblyReferences, r => r.Name == "System.Private.CoreLib");
    }

    [Theory]
    [InlineData(null)]
    [InlineData("Other.Core")]
    public void InconsistentMetadataAndEmissionSelectionIsDiagnosedBeforeWriting(string? metadataCore)
    {
        var compilation = Create(Options.WithMetadataImportOptions(metadataCore is null ? null : new MetadataImportOptions("System.Runtime"))
            .WithTargetCoreAssemblyName(metadataCore == "Other.Core" ? "Other.Core" : "System.Runtime"));
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == "RAVT003");
        using var output = new MemoryStream();
        Assert.False(compilation.Emit(output).Success);
        Assert.Equal(0, output.Length);
    }

    [Fact]
    public void ExplicitEmitOptionsCannotSilentlyOverrideTheProjectCore()
    {
        var compilation = Create(Options);
        using var output = new MemoryStream();
        var result = compilation.Emit(output, null, new EmitOptions(new AssemblyName("Other.Core")));
        Assert.False(result.Success);
        Assert.Contains(result.Diagnostics, d => d.Id == "RAVT003");
        Assert.Equal(0, output.Length);
    }

    [Fact]
    public void ClearingTheSelectionPreservesIndependentMetadataImportPolicy()
    {
        var options = Options.WithTargetCoreAssemblyName(null);
        Assert.Null(options.TargetCoreAssemblyName);
        Assert.Equal("System.Runtime", options.MetadataImportOptions!.CoreAssemblyName);
        using var output = new MemoryStream();
        Assert.True(Create(options).Emit(output).Success);
    }
}
