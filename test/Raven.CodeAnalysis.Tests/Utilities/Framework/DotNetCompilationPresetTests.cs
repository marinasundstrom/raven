using System.Reflection;

using Mono.Cecil;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class DotNetCompilationPresetTests
{
    private static MetadataReference[] References(string framework, bool includeConsole = true)
        => TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion(framework))
            .Where(path => includeConsole || Path.GetFileName(path) != "System.Console.dll")
            .Select(MetadataReference.CreateFromFile).ToArray();

    private static Compilation Create(CompilationOptions options, MetadataReference[] references)
        => Compilation.Create("Preset", [SyntaxTree.ParseText("""
            class Example {
                public static func Echo(value: string) -> string { return value }
            }
            """)], references, options);

    [Theory]
    [InlineData("net10.0")]
    [InlineData("net11.0")]
    public void PresetDiscoversSuppliedCoreForBindingAndEmission(string framework)
    {
        var references = References(framework);
        var options = CompilationOptions.DotNet
            .WithOutputKind(OutputKind.DynamicallyLinkedLibrary)
            .WithOptimizationLevel(OptimizationLevel.Release)
            .WithRunAnalyzers(false);
        var compilation = Create(options, references);
        var corePath = references.OfType<PortableExecutableReference>()
            .Single(reference => Path.GetFileName(reference.FilePath) == "System.Runtime.dll").FilePath;
        var expectedCore = AssemblyName.GetAssemblyName(corePath);
        Assert.NotNull(options.MetadataImportOptions);
        Assert.Null(options.MetadataImportOptions.CoreAssemblyName);
        Assert.Null(options.TargetCoreAssemblyName);
        Assert.Equal("System.Runtime", compilation.GetSpecialType(SpecialType.System_Object).ContainingAssembly.Name);
        Assert.Equal(expectedCore.FullName, compilation.CoreAssembly.FullName);

        using var output = new MemoryStream();
        var result = compilation.Emit(output);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        output.Position = 0;
        using var image = AssemblyDefinition.ReadAssembly(output);
        var method = image.MainModule.GetType("Example").Methods.Single(method => method.Name == "Echo");
        Assert.Equal(expectedCore.FullName, ((AssemblyNameReference)method.ReturnType.Scope).FullName);
        Assert.DoesNotContain(image.MainModule.AssemblyReferences, reference => reference.Name == "System.Private.CoreLib");
    }

    [Fact]
    public void PresetDoesNotImportHostTypesOrReuseAHostAssistedSession()
    {
        var references = References("net11.0", includeConsole: false);
        var host = Create(new CompilationOptions(OutputKind.DynamicallyLinkedLibrary), references);
        Assert.NotNull(host.GetTypeByMetadataName("System.Console"));
        var isolated = Create(CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary), references);
        isolated.AdoptIncrementalReuseFrom(host);
        Assert.Null(isolated.GetTypeByMetadataName("System.Console"));
        Assert.Equal("System.Runtime", isolated.GetSpecialType(SpecialType.System_Object).ContainingAssembly.Name);
        Assert.NotNull(host.GetTypeByMetadataName("System.Console"));
    }

    [Fact]
    public void PresetDoesNotAddReferencesWhenNoneAreSupplied()
    {
        var compilation = Create(CompilationOptions.DotNet, []);
        Assert.Empty(compilation.References);
        // Setup exceptions are an existing limitation; never substitute host definitions.
        Assert.Throws<FileNotFoundException>(() => compilation.GetDiagnostics());
    }

    [Fact]
    public void ExplicitEmitCoreCannotOverrideDiscoveredReferenceCore()
    {
        var compilation = Create(CompilationOptions.DotNet.WithOutputKind(OutputKind.DynamicallyLinkedLibrary), References("net11.0"));
        using var output = new MemoryStream();
        var result = compilation.Emit(output, null, new EmitOptions(new AssemblyName("Other.Core")));
        Assert.False(result.Success);
        Assert.Contains(result.Diagnostics, diagnostic => diagnostic.Id == "RAVT003");
        Assert.Equal(0, output.Length);
    }

    [Theory]
    [InlineData("System.Runtime", true)]
    [InlineData("Other.Core", false)]
    public void ExplicitTargetCoreIsValidatedAgainstDiscoveredCore(string targetCore, bool valid)
    {
        var compilation = Create(CompilationOptions.DotNet
            .WithOutputKind(OutputKind.DynamicallyLinkedLibrary)
            .WithTargetCoreAssemblyName(targetCore), References("net11.0"));
        using var output = new MemoryStream();
        var result = compilation.Emit(output);
        Assert.Equal(valid, result.Success);
        if (!valid)
        {
            Assert.Contains(result.Diagnostics, diagnostic => diagnostic.Id == "RAVT003");
            Assert.Equal(0, output.Length);
        }
    }

    [Fact]
    public void PresetCopiesDoNotChangeSubsequentDefaults()
    {
        var modified = CompilationOptions.DotNet.WithMetadataImportOptions(new MetadataImportOptions("Custom.Core"))
            .WithOutputKind(OutputKind.DynamicallyLinkedLibrary);
        var defaults = CompilationOptions.DotNet;
        Assert.Equal("Custom.Core", modified.MetadataImportOptions!.CoreAssemblyName);
        Assert.Equal(OutputKind.ConsoleApplication, defaults.OutputKind);
        Assert.NotNull(defaults.MetadataImportOptions);
        Assert.Null(defaults.MetadataImportOptions.CoreAssemblyName);
        Assert.Null(new CompilationOptions().MetadataImportOptions);
    }
}
