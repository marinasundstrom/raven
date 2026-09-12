using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class MetadataImportOptionsTests
{
    private const string Source = """
        import System.Console.*
        func Main() { WriteLine("test") }
        """;

    private static Compilation Create(bool isolated, bool console)
    {
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
        return Compilation.Create("ImportTest", [SyntaxTree.ParseText(Source)],
            paths.Where(p => console || Path.GetFileName(p) != "System.Console.dll")
                .Select(MetadataReference.CreateFromFile).ToArray(),
            new CompilationOptions(OutputKind.ConsoleApplication,
                metadataImportOptions: isolated ? new MetadataImportOptions("System.Runtime") : null));
    }

    [Fact]
    public void ExplicitReferencesBindAndEmit()
    {
        var compilation = Create(true, true);
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        Assert.Equal("System.Runtime", compilation.CoreAssembly.GetName().Name);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
    }

    [Fact]
    public void MissingConsoleDoesNotFallBackAfterHostCompilation()
    {
        Assert.Empty(Create(false, false).GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        var isolated = Create(true, false);
        Assert.Contains(isolated.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        Assert.Null(isolated.GetTypeByMetadataName("System.Console"));
        Assert.NotNull(Create(false, false).GetTypeByMetadataName("System.Console"));
    }

    [Fact]
    public void HostMetadataContextIsNotReusedAcrossImportModes()
    {
        var host = Create(false, false);
        Assert.NotNull(host.GetTypeByMetadataName("System.Console"));
        var isolated = Create(true, false);
        isolated.AdoptIncrementalReuseFrom(host);
        Assert.Null(isolated.GetTypeByMetadataName("System.Console"));
        Assert.Equal("System.Runtime", isolated.CoreAssembly.GetName().Name);
    }

    [Fact]
    public void MissingCoreDoesNotUseHostCore()
    {
        var compilation = Compilation.Create("NoCore", [SyntaxTree.ParseText("func Main() {}")], [],
            new CompilationOptions(metadataImportOptions: new MetadataImportOptions("System.Private.CoreLib"),
                outputKind: OutputKind.ConsoleApplication));
        Assert.Throws<FileNotFoundException>(() => compilation.GetDiagnostics());
    }

    [Fact]
    public void OptionCopiesPreserveImportPolicy()
    {
        var import = new MetadataImportOptions("System.Runtime");
        var options = new CompilationOptions().WithMetadataImportOptions(import)
            .WithOutputKind(OutputKind.DynamicallyLinkedLibrary).WithOptimizationLevel(OptimizationLevel.Release)
            .WithRunAnalyzers(false).WithAllowUnsafe(true);
        Assert.Equal(import, options.MetadataImportOptions);
        Assert.Null(options.WithMetadataImportOptions(null).MetadataImportOptions);
    }

    [Theory]
    [InlineData("")]
    [InlineData(" ")]
    [InlineData(null)]
    public void CoreIdentityIsRequired(string? name)
    {
        Assert.ThrowsAny<ArgumentException>(() => new MetadataImportOptions(name!));
    }
}
