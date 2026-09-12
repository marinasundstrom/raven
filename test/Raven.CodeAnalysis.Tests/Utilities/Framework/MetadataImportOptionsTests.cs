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

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void GenericVoidMetadataRetainsTypeIdentityWithoutChangingVoidReturns(bool returnFirst)
    {
        var compilation = Create(true, true);
        _ = compilation.GetDiagnostics();
        var loader = new ReflectionTypeLoader(compilation);
        var voidType = compilation.CoreAssembly.GetType("System.Void", throwOnError: true)!;
        var definition = compilation.CoreAssembly.GetType("System.Tuple`1", throwOnError: true)!;
        // MetadataLoadContext can inspect this shape even though host CLR execution
        // disallows Void generic arguments. Alternate targets can give it semantics.
        var constructed = definition.MakeGenericType(voidType);
        if (returnFirst)
            Assert.Equal(SpecialType.System_Unit, loader.ResolveType(voidType)!.SpecialType);
        var symbol = Assert.IsAssignableFrom<INamedTypeSymbol>(loader.ResolveType(constructed));
        Assert.Equal(SpecialType.System_Void, Assert.Single(symbol.TypeArguments).SpecialType);
        Assert.Equal(SpecialType.System_Unit, loader.ResolveType(voidType)!.SpecialType);
        var action = compilation.CoreAssembly.GetType("System.Action`1", throwOnError: true)!.MakeGenericType(voidType);
        var invoke = action.GetMethod("Invoke")!;
        Assert.Equal(SpecialType.System_Void, loader.ResolveType(Assert.Single(invoke.GetParameters()))!.SpecialType);
        Assert.Equal(SpecialType.System_Unit, loader.ResolveType(invoke.ReturnParameter)!.SpecialType);
        var pair = compilation.CoreAssembly.GetType("System.Collections.Generic.KeyValuePair`2", throwOnError: true)!
            .MakeGenericType(voidType, voidType);
        var output = pair.GetMethod("Deconstruct")!.GetParameters()[0];
        Assert.True(output.IsOut);
        Assert.Equal(SpecialType.System_Void, loader.ResolveType(output)!.SpecialType);
        var again = Assert.IsAssignableFrom<INamedTypeSymbol>(loader.ResolveType(constructed));
        Assert.Equal(SpecialType.System_Void, Assert.Single(again.TypeArguments).SpecialType);
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
