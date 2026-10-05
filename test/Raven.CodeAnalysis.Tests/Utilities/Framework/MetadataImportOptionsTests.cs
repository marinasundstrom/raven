using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public class MetadataImportOptionsTests
{
    [Fact]
    public void NativeAsyncProviderSelectionIsImmutableAndValidatesIdentity()
    {
        var baseline = new MetadataImportOptions("Core");
        var selected = baseline.WithAsyncAssemblyName("Tasks");
        Assert.Null(baseline.AsyncAssemblyName);
        Assert.Equal("Tasks", selected.AsyncAssemblyName);
        Assert.Null(selected.WithAsyncAssemblyName(null).AsyncAssemblyName);
        Assert.Equal("Core", selected.CoreAssemblyName);
        Assert.Throws<ArgumentException>(() => baseline.WithAsyncAssemblyName(" "));
        Assert.Throws<ArgumentException>(() => baseline.WithAsyncAssemblyName(""));
    }

    [Fact]
    public void PrimitiveProvidersAreCopiedAndRestrictedToSupportedDeclarations()
    {
        var providers = new Dictionary<SpecialType, string> { [SpecialType.System_Single] = "Numbers", [SpecialType.System_String] = "Text" };
        var options = new MetadataImportOptions("Core", providers);
        providers[SpecialType.System_Single] = "Changed";
        Assert.Equal("Numbers", options.PrimitiveAssemblies[SpecialType.System_Single]);
        Assert.Equal("Text", options.PrimitiveAssemblies[SpecialType.System_String]);
        Assert.Empty(new MetadataImportOptions("Core").PrimitiveAssemblies);
        Assert.Throws<ArgumentException>(() => new MetadataImportOptions("Core",
            new Dictionary<SpecialType, string> { [SpecialType.System_Object] = "Numbers" }));
        Assert.Throws<ArgumentException>(() => new MetadataImportOptions("Core",
            new Dictionary<SpecialType, string> { [SpecialType.System_Double] = " " }));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void SourceHandleOwnershipIsExplicitAndCanonicalizesNestedSignatures(bool selected)
    {
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net10.0"));
        using var core = Mono.Cecil.AssemblyDefinition.ReadAssembly(paths.Single(p => Path.GetFileName(p) == "System.Runtime.dll"));
        core.Name.Name = "NeoCLR.CoreProbe";
        core.Name.PublicKey = [];
        using var coreImage = new MemoryStream();
        core.Write(coreImage);
        var references = paths.Select(MetadataReference.CreateFromFile).Append(MetadataReference.CreateFromImage(coreImage.ToArray())).ToArray();
        var compilation = Compilation.Create("Handles", [SyntaxTree.ParseText("""
            namespace System
            public struct RuntimeTypeHandle { }
            public class HandleConsumer {
                static func Echo(value: RuntimeTypeHandle) -> System.RuntimeTypeHandle => value
                static func EchoArray(value: RuntimeTypeHandle[]) -> System.RuntimeTypeHandle[] => value
            }
            """)], references,
            CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary).WithRuntimeTypeOfContract(null)
                .WithMetadataImportOptions(new MetadataImportOptions("NeoCLR.CoreProbe", null,
                    selected ? [SpecialType.System_RuntimeTypeHandle] : [])));
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        var consumer = compilation.Assembly.GetTypeByMetadataName("System.HandleConsumer")!;
        var echo = Assert.Single(consumer.GetMembers("Echo").OfType<IMethodSymbol>());
        var element = Assert.IsAssignableFrom<IArrayTypeSymbol>(Assert.Single(consumer.GetMembers("EchoArray").OfType<IMethodSymbol>()).ReturnType).ElementType;
        var expected = selected ? compilation.GetSpecialType(SpecialType.System_RuntimeTypeHandle)
            : compilation.Assembly.GetTypeByMetadataName("System.RuntimeTypeHandle");
        Assert.True(SymbolEqualityComparer.Default.Equals(expected, echo.ReturnType));
        Assert.True(SymbolEqualityComparer.Default.Equals(expected, echo.Parameters[0].Type));
        Assert.True(SymbolEqualityComparer.Default.Equals(expected, element));
    }

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
    public void ExplicitCharProviderIsCopiedWithoutChangingDefaultCoreSelection()
    {
        var providers = new Dictionary<SpecialType, string> { [SpecialType.System_Char] = "TextLibrary" };
        var options = new MetadataImportOptions("System.Runtime", providers);
        providers[SpecialType.System_Char] = "Changed";
        Assert.Equal("TextLibrary", options.PrimitiveAssemblies[SpecialType.System_Char]);
        Assert.Empty(new MetadataImportOptions("System.Runtime").PrimitiveAssemblies);
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

    [Theory]
    [InlineData("net10.0")]
    [InlineData("net11.0")]
    public void DefaultImportSelectsSuppliedCoreBeforeHostFallback(string framework)
    {
        // Seed host-assisted discovery first, including another framework's paths.
        Assert.NotNull(Create(false, false).GetTypeByMetadataName("System.Console"));
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion(framework));
        var corePath = paths.Single(path => Path.GetFileName(path) == "System.Runtime.dll");
        var expectedIdentity = System.Reflection.AssemblyName.GetAssemblyName(corePath).FullName;
        var compilation = Compilation.Create("CoreSelection", [],
            paths.Select(MetadataReference.CreateFromFile).ToArray(),
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var objectType = compilation.GetSpecialType(SpecialType.System_Object);

        Assert.Equal(expectedIdentity, compilation.CoreAssembly.GetName().FullName);
        Assert.Equal("System.Runtime", objectType.ContainingAssembly?.Name);
        Assert.Equal(SpecialType.System_Object, objectType.SpecialType);
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
    public void HostImportsRecoverAfterIsolatedCompilation()
    {
        var isolated = Create(true, false);
        Assert.Null(isolated.GetTypeByMetadataName("System.Console"));

        var host = Create(false, false);
        host.AdoptIncrementalReuseFrom(isolated);

        Assert.NotNull(host.GetTypeByMetadataName("System.Console"));
        Assert.Empty(host.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        Assert.Null(isolated.GetTypeByMetadataName("System.Console"));
    }

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void ExplicitReferenceChangesMatchColdCompilation(bool includeConsole)
    {
        var previous = Create(true, !includeConsole);
        Assert.Equal(!includeConsole, previous.GetTypeByMetadataName("System.Console") is not null);

        var current = Create(true, includeConsole);
        current.AdoptIncrementalReuseFrom(previous);
        var cold = Create(true, includeConsole);

        Assert.Equal(includeConsole, current.GetTypeByMetadataName("System.Console") is not null);
        Assert.Equal(cold.GetDiagnostics().Select(d => d.ToString()),
            current.GetDiagnostics().Select(d => d.ToString()));
        Assert.Equal(!includeConsole, previous.GetTypeByMetadataName("System.Console") is not null);
    }

    [Fact]
    public void MissingCoreDoesNotUseHostCore()
    {
        var compilation = Compilation.Create("NoCore", [SyntaxTree.ParseText("func Main() {}")], [],
            new CompilationOptions(metadataImportOptions: new MetadataImportOptions("System.Private.CoreLib"),
                outputKind: OutputKind.ConsoleApplication));
        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal("RAVT004", diagnostic.Id);
        Assert.Contains("System.Private.CoreLib", diagnostic.GetMessage());
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
