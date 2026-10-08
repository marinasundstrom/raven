using Mono.Cecil;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Targets;

namespace Raven.CodeAnalysis.Tests;

public sealed class NeoClrProfileTests
{
    [Fact]
    public void NativeMetadataSelectionIsExplicitAndImmutable()
    {
        var original = new MetadataImportOptions("NativeCore");
        var native = original.WithNativeMetadata();
        Assert.False(original.UseNativeMetadata);
        Assert.True(native.UseNativeMetadata);
        Assert.True(native.WithObjectAssemblyName("NativeCore").UseNativeMetadata);
        Assert.False(native.WithNativeMetadata(false).UseNativeMetadata);
        Assert.False(CompilationOptions.NeoCLR.MetadataImportOptions!.UseNativeMetadata);
    }

    [Fact]
    public void NativeMetadataRejectsDotNetTargetWithoutLoadingReferences()
    {
        var options = CompilationOptions.DotNet.WithMetadataImportOptions(new MetadataImportOptions("NativeCore").WithNativeMetadata());
        var compilation = Compilation.Create("WrongTarget", [], [], options);
        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAVT003" &&
            diagnostic.GetMessage().Contains("requires the NeoCLR target"));
    }

    [Fact]
    public void NativeMetadataRejectsPortableReferencesInsteadOfFallingBack()
    {
        var options = CompilationOptions.NeoCLR.WithRuntimeTypeOfContract(null)
            .WithMetadataImportOptions(new MetadataImportOptions("NeoCLR.CoreProbe").WithNativeMetadata());
        var compilation = Compilation.Create("WrongReference", [], TestMetadataReferences.Default, options);
        Assert.Contains(compilation.GetDiagnostics(), diagnostic => diagnostic.Id == "RAVT004" &&
            diagnostic.GetMessage().Contains("semantic references exclusively"));
    }

    [Fact]
    public void PresetSelectsExplicitCliContractsAndPreservesThemThroughCopies()
    {
        var preset = CompilationOptions.NeoCLR;
        var options = preset.WithOutputKind(OutputKind.DynamicallyLinkedLibrary)
            .WithOptimizationLevel(OptimizationLevel.Release).WithRunAnalyzers(false);
        Assert.Equal(TargetPlatform.NeoCLR, options.TargetPlatform);
        Assert.Equal(new RuntimeDisposalContract("NeoCLR.CoreProbe", "System.Disposable", UseExceptionHandling: false), options.RuntimeDisposalContract);
        Assert.Null(CompilationOptions.DotNet.RuntimeDisposalContract);
        Assert.Equal("NeoCLR.CoreProbe", options.MetadataImportOptions!.CoreAssemblyName);
        Assert.Equal("NeoCLR.CoreProbe", options.TargetCoreAssemblyName);
        Assert.Equal(new RuntimeUnitContract("NeoCLR.CoreProbe", "System.Void"), options.RuntimeUnitContract);
        Assert.Equal(new RuntimeIterationContract("NeoCLR.CoreProbe", "System.Collections.Iterable`1",
            "System.Collections.Iterator`1", ArrayShapeTypeName: "System.Array`1"), options.RuntimeIterationContract);
        Assert.Equal(new RuntimePropagationContract("NeoCLR.CoreProbe", "System.Propagatable`3"), options.RuntimePropagationContract);
        Assert.Equal(new RuntimeTypeOfContract("NeoCLR.CoreProbe", "System.Introspection.TypeInfo",
            "System.Runtime.RuntimeContext"), options.RuntimeTypeOfContract);
        Assert.Equal(FrameworkProjectionMode.None, options.FrameworkProjectionMode);
        Assert.True(options.UseGraphemeChar);
        Assert.False(options.UseUnicodeScalarChar);
        Assert.True(options.UseHeapAsyncStateMachines);
        Assert.True(options.PropagateAsyncCancellation);
        Assert.False(options.CaptureAsyncExceptions);
        Assert.False(options.AllowArrayCovariance);
        Assert.False(options.AllowNullableValueTypes);
        Assert.Equal(OutputKind.ConsoleApplication, preset.OutputKind);
        Assert.NotSame(preset, CompilationOptions.NeoCLR);

        var dotnet = CompilationOptions.DotNet;
        Assert.Equal(TargetPlatform.DotNet, dotnet.TargetPlatform);
        Assert.Null(dotnet.TargetCoreAssemblyName);
        Assert.Null(dotnet.RuntimeUnitContract);
        Assert.False(dotnet.UseGraphemeChar);
        Assert.True(dotnet.AllowArrayCovariance);
        Assert.True(dotnet.AllowNullableValueTypes);
    }

    [Fact]
    public void UnionInterfaceNamesFollowExplicitAndLegacyNeoClrContracts()
    {
        Assert.Equal("System.Runtime.CompilerServices.UnionValue",
            new NeoClrCliRuntimeContract(CompilationOptions.NeoCLR).UnionInterfaceTypeName);
        Assert.Equal("System.Runtime.CompilerServices.UnionValue",
            new DotNetRuntimeContract(CompilationOptions.DotNet.WithTargetCoreAssemblyName("NeoCLR.CoreProbe")).UnionInterfaceTypeName);
        Assert.Equal("System.Runtime.CompilerServices.IUnion",
            new DotNetRuntimeContract(CompilationOptions.DotNet).UnionInterfaceTypeName);
    }

    [Fact]
    public void AsyncInterfaceNamesFollowTheSelectedTargetContract()
    {
        var special = SpecialType.System_Runtime_CompilerServices_IAsyncStateMachine;
        Assert.Equal("System.Runtime.CompilerServices.AsyncStateMachine",
            new NeoClrCliRuntimeContract(CompilationOptions.NeoCLR).GetSpecialTypeMetadataName(special));
        Assert.Equal("System.Runtime.CompilerServices.IAsyncStateMachine",
            new DotNetRuntimeContract(CompilationOptions.DotNet).GetSpecialTypeMetadataName(special));
        var dotnet = Compilation.Create("DotNetControl", [], TestMetadataReferences.Default, CompilationOptions.DotNet);
        Assert.Equal("IAsyncStateMachine", dotnet.GetSpecialType(special).Name);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void NeoClrCliImportRecognizesTheUnprefixedStateMachineInterface(bool legacyCoreSelection)
    {
        var corePath = TestMetadataReferences.Default.OfType<PortableExecutableReference>()
            .Single(reference => Path.GetFileName(reference.FilePath) == "System.Runtime.dll").FilePath!;
        using var core = AssemblyDefinition.ReadAssembly(corePath);
        core.Name.Name = "NeoCLR.CoreProbe";
        core.Name.PublicKey = [];
        core.MainModule.GetType("System.Runtime.CompilerServices.IAsyncStateMachine").Name = "AsyncStateMachine";
        using var image = new MemoryStream();
        core.Write(image);
        var source = SyntaxTree.ParseText("""
            public static class Consumer {
                public static func Pass(state: System.Runtime.CompilerServices.AsyncStateMachine) -> System.Runtime.CompilerServices.AsyncStateMachine => state
            }
            """);
        var compilation = Compilation.Create("NeoClrAsyncNames", [source],
            [.. TestMetadataReferences.Default, MetadataReference.CreateFromImage(image.ToArray())],
            CompilationOptions.NeoCLR.WithOutputKind(OutputKind.DynamicallyLinkedLibrary).WithRuntimeTypeOfContract(null)
                .WithTargetPlatform(legacyCoreSelection ? TargetPlatform.DotNet : TargetPlatform.NeoCLR));
        var machine = compilation.GetSpecialType(SpecialType.System_Runtime_CompilerServices_IAsyncStateMachine);
        Assert.Equal(TypeKind.Interface, machine.TypeKind);
        Assert.Equal("AsyncStateMachine", machine.Name);
        Assert.Equal("NeoCLR.CoreProbe", machine.ContainingAssembly.Name);
        Assert.Equal(SpecialType.System_Runtime_CompilerServices_IAsyncStateMachine, machine.SpecialType);
        using var emitted = new MemoryStream();
        var result = compilation.Emit(emitted);
        Assert.True(result.Success, string.Join("\n", result.Diagnostics));
        emitted.Position = 0;
        using var consumer = AssemblyDefinition.ReadAssembly(emitted);
        var pass = consumer.MainModule.GetType("Consumer").Methods.Single(m => m.Name == "Pass");
        Assert.Equal("System.Runtime.CompilerServices.AsyncStateMachine", pass.ReturnType.FullName);
        Assert.Equal("NeoCLR.CoreProbe", pass.ReturnType.Scope.Name);
    }

    public static IEnumerable<object[]> InvalidConfigurations()
    {
        yield return [CompilationOptions.DotNet.WithTargetPlatform(TargetPlatform.NeoCLR)];
        yield return [CompilationOptions.NeoCLR.WithMetadataImportOptions(null)];
        yield return [CompilationOptions.NeoCLR.WithMetadataImportOptions(new MetadataImportOptions("System.Runtime"))];
        yield return [CompilationOptions.NeoCLR.WithTargetCoreAssemblyName("System.Runtime")];
        yield return [CompilationOptions.NeoCLR.WithRuntimeUnitContract(null)];
        yield return [CompilationOptions.NeoCLR.WithRuntimeUnitContract(new("NeoCLR.CoreProbe", "System.ValueTuple"))];
    }

    [Theory]
    [MemberData(nameof(InvalidConfigurations))]
    public void InconsistentProfileRejectsBeforeLoadingOrWriting(CompilationOptions options)
    {
        var compilation = Compilation.Create("InvalidProfile", [SyntaxTree.ParseText("class Example {}")], [], options);
        var diagnostic = Assert.Single(compilation.GetDiagnostics());
        Assert.Equal("RAVT003", diagnostic.Id);
        Assert.Contains("neoCLR CLI profile", diagnostic.GetMessage());
        using var output = new MemoryStream();
        output.Write([1, 2, 3]);
        Assert.False(compilation.Emit(output).Success);
        Assert.Equal(new byte[] { 1, 2, 3 }, output.ToArray());
    }

    [Fact]
    public void ConsistentPresetStillRequiresSuppliedReferences()
    {
        var compilation = Compilation.Create("MissingReferences", [], [], CompilationOptions.NeoCLR);
        Assert.Equal("RAVT004", Assert.Single(compilation.GetDiagnostics()).Id);
        using var output = new MemoryStream();
        Assert.False(compilation.Emit(output).Success);
        Assert.Equal(0, output.Length);
    }
}
