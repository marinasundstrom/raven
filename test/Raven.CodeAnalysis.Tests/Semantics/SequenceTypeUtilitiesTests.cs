using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

public sealed class SequenceTypeUtilitiesTests
{
    [Theory]
    [Trait("Target", "Shared")]
    [InlineData("IEnumerable")]
    [InlineData("IAsyncEnumerable")]
    public void UnrelatedSameNamedTypesAreNotSequences(string name)
    {
        var compilation = Compilation.Create("SequenceIdentity", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default)
            .AddSyntaxTrees(SyntaxTree.ParseText($"class {name}<T> {{}}"));
        var definition = compilation.GetTypeByMetadataName(name + "`1")!;
        var constructed = definition.Construct(compilation.GetSpecialType(SpecialType.System_Int32));
        Assert.Null(SequenceTypeUtilities.TryGetElementType(compilation, constructed));
    }
    [Theory]
    [Trait("Target", "DotNet")]
    [InlineData("System.Collections.Generic.IEnumerable`1")]
    [InlineData("System.Collections.Generic.IAsyncEnumerable`1")]
    [InlineData("System.Collections.Generic.List`1")]
    public void DotNetCollectionsResolveCanonicalElements(string name)
    {
        var compilation = Compilation.Create("SequenceIdentity", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddReferences(TestMetadataReferences.Default);
        var element = compilation.GetSpecialType(SpecialType.System_Int32);
        var constructed = compilation.GetTypeByMetadataName(name)!.Construct(element);
        Assert.Same(element, SequenceTypeUtilities.TryGetElementType(compilation, constructed));
    }

    [Fact]
    [Trait("Target", "DotNet")]
    public void DotNetConfiguredIterationUsesSelectedInterfaceIdentity() => VerifyConfigured(TargetPlatform.DotNet);

    [Fact]
    [Trait("Target", "NeoCLR")]
    public void NeoClrConfiguredIterationUsesSelectedInterfaceIdentity() => VerifyConfigured(TargetPlatform.NeoCLR);

    [Fact]
    [Trait("Target", "DotNet")]
    public void ConfiguredIterationRejectsClassShapedProtocolAndMissingAsyncProtocol()
    {
        var contract = new RuntimeIterationContract("SequenceIdentity", "Sequence`1", "Cursor`1");
        var compilation = Compilation.Create("SequenceIdentity", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                .WithRuntimeIterationContract(contract))
            .AddReferences(TestMetadataReferences.Default)
            .AddSyntaxTrees(SyntaxTree.ParseText("class Sequence<T> {}"));
        var services = (SequenceTypeServices)compilation;
        Assert.Null(services.GetSequenceDefinition(asynchronous: false));
        Assert.Null(services.GetSequenceDefinition(asynchronous: true));
    }

    private static void VerifyConfigured(TargetPlatform target)
    {
        var contract = new RuntimeIterationContract("SequenceIdentity", "Sequence`1", "Cursor`1",
            Async: new RuntimeAsyncIterationContract("AsyncSequence`1", "AsyncCursor`1"));
        var compilation = Compilation.Create("SequenceIdentity", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
                .WithTargetPlatform(target).WithRuntimeIterationContract(contract))
            .AddReferences(TestMetadataReferences.Default)
            .AddSyntaxTrees(SyntaxTree.ParseText("interface Sequence<T> {}\ninterface AsyncSequence<T> {}"));
        var element = compilation.GetSpecialType(SpecialType.System_Int32);
        foreach (var name in new[] { "Sequence`1", "AsyncSequence`1" })
        {
            var constructed = compilation.GetTypeByMetadataName(name)!.Construct(element);
            Assert.Same(element, SequenceTypeUtilities.TryGetElementType(compilation, constructed));
        }
        var wrongAssembly = Compilation.Create("SequenceIdentity", compilation.Options.WithRuntimeIterationContract(contract with { AssemblyName = "Other" }))
            .AddReferences(TestMetadataReferences.Default)
            .AddSyntaxTrees(SyntaxTree.ParseText("interface Sequence<T> {}"));
        var mismatched = wrongAssembly.GetTypeByMetadataName("Sequence`1")!.Construct(element);
        Assert.Null(SequenceTypeUtilities.TryGetElementType(wrongAssembly, mismatched));
    }

}
