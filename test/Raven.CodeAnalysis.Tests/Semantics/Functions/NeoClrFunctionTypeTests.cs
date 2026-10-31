using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class NeoClrFunctionTypeTests : CompilationTestBase
{
    [Theory]
    [InlineData(null)]
    [InlineData("NeoCLR.CoreProbe")]
    public void ImportedNoResultFunctionsPreserveTheirContract(string? targetCore)
    {
        var compilation = CreateCompilation(new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
            .WithTargetCoreAssemblyName(targetCore));
        Assert.NotNull(compilation.GetTypeByMetadataName("System.Object"));
        var parameter = compilation.GetSpecialType(SpecialType.System_Int32);
        var noResult = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.CreateNoResultFunctionTypeSymbol([parameter]));
        Assert.Equal("Action", noResult.Name);
        Assert.Single(noResult.TypeArguments);
        Assert.True(SymbolEqualityComparer.Default.Equals(parameter, noResult.TypeArguments[0]));
        var sourceUnit = compilation.CreateFunctionTypeSymbol([parameter], compilation.GetSpecialType(SpecialType.System_Unit));
        Assert.Equal(targetCore is null, SymbolEqualityComparer.Default.Equals(noResult, sourceUnit));
    }

    [Theory]
    [InlineData(null, false)]
    [InlineData("System.Runtime", false)]
    [InlineData("OrdinaryLibrary", false)]
    [InlineData("neoclr.coreprobe", false)]
    [InlineData("NeoCLR.CoreProbe", true)]
    public void UnitFunctionsUseTargetTransport(string? targetCore, bool inhabitedResult)
    {
        var compilation = CreateCompilation(new CompilationOptions(OutputKind.DynamicallyLinkedLibrary)
            .WithTargetCoreAssemblyName(targetCore));
        Assert.NotNull(compilation.GetTypeByMetadataName("System.Object"));
        var unit = compilation.GetSpecialType(SpecialType.System_Unit);
        var parameter = compilation.GetSpecialType(SpecialType.System_Int32);
        foreach (var result in new[] { unit, compilation.GetSpecialType(SpecialType.System_Void) })
        {
            var function = Assert.IsAssignableFrom<INamedTypeSymbol>(
                compilation.CreateFunctionTypeSymbol([parameter], result));
            Assert.Equal(inhabitedResult ? "Func" : "Action", function.Name);
            Assert.Equal(inhabitedResult ? 2 : 1, function.TypeArguments.Length);
            if (inhabitedResult)
            {
                var generic = compilation.GetTypeByMetadataName("System.Func`2")!.Construct(parameter, unit);
                Assert.True(SymbolEqualityComparer.Default.Equals(generic, function));
            }
        }
        var valueFunction = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.CreateFunctionTypeSymbol([], parameter));
        Assert.Equal("Func", valueFunction.Name);
        Assert.True(SymbolEqualityComparer.Default.Equals(parameter, valueFunction.TypeArguments.Single()));
    }
}
