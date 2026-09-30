using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class NeoClrFunctionTypeTests : CompilationTestBase
{
    [Theory]
    [InlineData(null)]
    [InlineData("System.Runtime")]
    [InlineData("OrdinaryLibrary")]
    [InlineData("neoclr.coreprobe")]
    [InlineData("NeoCLR.CoreProbe")]
    public void UnitFunctionsKeepNominalDelegateTransport(string? targetCore)
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
            Assert.Equal("Action", function.Name);
            Assert.Single(function.TypeArguments);

        }
        var valueFunction = Assert.IsAssignableFrom<INamedTypeSymbol>(
            compilation.CreateFunctionTypeSymbol([], parameter));
        Assert.Equal("Func", valueFunction.Name);
        Assert.True(SymbolEqualityComparer.Default.Equals(parameter, valueFunction.TypeArguments.Single()));
    }
}
