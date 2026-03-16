using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class FixedSizeArrayMetadataTests : CompilationTestBase
{
    [Fact]
    public void FixedSizeArrays_RoundTripThroughMetadata()
    {
        const string source = """
class Buffer {
    public val Data: int[2] = [1, 2]

    public func Echo(values: int[3]) -> int[3] {
        return values
    }
}
""";

        var reference = TestMetadataFactory.CreateFileReferenceFromSource(source, "FixedSizeArrayMetadata");
        var compilation = Compilation.Create(
            "consumer",
            [],
            [.. TestMetadataReferences.Default, reference],
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var buffer = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetTypeByMetadataName("Buffer"));

        var data = Assert.IsAssignableFrom<IPropertySymbol>(buffer.GetMembers("Data").Single());
        var dataType = Assert.IsAssignableFrom<IArrayTypeSymbol>(data.Type);
        Assert.True(dataType.IsFixedArray);
        Assert.Equal(2, dataType.FixedSize);

        var echo = Assert.IsAssignableFrom<IMethodSymbol>(buffer.GetMembers("Echo").Single());
        var parameterType = Assert.IsAssignableFrom<IArrayTypeSymbol>(Assert.Single(echo.Parameters).Type);
        var returnType = Assert.IsAssignableFrom<IArrayTypeSymbol>(echo.ReturnType);

        Assert.True(parameterType.IsFixedArray);
        Assert.Equal(3, parameterType.FixedSize);
        Assert.True(returnType.IsFixedArray);
        Assert.Equal(3, returnType.FixedSize);
    }
}
