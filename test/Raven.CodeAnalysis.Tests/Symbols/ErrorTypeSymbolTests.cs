namespace Raven.CodeAnalysis.Tests.Symbols;

public sealed class ErrorTypeSymbolTests
{
    [Fact]
    public void Construct_IsTotalAndPreservesTheRecoverySymbol()
    {
        var compilation = Compilation.Create("error-type-construction");
        var errorType = Assert.IsAssignableFrom<IErrorTypeSymbol>(compilation.ErrorTypeSymbol);
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);

        Assert.Same(errorType, errorType.Construct());
        Assert.Same(errorType, errorType.Construct(intType));
        Assert.Same(errorType, errorType.OriginalDefinition);
        Assert.Same(errorType, errorType.ConstructedFrom);
        Assert.True(errorType.IsType);
        Assert.True(errorType.IsClosed);
        Assert.False(errorType.IsGenericType);
        Assert.Equal(TypeKind.Error, errorType.TypeKind);
    }
}
