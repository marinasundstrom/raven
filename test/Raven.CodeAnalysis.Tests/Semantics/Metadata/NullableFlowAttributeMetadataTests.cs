using System.Linq;

using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class NullableFlowAttributeMetadataTests : CompilationTestBase
{
    [Fact]
    public void MetadataParameter_ProjectsNotNullWhenAttributeAndArgument()
    {
        var (compilation, _) = CreateCompilation(string.Empty, references: TestMetadataReferences.DefaultWithExtensionMethods);
        var fixtureType = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetTypeByMetadataName("Raven.ExtensionMethodsFixture.NullableFlowFixture"));
        var method = fixtureType.GetMembers("IsMissing")
            .OfType<IMethodSymbol>()
            .Single();
        var attributes = method.Parameters[0].GetAttributes();
        var attribute = attributes.Single(attribute => attribute.AttributeClass?.Name == "NotNullWhenAttribute");

        Assert.Equal("System.Diagnostics.CodeAnalysis", attribute.AttributeClass.ContainingNamespace?.ToMetadataName());
        Assert.False(Assert.IsType<bool>(Assert.Single(attribute.ConstructorArguments).Value));
    }

    [Fact]
    public void MetadataReturn_ProjectsMaybeNullAttribute()
    {
        var (compilation, _) = CreateCompilation(string.Empty, references: TestMetadataReferences.DefaultWithExtensionMethods);
        var fixtureType = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetTypeByMetadataName("Raven.ExtensionMethodsFixture.NullableFlowFixture"));
        var method = fixtureType.GetMembers("FindOrDefault")
            .OfType<IMethodSymbol>()
            .Single();

        var attributes = method.GetReturnTypeAttributes();
        var attribute = Assert.Single(attributes, attribute => attribute.AttributeClass?.Name == "MaybeNullAttribute");

        Assert.Equal("System.Diagnostics.CodeAnalysis", attribute.AttributeClass.ContainingNamespace?.ToMetadataName());
    }
}
