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

    [Fact]
    public void MetadataReturn_ProjectsNotNullIfNotNullAttributeAndParameterName()
    {
        var (compilation, _) = CreateCompilation(string.Empty, references: TestMetadataReferences.DefaultWithExtensionMethods);
        var fixtureType = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetTypeByMetadataName("Raven.ExtensionMethodsFixture.NullableFlowFixture"));
        var method = fixtureType.GetMembers("Echo")
            .OfType<IMethodSymbol>()
            .Single();

        var attribute = Assert.Single(
            method.GetReturnTypeAttributes(),
            attribute => attribute.AttributeClass?.Name == "NotNullIfNotNullAttribute");

        Assert.Equal("System.Diagnostics.CodeAnalysis", attribute.AttributeClass.ContainingNamespace?.ToMetadataName());
        Assert.Equal("value", Assert.IsType<string>(Assert.Single(attribute.ConstructorArguments).Value));
    }

    [Fact]
    public void MetadataOutParameter_ProjectsNotNullAttribute()
    {
        var (compilation, _) = CreateCompilation(string.Empty, references: TestMetadataReferences.DefaultWithExtensionMethods);
        var fixtureType = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetTypeByMetadataName("Raven.ExtensionMethodsFixture.NullableFlowFixture"));
        var method = fixtureType.GetMembers("SetName")
            .OfType<IMethodSymbol>()
            .Single();
        var parameter = Assert.Single(method.Parameters);

        var attribute = Assert.Single(
            parameter.GetAttributes(),
            attribute => attribute.AttributeClass?.Name == "NotNullAttribute");

        Assert.Equal(RefKind.Out, parameter.RefKind);
        Assert.True(parameter.Type.IsNullable);
        Assert.Equal("System.Diagnostics.CodeAnalysis", attribute.AttributeClass.ContainingNamespace?.ToMetadataName());
    }

    [Fact]
    public void MetadataGenericOutParameter_ProjectsMaybeNullAttribute()
    {
        var (compilation, _) = CreateCompilation(string.Empty, references: TestMetadataReferences.DefaultWithExtensionMethods);
        var fixtureType = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetTypeByMetadataName("Raven.ExtensionMethodsFixture.NullableFlowFixture"));
        var method = fixtureType.GetMembers("SetDefault")
            .OfType<IMethodSymbol>()
            .Single();
        var parameter = Assert.Single(method.Parameters);

        var attribute = Assert.Single(
            parameter.GetAttributes(),
            attribute => attribute.AttributeClass?.Name == "MaybeNullAttribute");

        Assert.Equal(RefKind.Out, parameter.RefKind);
        Assert.IsAssignableFrom<ITypeParameterSymbol>(parameter.Type);
        Assert.Equal("System.Diagnostics.CodeAnalysis", attribute.AttributeClass.ContainingNamespace?.ToMetadataName());
    }
}
