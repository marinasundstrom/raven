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
    public void MetadataParameter_ProjectsMaybeNullWhenAttributeAndArgument()
    {
        var (compilation, _) = CreateCompilation(string.Empty, references: TestMetadataReferences.DefaultWithExtensionMethods);
        var fixtureType = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetTypeByMetadataName("Raven.ExtensionMethodsFixture.NullableFlowFixture"));
        var method = fixtureType.GetMembers("MaybeClear")
            .OfType<IMethodSymbol>()
            .Single();
        var parameter = method.Parameters[1];
        var attribute = Assert.Single(
            parameter.GetAttributes(),
            attribute => attribute.AttributeClass?.Name == "MaybeNullWhenAttribute");

        Assert.Equal(RefKind.Ref, parameter.RefKind);
        Assert.Equal("System.Diagnostics.CodeAnalysis", attribute.AttributeClass.ContainingNamespace?.ToMetadataName());
        Assert.True(Assert.IsType<bool>(Assert.Single(attribute.ConstructorArguments).Value));
    }

    [Theory]
    [InlineData("AcceptNull", "AllowNullAttribute", false)]
    [InlineData("RejectNull", "DisallowNullAttribute", true)]
    public void MetadataParameter_ProjectsInputNullabilityAttributeWithoutChangingDeclaredType(
        string methodName,
        string attributeName,
        bool isNullable)
    {
        var (compilation, _) = CreateCompilation(string.Empty, references: TestMetadataReferences.DefaultWithExtensionMethods);
        var fixtureType = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetTypeByMetadataName("Raven.ExtensionMethodsFixture.NullableFlowFixture"));
        var method = fixtureType.GetMembers(methodName)
            .OfType<IMethodSymbol>()
            .Single();
        var parameter = Assert.Single(method.Parameters);
        var attribute = Assert.Single(
            parameter.GetAttributes(),
            attribute => attribute.AttributeClass?.Name == attributeName);

        Assert.Equal(isNullable, parameter.Type.IsNullable);
        Assert.Equal("System.Diagnostics.CodeAnalysis", attribute.AttributeClass.ContainingNamespace?.ToMetadataName());
    }

    [Theory]
    [InlineData("RequiredName", "AllowNullAttribute", false)]
    [InlineData("OptionalName", "DisallowNullAttribute", true)]
    public void MetadataPropertySetter_ProjectsInputNullabilityAttributeWithoutChangingPropertyType(
        string propertyName,
        string attributeName,
        bool isNullable)
    {
        var (compilation, _) = CreateCompilation(string.Empty, references: TestMetadataReferences.DefaultWithExtensionMethods);
        var fixtureType = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.GetTypeByMetadataName("Raven.ExtensionMethodsFixture.NullableFlowFixture"));
        var property = fixtureType.GetMembers(propertyName)
            .OfType<IPropertySymbol>()
            .Single();
        var valueParameter = Assert.Single(Assert.IsAssignableFrom<IMethodSymbol>(property.SetMethod).Parameters);
        var attribute = Assert.Single(
            valueParameter.GetAttributes(),
            attribute => attribute.AttributeClass?.Name == attributeName);

        Assert.Equal(isNullable, property.Type.IsNullable);
        Assert.Equal("System.Diagnostics.CodeAnalysis", attribute.AttributeClass.ContainingNamespace?.ToMetadataName());
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
