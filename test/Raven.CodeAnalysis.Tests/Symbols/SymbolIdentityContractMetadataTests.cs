using System.Collections.Immutable;
using System.Reflection;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class SymbolIdentityContractMetadataTests
{
    [Theory]
    [InlineData("INullableTypeIdentity", "UnderlyingType:TypeSymbol")]
    [InlineData("IStructuralTupleIdentity", "TupleElements:ImmutableArray<FieldSymbol>")]
    [InlineData("IStructuralFunctionIdentity", "ParameterTypes:ImmutableArray<TypeSymbol>", "ParameterRefKinds:ImmutableArray<RefKind>", "ReturnType:TypeSymbol")]
    [InlineData("IUnitTypeIdentity", "RuntimeRepresentation:NamedTypeSymbol")]
    [InlineData("IConstructedTypeSubstitutionInfo", "DefinitionForSubstitution:NamedTypeSymbol", "ExplicitTypeArgumentsForSubstitution:ImmutableArray<TypeSymbol>")]
    public void IdentityProviderContractsPreserveInternalGetterOnlyShape(string name, params string[] expected)
    {
        var implementation = typeof(SymbolIdentityContractMetadataTests).Assembly.GetCustomAttributes<AssemblyMetadataAttribute>()
            .SingleOrDefault(attribute => attribute.Key == "CompilerImplementationAssembly")?.Value;
        var assembly = implementation is null ? typeof(TypedConstant).Assembly : Assembly.Load(implementation);
        var type = assembly.GetType("Raven.CodeAnalysis.Symbols." + name, throwOnError: true)!;
        Assert.True(type.IsInterface);
        Assert.False(type.IsPublic);
        Assert.Empty(type.GetInterfaces());
        var properties = type.GetProperties();
        Assert.Equal(expected.Order(), properties.Select(property => property.Name + ":" + Shape(property.PropertyType)).Order());
        foreach (var property in properties)
        {
            Assert.NotNull(property.GetMethod);
            Assert.True(property.GetMethod!.IsAbstract);
            Assert.Null(property.SetMethod);
        }
        Assert.Equal(properties.Length, type.GetMethods().Length);
    }

    private static string Shape(Type type)
    {
        if (type.IsGenericType)
        {
            Assert.Equal(typeof(ImmutableArray<>), type.GetGenericTypeDefinition());
            return "ImmutableArray<" + Shape(type.GetGenericArguments().Single()) + ">";
        }
        Assert.Equal("Raven.CodeAnalysis", type.Namespace);
        return type.Name;
    }
}
