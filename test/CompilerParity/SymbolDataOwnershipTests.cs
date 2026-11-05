extern alias Seed;

using System.Reflection;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Metadata;

namespace Raven.CompilerParity.Tests;

[Trait("Target", "DotNet")]
public sealed class SymbolDataOwnershipTests
{
    [Fact]
    public void DataTypesArePortOwnedWhileUnportedSymbolsRemainExplicitBootstrapDependencies()
    {
        var port = typeof(Raven.CodeAnalysis.Text.TextSpan).Assembly;
        var seed = typeof(Seed::Raven.CodeAnalysis.Symbol).Assembly;
        foreach (var type in new[] { typeof(TypedConstant), typeof(TypedConstantKind), typeof(AttributeData), typeof(MetadataImportOptions),
            typeof(MacroParameterBinding), typeof(MacroParameterSource), typeof(ResolvedAssemblyArtifact) })
            Assert.Same(port, type.Assembly);

        foreach (var property in new[] { typeof(TypedConstant).GetProperty("Type")!,
            typeof(AttributeData).GetProperty("AttributeClass")!, typeof(AttributeData).GetProperty("AttributeConstructor")!,
            typeof(MacroParameterBinding).GetProperty("Parameter")! })
            Assert.Same(seed, property.PropertyType.Assembly);
        Assert.Same(port, typeof(AttributeData).GetProperty("ApplicationSyntaxReference")!.PropertyType.Assembly);
        Assert.Same(port, typeof(AttributeData).GetProperty("ConstructorArguments")!.PropertyType.GenericTypeArguments.Single().Assembly);
    }

    [Fact]
    public void ReflectionIdentityHelpersArePortOwned()
    {
        var port = typeof(TypedConstant).Assembly;
        Assert.Same(port, typeof(Raven.CodeAnalysis.Symbols.PEReflectionTypeIdentity).Assembly);
        Assert.Same(port, typeof(Raven.CodeAnalysis.Symbols.PETypeIdentity).Assembly);
        Assert.Same(port, typeof(Raven.CodeAnalysis.Symbols.PETypeParameterIdentity).Assembly);
        Assert.Same(port, typeof(Raven.CodeAnalysis.Symbols.PETypeParameterIdentity).GetProperty("OwnerKind")!.PropertyType.Assembly);
    }

    [Fact]
    public void RuntimeContractDataIsPortOwned()
    {
        var port = typeof(TypedConstant).Assembly;
        foreach (var type in new[] { typeof(RuntimeUnitContract), typeof(RuntimeTypeOfContract),
            typeof(RuntimeSelfTypeContract), typeof(RuntimePropagationContract), typeof(RuntimeDisposalContract),
            typeof(RuntimeIterationContract), typeof(RuntimeAsyncIterationContract) })
            Assert.Same(port, type.Assembly);
        Assert.Same(port, typeof(RuntimeIterationContract).GetProperty("Async")!.PropertyType.Assembly);
    }

    [Fact]
    public void MetadataProviderContractsPreserveSignaturesAndProviderBoundaries()
    {
        string[] names = ["IArrayTypeProvider", "IExtensionReceiverResolver", "IExtensionTypeInfo",
            "IMethodLookupIdentity", "IMethodOverloadPriority", "IMethodParameterInfo",
            "INamespaceExtensionLookup", "INamespaceMemberContainer", "INestedTypeDiscovery",
            "IParameterConversionClassifier", "IParameterDefaultValueInfo", "InstanceFieldLayoutSymbol",
            "ImportedAssemblySymbol"];
        var port = typeof(TypedConstant).Assembly;
        var seed = typeof(Seed::Raven.CodeAnalysis.Symbol).Assembly;
        static string Shape(Type type) => type.IsGenericType
            ? type.GetGenericTypeDefinition().FullName + "<" + string.Join(",", type.GetGenericArguments().Select(Shape)) + ">"
            : type.FullName!;
        static string MethodShape(MethodInfo method) => method.Name + ":" + Shape(method.ReturnType) + "(" +
            string.Join(",", method.GetParameters().Select(parameter => parameter.Name + ":" + Shape(parameter.ParameterType) + ":out=" + parameter.IsOut)) + ")";
        foreach (var name in names)
        {
            var expected = seed.GetType("Raven.CodeAnalysis.Metadata." + name, true)!;
            var actual = port.GetType(expected.FullName!, true)!;
            Assert.True(actual.IsInterface);
            Assert.False(actual.IsPublic);
            Assert.Equal(expected.GetMethods().Select(MethodShape).Order(), actual.GetMethods().Select(MethodShape).Order());
            Assert.Equal(expected.GetInterfaces().Select(Shape).Order(), actual.GetInterfaces().Select(Shape).Order());
        }
        foreach (var type in new[] { typeof(TypedConstantKind), typeof(MacroParameterSource), typeof(ParameterConversionKind) })
        {
            var expected = seed.GetType(type.FullName!, true)!;
            Assert.Equal(Enum.GetUnderlyingType(expected), Enum.GetUnderlyingType(type));
            Assert.Equal(Enum.GetNames(expected), Enum.GetNames(type));
            Assert.Equal(Enum.GetValues(expected).Cast<object>().Select(Convert.ToInt64),
                Enum.GetValues(type).Cast<object>().Select(Convert.ToInt64));
        }
        var typeParameter = typeof(IMethodParameterInfo).GetMethod("TryGetParameterType")!.GetParameters()[1];
        Assert.True(typeParameter.GetCustomAttribute<System.Diagnostics.CodeAnalysis.NotNullWhenAttribute>()!.ReturnValue);
        Assert.Same(port, typeof(ImportedAssemblySymbol).GetProperty("ResolvedArtifact")!.PropertyType.Assembly);
        Assert.Same(typeof(Seed::Raven.CodeAnalysis.AssemblySymbol),
            Assert.Single(typeof(ImportedAssemblySymbol).GetInterfaces(), type => type.FullName == "Raven.CodeAnalysis.AssemblySymbol"));
    }
}
