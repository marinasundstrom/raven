using System.Reflection;
using System.Reflection.Emit;

using Raven.CodeAnalysis;

using Xunit;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class NullableReflectionIdentityTests
{
    [Fact]
    public void RuntimeConstructedNullableIsRecognized()
        => Assert.True(ReflectionTypeExtensions.IsNullableValueType(typeof(int?)));

    [Fact]
    public void OpenNullableDefinitionIsNotAConstructedNullableValue()
        => Assert.False(ReflectionTypeExtensions.IsNullableValueType(typeof(Nullable<>)));

    [Fact]
    public void MetadataConstructedNullableIsRecognizedAcrossReflectionContexts()
    {
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
        using var context = new MetadataLoadContext(new PathAssemblyResolver(paths), "System.Runtime");
        var core = context.LoadFromAssemblyName("System.Runtime");
        var definition = core.GetType("System.Nullable`1", true)!;
        var value = definition.MakeGenericType(core.GetType("System.Int32", true)!);
        Assert.True(value.IsConstructedGenericType);
        Assert.True(value.IsValueType);
        Assert.NotEqual(typeof(Nullable<>), value.GetGenericTypeDefinition());
        Assert.True(ReflectionTypeExtensions.IsNullableValueType(value));
        Assert.False(ReflectionTypeExtensions.IsNullableValueType(definition));
    }

    [Fact]
    public void NullableOfAnOpenTypeParameterIsStillConstructed()
    {
        var property = typeof(GenericBox<>).GetProperty(nameof(GenericBox<int>.Value))!;
        Assert.True(ReflectionTypeExtensions.IsNullableValueType(property.PropertyType));
    }

    [Fact]
    public void UserDefinedNullableNamesakeIsNotTheCoreNullableType()
    {
        var assembly = AssemblyBuilder.DefineDynamicAssembly(new AssemblyName("NullableNamesake"), AssemblyBuilderAccess.RunAndCollect);
        var module = assembly.DefineDynamicModule("main");
        var builder = module.DefineType("System.Nullable`1", TypeAttributes.Public | TypeAttributes.Sealed, typeof(ValueType));
        builder.DefineGenericParameters("T");
        var type = builder.CreateType()!.MakeGenericType(typeof(int));
        Assert.False(ReflectionTypeExtensions.IsNullableValueType(type));
        Assert.False(ReflectionTypeExtensions.IsNullableValueType(typeof(KeyValuePair<int, int>)));
    }

    private sealed class GenericBox<T> where T : struct
    {
        public T? Value { get; set; }
    }
}
