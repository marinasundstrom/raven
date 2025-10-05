using System;
using System.Collections.Generic;
using System.Reflection;
using System.Reflection.Emit;
using System.Reflection2;
using System.IO;

public static class PersistedAssemblyBuilderTests
{
    [Fact]
    public static void ToMetadataAssembly_CanRoundTripSimpleType()
    {
        var assemblyName = new AssemblyName("Reflection2.Dynamic");
        using var loadContext = CreateLoadContext();
        var persistedBuilder = new PersistedAssemblyBuilder(assemblyName, typeof(object).Assembly, Array.Empty<CustomAttributeBuilder>());
        var moduleBuilder = persistedBuilder.DefineDynamicModule("MainModule");
        var typeBuilder = moduleBuilder.DefineType("MyType", TypeAttributes.Public | TypeAttributes.Class);
        var fieldBuilder = typeBuilder.DefineField("Value", typeof(int), FieldAttributes.Public | FieldAttributes.Static);
        var ctorBuilder = typeBuilder.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, Type.EmptyTypes);
        var ctorIl = ctorBuilder.GetILGenerator();
        ctorIl.Emit(OpCodes.Ldarg_0);
        ctorIl.Emit(OpCodes.Call, typeof(object).GetConstructor(Type.EmptyTypes)!);
        ctorIl.Emit(OpCodes.Ret);

        var methodBuilder = typeBuilder.DefineMethod("GetValue", MethodAttributes.Public | MethodAttributes.Static, typeof(int), Type.EmptyTypes);
        var il = methodBuilder.GetILGenerator();
        il.Emit(OpCodes.Ldsfld, fieldBuilder);
        il.Emit(OpCodes.Ret);

        typeBuilder.CreateType();

        var metadataAssembly = persistedBuilder.ToMetadataAssembly(loadContext);
        var metadataType = metadataAssembly.GetType("MyType", throwOnError: true, ignoreCase: false)!.GetTypeInfo();

        Assert.Equal("MyType", metadataType.Name);
        Assert.Null(metadataType.Namespace);

        var fields = metadataType.GetFields(BindingFlags.Public | BindingFlags.Static);
        Assert.Single(fields);
        Assert.Equal("Value", fields[0].Name);
        Assert.Equal(typeof(int), fields[0].FieldType);

        var methods = metadataType.GetMethods(BindingFlags.Public | BindingFlags.Static | BindingFlags.DeclaredOnly);
        Assert.Contains(methods, m => m.Name == "GetValue" && m.ReturnType == typeof(int) && m.GetParameters().Length == 0);
    }

    [Fact]
    public static void MetadataTypes_SupportGenericConstruction()
    {
        var assemblyName = new AssemblyName("Reflection2.Generic");
        using var loadContext = CreateLoadContext();
        var persistedBuilder = new PersistedAssemblyBuilder(assemblyName, typeof(object).Assembly, Array.Empty<CustomAttributeBuilder>());
        var moduleBuilder = persistedBuilder.DefineDynamicModule("MainModule");

        // Payload type that will be used as a generic argument.
        var payloadBuilder = moduleBuilder.DefineType("Payload", TypeAttributes.Public | TypeAttributes.Class);
        payloadBuilder.DefineDefaultConstructor(MethodAttributes.Public);
        payloadBuilder.CreateType();

        // Generic container that stores a value of type T.
        var containerBuilder = moduleBuilder.DefineType("Container`1", TypeAttributes.Public | TypeAttributes.Class);
        var typeParameters = containerBuilder.DefineGenericParameters("T");
        var valueField = containerBuilder.DefineField("_value", typeParameters[0], FieldAttributes.Private);
        var ctor = containerBuilder.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, new[] { typeParameters[0] });
        var ctorIl = ctor.GetILGenerator();
        ctorIl.Emit(OpCodes.Ldarg_0);
        ctorIl.Emit(OpCodes.Call, typeof(object).GetConstructor(Type.EmptyTypes)!);
        ctorIl.Emit(OpCodes.Ldarg_0);
        ctorIl.Emit(OpCodes.Ldarg_1);
        ctorIl.Emit(OpCodes.Stfld, valueField);
        ctorIl.Emit(OpCodes.Ret);

        var getValue = containerBuilder.DefineMethod("GetValue", MethodAttributes.Public | MethodAttributes.HideBySig, typeParameters[0], Type.EmptyTypes);
        var getValueIl = getValue.GetILGenerator();
        getValueIl.Emit(OpCodes.Ldarg_0);
        getValueIl.Emit(OpCodes.Ldfld, valueField);
        getValueIl.Emit(OpCodes.Ret);
        containerBuilder.CreateType();

        // Generic factory method used to validate MakeGenericMethod with metadata types.
        var utilitiesBuilder = moduleBuilder.DefineType("Utilities", TypeAttributes.Public | TypeAttributes.Class | TypeAttributes.Sealed | TypeAttributes.Abstract);
        var createMethod = utilitiesBuilder.DefineMethod("Create", MethodAttributes.Public | MethodAttributes.Static | MethodAttributes.HideBySig);
        var methodParameters = createMethod.DefineGenericParameters("T");
        createMethod.SetReturnType(methodParameters[0]);
        var createIl = createMethod.GetILGenerator();
        createIl.DeclareLocal(methodParameters[0]);
        createIl.Emit(OpCodes.Ldloca_S, (byte)0);
        createIl.Emit(OpCodes.Initobj, methodParameters[0]);
        createIl.Emit(OpCodes.Ldloc_0);
        createIl.Emit(OpCodes.Ret);
        utilitiesBuilder.CreateType();

        var metadataAssembly = persistedBuilder.ToMetadataAssembly(loadContext);

        var payloadType = metadataAssembly.GetType("Payload", throwOnError: true, ignoreCase: false)!;
        var containerDefinition = metadataAssembly.GetType("Container`1", throwOnError: true, ignoreCase: false)!;
        var constructedContainer = containerDefinition.MakeGenericType(payloadType);

        Assert.False(constructedContainer.ContainsGenericParameters);
        Assert.Single(constructedContainer.GetGenericArguments(), payloadType);

        var constructedMethod = constructedContainer.GetMethod("GetValue", BindingFlags.Public | BindingFlags.Instance | BindingFlags.DeclaredOnly);
        Assert.NotNull(constructedMethod);
        Assert.Equal(payloadType, constructedMethod!.ReturnType);
        Assert.Empty(constructedMethod.GetParameters());

        var utilitiesType = metadataAssembly.GetType("Utilities", throwOnError: true, ignoreCase: false)!;
        var genericFactory = utilitiesType.GetMethod("Create", BindingFlags.Public | BindingFlags.Static | BindingFlags.DeclaredOnly);
        Assert.NotNull(genericFactory);
        Assert.True(genericFactory!.IsGenericMethodDefinition);

        var constructedFactory = genericFactory.MakeGenericMethod(payloadType);
        Assert.False(constructedFactory.ContainsGenericParameters);
        Assert.Single(constructedFactory.GetGenericArguments(), payloadType);
        Assert.Equal(payloadType, constructedFactory.ReturnType);
    }

    private static MetadataLoadContext CreateLoadContext()
    {
        var runtimeAssemblyLocation = typeof(object).Assembly.Location;
        var searchPaths = new List<string>();

        if (!string.IsNullOrEmpty(runtimeAssemblyLocation))
        {
            searchPaths.Add(runtimeAssemblyLocation);
            var runtimeDirectory = Path.GetDirectoryName(runtimeAssemblyLocation);
            if (!string.IsNullOrEmpty(runtimeDirectory))
            {
                searchPaths.Add(runtimeDirectory);
            }
        }

        searchPaths.Add(AppContext.BaseDirectory);

        var resolver = new PathMetadataAssemblyResolver(searchPaths);
        return new MetadataLoadContext(resolver);
    }
}
