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
