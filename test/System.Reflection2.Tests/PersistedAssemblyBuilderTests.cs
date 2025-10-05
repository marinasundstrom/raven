using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Globalization;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;
using System.Reflection2;
using System.IO;
using System.Reflection.Metadata;
using System.Runtime.Serialization;

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

        var propertyBuilder = typeBuilder.DefineProperty("Value", PropertyAttributes.None, typeof(int), Type.EmptyTypes);
        var getProperty = typeBuilder.DefineMethod("get_Value", MethodAttributes.Public | MethodAttributes.Static | MethodAttributes.HideBySig | MethodAttributes.SpecialName, typeof(int), Type.EmptyTypes);
        var getIl = getProperty.GetILGenerator();
        getIl.Emit(OpCodes.Ldsfld, fieldBuilder);
        getIl.Emit(OpCodes.Ret);

        var setProperty = typeBuilder.DefineMethod("set_Value", MethodAttributes.Public | MethodAttributes.Static | MethodAttributes.HideBySig | MethodAttributes.SpecialName, typeof(void), new[] { typeof(int) });
        var setIl = setProperty.GetILGenerator();
        setIl.Emit(OpCodes.Ldarg_0);
        setIl.Emit(OpCodes.Stsfld, fieldBuilder);
        setIl.Emit(OpCodes.Ret);

        propertyBuilder.SetGetMethod(getProperty);
        propertyBuilder.SetSetMethod(setProperty);

        var eventBuilder = typeBuilder.DefineEvent("Changed", EventAttributes.None, typeof(EventHandler));
        var addChanged = typeBuilder.DefineMethod("add_Changed", MethodAttributes.Public | MethodAttributes.Static | MethodAttributes.HideBySig | MethodAttributes.SpecialName, typeof(void), new[] { typeof(EventHandler) });
        addChanged.GetILGenerator().Emit(OpCodes.Ret);
        var removeChanged = typeBuilder.DefineMethod("remove_Changed", MethodAttributes.Public | MethodAttributes.Static | MethodAttributes.HideBySig | MethodAttributes.SpecialName, typeof(void), new[] { typeof(EventHandler) });
        removeChanged.GetILGenerator().Emit(OpCodes.Ret);
        eventBuilder.SetAddOnMethod(addChanged);
        eventBuilder.SetRemoveOnMethod(removeChanged);

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

        var getterMethod = metadataType.GetMethod("get_Value", BindingFlags.Public | BindingFlags.Static | BindingFlags.DeclaredOnly);
        var setterMethod = metadataType.GetMethod("set_Value", BindingFlags.Public | BindingFlags.Static | BindingFlags.DeclaredOnly, binder: null, types: new[] { typeof(int) }, modifiers: null);

        var properties = metadataType.GetProperties(BindingFlags.Public | BindingFlags.Static | BindingFlags.DeclaredOnly);
        Assert.Single(properties);
        var property = properties[0];
        Assert.Equal("Value", property.Name);
        Assert.Equal(typeof(int), property.PropertyType);
        Assert.True(property.CanRead);
        Assert.True(property.CanWrite);
        Assert.Same(getterMethod, property.GetMethod);
        Assert.Same(setterMethod, property.SetMethod);
        Assert.Empty(property.GetIndexParameters());

        var events = metadataType.GetEvents(BindingFlags.Public | BindingFlags.Static | BindingFlags.DeclaredOnly);
        Assert.Single(events);
        var @event = events[0];
        Assert.Equal("Changed", @event.Name);
        Assert.NotNull(@event.EventHandlerType);
        Assert.Equal(typeof(EventHandler).FullName, @event.EventHandlerType!.FullName);
        var addMethodInfo = metadataType.GetMethod("add_Changed", BindingFlags.Public | BindingFlags.Static | BindingFlags.DeclaredOnly);
        Assert.NotNull(addMethodInfo);
        Assert.Same(addMethodInfo!, @event.GetAddMethod(nonPublic: false));

        var removeMethodInfo = metadataType.GetMethod("remove_Changed", BindingFlags.Public | BindingFlags.Static | BindingFlags.DeclaredOnly);
        Assert.NotNull(removeMethodInfo);
        Assert.Same(removeMethodInfo!, @event.GetRemoveMethod(nonPublic: false));
        Assert.Same(@event, metadataType.GetEvent("Changed", BindingFlags.Public | BindingFlags.Static | BindingFlags.DeclaredOnly));
    }

    [Fact]
    public static void MetadataRuntimeBridge_DelegatesToRuntimeReflection()
    {
        var assemblyName = new AssemblyName("Reflection2.RuntimeBridge");
        var runtimeAssemblyBuilder = AssemblyBuilder.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.Run);
        var runtimeModuleBuilder = runtimeAssemblyBuilder.DefineDynamicModule("RuntimeModule");
        var runtimeTypeBuilder = runtimeModuleBuilder.DefineType("RuntimeBacked", TypeAttributes.Public | TypeAttributes.Class);

        var persistedBuilder = new PersistedAssemblyBuilder(assemblyName, typeof(object).Assembly, Array.Empty<CustomAttributeBuilder>());
        var metadataModuleBuilder = persistedBuilder.DefineDynamicModule("MainModule");
        var metadataTypeBuilder = metadataModuleBuilder.DefineType("RuntimeBacked", TypeAttributes.Public | TypeAttributes.Class);

        DefineRuntimeBackedMembers(runtimeTypeBuilder);
        DefineRuntimeBackedMembers(metadataTypeBuilder);

        var runtimeType = runtimeTypeBuilder.CreateType()!;
        metadataTypeBuilder.CreateType();

        var runtimeTypes = new Dictionary<string, Type>(StringComparer.Ordinal);
        var bridge = new RuntimeReflectionBridge(metadataType =>
        {
            if (metadataType.FullName is { } fullName && runtimeTypes.TryGetValue(fullName, out var mapped))
            {
                return mapped;
            }

            var qualifiedName = metadataType.AssemblyQualifiedName ?? metadataType.FullName ?? metadataType.Name;
            return qualifiedName is null ? null : Type.GetType(qualifiedName, throwOnError: false);
        });

        using var loadContext = CreateLoadContext(bridge);
        var metadataAssembly = persistedBuilder.ToMetadataAssembly(loadContext);
        var metadataType = metadataAssembly.GetType("RuntimeBacked", throwOnError: true, ignoreCase: false)!;
        runtimeTypes[metadataType.FullName ?? throw new InvalidOperationException()] = runtimeType;
        foreach (var nestedMetadataType in metadataType.GetNestedTypes(BindingFlags.Public | BindingFlags.NonPublic))
        {
            if (nestedMetadataType.FullName is { } nestedFullName)
            {
                var runtimeNested = runtimeType.GetNestedType(nestedMetadataType.Name, BindingFlags.Public | BindingFlags.NonPublic);
                if (runtimeNested is not null)
                {
                    runtimeTypes[nestedFullName] = runtimeNested;
                }
            }
        }

        var metadataField = metadataType.GetField("Value", BindingFlags.Public | BindingFlags.Static);
        Assert.NotNull(metadataField);
        metadataField!.SetValue(null, 10, BindingFlags.Default, binder: null, culture: null);
        Assert.Equal(10, runtimeType.GetField("Value", BindingFlags.Public | BindingFlags.Static)!.GetValue(null));
        Assert.Equal(10, metadataField.GetValue(null));

        var metadataProperty = metadataType.GetProperty("Value", BindingFlags.Public | BindingFlags.Static);
        Assert.NotNull(metadataProperty);
        metadataProperty!.SetValue(null, 20);
        Assert.Equal(20, metadataProperty.GetValue(null));
        Assert.Equal(20, runtimeType.GetProperty("Value", BindingFlags.Public | BindingFlags.Static)!.GetValue(null));

        var metadataMethod = metadataType.GetMethod("GetValue", BindingFlags.Public | BindingFlags.Static);
        Assert.NotNull(metadataMethod);
        Assert.Equal(20, metadataMethod!.Invoke(null, null));

        var echoPayload = metadataType.GetMethod("EchoPayload", BindingFlags.Public | BindingFlags.Static);
        Assert.NotNull(echoPayload);
        var payloadBinder = new PayloadBinder();
        var payloadResult = echoPayload!.Invoke(null, BindingFlags.Default, payloadBinder, new object?[] { "bridge" }, CultureInfo.InvariantCulture);
        Assert.Equal("bridge", payloadResult);

        var constructor = metadataType.GetConstructors(BindingFlags.Public | BindingFlags.Instance).Single();
        var instance = constructor.Invoke(BindingFlags.Default, binder: null, parameters: null, culture: null);
        Assert.NotNull(instance);
        Assert.Equal(runtimeType, instance!.GetType());

        var setInstance = metadataType.GetMethod("SetInstanceValue", BindingFlags.Public | BindingFlags.Instance);
        var getInstance = metadataType.GetMethod("GetInstanceValue", BindingFlags.Public | BindingFlags.Instance);
        Assert.NotNull(setInstance);
        Assert.NotNull(getInstance);
        setInstance!.Invoke(instance, new object?[] { 45 });
        Assert.Equal(45, getInstance!.Invoke(instance, null));

        var eventInfo = metadataType.GetEvent("Changed", BindingFlags.Public | BindingFlags.Static);
        Assert.NotNull(eventInfo);
        var handler = new EventHandler((_, _) => { });
        eventInfo!.AddEventHandler(null, handler);
        eventInfo.RemoveEventHandler(null, handler);
        Assert.Equal(1, runtimeType.GetField("AddCount", BindingFlags.Public | BindingFlags.Static)!.GetValue(null));
        Assert.Equal(1, runtimeType.GetField("RemoveCount", BindingFlags.Public | BindingFlags.Static)!.GetValue(null));

        metadataType.InvokeMember("Value", BindingFlags.SetProperty | BindingFlags.Static | BindingFlags.Public, binder: null, target: null, args: new object?[] { 123 }, modifiers: null, culture: null, namedParameters: null);
        var propertyValue = metadataType.InvokeMember("Value", BindingFlags.GetProperty | BindingFlags.Static | BindingFlags.Public, binder: null, target: null, args: null, modifiers: null, culture: null, namedParameters: null);
        Assert.Equal(123, propertyValue);
        Assert.Equal(123, runtimeType.GetProperty("Value", BindingFlags.Public | BindingFlags.Static)!.GetValue(null));
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

    [Fact]
    public static void MetadataTypes_SupportArrayAndByRefConstruction()
    {
        var assemblyName = new AssemblyName("Reflection2.Arrays");
        using var loadContext = CreateLoadContext();
        var persistedBuilder = new PersistedAssemblyBuilder(assemblyName, typeof(object).Assembly, Array.Empty<CustomAttributeBuilder>());
        var moduleBuilder = persistedBuilder.DefineDynamicModule("MainModule");

        var payloadBuilder = moduleBuilder.DefineType("Payload", TypeAttributes.Public | TypeAttributes.Class);
        payloadBuilder.DefineDefaultConstructor(MethodAttributes.Public);

        var consumerBuilder = moduleBuilder.DefineType("Consumer", TypeAttributes.Public | TypeAttributes.Class | TypeAttributes.Sealed | TypeAttributes.Abstract);

        var arrayMethod = consumerBuilder.DefineMethod("ProcessArray", MethodAttributes.Public | MethodAttributes.Static | MethodAttributes.HideBySig, typeof(void), new[] { payloadBuilder.MakeArrayType() });
        arrayMethod.GetILGenerator().Emit(OpCodes.Ret);

        var refMethod = consumerBuilder.DefineMethod("ProcessRef", MethodAttributes.Public | MethodAttributes.Static | MethodAttributes.HideBySig, typeof(void), new[] { payloadBuilder.MakeByRefType() });
        refMethod.DefineParameter(1, ParameterAttributes.None, "value");
        refMethod.GetILGenerator().Emit(OpCodes.Ret);

        payloadBuilder.CreateType();
        consumerBuilder.CreateType();

        var metadataAssembly = persistedBuilder.ToMetadataAssembly(loadContext);

        var payloadType = metadataAssembly.GetType("Payload", throwOnError: true, ignoreCase: false)!;
        var consumerType = metadataAssembly.GetType("Consumer", throwOnError: true, ignoreCase: false)!;

        var arrayType = payloadType.MakeArrayType();
        Assert.True(arrayType.IsArray);
        Assert.Equal(payloadType, arrayType.GetElementType());
        Assert.Same(arrayType, payloadType.MakeArrayType());

        var arrayMethodInfo = consumerType.GetMethod("ProcessArray", BindingFlags.Public | BindingFlags.Static | BindingFlags.DeclaredOnly);
        Assert.NotNull(arrayMethodInfo);
        Assert.Equal(arrayType, arrayMethodInfo!.GetParameters()[0].ParameterType);

        var multidimensional = payloadType.MakeArrayType(2);
        Assert.Equal(2, multidimensional.GetArrayRank());

        var byRefType = payloadType.MakeByRefType();
        Assert.True(byRefType.IsByRef);
        Assert.Same(byRefType, payloadType.MakeByRefType());

        var refMethodInfo = consumerType.GetMethod("ProcessRef", BindingFlags.Public | BindingFlags.Static | BindingFlags.DeclaredOnly, binder: null, types: new[] { byRefType }, modifiers: null);
        Assert.NotNull(refMethodInfo);
        Assert.Equal(byRefType, refMethodInfo!.GetParameters()[0].ParameterType);

        var pointerType = payloadType.MakePointerType();
        Assert.True(pointerType.IsPointer);
        Assert.Equal(payloadType, pointerType.GetElementType());
    }

    [Fact]
    public static void MetadataModule_ProducesMetadataFunctionPointerTypes()
    {
        var assemblyName = new AssemblyName("Reflection2.FunctionPointers");
        using var loadContext = CreateLoadContext();
        var persistedBuilder = new PersistedAssemblyBuilder(assemblyName, typeof(object).Assembly, Array.Empty<CustomAttributeBuilder>());
        var moduleBuilder = persistedBuilder.DefineDynamicModule("MainModule");

        // Ensure the module materializes so that the metadata module can be accessed.
        moduleBuilder.DefineType("Dummy", TypeAttributes.Public | TypeAttributes.Class).CreateType();

        var metadataAssembly = persistedBuilder.ToMetadataAssembly(loadContext);
        var metadataModule = (MetadataModule)metadataAssembly.Modules.Single();

        var pointerSignature = CreateFunctionPointerSignature(typeof(void), typeof(int));
        var getFunctionPointerType = typeof(MetadataModule).GetMethod("GetFunctionPointerType", BindingFlags.Instance | BindingFlags.NonPublic)
            ?? throw new InvalidOperationException("GetFunctionPointerType missing.");

        var first = (Type)getFunctionPointerType.Invoke(metadataModule, new object[] { pointerSignature })!;
        var second = (Type)getFunctionPointerType.Invoke(metadataModule, new object[] { pointerSignature })!;

        Assert.Equal("System.Reflection2.MetadataFunctionPointerType", first.GetType().FullName);
        Assert.Same(first, second);
        Assert.True(first.IsFunctionPointer);
        Assert.Equal("delegate*<System.Int32, System.Void>", first.ToString());
    }

    [Fact]
    public static void MetadataParameters_ExposeDefaultValues()
    {
        var assemblyName = new AssemblyName("Reflection2.ParameterDefaults");
        using var loadContext = CreateLoadContext();
        var persistedBuilder = new PersistedAssemblyBuilder(assemblyName, typeof(object).Assembly, Array.Empty<CustomAttributeBuilder>());
        var moduleBuilder = persistedBuilder.DefineDynamicModule("MainModule");

        var hostBuilder = moduleBuilder.DefineType("SignatureHost", TypeAttributes.Public | TypeAttributes.Class | TypeAttributes.Sealed | TypeAttributes.Abstract);
        var defaults = hostBuilder.DefineMethod("Defaults", MethodAttributes.Public | MethodAttributes.Static | MethodAttributes.HideBySig, typeof(void), new[] { typeof(int), typeof(string), typeof(DayOfWeek), typeof(object) });
        defaults.DefineParameter(1, ParameterAttributes.Optional | ParameterAttributes.HasDefault, "number").SetConstant(42);
        defaults.DefineParameter(2, ParameterAttributes.Optional | ParameterAttributes.HasDefault, "text").SetConstant("hello");
        defaults.DefineParameter(3, ParameterAttributes.Optional | ParameterAttributes.HasDefault, "day").SetConstant(DayOfWeek.Wednesday);
        defaults.DefineParameter(4, ParameterAttributes.Optional | ParameterAttributes.HasDefault, "sentinel").SetConstant(null);
        defaults.GetILGenerator().Emit(OpCodes.Ret);
        hostBuilder.CreateType();

        var metadataAssembly = persistedBuilder.ToMetadataAssembly(loadContext);
        var defaultsMethod = metadataAssembly.GetType("SignatureHost", throwOnError: true, ignoreCase: false)!
            .GetMethod("Defaults", BindingFlags.Public | BindingFlags.Static | BindingFlags.DeclaredOnly);
        Assert.NotNull(defaultsMethod);

        var parameters = defaultsMethod!.GetParameters();
        Assert.Equal(4, parameters.Length);

        Assert.True(parameters[0].HasDefaultValue);
        Assert.Equal(42, parameters[0].DefaultValue);

        Assert.True(parameters[1].HasDefaultValue);
        Assert.Equal("hello", parameters[1].DefaultValue);

        Assert.True(parameters[2].HasDefaultValue);
        Assert.Equal(DayOfWeek.Wednesday, parameters[2].DefaultValue);

        Assert.True(parameters[3].HasDefaultValue);
        Assert.Null(parameters[3].DefaultValue);
        Assert.Null(parameters[3].RawDefaultValue);
    }

    [Fact]
    public static void MetadataTypes_ExposeInterfaceMaps()
    {
        var assemblyName = new AssemblyName("Reflection2.Interfaces");
        using var loadContext = CreateLoadContext();
        var persistedBuilder = new PersistedAssemblyBuilder(assemblyName, typeof(object).Assembly, Array.Empty<CustomAttributeBuilder>());
        var moduleBuilder = persistedBuilder.DefineDynamicModule("MainModule");

        var implicitInterfaceBuilder = moduleBuilder.DefineType("IImplicit", TypeAttributes.Public | TypeAttributes.Interface | TypeAttributes.Abstract);
        implicitInterfaceBuilder.DefineMethod("DoWork", MethodAttributes.Public | MethodAttributes.Abstract | MethodAttributes.Virtual | MethodAttributes.HideBySig, typeof(void), Type.EmptyTypes);
        var implicitInterfaceType = implicitInterfaceBuilder.CreateType()!;

        var explicitInterfaceBuilder = moduleBuilder.DefineType("IExplicit", TypeAttributes.Public | TypeAttributes.Interface | TypeAttributes.Abstract);
        explicitInterfaceBuilder.DefineMethod("DoExplicit", MethodAttributes.Public | MethodAttributes.Abstract | MethodAttributes.Virtual | MethodAttributes.HideBySig, typeof(void), Type.EmptyTypes);
        var explicitInterfaceType = explicitInterfaceBuilder.CreateType()!;
        var explicitInterfaceMethod = explicitInterfaceType.GetMethod("DoExplicit")!;

        var workerBuilder = moduleBuilder.DefineType("Worker", TypeAttributes.Public | TypeAttributes.Class);
        workerBuilder.AddInterfaceImplementation(implicitInterfaceType);
        workerBuilder.AddInterfaceImplementation(explicitInterfaceType);

        var implicitImplementation = workerBuilder.DefineMethod("DoWork", MethodAttributes.Public | MethodAttributes.Virtual | MethodAttributes.Final | MethodAttributes.HideBySig | MethodAttributes.NewSlot, typeof(void), Type.EmptyTypes);
        implicitImplementation.GetILGenerator().Emit(OpCodes.Ret);

        var explicitImplementation = workerBuilder.DefineMethod("IExplicit.DoExplicit", MethodAttributes.Private | MethodAttributes.Virtual | MethodAttributes.Final | MethodAttributes.HideBySig | MethodAttributes.NewSlot, typeof(void), Type.EmptyTypes);
        explicitImplementation.GetILGenerator().Emit(OpCodes.Ret);
        workerBuilder.DefineMethodOverride(explicitImplementation, explicitInterfaceMethod);

        workerBuilder.CreateType();

        var metadataAssembly = persistedBuilder.ToMetadataAssembly(loadContext);
        var workerType = metadataAssembly.GetType("Worker", throwOnError: true, ignoreCase: false)!;

        var metadataImplicit = metadataAssembly.GetType("IImplicit", throwOnError: true, ignoreCase: false)!;
        var metadataExplicitInterface = metadataAssembly.GetType("IExplicit", throwOnError: true, ignoreCase: false)!;

        var implicitMap = workerType.GetInterfaceMap(metadataImplicit);
        Assert.Equal(workerType, implicitMap.TargetType);
        var implicitInterfaceMethod = metadataImplicit.GetMethod("DoWork", BindingFlags.Public | BindingFlags.Instance)!;
        var implicitIndex = Array.IndexOf(implicitMap.InterfaceMethods, implicitInterfaceMethod);
        Assert.True(implicitIndex >= 0, "Implicit interface method should be present in the map.");
        var workerDoWork = workerType.GetMethod("DoWork", BindingFlags.Public | BindingFlags.Instance | BindingFlags.DeclaredOnly);
        Assert.Same(workerDoWork, implicitMap.TargetMethods[implicitIndex]);

        var explicitMap = workerType.GetInterfaceMap(metadataExplicitInterface);
        Assert.Equal(workerType, explicitMap.TargetType);
        var metadataExplicitMethod = metadataExplicitInterface.GetMethod("DoExplicit", BindingFlags.Public | BindingFlags.Instance)!;
        var explicitIndex = Array.IndexOf(explicitMap.InterfaceMethods, metadataExplicitMethod);
        Assert.True(explicitIndex >= 0, "Explicit interface method should be present in the map.");
        var metadataExplicit = workerType.GetMethod("IExplicit.DoExplicit", BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.DeclaredOnly);
        Assert.Same(metadataExplicit, explicitMap.TargetMethods[explicitIndex]);
    }

    [Fact]
    public static void MetadataMembers_ExposeCustomAttributeData()
    {
        var assemblyName = new AssemblyName("Reflection2.Attributes");
        using var loadContext = CreateLoadContext();
        var persistedBuilder = new PersistedAssemblyBuilder(assemblyName, typeof(object).Assembly, Array.Empty<CustomAttributeBuilder>());
        var moduleBuilder = persistedBuilder.DefineDynamicModule("MainModule");

        var attributeBuilder = moduleBuilder.DefineType("SampleAttribute", TypeAttributes.Public | TypeAttributes.Class | TypeAttributes.Sealed, typeof(Attribute));
        var valueField = attributeBuilder.DefineField("_name", typeof(string), FieldAttributes.Private);
        var nameProperty = attributeBuilder.DefineProperty("Name", PropertyAttributes.None, typeof(string), Type.EmptyTypes);
        var getName = attributeBuilder.DefineMethod("get_Name", MethodAttributes.Public | MethodAttributes.SpecialName | MethodAttributes.HideBySig, typeof(string), Type.EmptyTypes);
        var getNameIl = getName.GetILGenerator();
        getNameIl.Emit(OpCodes.Ldarg_0);
        getNameIl.Emit(OpCodes.Ldfld, valueField);
        getNameIl.Emit(OpCodes.Ret);
        var setName = attributeBuilder.DefineMethod("set_Name", MethodAttributes.Public | MethodAttributes.SpecialName | MethodAttributes.HideBySig, typeof(void), new[] { typeof(string) });
        var setNameIl = setName.GetILGenerator();
        setNameIl.Emit(OpCodes.Ldarg_0);
        setNameIl.Emit(OpCodes.Ldarg_1);
        setNameIl.Emit(OpCodes.Stfld, valueField);
        setNameIl.Emit(OpCodes.Ret);
        nameProperty.SetGetMethod(getName);
        nameProperty.SetSetMethod(setName);
        var dayField = attributeBuilder.DefineField("Day", typeof(DayOfWeek), FieldAttributes.Public);
        var attributeCtor = attributeBuilder.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, new[] { typeof(string), typeof(int) });
        var ctorIl = attributeCtor.GetILGenerator();
        ctorIl.Emit(OpCodes.Ldarg_0);
        ctorIl.Emit(OpCodes.Call, typeof(Attribute).GetConstructor(BindingFlags.Instance | BindingFlags.NonPublic, binder: null, types: Type.EmptyTypes, modifiers: null)!);
        ctorIl.Emit(OpCodes.Ret);
        var attributeUsage = new CustomAttributeBuilder(typeof(AttributeUsageAttribute).GetConstructor(new[] { typeof(AttributeTargets) })!, new object[] { AttributeTargets.All });
        attributeBuilder.SetCustomAttribute(attributeUsage);
        attributeBuilder.CreateTypeInfo();
        var attributeConstructor = (ConstructorInfo)attributeCtor;
        var attributeNameProperty = (PropertyInfo)nameProperty;
        var attributeDayField = (FieldInfo)dayField;

        var typeBuilder = moduleBuilder.DefineType("AttributedType", TypeAttributes.Public | TypeAttributes.Class);
        var typeAttribute = new CustomAttributeBuilder(attributeConstructor, new object[] { "type", 1 }, new[] { attributeNameProperty }, new object[] { "TypeName" }, new[] { attributeDayField }, new object[] { DayOfWeek.Friday });
        typeBuilder.SetCustomAttribute(typeAttribute);

        var fieldBuilder = typeBuilder.DefineField("Value", typeof(int), FieldAttributes.Public);
        fieldBuilder.SetCustomAttribute(new CustomAttributeBuilder(attributeConstructor, new object[] { "field", 2 }));

        var methodBuilder = typeBuilder.DefineMethod("DoWork", MethodAttributes.Public, typeof(void), new[] { typeof(string) });
        var parameterBuilder = methodBuilder.DefineParameter(1, ParameterAttributes.None, "text");
        var methodAttribute = new CustomAttributeBuilder(attributeConstructor, new object[] { "method", 3 }, new[] { attributeNameProperty }, new object[] { "MethodName" }, Array.Empty<FieldInfo>(), Array.Empty<object>());
        methodBuilder.SetCustomAttribute(methodAttribute);
        parameterBuilder.SetCustomAttribute(new CustomAttributeBuilder(attributeConstructor, new object[] { "parameter", 4 }));
        methodBuilder.GetILGenerator().Emit(OpCodes.Ret);

        var propertyBuilder = typeBuilder.DefineProperty("Data", PropertyAttributes.None, typeof(string), Type.EmptyTypes);
        var getData = typeBuilder.DefineMethod("get_Data", MethodAttributes.Public | MethodAttributes.SpecialName | MethodAttributes.HideBySig, typeof(string), Type.EmptyTypes);
        var getDataIl = getData.GetILGenerator();
        getDataIl.Emit(OpCodes.Ldstr, "constant");
        getDataIl.Emit(OpCodes.Ret);
        propertyBuilder.SetGetMethod(getData);
        propertyBuilder.SetCustomAttribute(new CustomAttributeBuilder(attributeConstructor, new object[] { "property", 5 }, new[] { attributeNameProperty }, new object[] { "PropertyName" }, new[] { attributeDayField }, new object[] { DayOfWeek.Sunday }));

        typeBuilder.CreateType();

        var metadataAssembly = persistedBuilder.ToMetadataAssembly(loadContext);
        var attributeMetadataType = metadataAssembly.GetType("SampleAttribute", throwOnError: true, ignoreCase: false)!;
        var metadataType = metadataAssembly.GetType("AttributedType", throwOnError: true, ignoreCase: false)!.GetTypeInfo();

        var typeAttributes = metadataType.GetCustomAttributesData();
        Assert.Single(typeAttributes);
        var appliedTypeAttribute = typeAttributes[0];
        Assert.Equal(attributeMetadataType, appliedTypeAttribute.AttributeType);
        Assert.Equal("type", appliedTypeAttribute.ConstructorArguments[0].Value);
        Assert.Equal(1, appliedTypeAttribute.ConstructorArguments[1].Value);
        Assert.Equal("TypeName", appliedTypeAttribute.NamedArguments.Single(a => a.MemberName == "Name").TypedValue.Value);
        var typeDayArgument = appliedTypeAttribute.NamedArguments.Single(a => a.MemberName == "Day").TypedValue;
        Assert.IsType<MetadataType>(typeDayArgument.ArgumentType);
        Assert.Equal("System.DayOfWeek", typeDayArgument.ArgumentType.FullName);
        Assert.Equal(DayOfWeek.Friday, ConvertEnumValue<DayOfWeek>(typeDayArgument));

        var metadataField = metadataType.GetField("Value", BindingFlags.Public | BindingFlags.Instance);
        Assert.NotNull(metadataField);
        var fieldAttributes = metadataField!.GetCustomAttributesData();
        Assert.Single(fieldAttributes);
        Assert.Equal("field", fieldAttributes[0].ConstructorArguments[0].Value);

        var metadataMethod = metadataType.GetMethod("DoWork", BindingFlags.Public | BindingFlags.Instance);
        Assert.NotNull(metadataMethod);
        var methodAttributesData = metadataMethod!.GetCustomAttributesData();
        Assert.Single(methodAttributesData);
        Assert.Equal("method", methodAttributesData[0].ConstructorArguments[0].Value);
        Assert.Equal("MethodName", methodAttributesData[0].NamedArguments.Single(a => a.MemberName == "Name").TypedValue.Value);

        var parameter = metadataMethod.GetParameters().Single();
        var parameterAttributes = parameter.GetCustomAttributesData();
        Assert.Single(parameterAttributes);
        Assert.Equal("parameter", parameterAttributes[0].ConstructorArguments[0].Value);

        var metadataProperty = metadataType.GetProperty("Data", BindingFlags.Public | BindingFlags.Instance);
        Assert.NotNull(metadataProperty);
        var propertyAttributes = metadataProperty!.GetCustomAttributesData();
        Assert.Single(propertyAttributes);
        Assert.Equal("property", propertyAttributes[0].ConstructorArguments[0].Value);
        Assert.Equal("PropertyName", propertyAttributes[0].NamedArguments.Single(a => a.MemberName == "Name").TypedValue.Value);
        var propertyDayArgument = propertyAttributes[0].NamedArguments.Single(a => a.MemberName == "Day").TypedValue;
        Assert.IsType<MetadataType>(propertyDayArgument.ArgumentType);
        Assert.Equal("System.DayOfWeek", propertyDayArgument.ArgumentType.FullName);
        Assert.Equal(DayOfWeek.Sunday, ConvertEnumValue<DayOfWeek>(propertyDayArgument));

        Assert.True(metadataType.IsDefined(attributeMetadataType, inherit: false));
        Assert.True(metadataMethod.IsDefined(attributeMetadataType, inherit: false));
        Assert.True(metadataField.IsDefined(attributeMetadataType, inherit: false));
    }

    private static void DefineRuntimeBackedMembers(TypeBuilder typeBuilder)
    {
        var valueField = typeBuilder.DefineField("Value", typeof(int), FieldAttributes.Public | FieldAttributes.Static);
        var instanceField = typeBuilder.DefineField("_instanceValue", typeof(int), FieldAttributes.Private);
        var addCountField = typeBuilder.DefineField("AddCount", typeof(int), FieldAttributes.Public | FieldAttributes.Static);
        var removeCountField = typeBuilder.DefineField("RemoveCount", typeof(int), FieldAttributes.Public | FieldAttributes.Static);

        var ctorBuilder = typeBuilder.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, Type.EmptyTypes);
        var ctorIl = ctorBuilder.GetILGenerator();
        ctorIl.Emit(OpCodes.Ldarg_0);
        ctorIl.Emit(OpCodes.Call, typeof(object).GetConstructor(Type.EmptyTypes)!);
        ctorIl.Emit(OpCodes.Ret);

        var getValueMethod = typeBuilder.DefineMethod("GetValue", MethodAttributes.Public | MethodAttributes.Static | MethodAttributes.HideBySig, typeof(int), Type.EmptyTypes);
        var getValueIl = getValueMethod.GetILGenerator();
        getValueIl.Emit(OpCodes.Ldsfld, valueField);
        getValueIl.Emit(OpCodes.Ret);

        var propertyBuilder = typeBuilder.DefineProperty("Value", PropertyAttributes.None, typeof(int), Type.EmptyTypes);
        var getProperty = typeBuilder.DefineMethod("get_Value", MethodAttributes.Public | MethodAttributes.Static | MethodAttributes.HideBySig | MethodAttributes.SpecialName, typeof(int), Type.EmptyTypes);
        var getPropertyIl = getProperty.GetILGenerator();
        getPropertyIl.Emit(OpCodes.Ldsfld, valueField);
        getPropertyIl.Emit(OpCodes.Ret);
        var setProperty = typeBuilder.DefineMethod("set_Value", MethodAttributes.Public | MethodAttributes.Static | MethodAttributes.HideBySig | MethodAttributes.SpecialName, typeof(void), new[] { typeof(int) });
        var setPropertyIl = setProperty.GetILGenerator();
        setPropertyIl.Emit(OpCodes.Ldarg_0);
        setPropertyIl.Emit(OpCodes.Stsfld, valueField);
        setPropertyIl.Emit(OpCodes.Ret);
        propertyBuilder.SetGetMethod(getProperty);
        propertyBuilder.SetSetMethod(setProperty);

        var setInstanceMethod = typeBuilder.DefineMethod("SetInstanceValue", MethodAttributes.Public | MethodAttributes.HideBySig, typeof(void), new[] { typeof(int) });
        var setInstanceIl = setInstanceMethod.GetILGenerator();
        setInstanceIl.Emit(OpCodes.Ldarg_0);
        setInstanceIl.Emit(OpCodes.Ldarg_1);
        setInstanceIl.Emit(OpCodes.Stfld, instanceField);
        setInstanceIl.Emit(OpCodes.Ret);

        var getInstanceMethod = typeBuilder.DefineMethod("GetInstanceValue", MethodAttributes.Public | MethodAttributes.HideBySig, typeof(int), Type.EmptyTypes);
        var getInstanceIl = getInstanceMethod.GetILGenerator();
        getInstanceIl.Emit(OpCodes.Ldarg_0);
        getInstanceIl.Emit(OpCodes.Ldfld, instanceField);
        getInstanceIl.Emit(OpCodes.Ret);

        var eventBuilder = typeBuilder.DefineEvent("Changed", EventAttributes.None, typeof(EventHandler));
        var addMethod = typeBuilder.DefineMethod("add_Changed", MethodAttributes.Public | MethodAttributes.Static | MethodAttributes.HideBySig | MethodAttributes.SpecialName, typeof(void), new[] { typeof(EventHandler) });
        var addIl = addMethod.GetILGenerator();
        addIl.Emit(OpCodes.Ldsfld, addCountField);
        addIl.Emit(OpCodes.Ldc_I4_1);
        addIl.Emit(OpCodes.Add);
        addIl.Emit(OpCodes.Stsfld, addCountField);
        addIl.Emit(OpCodes.Ret);
        var removeMethod = typeBuilder.DefineMethod("remove_Changed", MethodAttributes.Public | MethodAttributes.Static | MethodAttributes.HideBySig | MethodAttributes.SpecialName, typeof(void), new[] { typeof(EventHandler) });
        var removeIl = removeMethod.GetILGenerator();
        removeIl.Emit(OpCodes.Ldsfld, removeCountField);
        removeIl.Emit(OpCodes.Ldc_I4_1);
        removeIl.Emit(OpCodes.Add);
        removeIl.Emit(OpCodes.Stsfld, removeCountField);
        removeIl.Emit(OpCodes.Ret);
        eventBuilder.SetAddOnMethod(addMethod);
        eventBuilder.SetRemoveOnMethod(removeMethod);

        var payloadType = typeBuilder.DefineNestedType(
            "Payload",
            TypeAttributes.NestedPublic | TypeAttributes.Class | TypeAttributes.Sealed | TypeAttributes.AutoLayout | TypeAttributes.BeforeFieldInit);
        var payloadField = payloadType.DefineField("Text", typeof(string), FieldAttributes.Public);
        payloadType.DefineDefaultConstructor(MethodAttributes.Public);

        var echoPayload = typeBuilder.DefineMethod(
            "EchoPayload",
            MethodAttributes.Public | MethodAttributes.Static | MethodAttributes.HideBySig,
            typeof(string),
            new[] { payloadType });
        var echoPayloadIl = echoPayload.GetILGenerator();
        echoPayloadIl.Emit(OpCodes.Ldarg_0);
        echoPayloadIl.Emit(OpCodes.Ldfld, payloadField);
        echoPayloadIl.Emit(OpCodes.Ret);

        payloadType.CreateType();
    }

    private static MetadataLoadContext CreateLoadContext(IMetadataRuntimeBridge? runtimeBridge = null)
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
        return new MetadataLoadContext(resolver, runtimeBridge);
    }

    #pragma warning disable SYSLIB0050 // Formatter-based serialization is obsolete and should not be used.
    private static MethodSignature<Type> CreateFunctionPointerSignature(Type returnType, params Type[] parameterTypes)
    {
        var methodSignatureType = typeof(MethodSignature<>).Assembly.GetType("System.Reflection.Metadata.MethodSignature`1")!.MakeGenericType(typeof(Type));
        var signature = FormatterServices.GetUninitializedObject(methodSignatureType);
        SetSignatureField(signature, "<Header>k__BackingField", new SignatureHeader(SignatureKind.Method, SignatureCallingConvention.Default, SignatureAttributes.None));
        SetSignatureField(signature, "<ReturnType>k__BackingField", returnType);
        SetSignatureField(signature, "<RequiredParameterCount>k__BackingField", parameterTypes.Length);
        SetSignatureField(signature, "<GenericParameterCount>k__BackingField", 0);
        SetSignatureField(signature, "<ParameterTypes>k__BackingField", ImmutableArray.Create(parameterTypes));
        return (MethodSignature<Type>)signature;
    }
    #pragma warning restore SYSLIB0050

    private static void SetSignatureField(object instance, string name, object value)
    {
        var field = instance.GetType().GetField(name, BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public)
            ?? throw new InvalidOperationException(name);
        field.SetValue(instance, value);
    }

    private static TEnum ConvertEnumValue<TEnum>(CustomAttributeTypedArgument argument)
        where TEnum : struct, Enum
    {
        if (argument.Value is TEnum enumValue)
        {
            return enumValue;
        }

        if (argument.Value is null)
        {
            return default;
        }

        return (TEnum)Enum.ToObject(typeof(TEnum), argument.Value);
    }

    #nullable disable
    private sealed class PayloadBinder : Binder
    {
        public override object ChangeType(object value, Type type, CultureInfo culture)
        {
            if (value is string text && type.GetField("Text", BindingFlags.Public | BindingFlags.Instance) is { } field)
            {
                var instance = Activator.CreateInstance(type) ?? throw new InvalidOperationException("Unable to create payload instance.");
                field.SetValue(instance, text);
                return instance;
            }

            return Convert.ChangeType(value, type, culture ?? CultureInfo.InvariantCulture)!;
        }

        public override FieldInfo BindToField(BindingFlags bindingAttr, FieldInfo[] match, object value, CultureInfo culture)
            => SelectFirst(match);

        public override MethodBase BindToMethod(BindingFlags bindingAttr, MethodBase[] match, ref object[] args, ParameterModifier[] modifiers, CultureInfo culture, string[] names, out object state)
        {
            state = null!;
            return SelectFirst(match);
        }

        public override void ReorderArgumentArray(ref object[] args, object state)
        {
        }

        public override MethodBase SelectMethod(BindingFlags bindingAttr, MethodBase[] match, Type[] types, ParameterModifier[] modifiers)
        {
            foreach (var candidate in match)
            {
                var parameters = candidate.GetParameters();
                if (parameters.Length == types.Length && parameters.Select(p => p.ParameterType).SequenceEqual(types))
                {
                    return candidate;
                }
            }

            return SelectFirst(match);
        }

        public override PropertyInfo SelectProperty(BindingFlags bindingAttr, PropertyInfo[] match, Type returnType, Type[] indexes, ParameterModifier[] modifiers)
        {
            foreach (var property in match)
            {
                if ((returnType is null || property.PropertyType == returnType)
                    && (indexes.Length == 0 || ParametersMatch(property.GetIndexParameters(), indexes)))
                {
                    return property;
                }
            }

            return SelectFirst(match);
        }

        private static bool ParametersMatch(ParameterInfo[] parameters, Type[] indexes)
        {
            if (parameters.Length != indexes.Length)
            {
                return false;
            }

            for (var i = 0; i < parameters.Length; i++)
            {
                if (parameters[i].ParameterType != indexes[i])
                {
                    return false;
                }
            }

            return true;
        }

        private static T SelectFirst<T>(IReadOnlyList<T> items)
            where T : class
        {
            if (items.Count == 0)
            {
                throw new MissingMemberException("Binder could not resolve a match.");
            }

            return items[0] ?? throw new MissingMemberException("Binder resolved a null match.");
        }
    }
    #nullable restore
}
