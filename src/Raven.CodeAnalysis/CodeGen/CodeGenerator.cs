using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Reflection.PortableExecutable;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.CodeGen;

internal class CodeGenerator
{
    readonly Dictionary<ITypeSymbol, TypeGenerator> _typeGenerators = new Dictionary<ITypeSymbol, TypeGenerator>(SymbolEqualityComparer.Default);
    readonly Dictionary<SourceSymbol, MemberInfo> _mappings = new Dictionary<SourceSymbol, MemberInfo>(SymbolEqualityComparer.Default);
    readonly Dictionary<MemberBuilderCacheKey, MemberInfo> _constructedMappings = new Dictionary<MemberBuilderCacheKey, MemberInfo>();
    readonly Dictionary<ITypeParameterSymbol, Stack<Type>> _genericParameterMap = new(SymbolEqualityComparer.Default);
    readonly Dictionary<IMethodSymbol, MethodInfo> _runtimeMethodCache = new Dictionary<IMethodSymbol, MethodInfo>(SymbolEqualityComparer.Default);
    readonly Dictionary<IMethodSymbol, ConstructorInfo> _runtimeConstructorCache = new Dictionary<IMethodSymbol, ConstructorInfo>(SymbolEqualityComparer.Default);

    public IILBuilderFactory ILBuilderFactory { get; set; } = ReflectionEmitILBuilderFactory.Instance;

    internal IMethodSymbol? CurrentEmittingMethod { get; set; }

    public void AddMemberBuilder(SourceSymbol symbol, MemberInfo memberInfo)
        => AddMemberBuilder(symbol, memberInfo, substitution: default);

    public void AddMemberBuilder(SourceSymbol symbol, MemberInfo memberInfo, ImmutableArray<ITypeSymbol> substitution)
    {
        if (!substitution.IsDefaultOrEmpty)
        {
            _constructedMappings[new MemberBuilderCacheKey(symbol, substitution)] = memberInfo;
            return;
        }

        _mappings[symbol] = memberInfo;
    }

    public MemberInfo? GetMemberBuilder(SourceSymbol symbol)
    {
        if (_mappings.TryGetValue(symbol, out var memberInfo))
            return memberInfo;

        if (symbol is SourceFieldSymbol fieldSymbol)
        {
            if (fieldSymbol.ContainingType is not INamedTypeSymbol containingType)
                throw new KeyNotFoundException($"Missing containing type for field '{fieldSymbol.Name}'.");

            var typeGenerator = GetOrCreateTypeGenerator(containingType);

            if (typeGenerator.TypeBuilder is null)
                typeGenerator.DefineTypeBuilder();

            return typeGenerator.EnsureFieldBuilder(fieldSymbol);
        }

        throw new KeyNotFoundException($"Missing member builder for '{symbol.Name}'.");
    }

    internal bool HasMemberBuilder(SourceSymbol symbol)
        => _mappings.ContainsKey(symbol);

    internal bool TryGetMemberBuilder(SourceSymbol symbol, ImmutableArray<ITypeSymbol> substitution, out MemberInfo memberInfo)
    {
        if (!substitution.IsDefaultOrEmpty)
        {
            if (_constructedMappings.TryGetValue(new MemberBuilderCacheKey(symbol, substitution), out memberInfo!))
                return true;

            memberInfo = null!;
            return false;
        }

        return _mappings.TryGetValue(symbol, out memberInfo!);
    }

    internal bool TryGetMemberBuilder(SourceSymbol symbol, out MemberInfo memberInfo)
        => TryGetMemberBuilder(symbol, substitution: default, out memberInfo!);

    internal bool TryGetRuntimeMethod(IMethodSymbol symbol, out MethodInfo methodInfo)
        => _runtimeMethodCache.TryGetValue(symbol, out methodInfo);

    internal MethodInfo CacheRuntimeMethod(IMethodSymbol symbol, MethodInfo methodInfo)
    {
        _runtimeMethodCache[symbol] = methodInfo;
        return methodInfo;
    }

    internal bool TryGetRuntimeConstructor(IMethodSymbol symbol, out ConstructorInfo constructorInfo)
        => _runtimeConstructorCache.TryGetValue(symbol, out constructorInfo);

    internal ConstructorInfo CacheRuntimeConstructor(IMethodSymbol symbol, ConstructorInfo constructorInfo)
    {
        _runtimeConstructorCache[symbol] = constructorInfo;
        return constructorInfo;
    }

    internal Type CacheRuntimeTypeParameter(ITypeParameterSymbol symbol, Type type)
    {
        var stack = GetOrCreateGenericParameterStack(symbol);
        stack.Clear();
        stack.Push(type);
        return type;
    }

    internal void RegisterGenericParameters(ImmutableArray<ITypeParameterSymbol> parameters, GenericTypeParameterBuilder[] builders)
    {
        if (parameters.IsDefaultOrEmpty || builders.Length == 0)
            return;

        var count = Math.Min(parameters.Length, builders.Length);

        for (var i = 0; i < count; i++)
        {
            var parameter = parameters[i];
            var builder = builders[i];
            var stack = GetOrCreateGenericParameterStack(parameter);
            if (stack.Count == 0 || !ReferenceEquals(stack.Peek(), builder))
                stack.Push(builder);
        }

        if (count > 0 && parameters[0].ContainingSymbol is SynthesizedAsyncStateMachineTypeSymbol stateMachine)
        {
            foreach (var mapping in stateMachine.TypeParameterMappings)
            {
                if (!_genericParameterMap.TryGetValue(mapping.StateMachineParameter, out var synthesizedStack) || synthesizedStack.Count == 0)
                    continue;

                var mapped = synthesizedStack.Peek();
                var asyncStack = GetOrCreateGenericParameterStack(mapping.AsyncParameter);
                if (asyncStack.Count == 0)
                    asyncStack.Push(mapped);
            }
        }

        for (var i = 0; i < count; i++)
        {
            ApplyGenericParameterConstraints(parameters[i], builders[i]);
        }
    }

    internal void UnregisterGenericParameters(ImmutableArray<ITypeParameterSymbol> parameters)
    {
        if (parameters.IsDefaultOrEmpty)
            return;

        foreach (var parameter in parameters)
        {
            if (_genericParameterMap.TryGetValue(parameter, out var stack) && stack.Count > 0)
                stack.Pop();
        }
    }

    private Stack<Type> GetOrCreateGenericParameterStack(ITypeParameterSymbol parameter)
    {
        if (!_genericParameterMap.TryGetValue(parameter, out var stack))
        {
            stack = new Stack<Type>();
            _genericParameterMap[parameter] = stack;
        }

        return stack;
    }

    private void ApplyGenericParameterConstraints(ITypeParameterSymbol parameter, GenericTypeParameterBuilder builder)
    {
        var attributes = GenericParameterAttributes.None;

        if ((parameter.ConstraintKind & TypeParameterConstraintKind.ReferenceType) != 0)
            attributes |= GenericParameterAttributes.ReferenceTypeConstraint;

        if ((parameter.ConstraintKind & TypeParameterConstraintKind.ValueType) != 0)
            attributes |= GenericParameterAttributes.NotNullableValueTypeConstraint;

        if ((parameter.ConstraintKind & TypeParameterConstraintKind.Constructor) != 0)
            attributes |= GenericParameterAttributes.DefaultConstructorConstraint;

        builder.SetGenericParameterAttributes(attributes);

        if ((parameter.ConstraintKind & TypeParameterConstraintKind.NotNull) != 0)
            builder.SetCustomAttribute(CreateNullableAnnotationAttribute(isNullable: false));

        if (parameter.ConstraintTypes.IsDefaultOrEmpty)
            return;

        Type? baseType = null;
        List<Type>? interfaces = null;

        foreach (var constraintType in parameter.ConstraintTypes)
        {
            var constraintClrType = constraintType.GetClrType(this);

            if (constraintClrType.IsInterface)
            {
                interfaces ??= new List<Type>();
                interfaces.Add(constraintClrType);
            }
            else
            {
                baseType = constraintClrType;
            }
        }

        if (baseType is not null)
            builder.SetBaseTypeConstraint(baseType);

        if (interfaces is { Count: > 0 })
            builder.SetInterfaceConstraints(interfaces.ToArray());
    }

    private readonly Compilation _compilation;

    public Compilation Compilation => _compilation;

    public PersistedAssemblyBuilder AssemblyBuilder { get; private set; }
    public ModuleBuilder ModuleBuilder { get; private set; }

    private MethodBase? EntryPoint { get; set; }

    public Type? TypeUnionAttributeType { get; private set; }
    public Type? ExtensionMarkerNameAttributeType { get; private set; }
    public Type? NullType { get; private set; }
    public Type? NullableAttributeType { get; private set; }
    public Type? TupleElementNamesAttributeType { get; private set; }
    public Type? DiscriminatedUnionAttributeType { get; private set; }
    public Type? DiscriminatedUnionCaseAttributeType { get; private set; }
    public Type? ExtensionAttributeType { get; private set; }
    public Type? UnitType { get; private set; }
    ConstructorInfo? _nullableCtor;
    ConstructorInfo? _tupleElementNamesCtor;
    ConstructorInfo? _discriminatedUnionCtor;
    ConstructorInfo? _discriminatedUnionCaseCtor;
    ConstructorInfo? _extensionMarkerNameCtor;
    ConstructorInfo? _extensionAttributeCtor;

    readonly bool _emitTypeUnionAttribute = true;
    readonly bool _emitNullType = true;
    bool _emitExtensionMarkerNameAttribute = true;

    internal void ApplyCustomAttributes(ImmutableArray<AttributeData> attributes, Action<CustomAttributeBuilder> apply)
    {
        if (attributes.IsDefaultOrEmpty)
            return;

        foreach (var attribute in attributes)
        {
            var builder = CreateCustomAttribute(attribute);
            if (builder is not null)
                apply(builder);
        }
    }

    internal CustomAttributeBuilder? CreateCustomAttribute(AttributeData attribute)
    {
        if (attribute is null)
            return null;

        var constructor = ResolveAttributeConstructor(attribute);
        if (constructor is null)
            return null;

        var parameters = attribute.AttributeConstructor.Parameters;
        var args = new object?[attribute.ConstructorArguments.Length];

        for (var i = 0; i < attribute.ConstructorArguments.Length; i++)
        {
            var parameterType = i < parameters.Length ? parameters[i].Type : null;
            var parameterClrType = parameterType is not null ? parameterType.GetClrType(this) : null;
            args[i] = GetAttributeValue(attribute.ConstructorArguments[i], parameterClrType, parameterType);
        }

        var attributeType = constructor.DeclaringType ?? attribute.AttributeClass.GetClrType(this);

        List<PropertyInfo>? properties = null;
        List<object?>? propertyValues = null;
        List<FieldInfo>? fields = null;
        List<object?>? fieldValues = null;

        foreach (var (name, value) in attribute.NamedArguments)
        {
            var property = attributeType.GetProperty(name, BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);
            if (property is not null)
            {
                properties ??= new List<PropertyInfo>();
                propertyValues ??= new List<object?>();
                properties.Add(property);
                propertyValues.Add(GetAttributeValue(value, property.PropertyType, value.Type));
                continue;
            }

            var field = attributeType.GetField(name, BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);
            if (field is not null)
            {
                fields ??= new List<FieldInfo>();
                fieldValues ??= new List<object?>();
                fields.Add(field);
                fieldValues.Add(GetAttributeValue(value, field.FieldType, value.Type));
            }
        }

        return new CustomAttributeBuilder(
            constructor,
            args,
            properties is not null ? properties.ToArray() : Array.Empty<PropertyInfo>(),
            propertyValues is not null ? propertyValues.ToArray() : Array.Empty<object?>(),
            fields is not null ? fields.ToArray() : Array.Empty<FieldInfo>(),
            fieldValues is not null ? fieldValues.ToArray() : Array.Empty<object?>());
    }

    private ConstructorInfo? ResolveAttributeConstructor(AttributeData attribute)
    {
        var constructorSymbol = attribute.AttributeConstructor;

        if (constructorSymbol is SourceMethodSymbol sourceConstructor)
        {
            if (GetMemberBuilder(sourceConstructor) is ConstructorInfo sourceCtorInfo)
                return sourceCtorInfo;
        }

        var attributeType = attribute.AttributeClass.GetClrType(this);
        var parameterTypes = constructorSymbol.Parameters
            .Select(p => p.Type.GetClrType(this))
            .ToArray();

        return attributeType.GetConstructor(parameterTypes);
    }

    private object? GetAttributeValue(TypedConstant constant, Type? targetClrType, ITypeSymbol? targetSymbol)
    {
        switch (constant.Kind)
        {
            case TypedConstantKind.Null:
                return null;
            case TypedConstantKind.Type:
                return constant.Value switch
                {
                    ITypeSymbol typeSymbol => typeSymbol.GetClrType(this),
                    Type type => type,
                    _ => null
                };
            case TypedConstantKind.Array:
                {
                    var values = constant.Values;
                    if (values.IsDefaultOrEmpty)
                        return Array.CreateInstance((targetClrType ?? typeof(object)).GetElementType() ?? typeof(object), 0);

                    var arraySymbol = targetSymbol as IArrayTypeSymbol ?? constant.Type as IArrayTypeSymbol;
                    var elementSymbol = arraySymbol?.ElementType;
                    var elementClrType = targetClrType?.GetElementType()
                        ?? (elementSymbol is not null ? elementSymbol.GetClrType(this) : typeof(object));

                    var array = Array.CreateInstance(elementClrType, values.Length);
                    for (var i = 0; i < values.Length; i++)
                    {
                        array.SetValue(GetAttributeValue(values[i], elementClrType, elementSymbol), i);
                    }

                    return array;
                }
            case TypedConstantKind.Enum:
                {
                    if (constant.Value is null)
                        return null;

                    var enumType = targetClrType ?? (constant.Type as INamedTypeSymbol)?.GetClrType(this);
                    if (enumType is not null && enumType.IsEnum)
                        return Enum.ToObject(enumType, constant.Value);

                    return constant.Value;
                }
            case TypedConstantKind.Primitive:
                return constant.Value;
            case TypedConstantKind.Error:
            default:
                return null;
        }
    }

    internal CustomAttributeBuilder? CreateNullableAttribute(ITypeSymbol type)
    {
        var needsNullable = false;

        if (type is NullableTypeSymbol nt && !nt.UnderlyingType.IsValueType)
        {
            needsNullable = true;
        }
        else if (type is ITypeUnionSymbol u)
        {
            var flat = Flatten(u.Types).ToArray();
            if (flat.Any(t => t.TypeKind == TypeKind.Null))
            {
                var nonNull = flat.Where(t => t.TypeKind != TypeKind.Null).ToArray();
                if (!(nonNull.Length == 1 && nonNull[0].IsValueType))
                    needsNullable = true;
            }
        }

        if (!needsNullable)
            return null;

        EnsureNullableAttributeType();
        return new CustomAttributeBuilder(_nullableCtor!, new object[] { (byte)2 });
    }

    internal CustomAttributeBuilder CreateNullableAnnotationAttribute(bool isNullable)
    {
        EnsureNullableAttributeType();
        return new CustomAttributeBuilder(_nullableCtor!, new object[] { isNullable ? (byte)2 : (byte)1 });
    }

    internal CustomAttributeBuilder? CreateTupleElementNamesAttribute(ITypeSymbol type)
    {
        if (type is null)
            return null;

        var transformNames = new List<string?>();
        if (!TryCollectTupleElementNames(type, transformNames))
            return null;

        EnsureTupleElementNamesAttributeType();
        return new CustomAttributeBuilder(_tupleElementNamesCtor!, new object?[] { transformNames.ToArray() });
    }

    internal CustomAttributeBuilder CreateDiscriminatedUnionAttribute()
    {
        EnsureDiscriminatedUnionAttributeType();
        return new CustomAttributeBuilder(_discriminatedUnionCtor!, Array.Empty<object?>());
    }

    internal CustomAttributeBuilder CreateDiscriminatedUnionCaseAttribute(Type unionType)
    {
        if (unionType is null)
            throw new ArgumentNullException(nameof(unionType));

        EnsureDiscriminatedUnionCaseAttributeType();
        return new CustomAttributeBuilder(_discriminatedUnionCaseCtor!, new object?[] { unionType });
    }

    static IEnumerable<ITypeSymbol> Flatten(IEnumerable<ITypeSymbol> types)
        => types.SelectMany(t => t is ITypeUnionSymbol u ? Flatten(u.Types) : new[] { t });

    void EnsureNullableAttributeType()
    {
        if (NullableAttributeType is not null)
            return;

        var attrBuilder = ModuleBuilder.DefineType(
            "System.Runtime.CompilerServices.NullableAttribute",
            TypeAttributes.Public | TypeAttributes.Class | TypeAttributes.Sealed,
            typeof(Attribute));

        var ctorBuilder = attrBuilder.DefineConstructor(
            MethodAttributes.Public,
            CallingConventions.Standard,
            new[] { typeof(byte) });

        var il = ctorBuilder.GetILGenerator();
        var baseCtor = typeof(Attribute).GetConstructor(
            BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic,
            null,
            Type.EmptyTypes,
            null);
        if (baseCtor is null)
            throw new InvalidOperationException("Missing Attribute base constructor.");

        il.Emit(OpCodes.Ldarg_0);
        il.Emit(OpCodes.Call, baseCtor);
        il.Emit(OpCodes.Ret);

        NullableAttributeType = attrBuilder.CreateType();
        _nullableCtor = NullableAttributeType.GetConstructor(new[] { typeof(byte) });
    }

    void EnsureTupleElementNamesAttributeType()
    {
        if (TupleElementNamesAttributeType is not null)
            return;

        TupleElementNamesAttributeType = Compilation.ResolveRuntimeType("System.Runtime.CompilerServices.TupleElementNamesAttribute")
            ?? throw new InvalidOperationException("Type 'System.Runtime.CompilerServices.TupleElementNamesAttribute' not found in runtime assemblies.");

        _tupleElementNamesCtor = TupleElementNamesAttributeType.GetConstructor(new[] { typeof(string[]) })
            ?? throw new InvalidOperationException("Missing TupleElementNamesAttribute(string[]) constructor.");
    }

    void EnsureDiscriminatedUnionAttributeType()
    {
        if (DiscriminatedUnionAttributeType is not null)
            return;

        if (!_compilation.Options.EmbedCoreTypes)
        {
            TryBindRuntimeCoreTypes();
            if (DiscriminatedUnionAttributeType is not null)
                return;
        }

        var attributeType = Compilation.GetTypeByMetadataName("System.Attribute").GetClrType(this);

        var attrBuilder = ModuleBuilder.DefineType(
            "System.Runtime.CompilerServices.DiscriminatedUnionAttribute",
            TypeAttributes.Public | TypeAttributes.Class | TypeAttributes.Sealed,
            attributeType);

        var ctorBuilder = attrBuilder.DefineConstructor(
            MethodAttributes.Public,
            CallingConventions.Standard,
            Type.EmptyTypes);

        var il = ctorBuilder.GetILGenerator();
        var baseCtor = attributeType.GetConstructor(
            BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic,
            binder: null,
            types: Type.EmptyTypes,
            modifiers: null);
        if (baseCtor is null)
            throw new InvalidOperationException("Missing Attribute base constructor.");

        il.Emit(OpCodes.Ldarg_0);
        il.Emit(OpCodes.Call, baseCtor);
        il.Emit(OpCodes.Ret);

        DiscriminatedUnionAttributeType = attrBuilder.CreateType();
        _discriminatedUnionCtor = DiscriminatedUnionAttributeType.GetConstructor(Type.EmptyTypes)
            ?? throw new InvalidOperationException("Missing DiscriminatedUnionAttribute() constructor.");
    }

    void EnsureDiscriminatedUnionCaseAttributeType()
    {
        if (DiscriminatedUnionCaseAttributeType is not null)
            return;

        if (!_compilation.Options.EmbedCoreTypes)
        {
            TryBindRuntimeCoreTypes();
            if (DiscriminatedUnionCaseAttributeType is not null)
                return;
        }

        var attributeType = Compilation.GetTypeByMetadataName("System.Attribute").GetClrType(this);
        var typeType = Compilation.GetSpecialType(SpecialType.System_Type).GetClrType(this);

        var attrBuilder = ModuleBuilder.DefineType(
            "System.Runtime.CompilerServices.DiscriminatedUnionCaseAttribute",
            TypeAttributes.Public | TypeAttributes.Class | TypeAttributes.Sealed,
            attributeType);

        var unionTypeField = attrBuilder.DefineField(
            "_discriminatedUnionType",
            typeType,
            FieldAttributes.Private | FieldAttributes.InitOnly);

        var propertyBuilder = attrBuilder.DefineProperty(
            "DiscriminatedUnionType",
            PropertyAttributes.None,
            typeType,
            null);

        var getterMethod = attrBuilder.DefineMethod(
            "get_DiscriminatedUnionType",
            MethodAttributes.Public | MethodAttributes.HideBySig | MethodAttributes.SpecialName,
            typeType,
            Type.EmptyTypes);

        var getterIl = getterMethod.GetILGenerator();
        getterIl.Emit(OpCodes.Ldarg_0);
        getterIl.Emit(OpCodes.Ldfld, unionTypeField);
        getterIl.Emit(OpCodes.Ret);

        propertyBuilder.SetGetMethod(getterMethod);

        var ctorBuilder = attrBuilder.DefineConstructor(
            MethodAttributes.Public,
            CallingConventions.Standard,
            new[] { typeType });

        var il = ctorBuilder.GetILGenerator();
        var baseCtor = attributeType.GetConstructor(
            BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic,
            binder: null,
            types: Type.EmptyTypes,
            modifiers: null);
        if (baseCtor is null)
            throw new InvalidOperationException("Missing Attribute base constructor.");

        il.Emit(OpCodes.Ldarg_0);
        il.Emit(OpCodes.Call, baseCtor);
        il.Emit(OpCodes.Ldarg_0);
        il.Emit(OpCodes.Ldarg_1);
        il.Emit(OpCodes.Stfld, unionTypeField);
        il.Emit(OpCodes.Ret);

        DiscriminatedUnionCaseAttributeType = attrBuilder.CreateType();
        _discriminatedUnionCaseCtor = DiscriminatedUnionCaseAttributeType.GetConstructor(new[] { typeType })
            ?? throw new InvalidOperationException("Missing DiscriminatedUnionCaseAttribute(Type) constructor.");
    }

    bool TryCollectTupleElementNames(ITypeSymbol type, List<string?> transformNames)
    {
        if (type is null)
            return false;

        var start = transformNames.Count;
        var hasAnyNames = false;

        switch (type)
        {
            case INamedTypeSymbol named when named.TypeKind == TypeKind.Tuple:
                {
                    var tupleElements = named.TupleElements;
                    if (tupleElements.IsDefaultOrEmpty)
                        break;

                    for (var i = 0; i < tupleElements.Length; i++)
                    {
                        var element = tupleElements[i];
                        var elementName = element.Name;
                        var isExplicit = !string.IsNullOrEmpty(elementName) && elementName != $"Item{i + 1}";
                        transformNames.Add(isExplicit ? elementName : null);

                        if (isExplicit)
                            hasAnyNames = true;

                        if (TryCollectTupleElementNames(element.Type, transformNames))
                            hasAnyNames = true;
                    }

                    break;
                }
            case NullableTypeSymbol nullableType:
                hasAnyNames |= TryCollectTupleElementNames(nullableType.UnderlyingType, transformNames);
                break;
            case IArrayTypeSymbol arrayType:
                hasAnyNames |= TryCollectTupleElementNames(arrayType.ElementType, transformNames);
                break;
            case IPointerTypeSymbol pointerType:
                hasAnyNames |= TryCollectTupleElementNames(pointerType.PointedAtType, transformNames);
                break;
            case IAddressTypeSymbol addressType:
                hasAnyNames |= TryCollectTupleElementNames(addressType.ReferencedType, transformNames);
                break;
            case ITypeUnionSymbol unionType:
                foreach (var member in unionType.Types)
                    hasAnyNames |= TryCollectTupleElementNames(member, transformNames);
                break;
            case INamedTypeSymbol namedType:
                if (!namedType.TypeArguments.IsDefaultOrEmpty)
                {
                    foreach (var typeArgument in namedType.TypeArguments)
                        hasAnyNames |= TryCollectTupleElementNames(typeArgument, transformNames);
                }
                break;
        }

        if (!hasAnyNames)
        {
            var added = transformNames.Count - start;
            if (added > 0)
                transformNames.RemoveRange(start, added);
        }

        return hasAnyNames;
    }

    public CodeGenerator(Compilation compilation)
    {
        _compilation = compilation;
    }

    public Type? GetTypeBuilder(INamedTypeSymbol namedTypeSymbol)
    {
        var e = _typeGenerators[namedTypeSymbol];

        return e.Type ?? e?.TypeBuilder;
    }

    public void Emit(Stream peStream, Stream? pdbStream)
    {
        try
        {
            var assemblyName = new AssemblyName(_compilation.AssemblyName)
            {
                Version = new Version(1, 0, 0, 0)
            };

            AssemblyBuilder = new PersistedAssemblyBuilder(assemblyName, _compilation.RuntimeCoreAssembly);
            ModuleBuilder = AssemblyBuilder.DefineDynamicModule(_compilation.AssemblyName);

            DetermineShimTypeRequirements();

            if (!_compilation.Options.EmbedCoreTypes)
                TryBindRuntimeCoreTypes();

            if (_emitTypeUnionAttribute && (TypeUnionAttributeType is null || _compilation.Options.EmbedCoreTypes))
                CreateTypeUnionAttribute();
            if (_emitExtensionMarkerNameAttribute && (ExtensionMarkerNameAttributeType is null || _compilation.Options.EmbedCoreTypes))
                CreateExtensionMarkerNameAttributeType();
            if (_emitNullType && (NullType is null || _compilation.Options.EmbedCoreTypes))
                CreateNullStruct();
            if (UnitType is null || _compilation.Options.EmbedCoreTypes)
                CreateUnitStruct();

            DefineTypeBuilders();

            DefineMemberBuilders();

            EmitMemberILBodies();

            CreateTypes();

            var entryPointSymbol = _compilation.Options.OutputKind == OutputKind.ConsoleApplication
                ? _compilation.GetEntryPoint()
                : null;
            MethodGenerator? entryPointGenerator = null;

            if (entryPointSymbol is not null)
            {
                foreach (var typeGenerator in _typeGenerators.Values)
                {
                    var generator = typeGenerator.GetMethodGenerator(entryPointSymbol);
                    if (generator is not null)
                    {
                        entryPointGenerator = generator;
                        break;
                    }
                }

                if (entryPointGenerator is null)
                {
                    throw new InvalidOperationException("Failed to locate entry point method.");
                }
            }
            else
            {
                entryPointGenerator = _typeGenerators.Values
                    .SelectMany(x => x.MethodGenerators)
                    .FirstOrDefault(x => x.IsEntryPointCandidate);
            }

            EntryPoint = entryPointGenerator?.MethodBase;

            MetadataBuilder metadataBuilder = AssemblyBuilder.GenerateMetadata(out BlobBuilder ilStream, out _, out MetadataBuilder pdbBuilder);
            MethodDefinitionHandle entryPointHandle = EntryPoint is not null
                ? MetadataTokens.MethodDefinitionHandle(EntryPoint.MetadataToken)
                : default;
            DebugDirectoryBuilder debugDirectoryBuilder = EmitPdb(pdbBuilder, metadataBuilder.GetRowCounts(), entryPointHandle);

            Characteristics imageCharacteristics = _compilation.Options.OutputKind switch
            {
                OutputKind.ConsoleApplication => Characteristics.ExecutableImage,
                OutputKind.DynamicallyLinkedLibrary => Characteristics.Dll,
                _ => Characteristics.Dll,
            };

            ManagedPEBuilder peBuilder = new ManagedPEBuilder(
                            header: new PEHeaderBuilder(imageCharacteristics: imageCharacteristics, subsystem: Subsystem.WindowsCui),
                            metadataRootBuilder: new MetadataRootBuilder(metadataBuilder),
                            ilStream: ilStream,
                            debugDirectoryBuilder: debugDirectoryBuilder,
                            entryPoint: entryPointHandle);

            BlobBuilder peBlob = new BlobBuilder();
            peBuilder.Serialize(peBlob);

            peBlob.WriteContentTo(peStream);
        }
        catch (Exception ex)
        {
            if (CurrentEmittingMethod is { } method)
                throw new InvalidOperationException($"Emission failed while processing method '{method.ToDisplayString()}'", ex);

            throw;
        }
    }

    private void DetermineShimTypeRequirements()
    {
        _emitExtensionMarkerNameAttribute = Compilation.SyntaxTrees
            .SelectMany(tree => tree.GetRoot().DescendantNodes().OfType<ExtensionDeclarationSyntax>())
            .Any();

        var types = Compilation.Module.GlobalNamespace
            .GetAllMembersRecursive()
            .OfType<ITypeSymbol>()
            .Where(t => t.DeclaringSyntaxReferences.Length > 0);

        foreach (var type in types)
        {
            foreach (var member in type.GetMembers())
            {
                switch (member)
                {
                    case IMethodSymbol method:
                        CheckType(method.ReturnType);
                        foreach (var p in method.Parameters)
                            CheckType(p.Type);
                        break;
                    case IPropertySymbol prop:
                        CheckType(prop.Type);
                        break;
                    case IFieldSymbol field:
                        CheckType(field.Type);
                        break;
                }
            }
        }

        static void CheckType(ITypeSymbol typeSymbol)
        {
            if (typeSymbol is null)
                return;

            if (typeSymbol is LiteralTypeSymbol literal)
            {
                CheckType(literal.UnderlyingType);
                return;
            }


            /*
                            if (typeSymbol.IsTypeUnion && typeSymbol is ITypeUnionSymbol union)
                            {
                                _emitTypeUnionAttribute = true;
                                foreach (var t in union.Types)
                                {
                                    if (t.TypeKind == TypeKind.Null)
                                        _emitNullType = true;
                                    CheckType(t);
                                }
                                return;
                            }

                            if (typeSymbol.TypeKind == TypeKind.Null)
                            {
                                _emitNullType = true;
                                return;
                            }
            */

            if (typeSymbol is INamedTypeSymbol named && named.IsGenericType)
            {
                foreach (var arg in named.TypeArguments)
                    CheckType(arg);
            }
            else if (typeSymbol is IArrayTypeSymbol array)
            {
                CheckType(array.ElementType);
            }
        }
    }

    private void TryBindRuntimeCoreTypes()
    {
        TypeUnionAttributeType ??= Compilation.ResolveRuntimeType("System.Runtime.CompilerServices.TypeUnionAttribute")
            ?? Compilation.ResolveRuntimeType("TypeUnionAttribute");
        ExtensionMarkerNameAttributeType ??= Compilation.ResolveRuntimeType("System.Runtime.CompilerServices.ExtensionMarkerNameAttribute");
        _extensionMarkerNameCtor ??= ExtensionMarkerNameAttributeType?.GetConstructor(new[] { typeof(string) });
        ExtensionAttributeType ??= Compilation.ResolveRuntimeType("System.Runtime.CompilerServices.ExtensionAttribute");
        _extensionAttributeCtor ??= ExtensionAttributeType?.GetConstructor(Type.EmptyTypes);
        NullType ??= Compilation.ResolveRuntimeType("Null");
        UnitType ??= Compilation.ResolveRuntimeType("System.Unit");

        if (DiscriminatedUnionAttributeType is null)
        {
            DiscriminatedUnionAttributeType = Compilation.ResolveRuntimeType(
                "System.Runtime.CompilerServices.DiscriminatedUnionAttribute");
            _discriminatedUnionCtor = DiscriminatedUnionAttributeType?.GetConstructor(Type.EmptyTypes);
        }

        if (DiscriminatedUnionCaseAttributeType is null)
        {
            DiscriminatedUnionCaseAttributeType = Compilation.ResolveRuntimeType(
                "System.Runtime.CompilerServices.DiscriminatedUnionCaseAttribute");

            var typeType = Compilation.GetSpecialType(SpecialType.System_Type).GetClrType(this);

            _discriminatedUnionCaseCtor = DiscriminatedUnionCaseAttributeType?.GetConstructor(new[] { typeType });
        }
    }

    private void CreateTypeUnionAttribute()
    {
        if (TypeUnionAttributeType is not null)
            return;

        // Define the attribute class
        var attrBuilder = ModuleBuilder.DefineType(
            "System.Runtime.CompilerServices.TypeUnionAttribute",
            TypeAttributes.Public | TypeAttributes.Class | TypeAttributes.Sealed,
            typeof(Attribute));

        // Mark as AttributeUsage (optional)
        var attrUsageCtor = typeof(AttributeUsageAttribute).GetConstructor([typeof(AttributeTargets)]);
        var attrUsageBuilder = new CustomAttributeBuilder(attrUsageCtor, [AttributeTargets.Parameter | AttributeTargets.Field | AttributeTargets.ReturnValue | AttributeTargets.Property]);
        attrBuilder.SetCustomAttribute(attrUsageBuilder);

        // Define a private readonly field: private readonly object[] _types;
        var typesField = attrBuilder.DefineField(
            "_types",
            typeof(object[]),
            FieldAttributes.Private | FieldAttributes.InitOnly);

        // Define the public property: public object[] Types { get; }
        var propBuilder = attrBuilder.DefineProperty(
            "Types",
            PropertyAttributes.None,
            typeof(object[]),
            null);

        var getterMethod = attrBuilder.DefineMethod(
            "get_Types",
            MethodAttributes.Public | MethodAttributes.HideBySig | MethodAttributes.SpecialName,
            typeof(object[]),
            Type.EmptyTypes);

        var ilGet = getterMethod.GetILGenerator();
        ilGet.Emit(OpCodes.Ldarg_0); // this
        ilGet.Emit(OpCodes.Ldfld, typesField); // _types
        ilGet.Emit(OpCodes.Ret);

        // Attach the getter to the property
        propBuilder.SetGetMethod(getterMethod);

        // Define the constructor: public TypeUnionAttribute(params object[] types)
        var ctorBuilder = attrBuilder.DefineConstructor(
            MethodAttributes.Public,
            CallingConventions.Standard,
            new[] { typeof(object[]) });

        // Add [ParamArray] attribute to the parameter
        var paramArrayAttrCtor = typeof(ParamArrayAttribute).GetConstructor(Type.EmptyTypes);
        var paramBuilder = ctorBuilder.DefineParameter(1, ParameterAttributes.None, "types");
        var paramArrayAttr = new CustomAttributeBuilder(paramArrayAttrCtor, Array.Empty<object>());
        paramBuilder.SetCustomAttribute(paramArrayAttr);

        // Emit constructor body
        var ilCtor = ctorBuilder.GetILGenerator();
        var attributeCtor = typeof(Attribute).GetConstructor(BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic, null, Type.EmptyTypes, null);
        if (attributeCtor is null)
            throw new InvalidOperationException("Missing Attribute base constructor.");

        ilCtor.Emit(OpCodes.Ldarg_0);
        ilCtor.Emit(OpCodes.Call, attributeCtor);

        ilCtor.Emit(OpCodes.Ldarg_0); // this
        ilCtor.Emit(OpCodes.Ldarg_1); // types (argument)
        ilCtor.Emit(OpCodes.Stfld, typesField); // this._types = types

        ilCtor.Emit(OpCodes.Ret);

        // Create the type
        TypeUnionAttributeType = attrBuilder.CreateType();
    }

    private void CreateExtensionMarkerNameAttributeType()
    {
        if (ExtensionMarkerNameAttributeType is not null)
            return;

        var attrBuilder = ModuleBuilder.DefineType(
            "System.Runtime.CompilerServices.ExtensionMarkerNameAttribute",
            TypeAttributes.Public | TypeAttributes.Class | TypeAttributes.Sealed,
            typeof(Attribute));

        var attrUsageCtor = typeof(AttributeUsageAttribute).GetConstructor([typeof(AttributeTargets)]);
        var attrUsageBuilder = new CustomAttributeBuilder(
            attrUsageCtor,
            [AttributeTargets.Class | AttributeTargets.Struct | AttributeTargets.Enum | AttributeTargets.Method | AttributeTargets.Property | AttributeTargets.Field | AttributeTargets.Event | AttributeTargets.Interface | AttributeTargets.Delegate]);
        attrBuilder.SetCustomAttribute(attrUsageBuilder);

        var nameField = attrBuilder.DefineField(
            "<Name>k__BackingField",
            typeof(string),
            FieldAttributes.Private | FieldAttributes.InitOnly);

        var propBuilder = attrBuilder.DefineProperty(
            "Name",
            PropertyAttributes.None,
            typeof(string),
            null);

        var getterMethod = attrBuilder.DefineMethod(
            "get_Name",
            MethodAttributes.Public | MethodAttributes.HideBySig | MethodAttributes.SpecialName,
            typeof(string),
            Type.EmptyTypes);

        var ilGet = getterMethod.GetILGenerator();
        ilGet.Emit(OpCodes.Ldarg_0);
        ilGet.Emit(OpCodes.Ldfld, nameField);
        ilGet.Emit(OpCodes.Ret);

        propBuilder.SetGetMethod(getterMethod);

        var ctorBuilder = attrBuilder.DefineConstructor(
            MethodAttributes.Public,
            CallingConventions.Standard,
            new[] { typeof(string) });

        var ilCtor = ctorBuilder.GetILGenerator();
        var attributeCtor = typeof(Attribute).GetConstructor(BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic, null, Type.EmptyTypes, null);
        if (attributeCtor is null)
            throw new InvalidOperationException("Missing Attribute base constructor.");

        ilCtor.Emit(OpCodes.Ldarg_0);
        ilCtor.Emit(OpCodes.Call, attributeCtor);
        ilCtor.Emit(OpCodes.Ldarg_0);
        ilCtor.Emit(OpCodes.Ldarg_1);
        ilCtor.Emit(OpCodes.Stfld, nameField);
        ilCtor.Emit(OpCodes.Ret);

        ExtensionMarkerNameAttributeType = attrBuilder.CreateType();
        _extensionMarkerNameCtor = ExtensionMarkerNameAttributeType.GetConstructor(new[] { typeof(string) });
    }

    internal CustomAttributeBuilder? CreateExtensionMarkerNameAttribute(string markerName)
    {
        if (string.IsNullOrWhiteSpace(markerName))
            return null;

        if (ExtensionMarkerNameAttributeType is null || _extensionMarkerNameCtor is null)
        {
            if (_emitExtensionMarkerNameAttribute)
                CreateExtensionMarkerNameAttributeType();
        }

        if (_extensionMarkerNameCtor is null)
            return null;

        return new CustomAttributeBuilder(_extensionMarkerNameCtor, new object[] { markerName });
    }

    internal CustomAttributeBuilder? CreateExtensionAttributeBuilder()
    {
        if (ExtensionAttributeType is null)
            ExtensionAttributeType = Compilation.ResolveRuntimeType("System.Runtime.CompilerServices.ExtensionAttribute");

        _extensionAttributeCtor ??= ExtensionAttributeType?.GetConstructor(Type.EmptyTypes);
        if (_extensionAttributeCtor is null)
            return null;

        return new CustomAttributeBuilder(_extensionAttributeCtor, Array.Empty<object>());
    }

    private void CreateNullStruct()
    {
        if (NullType is not null)
            return;

        var nullBuilder = ModuleBuilder.DefineType(
            "Null",
            TypeAttributes.Public | TypeAttributes.Sealed | TypeAttributes.SequentialLayout,
            typeof(ValueType));

        NullType = nullBuilder.CreateType();
    }

    private void CreateUnitStruct()
    {
        if (UnitType is not null)
            return;

        var unitBuilder = ModuleBuilder.DefineType(
            "System.Unit",
            TypeAttributes.Public | TypeAttributes.Sealed | TypeAttributes.SequentialLayout,
            Compilation.GetTypeByMetadataName("System.ValueType").GetClrType(this));

        var valueField = unitBuilder.DefineField(
            "Value",
            unitBuilder,
            FieldAttributes.Public | FieldAttributes.Static | FieldAttributes.InitOnly);

        var equalsMethod = unitBuilder.DefineMethod(
            "Equals",
            MethodAttributes.Public | MethodAttributes.HideBySig | MethodAttributes.Virtual | MethodAttributes.Final,
            Compilation.GetTypeByMetadataName("System.Boolean").GetClrType(this),
            new[] { unitBuilder });
        var ilEquals = equalsMethod.GetILGenerator();
        ilEquals.Emit(OpCodes.Ldc_I4_1);
        ilEquals.Emit(OpCodes.Ret);

        var equalsObjMethod = unitBuilder.DefineMethod(
            "Equals",
            MethodAttributes.Public | MethodAttributes.HideBySig | MethodAttributes.Virtual | MethodAttributes.Final,
            Compilation.GetTypeByMetadataName("System.Boolean").GetClrType(this),
            new[] { Compilation.GetTypeByMetadataName("System.Object").GetClrType(this) });
        var ilEqualsObj = equalsObjMethod.GetILGenerator();
        ilEqualsObj.Emit(OpCodes.Ldarg_1);
        ilEqualsObj.Emit(OpCodes.Isinst, unitBuilder);
        ilEqualsObj.Emit(OpCodes.Ldnull);
        ilEqualsObj.Emit(OpCodes.Cgt_Un);
        ilEqualsObj.Emit(OpCodes.Ret);

        var getHashCodeMethod = unitBuilder.DefineMethod(
            "GetHashCode",
            MethodAttributes.Public | MethodAttributes.HideBySig | MethodAttributes.Virtual,
            Compilation.GetTypeByMetadataName("System.Int32").GetClrType(this),
            Type.EmptyTypes);
        var ilHash = getHashCodeMethod.GetILGenerator();
        ilHash.Emit(OpCodes.Ldc_I4_0);
        ilHash.Emit(OpCodes.Ret);

        var toStringMethod = unitBuilder.DefineMethod(
            "ToString",
            MethodAttributes.Public | MethodAttributes.HideBySig | MethodAttributes.Virtual,
            Compilation.GetTypeByMetadataName("System.String").GetClrType(this),
            Type.EmptyTypes);
        var ilToString = toStringMethod.GetILGenerator();
        ilToString.Emit(OpCodes.Ldstr, "()");
        ilToString.Emit(OpCodes.Ret);

        UnitType = unitBuilder.CreateType();
    }

    private void DefineTypeBuilders()
    {
        EnsureAsyncStateMachines();
        EnsureIteratorStateMachines();

        var declaredTypes = Compilation.Module.GlobalNamespace
            .GetAllMembersRecursive()
            .OfType<ITypeSymbol>()
            .Where(t => t.DeclaringSyntaxReferences.Length > 0)
            .ToArray();

        var extensionTypes = Compilation.SyntaxTrees
            .Select(tree => (tree, model: Compilation.GetSemanticModel(tree)))
            .SelectMany(tuple => tuple.tree.GetRoot()
                .DescendantNodes()
                .OfType<ExtensionDeclarationSyntax>()
                .Select(decl => tuple.model.GetDeclaredSymbol(decl)))
            .OfType<ITypeSymbol>()
            .Where(t => t.DeclaringSyntaxReferences.Length > 0)
            .ToArray();

        var allTypes = declaredTypes
            .Cast<ISymbol>()
            .Concat(extensionTypes)
            .Distinct(SymbolEqualityComparer.Default)
            .OfType<ITypeSymbol>()
            .ToArray();

        var unionCaseTypes = declaredTypes
            .OfType<IDiscriminatedUnionSymbol>()
            .SelectMany(union => union.Cases)
            .OfType<ITypeSymbol>()
            .Where(t => t.DeclaringSyntaxReferences.Length > 0)
            .ToArray();

        var synthesizedAsyncTypes = Compilation.GetSynthesizedAsyncStateMachineTypes().ToArray();
        var synthesizedDelegates = Compilation.GetSynthesizedDelegateTypes().ToArray();
        var synthesizedIterators = Compilation.GetSynthesizedIteratorTypes().ToArray();

        foreach (var typeSymbol in allTypes)
        {
            GetOrCreateTypeGenerator(typeSymbol);
        }

        foreach (var unionCaseType in unionCaseTypes)
        {
            GetOrCreateTypeGenerator(unionCaseType);
        }

        foreach (var asyncType in synthesizedAsyncTypes)
        {
            GetOrCreateTypeGenerator(asyncType);
        }

        foreach (var delegateType in synthesizedDelegates)
        {
            GetOrCreateTypeGenerator(delegateType);
        }

        foreach (var iteratorType in synthesizedIterators)
        {
            GetOrCreateTypeGenerator(iteratorType);
        }

        var visited = new HashSet<ITypeSymbol>(SymbolEqualityComparer.Default);
        var visiting = new HashSet<ITypeSymbol>(SymbolEqualityComparer.Default);

        foreach (var typeSymbol in allTypes)
        {
            EnsureTypeBuilderDefined(typeSymbol, visited, visiting);
        }

        foreach (var unionCaseType in unionCaseTypes)
        {
            EnsureTypeBuilderDefined(unionCaseType, visited, visiting);
        }

        foreach (var asyncType in synthesizedAsyncTypes)
        {
            EnsureTypeBuilderDefined(asyncType, visited, visiting);
        }

        foreach (var delegateType in synthesizedDelegates)
        {
            EnsureTypeBuilderDefined(delegateType, visited, visiting);
        }

        foreach (var iteratorType in synthesizedIterators)
        {
            EnsureTypeBuilderDefined(iteratorType, visited, visiting);
        }
    }

    private void EnsureAsyncStateMachines()
    {
        var processedTopLevelMethods = new HashSet<SourceMethodSymbol>(ReferenceEqualityComparer.Instance);

        foreach (var tree in Compilation.SyntaxTrees)
        {
            var semanticModel = Compilation.GetSemanticModel(tree);
            var root = tree.GetRoot();

            RewriteAsyncLambdas(semanticModel, root);

            foreach (var methodDeclaration in root.DescendantNodes().OfType<MethodDeclarationSyntax>())
            {
                if (semanticModel.GetDeclaredSymbol(methodDeclaration) is not SourceMethodSymbol methodSymbol)
                    continue;

                TryRewriteAsyncMethod(
                    semanticModel,
                    methodSymbol,
                    methodDeclaration.Body,
                    methodDeclaration.ExpressionBody);
            }

            foreach (var functionStatement in root.DescendantNodes().OfType<FunctionStatementSyntax>())
            {
                if (semanticModel.GetDeclaredSymbol(functionStatement) is not SourceMethodSymbol functionSymbol)
                    continue;

                TryRewriteAsyncMethod(
                    semanticModel,
                    functionSymbol,
                    functionStatement.Body,
                    functionStatement.ExpressionBody);
            }

            foreach (var accessor in root.DescendantNodes().OfType<AccessorDeclarationSyntax>())
            {
                if (semanticModel.GetDeclaredSymbol(accessor) is not SourceMethodSymbol accessorSymbol)
                    continue;

                TryRewriteAsyncMethod(
                    semanticModel,
                    accessorSymbol,
                    accessor.Body,
                    accessor.ExpressionBody);
            }

            if (root is CompilationUnitSyntax compilationUnit)
            {
                TryRewriteTopLevelAsyncMethod(semanticModel, compilationUnit, processedTopLevelMethods);
            }
        }
    }

    private void RewriteAsyncLambdas(SemanticModel semanticModel, SyntaxNode root)
    {
        foreach (var lambdaSyntax in root.DescendantNodes().OfType<LambdaExpressionSyntax>())
        {
            if (semanticModel.GetBoundNode(lambdaSyntax) is not BoundLambdaExpression boundLambda)
                continue;

            if (boundLambda.Symbol is not SourceLambdaSymbol sourceLambda || !sourceLambda.IsAsync)
                continue;

            var lambdaBody = ConvertToBlockStatement(sourceLambda, boundLambda.Body);
            if (!AsyncLowerer.ShouldRewrite(sourceLambda, lambdaBody))
                continue;

            var closureSelfType = TryGetAsyncLambdaClosureType(sourceLambda);
            var rewritten = AsyncLowerer.Rewrite(sourceLambda, lambdaBody, selfType: closureSelfType);
            if (rewritten.StateMachine is not null)
            {
                var generator = GetOrCreateTypeGenerator(rewritten.StateMachine);
                generator.DefineTypeBuilder();
            }
        }
    }

    private static BoundBlockStatement ConvertToBlockStatement(SourceLambdaSymbol lambda, BoundExpression body)
    {
        if (body is BoundBlockExpression blockExpression)
            return new BoundBlockStatement(blockExpression.Statements, blockExpression.LocalsToDispose);

        if (lambda.ReturnType.SpecialType == SpecialType.System_Unit)
            return new BoundBlockStatement(new[] { new BoundExpressionStatement(body) });

        return new BoundBlockStatement(new[] { new BoundReturnStatement(body) });
    }

    private void TryRewriteTopLevelAsyncMethod(
        SemanticModel semanticModel,
        CompilationUnitSyntax compilationUnit,
        HashSet<SourceMethodSymbol> processed)
    {
        var globalStatements = GetTopLevelGlobalStatements(compilationUnit).ToArray();
        if (globalStatements.Length == 0)
            return;

        static TopLevelBinder? FindTopLevelBinder(Binder? binder)
        {
            for (var current = binder; current is not null; current = current.ParentBinder)
            {
                if (current is TopLevelBinder topLevel)
                    return topLevel;
            }

            return null;
        }

        var binder = semanticModel.GetBinder(compilationUnit);
        var topLevelBinder = FindTopLevelBinder(binder);

        if (topLevelBinder is null)
            topLevelBinder = FindTopLevelBinder(semanticModel.GetBinder(globalStatements[0]));

        if (topLevelBinder is null)
            return;

        if (topLevelBinder.MainMethod is not SourceMethodSymbol mainMethod)
            return;

        if (!processed.Add(mainMethod))
            return;

        var boundStatements = new List<BoundStatement>(globalStatements.Length);
        foreach (var global in globalStatements)
        {
            if (semanticModel.GetBoundNode(global.Statement) is BoundStatement bound)
                boundStatements.Add(bound);
        }

        if (boundStatements.Count == 0)
            return;

        var localsToDispose = ImmutableArray.CreateBuilder<ILocalSymbol>();
        foreach (var global in globalStatements)
        {
            if (global.Statement is UsingDeclarationStatementSyntax usingDeclaration)
            {
                foreach (var declarator in usingDeclaration.Declaration.Declarators)
                {
                    if (semanticModel.GetDeclaredSymbol(declarator) is ILocalSymbol local)
                        localsToDispose.Add(local);
                }
            }
        }

        var body = new BoundBlockStatement(boundStatements, localsToDispose.ToImmutable());

        RewriteAsyncLambdas(body);

        if (!AsyncLowerer.ShouldRewrite(mainMethod, body))
            return;

        var rewritten = AsyncLowerer.Rewrite(mainMethod, body);
        semanticModel.CacheBoundNode(compilationUnit, rewritten);
    }

    private static IEnumerable<GlobalStatementSyntax> GetTopLevelGlobalStatements(CompilationUnitSyntax compilationUnit)
    {
        foreach (var member in compilationUnit.Members)
        {
            switch (member)
            {
                case GlobalStatementSyntax global:
                    yield return global;
                    break;
                case FileScopedNamespaceDeclarationSyntax fileScoped:
                    foreach (var nested in fileScoped.Members.OfType<GlobalStatementSyntax>())
                        yield return nested;
                    break;
            }
        }
    }

    private void TryRewriteAsyncMethod(
        SemanticModel semanticModel,
        SourceMethodSymbol methodSymbol,
        BlockStatementSyntax? bodySyntax,
        ArrowExpressionClauseSyntax? expressionBody)
    {
        var boundBody = TryGetBoundBody(semanticModel, methodSymbol, bodySyntax, expressionBody);
        if (boundBody is null)
            return;

        RewriteAsyncLambdas(boundBody);

        if (!AsyncLowerer.ShouldRewrite(methodSymbol, boundBody))
            return;

        var rewritten = AsyncLowerer.Rewrite(methodSymbol, boundBody);

        if (bodySyntax is not null)
        {
            semanticModel.CacheBoundNode(bodySyntax, rewritten);
        }
        else if (expressionBody is not null)
        {
            semanticModel.CacheBoundNode(expressionBody, rewritten);
        }
    }

    private static BoundBlockStatement? TryGetBoundBody(
        SemanticModel semanticModel,
        SourceMethodSymbol methodSymbol,
        BlockStatementSyntax? bodySyntax,
        ArrowExpressionClauseSyntax? expressionBody)
    {
        if (bodySyntax is not null)
        {
            return semanticModel.GetBoundNode(bodySyntax) as BoundBlockStatement;
        }

        if (expressionBody is not null)
        {
            var bound = semanticModel.GetBoundNode(expressionBody);
            return ConvertToBlockStatement(methodSymbol, bound);
        }

        return null;
    }

    private static BoundBlockStatement? ConvertToBlockStatement(SourceMethodSymbol methodSymbol, BoundNode? bound)
    {
        switch (bound)
        {
            case BoundBlockStatement block:
                return block;
            case BoundBlockExpression blockExpression:
                return new BoundBlockStatement(blockExpression.Statements, blockExpression.LocalsToDispose);
            case BoundExpression expression:
                var statements = new List<BoundStatement>();
                if (methodSymbol.ReturnType.SpecialType == SpecialType.System_Unit)
                {
                    statements.Add(new BoundExpressionStatement(expression));
                }
                else
                {
                    statements.Add(new BoundReturnStatement(expression));
                }

                return new BoundBlockStatement(statements);
            default:
                return null;
        }
    }

    private void RewriteAsyncLambdas(BoundNode node)
    {
        var collector = new AsyncLambdaCollector();
        collector.Visit(node);

        if (collector.AsyncLambdas.Count == 0)
            return;

        var rewritten = new HashSet<SourceLambdaSymbol>(ReferenceEqualityComparer.Instance);

        foreach (var lambda in collector.AsyncLambdas)
        {
            if (lambda.Symbol is not SourceLambdaSymbol sourceLambda)
                continue;

            if (!rewritten.Add(sourceLambda))
                continue;

            var block = ConvertLambdaToBlockStatement(sourceLambda, lambda.Body);

            if (!AsyncLowerer.ShouldRewrite(sourceLambda, block))
                continue;

            var closureSelfType = TryGetAsyncLambdaClosureType(sourceLambda);
            var rewrittenLambda = AsyncLowerer.Rewrite(sourceLambda, block, selfType: closureSelfType);

            if (rewrittenLambda.StateMachine is not null)
            {
                var generator = GetOrCreateTypeGenerator(rewrittenLambda.StateMachine);
                generator.DefineTypeBuilder();
            }
        }
    }

    private ITypeSymbol? TryGetAsyncLambdaClosureType(SourceLambdaSymbol lambda)
    {
        if (!lambda.HasCaptures)
            return null;

        if (lambda.ContainingType is null)
            return null;

        var containingGenerator = GetOrCreateTypeGenerator(lambda.ContainingType);

        if (containingGenerator.TypeBuilder is null)
            containingGenerator.DefineTypeBuilder();

        if (containingGenerator.TryGetLambdaClosure(lambda, out var closure))
            return closure.Symbol;

        return containingGenerator.EnsureLambdaClosure(lambda).Symbol;
    }

    private static BoundBlockStatement ConvertLambdaToBlockStatement(SourceLambdaSymbol lambda, BoundExpression body)
    {
        if (body is BoundBlockExpression blockExpression)
            return new BoundBlockStatement(blockExpression.Statements, blockExpression.LocalsToDispose);

        if (lambda.ReturnType.SpecialType == SpecialType.System_Unit)
            return new BoundBlockStatement(new[] { new BoundExpressionStatement(body) });

        return new BoundBlockStatement(new[] { new BoundReturnStatement(body) });
    }

    private void EnsureIteratorStateMachines()
    {
        foreach (var tree in Compilation.SyntaxTrees)
        {
            var semanticModel = Compilation.GetSemanticModel(tree);
            var root = tree.GetRoot();

            foreach (var methodDeclaration in root.DescendantNodes().OfType<MethodDeclarationSyntax>())
            {
                if (methodDeclaration.Body is null)
                    continue;

                if (semanticModel.GetDeclaredSymbol(methodDeclaration) is not SourceMethodSymbol methodSymbol)
                    continue;

                if (semanticModel.GetBoundNode(methodDeclaration.Body) is not BoundBlockStatement boundBody)
                    continue;

                if (!IteratorLowerer.ShouldRewrite(methodSymbol, boundBody))
                    continue;

                IteratorLowerer.Rewrite(methodSymbol, boundBody);
            }

            foreach (var functionStatement in root.DescendantNodes().OfType<FunctionStatementSyntax>())
            {
                if (functionStatement.Body is null)
                    continue;

                if (semanticModel.GetDeclaredSymbol(functionStatement) is not SourceMethodSymbol functionSymbol)
                    continue;

                if (semanticModel.GetBoundNode(functionStatement.Body) is not BoundBlockStatement functionBody)
                    continue;

                if (!IteratorLowerer.ShouldRewrite(functionSymbol, functionBody))
                    continue;

                IteratorLowerer.Rewrite(functionSymbol, functionBody);
            }
        }
    }

    internal TypeGenerator GetOrCreateTypeGenerator(ITypeSymbol typeSymbol)
    {
        if (!_typeGenerators.TryGetValue(typeSymbol, out var generator))
        {
            generator = new TypeGenerator(this, typeSymbol);
            _typeGenerators[typeSymbol] = generator;
        }

        return generator;
    }

    private void EnsureTypeBuilderDefined(ITypeSymbol typeSymbol, HashSet<ITypeSymbol> visited, HashSet<ITypeSymbol> visiting)
    {
        var originalTypeSymbol = typeSymbol;
        typeSymbol = GetDefinitionTypeSymbol(typeSymbol);

        if (visited.Contains(typeSymbol))
            return;

        if (!visiting.Add(typeSymbol))
            return;

        if (!_typeGenerators.TryGetValue(typeSymbol, out var generator))
        {
            visiting.Remove(typeSymbol);
            return;
        }

        if (generator.TypeBuilder is null && generator.Type is null)
        {
            if (originalTypeSymbol is INamedTypeSymbol named)
            {
                var baseType = named.BaseType;
                if (baseType is not null)
                    EnsureTypeDependencies(baseType, visited, visiting);

                foreach (var interfaceType in named.Interfaces)
                    EnsureTypeDependencies(interfaceType, visited, visiting);

                foreach (var typeArgument in named.TypeArguments)
                    EnsureTypeDependencies(typeArgument, visited, visiting);
            }
        }

        if (generator.TypeBuilder is null)
            generator.DefineTypeBuilder();

        visiting.Remove(typeSymbol);
        visited.Add(typeSymbol);
    }

    private static ITypeSymbol GetDefinitionTypeSymbol(ITypeSymbol typeSymbol)
    {
        if (typeSymbol is INamedTypeSymbol named &&
            named.IsGenericType &&
            named.ConstructedFrom is INamedTypeSymbol definition &&
            !ReferenceEquals(named, definition))
        {
            return definition;
        }

        return typeSymbol;
    }

    private void EnsureTypeDependencies(ITypeSymbol typeSymbol, HashSet<ITypeSymbol> visited, HashSet<ITypeSymbol> visiting)
    {
        if (typeSymbol.IsAlias && typeSymbol is IAliasSymbol { UnderlyingSymbol: ITypeSymbol aliasType })
        {
            EnsureTypeDependencies(aliasType, visited, visiting);
            return;
        }

        switch (typeSymbol)
        {
            case ITupleTypeSymbol tupleType:
                foreach (var element in tupleType.TupleElements)
                    EnsureTypeDependencies(element.Type, visited, visiting);
                break;
            case INamedTypeSymbol named:
                {
                    var definition = GetDefinitionTypeSymbol(named);
                    if (definition.DeclaringSyntaxReferences.Length > 0)
                        EnsureTypeBuilderDefined(definition, visited, visiting);

                    if (named.TypeArguments.IsDefaultOrEmpty)
                        break;

                    foreach (var typeArgument in named.TypeArguments)
                        EnsureTypeDependencies(typeArgument, visited, visiting);

                    break;
                }
            case IArrayTypeSymbol arrayType:
                EnsureTypeDependencies(arrayType.ElementType, visited, visiting);
                break;
            case ByRefTypeSymbol byRefType:
                EnsureTypeDependencies(byRefType.ElementType, visited, visiting);
                break;
            case IPointerTypeSymbol pointerType:
                EnsureTypeDependencies(pointerType.PointedAtType, visited, visiting);
                break;
            case NullableTypeSymbol nullableType:
                EnsureTypeDependencies(nullableType.UnderlyingType, visited, visiting);
                break;
            case LiteralTypeSymbol literalType:
                EnsureTypeDependencies(literalType.UnderlyingType, visited, visiting);
                break;
            case ITypeUnionSymbol unionType:
                var emission = unionType.GetUnionEmissionInfo(Compilation);
                EnsureTypeDependencies(emission.UnderlyingTypeSymbol, visited, visiting);
                break;
        }
    }

    private void DefineMemberBuilders()
    {
        foreach (var typeGenerator in _typeGenerators.Values)
        {
            typeGenerator.DefineMemberBuilders();
        }

        foreach (var typeGenerator in _typeGenerators.Values)
        {
            typeGenerator.CompleteInterfaceImplementations();
        }
    }

    private void CreateTypes()
    {
        foreach (var typeGenerator in _typeGenerators.Values)
        {
            typeGenerator.CreateType();
        }
    }

    private void EmitMemberILBodies()
    {
        while (true)
        {
            var pending = _typeGenerators.Values
                .Where(static generator => generator.MethodGenerators.Any(method => !method.HasEmittedBody))
                .ToList();

            if (pending.Count == 0)
                break;

            foreach (var typeGenerator in pending)
            {
                typeGenerator.EmitMemberILBodies();
            }
        }
    }

    static DebugDirectoryBuilder EmitPdb(MetadataBuilder pdbBuilder, ImmutableArray<int> rowCounts, MethodDefinitionHandle entryPointHandle)
    {
        BlobBuilder portablePdbBlob = new BlobBuilder();
        PortablePdbBuilder portablePdbBuilder = new PortablePdbBuilder(pdbBuilder, rowCounts, entryPointHandle);
        BlobContentId pdbContentId = portablePdbBuilder.Serialize(portablePdbBlob);

        // In case saving PDB to a file
        using FileStream fileStream = new FileStream("MyAssemblyEmbeddedSource.pdb", FileMode.Create, FileAccess.Write);
        portablePdbBlob.WriteContentTo(fileStream);

        DebugDirectoryBuilder debugDirectoryBuilder = new DebugDirectoryBuilder();
        debugDirectoryBuilder.AddCodeViewEntry("MyAssemblyEmbeddedSource.pdb", pdbContentId, portablePdbBuilder.FormatVersion);

        // In case embedded in PE:
        // debugDirectoryBuilder.AddEmbeddedPortablePdbEntry(portablePdbBlob, portablePdbBuilder.FormatVersion);
        return debugDirectoryBuilder;
    }

    public bool TryGetRuntimeTypeForSymbol(INamedTypeSymbol symbol, out Type type)
    {
        if (_typeGenerators.TryGetValue(symbol, out var builder))
        {
            if (builder.TypeBuilder is not null)
            {
                type = builder.TypeBuilder;
                return true;
            }

            if (builder.Type is not null)
            {
                type = builder.Type;
                return true;
            }
        }

        type = null!;
        return false;
    }

    internal bool TryGetRuntimeTypeForTypeParameter(ITypeParameterSymbol symbol, out Type type)
    {
        if (_genericParameterMap.TryGetValue(symbol, out var stack) && stack.Count > 0)
        {
            type = stack.Peek();
            return true;
        }

        if (symbol is PETypeParameterSymbol peTypeParameter && TryResolveMetadataTypeParameter(peTypeParameter, out var resolved))
        {
            type = CacheRuntimeTypeParameter(symbol, resolved);
            return true;
        }

        type = null!;
        return false;
    }

    private bool TryResolveMetadataTypeParameter(PETypeParameterSymbol symbol, out Type type)
    {
        if (symbol.ContainingSymbol is INamedTypeSymbol containingType)
        {
            var runtimeType = containingType.GetClrTypeTreatingUnitAsVoid(this);
            var parameters = runtimeType.IsGenericTypeDefinition
                ? runtimeType.GetTypeInfo().GenericTypeParameters
                : runtimeType.GetGenericTypeDefinition().GetTypeInfo().GenericTypeParameters;

            var ordinal = symbol.Ordinal;
            if ((uint)ordinal < (uint)parameters.Length)
            {
                type = parameters[ordinal];
                return true;
            }
        }
        else if (symbol.ContainingSymbol is IMethodSymbol containingMethod)
        {
            if (TryGetRuntimeMethod(containingMethod, out var methodInfo))
            {
                var parameters = methodInfo.IsGenericMethodDefinition
                    ? methodInfo.GetGenericArguments()
                    : methodInfo.GetGenericMethodDefinition().GetGenericArguments();

                var ordinal = symbol.Ordinal;
                if ((uint)ordinal < (uint)parameters.Length)
                {
                    type = parameters[ordinal];
                    return true;
                }
            }
        }

        type = null!;
        return false;
    }

    private sealed class AsyncLambdaCollector : BoundTreeWalker
    {
        public List<BoundLambdaExpression> AsyncLambdas { get; } = new();

        public override void VisitLambdaExpression(BoundLambdaExpression node)
        {
            AsyncLambdas.Add(node);
            base.VisitLambdaExpression(node);
        }
    }

}

internal readonly struct MemberBuilderCacheKey : IEquatable<MemberBuilderCacheKey>
{
    public MemberBuilderCacheKey(SourceSymbol symbol, ImmutableArray<ITypeSymbol> substitution)
    {
        Symbol = symbol ?? throw new ArgumentNullException(nameof(symbol));
        Substitution = substitution;
    }

    public SourceSymbol Symbol { get; }

    public ImmutableArray<ITypeSymbol> Substitution { get; }

    public bool Equals(MemberBuilderCacheKey other)
    {
        if (!SymbolEqualityComparer.Default.Equals(Symbol, other.Symbol))
            return false;

        if (Substitution.IsDefaultOrEmpty && other.Substitution.IsDefaultOrEmpty)
            return true;

        if (Substitution.Length != other.Substitution.Length)
            return false;

        for (var i = 0; i < Substitution.Length; i++)
        {
            if (!SymbolEqualityComparer.Default.Equals(Substitution[i], other.Substitution[i]))
                return false;
        }

        return true;
    }

    public override bool Equals(object? obj)
        => obj is MemberBuilderCacheKey other && Equals(other);

    public override int GetHashCode()
    {
        var hash = SymbolEqualityComparer.Default.GetHashCode(Symbol);

        if (!Substitution.IsDefaultOrEmpty)
        {
            for (var i = 0; i < Substitution.Length; i++)
            {
                hash = HashCode.Combine(hash, SymbolEqualityComparer.Default.GetHashCode(Substitution[i]));
            }
        }

        return hash;
    }
}
