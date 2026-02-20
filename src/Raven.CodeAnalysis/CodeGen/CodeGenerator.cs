using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Reflection.PortableExecutable;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using static Raven.CodeAnalysis.CodeGen.DebugUtils;

namespace Raven.CodeAnalysis.CodeGen;

internal class CodeGenerator
{
    readonly Stopwatch _stopwatch = new Stopwatch();

    readonly Dictionary<ITypeSymbol, TypeGenerator> _typeGenerators = new Dictionary<ITypeSymbol, TypeGenerator>(SymbolEqualityComparer.Default);
    readonly Dictionary<SourceSymbol, MemberInfo> _mappings = new Dictionary<SourceSymbol, MemberInfo>(SymbolEqualityComparer.Default);
    readonly Dictionary<MemberBuilderCacheKey, MemberInfo> _constructedMappings = new Dictionary<MemberBuilderCacheKey, MemberInfo>();
    readonly Dictionary<RuntimeTypeParameterKey, Stack<Type>> _genericParameterMap = new();
    readonly Dictionary<IMethodSymbol, MethodInfo> _runtimeMethodCache = new Dictionary<IMethodSymbol, MethodInfo>(ReferenceEqualityComparer.Instance);
    readonly Dictionary<IMethodSymbol, ConstructorInfo> _runtimeConstructorCache = new Dictionary<IMethodSymbol, ConstructorInfo>(ReferenceEqualityComparer.Instance);

    public IILBuilderFactory ILBuilderFactory { get; set; } = ReflectionEmitILBuilderFactory.Instance;
    internal RuntimeTypeMap RuntimeTypeMap { get; }
    internal IRuntimeSymbolResolver RuntimeSymbolResolver { get; }

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
        PrintDebug($"[CodeGen:TypeParam] Cache runtime type parameter {symbol.Name} (ordinal={symbol.Ordinal}) => {type}");
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

            var owner = builder.DeclaringMethod is null ? "type" : "method";
            PrintDebug($"[CodeGen:TypeParam] Register generic parameter {parameter.Name} (ordinal={parameter.Ordinal}, symbolOwner={parameter.OwnerKind}) => {builder} (owner={owner}, isMethodParam={builder.IsGenericMethodParameter}, isTypeParam={builder.IsGenericTypeParameter})");
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
            if (TryGetGenericParameterStack(parameter, out var stack) && stack.Count > 0)
            {
                PrintDebug($"[CodeGen:TypeParam] Unregister generic parameter {parameter.Name} (ordinal={parameter.Ordinal})");
                stack.Pop();
            }
        }
    }

    private Stack<Type> GetOrCreateGenericParameterStack(ITypeParameterSymbol parameter)
    {
        var key = RuntimeTypeParameterKey.Create(parameter);
        if (!_genericParameterMap.TryGetValue(key, out var stack))
        {
            stack = new Stack<Type>();
            _genericParameterMap[key] = stack;
        }

        return stack;
    }

    private bool TryGetGenericParameterStack(ITypeParameterSymbol parameter, out Stack<Type> stack)
    {
        var key = RuntimeTypeParameterKey.Create(parameter);
        return _genericParameterMap.TryGetValue(key, out stack!);
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
            var constraintClrType = TypeSymbolExtensionsForCodeGen.GetClrType(constraintType, this);

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
    public Type? ClosedHierarchyAttributeType { get; private set; }
    ConstructorInfo? _nullableCtor;
    ConstructorInfo? _tupleElementNamesCtor;
    ConstructorInfo? _discriminatedUnionCtor;
    ConstructorInfo? _discriminatedUnionCaseCtor;
    ConstructorInfo? _extensionMarkerNameCtor;
    ConstructorInfo? _extensionAttributeCtor;
    ConstructorInfo? _closedHierarchyCtor;

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
            var parameterClrType = parameterType is not null ? TypeSymbolExtensionsForCodeGen.GetClrType(parameterType, this) : null;
            args[i] = GetAttributeValue(attribute.ConstructorArguments[i], parameterClrType, parameterType);
        }

        var attributeType = constructor.DeclaringType ?? TypeSymbolExtensionsForCodeGen.GetClrType(attribute.AttributeClass, this);

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

        var attributeType = TypeSymbolExtensionsForCodeGen.GetClrType(attribute.AttributeClass, this);
        var parameterTypes = constructorSymbol.Parameters
            .Select(p => TypeSymbolExtensionsForCodeGen.GetClrType(p.Type, this))
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
                    ITypeSymbol typeSymbol => TypeSymbolExtensionsForCodeGen.GetClrType(typeSymbol, this),
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
                        ?? (elementSymbol is not null ? TypeSymbolExtensionsForCodeGen.GetClrType(elementSymbol, this) : typeof(object));

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

                    var enumType = targetClrType ?? (constant.Type as INamedTypeSymbol is INamedTypeSymbol enumSymbol ? TypeSymbolExtensionsForCodeGen.GetClrType(enumSymbol, this) : null);
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

    internal CustomAttributeBuilder CreateClosedHierarchyAttribute(Type[] permittedTypes)
    {
        EnsureClosedHierarchyAttributeType();
        return new CustomAttributeBuilder(_closedHierarchyCtor!, new object[] { permittedTypes });
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

        var attributeType = TypeSymbolExtensionsForCodeGen.GetClrType(Compilation.GetTypeByMetadataName("System.Attribute"), this);

        var attrBuilder = ModuleBuilder.DefineType(
            "System.Runtime.CompilerServices.UnionAttribute",
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
            ?? throw new InvalidOperationException("Missing UnionAttribute() constructor.");
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

        var attributeType = TypeSymbolExtensionsForCodeGen.GetClrType(Compilation.GetTypeByMetadataName("System.Attribute"), this);
        var typeType = TypeSymbolExtensionsForCodeGen.GetClrType(Compilation.GetSpecialType(SpecialType.System_Type), this);

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

    void EnsureClosedHierarchyAttributeType()
    {
        if (ClosedHierarchyAttributeType is not null)
            return;

        var attributeType = TypeSymbolExtensionsForCodeGen.GetClrType(Compilation.GetTypeByMetadataName("System.Attribute"), this);
        var typeType = TypeSymbolExtensionsForCodeGen.GetClrType(Compilation.GetSpecialType(SpecialType.System_Type), this);

        var attrBuilder = ModuleBuilder.DefineType(
            "System.Runtime.CompilerServices.ClosedHierarchyAttribute",
            TypeAttributes.Public | TypeAttributes.Class | TypeAttributes.Sealed,
            attributeType);

        var permittedTypesField = attrBuilder.DefineField(
            "_permittedTypes",
            typeType.MakeArrayType(),
            FieldAttributes.Private | FieldAttributes.InitOnly);

        var propertyBuilder = attrBuilder.DefineProperty(
            "PermittedTypes",
            PropertyAttributes.None,
            typeType.MakeArrayType(),
            null);

        var getterMethod = attrBuilder.DefineMethod(
            "get_PermittedTypes",
            MethodAttributes.Public | MethodAttributes.HideBySig | MethodAttributes.SpecialName,
            typeType.MakeArrayType(),
            Type.EmptyTypes);

        var getterIl = getterMethod.GetILGenerator();
        getterIl.Emit(OpCodes.Ldarg_0);
        getterIl.Emit(OpCodes.Ldfld, permittedTypesField);
        getterIl.Emit(OpCodes.Ret);

        propertyBuilder.SetGetMethod(getterMethod);

        var ctorBuilder = attrBuilder.DefineConstructor(
            MethodAttributes.Public,
            CallingConventions.Standard,
            new[] { typeType.MakeArrayType() });

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
        il.Emit(OpCodes.Stfld, permittedTypesField);
        il.Emit(OpCodes.Ret);

        ClosedHierarchyAttributeType = attrBuilder.CreateType();
        _closedHierarchyCtor = ClosedHierarchyAttributeType.GetConstructor(new[] { typeType.MakeArrayType() })
            ?? throw new InvalidOperationException("Missing ClosedHierarchyAttribute(Type[]) constructor.");
    }

    bool TryCollectTupleElementNames(ITypeSymbol type, List<string?> transformNames)
    {
        var visiting = new HashSet<ITypeSymbol>(ReferenceEqualityComparer.Instance);
        return TryCollectTupleElementNames(type, transformNames, visiting);
    }

    bool TryCollectTupleElementNames(ITypeSymbol type, List<string?> transformNames, HashSet<ITypeSymbol> visiting)
    {
        if (type is null)
            return false;

        if (!visiting.Add(type))
            return false;

        var start = transformNames.Count;
        var hasAnyNames = false;

        try
        {
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

                            if (TryCollectTupleElementNames(element.Type, transformNames, visiting))
                                hasAnyNames = true;
                        }

                        break;
                    }
                case NullableTypeSymbol nullableType:
                    hasAnyNames |= TryCollectTupleElementNames(nullableType.UnderlyingType, transformNames, visiting);
                    break;
                case IArrayTypeSymbol arrayType:
                    hasAnyNames |= TryCollectTupleElementNames(arrayType.ElementType, transformNames, visiting);
                    break;
                case IPointerTypeSymbol pointerType:
                    hasAnyNames |= TryCollectTupleElementNames(pointerType.PointedAtType, transformNames, visiting);
                    break;
                case IAddressTypeSymbol addressType:
                    hasAnyNames |= TryCollectTupleElementNames(addressType.ReferencedType, transformNames, visiting);
                    break;
                case ITypeUnionSymbol unionType:
                    foreach (var member in unionType.Types)
                        hasAnyNames |= TryCollectTupleElementNames(member, transformNames, visiting);
                    break;
                case INamedTypeSymbol namedType:
                    var typeArguments = namedType is ConstructedNamedTypeSymbol constructed
                        ? constructed.GetExplicitTypeArgumentsForInference()
                        : namedType.TypeArguments;

                    if (!typeArguments.IsDefaultOrEmpty)
                    {
                        foreach (var typeArgument in typeArguments)
                            hasAnyNames |= TryCollectTupleElementNames(typeArgument, transformNames, visiting);
                    }

                    break;
            }
        }
        finally
        {
            visiting.Remove(type);
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
        RuntimeTypeMap = new RuntimeTypeMap(this);
        RuntimeSymbolResolver = new RuntimeSymbolResolver(this);
    }

    public Type? GetTypeBuilder(INamedTypeSymbol namedTypeSymbol)
    {
        var e = _typeGenerators[namedTypeSymbol];

        return e.Type ?? e?.TypeBuilder;
    }

    public void Emit(Stream peStream, Stream? pdbStream)
    {
        _stopwatch.Reset();
        _stopwatch.Start();

        PrintDebug($"Starting emitting code...");

        try
        {
            PrintDebug("Starting code generation emission.");
            var assemblyName = new AssemblyName(_compilation.AssemblyName)
            {
                Version = new Version(1, 0, 0, 0)
            };

            AssemblyBuilder = new PersistedAssemblyBuilder(assemblyName, _compilation.EmitCoreAssembly);
            ModuleBuilder = AssemblyBuilder.DefineDynamicModule(_compilation.AssemblyName);
            ApplyCustomAttributes(_compilation.Assembly.GetAttributes(), attribute => AssemblyBuilder.SetCustomAttribute(attribute));

            DetermineShimTypeRequirements();
            PrintDebug("Determined shim type requirements.");

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
            PrintDebug("Type builders defined.");

            DefineMemberBuilders();
            PrintDebug("Member builders defined.");

            EmitMemberILBodies();
            PrintDebug("Member IL bodies emitted.");

            CreateTypes();
            PrintDebug("All types created.");

            var asyncStateMachines = Compilation.GetSynthesizedAsyncStateMachineTypes().ToArray();
            foreach (var asyncStateMachine in asyncStateMachines)
            {
                var generator = GetOrCreateTypeGenerator(asyncStateMachine);
                if (generator.TypeBuilder is null)
                    generator.DefineTypeBuilder();

                if (generator.TypeBuilder is { } builder && !builder.IsCreated())
                {
                    PrintDebug($"Creating async state machine type: {asyncStateMachine.ToDisplayString()}");
                    generator.CreateType();
                }
            }

            var deferredTypes = _typeGenerators.Values
                .Where(generator => generator.TypeBuilder is { } builder && !builder.IsCreated())
                .ToArray();

            if (deferredTypes.Length > 0)
            {
                PrintDebug($"Found {deferredTypes.Length} type builders not created after CreateTypes.");
                foreach (var generator in deferredTypes)
                {
                    PrintDebug($"Creating deferred type: {generator.TypeSymbol.ToDisplayString()}");
                    generator.CreateType();
                }
            }

            var deferredMethodTypes = _typeGenerators.Values
                .SelectMany(generator => generator.MethodGenerators)
                .Select(generator => generator.MethodBase?.DeclaringType)
                .OfType<TypeBuilder>()
                .Where(builder => !builder.IsCreated())
                .Distinct()
                .ToArray();

            if (deferredMethodTypes.Length > 0)
            {
                PrintDebug($"Found {deferredMethodTypes.Length} method declaring types not created after CreateTypes.");
                foreach (var builder in deferredMethodTypes)
                {
                    PrintDebug($"Creating declaring type for method: {builder.FullName ?? builder.Name}");
                    var owner = _typeGenerators.Values.FirstOrDefault(generator => ReferenceEquals(generator.TypeBuilder, builder));
                    owner?.CreateType();
                }
            }

            var entryPointSymbol = _compilation.GetEntryPoint();
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
            EntryPoint = entryPointGenerator?.MethodBase;
            if (EntryPoint is not null)
                PrintDebug($"Selected entry point: {EntryPoint.Name}");

            PrintUncreatedModuleTypeBuilders();

            MetadataBuilder metadataBuilder = AssemblyBuilder.GenerateMetadata(out BlobBuilder ilStream, out _, out MetadataBuilder pdbBuilder);
            PrintDebug("Generated assembly metadata.");
            MethodDefinitionHandle entryPointHandle = EntryPoint is not null
                ? MetadataTokens.MethodDefinitionHandle(EntryPoint.MetadataToken)
                : default;
            DebugDirectoryBuilder debugDirectoryBuilder = EmitPdb(
                pdbBuilder,
                metadataBuilder.GetRowCounts(),
                entryPointHandle,
                pdbStream,
                _compilation.AssemblyName);

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

            using var rawPeStream = new MemoryStream();
            peBlob.WriteContentTo(rawPeStream);

            // Reflection.Emit binds core types to System.Private.CoreLib runtime identities.
            // Normalize to System.Runtime for consumer-facing assemblies while retaining
            // System.Private.CoreLib where required for non-forwarded runtime types.
            if (_compilation.Options.OutputKind == OutputKind.DynamicallyLinkedLibrary)
                AssemblyReferenceNormalizer.NormalizeCoreLibReference(rawPeStream, peStream);
            else
                rawPeStream.WriteTo(peStream);
        }
        catch (Exception ex)
        {
            if (CurrentEmittingMethod is { } method)
                throw new InvalidOperationException($"Emission failed while processing method '{method.ToDisplayString()}'", ex);

            throw;
        }
        finally
        {
            _stopwatch.Stop();
            PrintDebug($"Emitted code in {_stopwatch.ElapsedMilliseconds} ms");
        }
    }

    private void DetermineShimTypeRequirements()
    {
        _emitExtensionMarkerNameAttribute = Compilation.SyntaxTrees
            .SelectMany(tree => tree.GetRoot().DescendantNodes().OfType<ExtensionDeclarationSyntax>())
            .Any();
        // Intentionally avoid recursive type walks here. On recursive constructed
        // generic graphs (e.g. nested option extensions), forcing TypeArguments can
        // recurse indefinitely during core emission.
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
                "System.Runtime.CompilerServices.UnionAttribute")
                ?? Compilation.ResolveRuntimeType("System.Runtime.CompilerServices.DiscriminatedUnionAttribute");
            _discriminatedUnionCtor = DiscriminatedUnionAttributeType?.GetConstructor(Type.EmptyTypes);
        }

        if (DiscriminatedUnionCaseAttributeType is null)
        {
            DiscriminatedUnionCaseAttributeType = Compilation.ResolveRuntimeType(
                "System.Runtime.CompilerServices.DiscriminatedUnionCaseAttribute");

            var typeType = TypeSymbolExtensionsForCodeGen.GetClrType(Compilation.GetSpecialType(SpecialType.System_Type), this);

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
            TypeSymbolExtensionsForCodeGen.GetClrType(Compilation.GetTypeByMetadataName("System.ValueType"), this));

        var valueField = unitBuilder.DefineField(
            "Value",
            unitBuilder,
            FieldAttributes.Public | FieldAttributes.Static | FieldAttributes.InitOnly);

        var equalsMethod = unitBuilder.DefineMethod(
            "Equals",
            MethodAttributes.Public | MethodAttributes.HideBySig | MethodAttributes.Virtual | MethodAttributes.Final,
            TypeSymbolExtensionsForCodeGen.GetClrType(Compilation.GetTypeByMetadataName("System.Boolean"), this),
            new[] { unitBuilder });
        var ilEquals = equalsMethod.GetILGenerator();
        ilEquals.Emit(OpCodes.Ldc_I4_1);
        ilEquals.Emit(OpCodes.Ret);

        var equalsObjMethod = unitBuilder.DefineMethod(
            "Equals",
            MethodAttributes.Public | MethodAttributes.HideBySig | MethodAttributes.Virtual | MethodAttributes.Final,
            TypeSymbolExtensionsForCodeGen.GetClrType(Compilation.GetTypeByMetadataName("System.Boolean"), this),
            new[] { TypeSymbolExtensionsForCodeGen.GetClrType(Compilation.GetTypeByMetadataName("System.Object"), this) });
        var ilEqualsObj = equalsObjMethod.GetILGenerator();
        ilEqualsObj.Emit(OpCodes.Ldarg_1);
        ilEqualsObj.Emit(OpCodes.Isinst, unitBuilder);
        ilEqualsObj.Emit(OpCodes.Ldnull);
        ilEqualsObj.Emit(OpCodes.Cgt_Un);
        ilEqualsObj.Emit(OpCodes.Ret);

        var getHashCodeMethod = unitBuilder.DefineMethod(
            "GetHashCode",
            MethodAttributes.Public | MethodAttributes.HideBySig | MethodAttributes.Virtual,
            TypeSymbolExtensionsForCodeGen.GetClrType(Compilation.GetTypeByMetadataName("System.Int32"), this),
            Type.EmptyTypes);
        var ilHash = getHashCodeMethod.GetILGenerator();
        ilHash.Emit(OpCodes.Ldc_I4_0);
        ilHash.Emit(OpCodes.Ret);

        var toStringMethod = unitBuilder.DefineMethod(
            "ToString",
            MethodAttributes.Public | MethodAttributes.HideBySig | MethodAttributes.Virtual,
            TypeSymbolExtensionsForCodeGen.GetClrType(Compilation.GetTypeByMetadataName("System.String"), this),
            Type.EmptyTypes);
        var ilToString = toStringMethod.GetILGenerator();
        ilToString.Emit(OpCodes.Ldstr, "()");
        ilToString.Emit(OpCodes.Ret);

        UnitType = unitBuilder.CreateType();
    }

    private void DefineTypeBuilders()
    {
        PrintDebug("Defining type builders...");

        EnsureLoweredBoundNodes();

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

    private void EnsureLoweredBoundNodes()
    {
        foreach (var tree in Compilation.SyntaxTrees)
        {
            var semanticModel = Compilation.GetSemanticModel(tree);
            var root = tree.GetRoot();

            if (root is CompilationUnitSyntax compilationUnit)
            {
                var hasTopLevelStatements = compilationUnit.Members.Any(static member =>
                    member is GlobalStatementSyntax ||
                    member is FileScopedNamespaceDeclarationSyntax fileScoped && fileScoped.Members.OfType<GlobalStatementSyntax>().Any());

                if (hasTopLevelStatements &&
                    Compilation.Options.OutputKind == OutputKind.ConsoleApplication)
                    semanticModel.GetBoundNode(compilationUnit, BoundTreeView.Lowered);
            }

            foreach (var methodDeclaration in root.DescendantNodes().OfType<MethodDeclarationSyntax>())
            {
                if (methodDeclaration.Body is not null)
                    semanticModel.GetBoundNode(methodDeclaration.Body, BoundTreeView.Lowered);

                if (methodDeclaration.ExpressionBody is not null)
                    semanticModel.GetBoundNode(methodDeclaration.ExpressionBody, BoundTreeView.Lowered);
            }

            foreach (var functionStatement in root.DescendantNodes().OfType<FunctionStatementSyntax>())
            {
                if (functionStatement.Body is not null)
                    semanticModel.GetBoundNode(functionStatement.Body, BoundTreeView.Lowered);

                if (functionStatement.ExpressionBody is not null)
                    semanticModel.GetBoundNode(functionStatement.ExpressionBody, BoundTreeView.Lowered);
            }

            foreach (var accessor in root.DescendantNodes().OfType<AccessorDeclarationSyntax>())
            {
                if (accessor.Body is not null)
                    semanticModel.GetBoundNode(accessor.Body, BoundTreeView.Lowered);

                if (accessor.ExpressionBody is not null)
                    semanticModel.GetBoundNode(accessor.ExpressionBody, BoundTreeView.Lowered);
            }

            foreach (var lambda in root.DescendantNodes().OfType<LambdaExpressionSyntax>())
                semanticModel.GetBoundNode(lambda, BoundTreeView.Lowered);
        }
    }

    internal TypeGenerator GetOrCreateTypeGenerator(ITypeSymbol typeSymbol)
    {
        var definitionTypeSymbol = GetDefinitionTypeSymbol(typeSymbol);
        if (!_typeGenerators.TryGetValue(definitionTypeSymbol, out var generator))
        {
            generator = new TypeGenerator(this, definitionTypeSymbol);
            _typeGenerators[definitionTypeSymbol] = generator;
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
        {
            PrintDebug($"Defining type builder for {typeSymbol.ToDisplayString()}");
            generator.DefineTypeBuilder();
        }

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
            case RefTypeSymbol refTypeType:
                EnsureTypeDependencies(refTypeType.ElementType, visited, visiting);
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
        PrintDebug("Defining member builders for all types.");
        foreach (var typeGenerator in _typeGenerators.Values)
        {
            typeGenerator.DefineMemberBuilders();
        }

        PrintDebug("Completing interface implementations for all types.");
        foreach (var typeGenerator in _typeGenerators.Values)
        {
            typeGenerator.CompleteInterfaceImplementations();
        }
    }

    private void CreateTypes()
    {
        PrintDebug("Creating runtime types.");
        foreach (var typeGenerator in _typeGenerators.Values
            .OrderBy(generator => GetContainingTypeDepth(generator.TypeSymbol)))
        {
            typeGenerator.CreateType();
        }
    }

    private static int GetContainingTypeDepth(ITypeSymbol typeSymbol)
    {
        var depth = 0;
        var current = (typeSymbol as INamedTypeSymbol)?.ContainingType;
        while (current is not null)
        {
            depth++;
            current = current.ContainingType;
        }

        return depth;
    }

    private void PrintUncreatedModuleTypeBuilders()
    {
        var builders = new HashSet<TypeBuilder>();
        var moduleType = ModuleBuilder.GetType();

        foreach (var field in moduleType.GetFields(BindingFlags.Instance | BindingFlags.NonPublic))
        {
            var value = field.GetValue(ModuleBuilder);
            if (value is null)
                continue;

            if (value is TypeBuilder singleBuilder)
            {
                builders.Add(singleBuilder);
                continue;
            }

            if (value is string)
                continue;

            if (value is System.Collections.IEnumerable enumerable)
            {
                foreach (var item in enumerable)
                {
                    if (item is TypeBuilder builder)
                        builders.Add(builder);
                }
            }
        }

        var uncreatedBuilders = builders.Where(builder => !builder.IsCreated()).ToArray();
        if (uncreatedBuilders.Length == 0)
            return;

        PrintDebug($"Found {uncreatedBuilders.Length} uncreated module type builders.");
        foreach (var builder in uncreatedBuilders)
        {
            PrintDebug($"Uncreated module type builder: {builder.FullName ?? builder.Name}");

            if (string.Equals(builder.Name, "<Module>", StringComparison.Ordinal))
                continue;

            try
            {
                builder.CreateType();
                PrintDebug($"Created module type builder: {builder.FullName ?? builder.Name}");
            }
            catch (Exception ex)
            {
                PrintDebug($"Failed to create module type builder: {builder.FullName ?? builder.Name} ({ex.GetType().Name})");
            }
        }
    }

    private void EmitMemberILBodies()
    {
        PrintDebug("Emitting IL bodies for members.");
        foreach (var typeGenerator in _typeGenerators.Values.ToArray())
        {
            typeGenerator.EmitMemberILBodies();
        }
    }

    static DebugDirectoryBuilder EmitPdb(
        MetadataBuilder pdbBuilder,
        ImmutableArray<int> rowCounts,
        MethodDefinitionHandle entryPointHandle,
        Stream? pdbStream,
        string assemblyName)
    {
        BlobBuilder portablePdbBlob = new BlobBuilder();
        PortablePdbBuilder portablePdbBuilder = new PortablePdbBuilder(pdbBuilder, rowCounts, entryPointHandle);
        BlobContentId pdbContentId = portablePdbBuilder.Serialize(portablePdbBlob);

        if (pdbStream is not null)
            portablePdbBlob.WriteContentTo(pdbStream);

        var pdbFileName = $"{assemblyName}.pdb";
        if (pdbStream is FileStream fileStream && !string.IsNullOrWhiteSpace(fileStream.Name))
            pdbFileName = Path.GetFileName(fileStream.Name);

        DebugDirectoryBuilder debugDirectoryBuilder = new DebugDirectoryBuilder();
        debugDirectoryBuilder.AddCodeViewEntry(pdbFileName, pdbContentId, portablePdbBuilder.FormatVersion);

        // In case embedded in PE:
        // debugDirectoryBuilder.AddEmbeddedPortablePdbEntry(portablePdbBlob, portablePdbBuilder.FormatVersion);
        return debugDirectoryBuilder;
    }

    public bool TryGetRuntimeTypeForSymbol(INamedTypeSymbol symbol, out Type type)
    {
        if (symbol is ConstructedNamedTypeSymbol constructed &&
            constructed.ConstructedFrom is INamedTypeSymbol definition &&
            !SymbolEqualityComparer.Default.Equals(constructed, definition))
        {
            if (TryGetRuntimeTypeForSymbol(definition, out var definitionType))
            {
                if (!definitionType.IsGenericTypeDefinition && !definitionType.ContainsGenericParameters)
                {
                    type = definitionType;
                    return true;
                }

                var typeArguments = constructed.TypeArguments
                    .Select(arg => TypeSymbolExtensionsForCodeGen.GetClrType(arg, this))
                    .ToArray();

                type = typeArguments.Length == 0
                    ? definitionType
                    : definitionType.MakeGenericType(typeArguments);
                return true;
            }
        }

        var symbolDefinition = (INamedTypeSymbol)GetDefinitionTypeSymbol(symbol);
        if (_typeGenerators.TryGetValue(symbolDefinition, out var builder))
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

    internal bool TryEnsureRuntimeTypeForSymbol(INamedTypeSymbol symbol, out Type type)
    {
        if (TryGetRuntimeTypeForSymbol(symbol, out type))
            return true;

        var symbolDefinition = (INamedTypeSymbol)GetDefinitionTypeSymbol(symbol);
        if (_typeGenerators.TryGetValue(symbolDefinition, out var generator))
        {
            if (generator.TypeBuilder is null && generator.Type is null)
                generator.DefineTypeBuilder();

            return TryGetRuntimeTypeForSymbol(symbol, out type);
        }

        type = null!;
        return false;
    }

    internal bool TryGetRuntimeTypeForTypeParameter(ITypeParameterSymbol symbol, out Type type)
        => RuntimeTypeMap.TryResolveTypeParameter(symbol, RuntimeTypeUsage.Signature, out type);

    internal bool TryResolveRuntimeTypeParameter(ITypeParameterSymbol symbol, RuntimeTypeUsage usage, out Type type)
    {
        var normalized = (ITypeParameterSymbol)(symbol.OriginalDefinition ?? symbol);

        if (CodeGenFlags.PrintDebug)
        {
            var ownerIdentity = normalized.OwnerKind switch
            {
                TypeParameterOwnerKind.Method => RuntimeTypeParameterKey.GetMethodOwnerIdentity(
                    RuntimeTypeParameterKey.NormalizeMethodOwner(normalized.DeclaringMethodParameterOwner)),
                TypeParameterOwnerKind.Type => RuntimeTypeParameterKey.GetTypeOwnerIdentity(
                    RuntimeTypeParameterKey.NormalizeTypeOwner(normalized.DeclaringTypeParameterOwner)),
                _ => null
            };

            PrintDebug(
                $"[CodeGen:TypeParam] Lookup {symbol.Name} (ordinal={symbol.Ordinal}, owner={symbol.OwnerKind}, ownerId={ownerIdentity ?? "<null>"})");
        }

        if (TryGetGenericParameterStack(symbol, out var stack) && stack.Count > 0)
        {
            if (symbol.OwnerKind == TypeParameterOwnerKind.Method && usage == RuntimeTypeUsage.MethodBody)
            {
                foreach (var candidate in stack)
                {
                    if (!candidate.IsGenericParameter || !candidate.IsGenericMethodParameter || candidate.IsGenericTypeParameter)
                        continue;

                    if (!IsSignaturePlaceholderType(candidate))
                        continue;

                    if (CodeGenFlags.PrintDebug)
                    {
                        PrintDebug(
                            $"[CodeGen:TypeParam] Lookup preferred signature placeholder {symbol.Name} -> {candidate} (depth={stack.Count})");
                    }

                    type = candidate;
                    return true;
                }
            }

            if (CodeGenFlags.PrintDebug)
            {
                var candidate = stack.Peek();
                PrintDebug(
                    $"[CodeGen:TypeParam] Lookup hit {symbol.Name} -> {candidate} (isMethodParam={candidate.IsGenericMethodParameter}, isTypeParam={candidate.IsGenericTypeParameter}, depth={stack.Count})");
            }

            type = stack.Peek();
            return true;
        }

        if (normalized.OwnerKind == TypeParameterOwnerKind.Method &&
            TryResolveFromCurrentEmittingMethod(normalized, usage, out var currentMethodType))
        {
            type = CacheRuntimeTypeParameter(symbol, currentMethodType);
            return true;
        }

        if (normalized is PETypeParameterSymbol normalizedPeTypeParameter &&
            TryResolveMetadataTypeParameter(normalizedPeTypeParameter, out var normalizedResolved))
        {
            type = CacheRuntimeTypeParameter(symbol, normalizedResolved);
            return true;
        }

        if (symbol is PETypeParameterSymbol peTypeParameter && TryResolveMetadataTypeParameter(peTypeParameter, out var resolved))
        {
            type = CacheRuntimeTypeParameter(symbol, resolved);
            return true;
        }

        if (normalized.Ordinal >= 0)
        {
            if (normalized.OwnerKind == TypeParameterOwnerKind.Method)
            {
                type = CacheRuntimeTypeParameter(symbol, Type.MakeGenericMethodParameter(normalized.Ordinal));
                return true;
            }
        }

        type = null!;
        if (CodeGenFlags.PrintDebug)
            PrintDebug($"[CodeGen:TypeParam] Lookup miss {symbol.Name}");
        return false;
    }

    private bool TryResolveFromCurrentEmittingMethod(
        ITypeParameterSymbol normalizedMethodParameter,
        RuntimeTypeUsage usage,
        out Type type)
    {
        type = null!;

        var currentMethod = CurrentEmittingMethod;
        if (currentMethod is null || currentMethod.TypeParameters.IsDefaultOrEmpty)
            return false;

        var ordinal = normalizedMethodParameter.Ordinal;
        if ((uint)ordinal >= (uint)currentMethod.TypeParameters.Length)
            return false;

        var currentTypeParameter = currentMethod.TypeParameters[ordinal];
        if (!TryGetGenericParameterStack(currentTypeParameter, out var currentStack) || currentStack.Count == 0)
            return false;

        if (usage == RuntimeTypeUsage.MethodBody)
        {
            foreach (var candidate in currentStack)
            {
                if (!candidate.IsGenericParameter || !candidate.IsGenericMethodParameter || candidate.IsGenericTypeParameter)
                    continue;

                if (!IsSignaturePlaceholderType(candidate))
                    continue;

                type = candidate;
                return true;
            }
        }

        type = currentStack.Peek();
        return true;
    }

    private static bool IsSignaturePlaceholderType(Type type)
    {
        try
        {
            _ = type.Assembly;
            return false;
        }
        catch (NotSupportedException)
        {
            return true;
        }
    }

    private readonly struct RuntimeTypeParameterKey : IEquatable<RuntimeTypeParameterKey>
    {
        private RuntimeTypeParameterKey(
            TypeParameterOwnerKind ownerKind,
            int ordinal,
            string? methodOwnerIdentity,
            string? typeOwnerIdentity)
        {
            OwnerKind = ownerKind;
            Ordinal = ordinal;
            MethodOwnerIdentity = methodOwnerIdentity;
            TypeOwnerIdentity = typeOwnerIdentity;
        }

        public TypeParameterOwnerKind OwnerKind { get; }
        public int Ordinal { get; }
        public string? MethodOwnerIdentity { get; }
        public string? TypeOwnerIdentity { get; }

        public static RuntimeTypeParameterKey Create(ITypeParameterSymbol parameter)
        {
            var normalized = (ITypeParameterSymbol)(parameter.OriginalDefinition ?? parameter);
            return normalized.OwnerKind switch
            {
                TypeParameterOwnerKind.Method => new RuntimeTypeParameterKey(
                    normalized.OwnerKind,
                    normalized.Ordinal,
                    GetMethodOwnerIdentity(NormalizeMethodOwner(normalized.DeclaringMethodParameterOwner)),
                    null),
                TypeParameterOwnerKind.Type => new RuntimeTypeParameterKey(
                    normalized.OwnerKind,
                    normalized.Ordinal,
                    null,
                    GetTypeOwnerIdentity(NormalizeTypeOwner(normalized.DeclaringTypeParameterOwner))),
                _ => new RuntimeTypeParameterKey(normalized.OwnerKind, normalized.Ordinal, null, null),
            };
        }

        public bool Equals(RuntimeTypeParameterKey other)
        {
            return OwnerKind == other.OwnerKind &&
                   Ordinal == other.Ordinal &&
                   string.Equals(MethodOwnerIdentity, other.MethodOwnerIdentity, StringComparison.Ordinal) &&
                   string.Equals(TypeOwnerIdentity, other.TypeOwnerIdentity, StringComparison.Ordinal);
        }

        public override bool Equals(object? obj)
            => obj is RuntimeTypeParameterKey other && Equals(other);

        public override int GetHashCode()
            => HashCode.Combine(
                (int)OwnerKind,
                Ordinal,
                MethodOwnerIdentity is null ? 0 : StringComparer.Ordinal.GetHashCode(MethodOwnerIdentity),
                TypeOwnerIdentity is null ? 0 : StringComparer.Ordinal.GetHashCode(TypeOwnerIdentity));

        internal static IMethodSymbol? NormalizeMethodOwner(IMethodSymbol? method)
        {
            if (method is null)
                return null;

            while (method.UnderlyingSymbol is IMethodSymbol underlying &&
                   !ReferenceEquals(underlying, method))
            {
                method = underlying;
            }

            if (method is ConstructedMethodSymbol constructed)
                method = constructed.Definition;

            return (IMethodSymbol?)(method.OriginalDefinition ?? method);
        }

        internal static string? GetMethodOwnerIdentity(IMethodSymbol? method)
        {
            if (method is null)
                return null;

            var containingType = method.ContainingType?.ToFullyQualifiedMetadataName() ?? "<global>";
            return $"{containingType}::{method.MetadataName}/{method.Arity}/{method.Parameters.Length}";
        }

        internal static INamedTypeSymbol? NormalizeTypeOwner(INamedTypeSymbol? type)
        {
            if (type is null)
                return null;

            while (type.UnderlyingSymbol is INamedTypeSymbol underlying &&
                   !ReferenceEquals(underlying, type))
            {
                type = underlying;
            }

            if (type is IConstructedTypeSubstitutionInfo constructed)
                return constructed.DefinitionForSubstitution;

            return (INamedTypeSymbol?)(type.OriginalDefinition ?? type);
        }

        internal static string? GetTypeOwnerIdentity(INamedTypeSymbol? type)
        {
            return type?.ToFullyQualifiedMetadataName();
        }
    }

    private bool TryResolveMetadataTypeParameter(PETypeParameterSymbol symbol, out Type type)
    {
        if (symbol.OwnerKind == TypeParameterOwnerKind.Type &&
            symbol.DeclaringTypeParameterOwner is INamedTypeSymbol containingType)
        {
            var runtimeType = RuntimeSymbolResolver.GetType(containingType, treatUnitAsVoid: true);
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
        else if (symbol.OwnerKind == TypeParameterOwnerKind.Method &&
                 symbol.DeclaringMethodParameterOwner is IMethodSymbol containingMethod)
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

            try
            {
                var resolvedMethod = RuntimeSymbolResolver.GetMethodInfo(containingMethod);
                var parameters = resolvedMethod.IsGenericMethodDefinition
                    ? resolvedMethod.GetGenericArguments()
                    : resolvedMethod.GetGenericMethodDefinition().GetGenericArguments();

                if ((uint)symbol.Ordinal < (uint)parameters.Length)
                {
                    type = parameters[symbol.Ordinal];
                    return true;
                }
            }
            catch (InvalidOperationException)
            {
            }
        }

        type = null!;
        return false;
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
