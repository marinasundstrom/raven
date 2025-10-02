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

namespace Raven.CodeAnalysis.CodeGen;

internal class CodeGenerator
{
    readonly Dictionary<ITypeSymbol, TypeGenerator> _typeGenerators = new Dictionary<ITypeSymbol, TypeGenerator>(SymbolEqualityComparer.Default);
    readonly Dictionary<SourceSymbol, MemberInfo> _mappings = new Dictionary<SourceSymbol, MemberInfo>(SymbolEqualityComparer.Default);
    readonly Dictionary<ITypeParameterSymbol, Type> _genericParameterMap = new Dictionary<ITypeParameterSymbol, Type>(SymbolEqualityComparer.Default);

    public void AddMemberBuilder(SourceSymbol symbol, MemberInfo memberInfo) => _mappings[symbol] = memberInfo;

    public MemberInfo? GetMemberBuilder(SourceSymbol symbol) => _mappings[symbol];

    internal void RegisterGenericParameters(ImmutableArray<ITypeParameterSymbol> parameters, GenericTypeParameterBuilder[] builders)
    {
        if (parameters.IsDefaultOrEmpty || builders.Length == 0)
            return;

        var count = Math.Min(parameters.Length, builders.Length);

        for (var i = 0; i < count; i++)
        {
            var parameter = parameters[i];
            var builder = builders[i];
            _genericParameterMap[parameter] = builder;
        }

        for (var i = 0; i < count; i++)
        {
            ApplyGenericParameterConstraints(parameters[i], builders[i]);
        }
    }

    private void ApplyGenericParameterConstraints(ITypeParameterSymbol parameter, GenericTypeParameterBuilder builder)
    {
        var attributes = GenericParameterAttributes.None;

        if ((parameter.ConstraintKind & TypeParameterConstraintKind.ReferenceType) != 0)
            attributes |= GenericParameterAttributes.ReferenceTypeConstraint;

        if ((parameter.ConstraintKind & TypeParameterConstraintKind.ValueType) != 0)
            attributes |= GenericParameterAttributes.NotNullableValueTypeConstraint;

        builder.SetGenericParameterAttributes(attributes);

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
    public Type? NullType { get; private set; }
    public Type? NullableAttributeType { get; private set; }
    public Type? UnitType { get; private set; }
    ConstructorInfo? _nullableCtor;

    bool _emitTypeUnionAttribute;
    bool _emitNullType;

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
        else if (type is IUnionTypeSymbol u)
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

    static IEnumerable<ITypeSymbol> Flatten(IEnumerable<ITypeSymbol> types)
        => types.SelectMany(t => t is IUnionTypeSymbol u ? Flatten(u.Types) : new[] { t });

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
        var assemblyName = new AssemblyName(_compilation.AssemblyName)
        {
            Version = new Version(1, 0, 0, 0)
        };

        AssemblyBuilder = new PersistedAssemblyBuilder(assemblyName, _compilation.CoreAssembly);
        ModuleBuilder = AssemblyBuilder.DefineDynamicModule(_compilation.AssemblyName);

        DetermineShimTypeRequirements();

        if (_emitTypeUnionAttribute)
            CreateTypeUnionAttribute();
        if (_emitNullType)
            CreateNullStruct();
        CreateUnitStruct();

        DefineTypeBuilders();

        DefineMemberBuilders();

        EmitMemberILBodies();

        CreateTypes();

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

    private void DetermineShimTypeRequirements()
    {
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

        void CheckType(ITypeSymbol typeSymbol)
        {
            if (typeSymbol is null)
                return;

            if (typeSymbol is LiteralTypeSymbol literal)
            {
                CheckType(literal.UnderlyingType);
                return;
            }

            if (typeSymbol.IsUnion && typeSymbol is IUnionTypeSymbol union)
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

    private void CreateTypeUnionAttribute()
    {
        // Define the attribute class
        var attrBuilder = ModuleBuilder.DefineType(
            "TypeUnionAttribute",
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

    private void CreateNullStruct()
    {
        var nullBuilder = ModuleBuilder.DefineType(
            "Null",
            TypeAttributes.Public | TypeAttributes.Sealed | TypeAttributes.SequentialLayout,
            typeof(ValueType));

        NullType = nullBuilder.CreateType();
    }

    private void CreateUnitStruct()
    {
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
        var types = Compilation.Module.GlobalNamespace
            .GetAllMembersRecursive()
            .OfType<ITypeSymbol>()
            .Where(t => t.DeclaringSyntaxReferences.Length > 0)
            .ToArray();

        var synthesizedDelegates = Compilation.GetSynthesizedDelegateTypes().ToArray();

        foreach (var typeSymbol in types)
        {
            GetOrCreateTypeGenerator(typeSymbol);
        }

        foreach (var delegateType in synthesizedDelegates)
        {
            GetOrCreateTypeGenerator(delegateType);
        }

        var visited = new HashSet<ITypeSymbol>(SymbolEqualityComparer.Default);
        var visiting = new HashSet<ITypeSymbol>(SymbolEqualityComparer.Default);

        foreach (var typeSymbol in types)
        {
            EnsureTypeBuilderDefined(typeSymbol, visited, visiting);
        }

        foreach (var delegateType in synthesizedDelegates)
        {
            EnsureTypeBuilderDefined(delegateType, visited, visiting);
        }
    }

    private TypeGenerator GetOrCreateTypeGenerator(ITypeSymbol typeSymbol)
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
        if (visited.Contains(typeSymbol))
            return;

        if (!visiting.Add(typeSymbol))
            return;

        if (!_typeGenerators.TryGetValue(typeSymbol, out var generator))
        {
            visiting.Remove(typeSymbol);
            return;
        }

        if (generator.TypeBuilder is null && generator.Type is null && typeSymbol is INamedTypeSymbol named)
        {
            var baseType = named.BaseType;
            if (baseType is not null && baseType.DeclaringSyntaxReferences.Length > 0)
                EnsureTypeBuilderDefined(baseType, visited, visiting);

            foreach (var interfaceType in named.Interfaces)
            {
                if (interfaceType.DeclaringSyntaxReferences.Length > 0)
                    EnsureTypeBuilderDefined(interfaceType, visited, visiting);
            }
        }

        if (generator.TypeBuilder is null)
            generator.DefineTypeBuilder();

        visiting.Remove(typeSymbol);
        visited.Add(typeSymbol);
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
        foreach (var typeGenerator in _typeGenerators.Values)
        {
            typeGenerator.EmitMemberILBodies();
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
            type = builder.TypeBuilder!; //.CreateType();
            return true;
        }

        type = null!;
        return false;
    }

    internal bool TryGetRuntimeTypeForTypeParameter(ITypeParameterSymbol symbol, out Type type)
    {
        return _genericParameterMap.TryGetValue(symbol, out type!);
    }

}
