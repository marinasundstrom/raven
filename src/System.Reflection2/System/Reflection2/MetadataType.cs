namespace System.Reflection2;

using System;
using System.Globalization;
using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;
using System.Collections.Generic;
using System.Linq;

/// <summary>
/// Reflection-only <see cref="TypeInfo"/> implementation backed by metadata.
/// </summary>
public sealed class MetadataType : TypeInfo
{
    private readonly MetadataModule _module;
    private readonly MetadataReader _reader;
    private readonly TypeDefinitionHandle _handle;
    private readonly TypeDefinition _definition;
    private readonly Lazy<string?> _namespace;
    private readonly Lazy<string> _name;
    private readonly Lazy<string?> _fullName;
    private readonly Lazy<Type?> _baseType;
    private readonly Lazy<IReadOnlyList<Type>> _interfaces;
    private readonly Lazy<IReadOnlyList<MetadataFieldInfo>> _fields;
    private readonly Lazy<IReadOnlyList<MetadataPropertyInfo>> _properties;
    private readonly Lazy<IReadOnlyList<MetadataEventInfo>> _events;
    private readonly Lazy<MethodCollection> _methodCollection;
    private readonly Lazy<IReadOnlyList<Type>> _genericArguments;
    private readonly Lazy<IList<CustomAttributeData>> _customAttributes;
    private readonly Type[]? _instantiatedTypeArguments;

    internal MetadataType(MetadataModule module, TypeDefinitionHandle handle)
        : this(module, handle, null)
    {
    }

    internal MetadataType(MetadataModule module, TypeDefinitionHandle handle, Type[]? typeArguments)
    {
        _module = module;
        _reader = module.Reader;
        _handle = handle;
        _definition = _reader.GetTypeDefinition(handle);
        _instantiatedTypeArguments = typeArguments;

        _namespace = new Lazy<string?>(() => _definition.Namespace.IsNil ? null : _reader.GetString(_definition.Namespace));
        _name = new Lazy<string>(() => _reader.GetString(_definition.Name));
        _fullName = new Lazy<string?>(CreateFullName);
        _baseType = new Lazy<Type?>(ResolveBaseType);
        _interfaces = new Lazy<IReadOnlyList<Type>>(ResolveInterfaces);
        _fields = new Lazy<IReadOnlyList<MetadataFieldInfo>>(ResolveFields);
        _methodCollection = new Lazy<MethodCollection>(ResolveMethodCollection);
        _properties = new Lazy<IReadOnlyList<MetadataPropertyInfo>>(ResolveProperties);
        _events = new Lazy<IReadOnlyList<MetadataEventInfo>>(ResolveEvents);
        _genericArguments = new Lazy<IReadOnlyList<Type>>(ResolveGenericArguments);
        _customAttributes = new Lazy<IList<CustomAttributeData>>(() => MetadataCustomAttributeDecoder.Decode(_module, _definition.GetCustomAttributes(), this, null));
    }

    public MetadataReader Reader => _reader;

    internal MetadataModule MetadataModule => _module;

    internal TypeDefinitionHandle Handle => _handle;

    private bool IsConstructedGeneric => _instantiatedTypeArguments is not null;

    private MethodCollection MethodsAndConstructors => _methodCollection.Value;

    public override Assembly Assembly => _module.Assembly;

    public override string? Namespace => _namespace.Value;

    public override string Name => _name.Value;

    public override string? FullName
    {
        get
        {
            if (!IsConstructedGeneric)
            {
                return _fullName.Value;
            }

            var definitionName = _fullName.Value;
            if (definitionName is null)
            {
                return null;
            }

            var arguments = string.Join(",", GetGenericArguments().Select(a => a.AssemblyQualifiedName ?? a.FullName ?? a.Name));
            return $"{definitionName}[{arguments}]";
        }
    }

    public override string? AssemblyQualifiedName
    {
        get
        {
            var fullName = FullName;
            return fullName is null ? null : $"{fullName}, {Assembly.FullName}";
        }
    }

    public override Module Module => _module;

    public override Type? BaseType => _baseType.Value;

    public override Guid GUID => Guid.Empty;

    public override Type? DeclaringType
    {
        get
        {
            var declaring = _definition.GetDeclaringType();
            return declaring.IsNil ? null : _module.ResolveType(declaring, null);
        }
    }

    public override Type UnderlyingSystemType => this;

    public override int MetadataToken => MetadataTokens.GetToken(_handle);

    public override bool ContainsGenericParameters
    {
        get
        {
            if (!IsConstructedGeneric)
            {
                return _definition.GetGenericParameters().Any();
            }

            return _instantiatedTypeArguments!.Any(t => t.ContainsGenericParameters);
        }
    }

    public override bool IsGenericType => _definition.GetGenericParameters().Any() || IsConstructedGeneric;

    public override bool IsGenericTypeDefinition => !IsConstructedGeneric && _definition.GetGenericParameters().Any();

    public override IEnumerable<CustomAttributeData> CustomAttributes => _customAttributes.Value;

    public override IList<CustomAttributeData> GetCustomAttributesData()
        => _customAttributes.Value;

    public override object[] GetCustomAttributes(bool inherit)
    {
        var bridge = _module.RuntimeBridge;
        if (bridge is null)
        {
            throw new NotSupportedException("Materializing attribute instances is not supported in metadata-only context.");
        }

        return bridge.GetCustomAttributes(this, attributeType: null, inherit);
    }

    public override object[] GetCustomAttributes(Type attributeType, bool inherit)
    {
        if (attributeType is null)
        {
            throw new ArgumentNullException(nameof(attributeType));
        }

        var bridge = _module.RuntimeBridge;
        if (bridge is null)
        {
            throw new NotSupportedException("Materializing attribute instances is not supported in metadata-only context.");
        }

        return bridge.GetCustomAttributes(this, attributeType, inherit);
    }

    public override bool IsDefined(Type attributeType, bool inherit)
    {
        if (attributeType is null)
        {
            throw new ArgumentNullException(nameof(attributeType));
        }

        var bridge = _module.RuntimeBridge;
        if (bridge is not null)
        {
            return bridge.IsDefined(this, attributeType, inherit);
        }

        return _customAttributes.Value.Any(a => attributeType.IsAssignableFrom(a.AttributeType));
    }

    public override ConstructorInfo[] GetConstructors(BindingFlags bindingAttr)
        => FilterConstructors(bindingAttr).ToArray();

    public override EventInfo? GetEvent(string name, BindingFlags bindingAttr)
        => FilterMembers(_events.Value, name, bindingAttr).FirstOrDefault();

    public override EventInfo[] GetEvents(BindingFlags bindingAttr)
        => FilterMembers(_events.Value, bindingAttr).Cast<EventInfo>().ToArray();

    public override FieldInfo? GetField(string name, BindingFlags bindingAttr)
        => FilterMembers(_fields.Value, name, bindingAttr).FirstOrDefault();

    public override FieldInfo[] GetFields(BindingFlags bindingAttr)
        => FilterMembers(_fields.Value, bindingAttr).Cast<FieldInfo>().ToArray();

    public override Type? GetInterface(string name, bool ignoreCase)
    {
        var comparison = ignoreCase ? StringComparison.OrdinalIgnoreCase : StringComparison.Ordinal;
        return _interfaces.Value.FirstOrDefault(t => string.Equals(t.FullName, name, comparison));
    }

    public override Type[] GetInterfaces() => _interfaces.Value.ToArray();

    public override MemberInfo[] GetMembers(BindingFlags bindingAttr)
        => FilterMembers(bindingAttr).ToArray();

    public override MethodInfo[] GetMethods(BindingFlags bindingAttr)
        => FilterMembers(MethodsAndConstructors.Methods, bindingAttr).Cast<MethodInfo>().ToArray();

    public override Type? GetNestedType(string name, BindingFlags bindingAttr)
        => GetNestedTypes(bindingAttr).FirstOrDefault(t => string.Equals(t.Name, name, bindingAttr.HasFlag(BindingFlags.IgnoreCase) ? StringComparison.OrdinalIgnoreCase : StringComparison.Ordinal));

    public override Type[] GetNestedTypes(BindingFlags bindingAttr)
    {
        var list = new List<Type>();
        foreach (var handle in _definition.GetNestedTypes())
        {
            var nested = _module.ResolveType(handle, null);
            if (BindingFlagsMatch(bindingAttr, nested))
            {
                list.Add(nested);
            }
        }

        return list.ToArray();
    }

    public override PropertyInfo[] GetProperties(BindingFlags bindingAttr)
        => FilterMembers(_properties.Value, bindingAttr).Cast<PropertyInfo>().ToArray();

    public override object? InvokeMember(string name, BindingFlags invokeAttr, Binder? binder, object? target, object?[]? args, ParameterModifier[]? modifiers, CultureInfo? culture, string[]? namedParameters)
    {
        var bridge = _module.RuntimeBridge;
        if (bridge is null)
        {
            throw new NotSupportedException("Invocation is not supported in metadata-only context. Configure MetadataLoadContext.RuntimeBridge to enable invocation.");
        }

        return bridge.InvokeMember(this, name, invokeAttr, binder, target, args, modifiers, culture, namedParameters);
    }

    public override bool IsAssignableFrom(Type? c)
    {
        if (c is null)
        {
            return false;
        }

        if (Equals(c))
        {
            return true;
        }

        var info = c.GetTypeInfo();
        var current = info.BaseType;
        while (current is not null)
        {
            if (Equals(current))
            {
                return true;
            }

            current = current.BaseType;
        }

        return _interfaces.Value.Any(i => i.Equals(c));
    }

    public override bool IsAssignableFrom(TypeInfo? typeInfo)
        => typeInfo is not null && IsAssignableFrom(typeInfo.AsType());

    public override bool IsSubclassOf(Type c)
    {
        var baseType = BaseType;
        while (baseType is not null)
        {
            if (baseType.Equals(c))
            {
                return true;
            }

            baseType = baseType.BaseType;
        }

        return false;
    }

    public override InterfaceMapping GetInterfaceMap(Type interfaceType)
    {
        if (interfaceType is null)
        {
            throw new ArgumentNullException(nameof(interfaceType));
        }

        if (!interfaceType.IsInterface)
        {
            throw new ArgumentException($"Type '{interfaceType}' is not an interface.", nameof(interfaceType));
        }

        var implementedInterfaces = GetInterfaces();
        Type? resolvedInterface = null;
        foreach (var candidate in implementedInterfaces)
        {
            if (InterfaceMatches(candidate, interfaceType))
            {
                resolvedInterface = candidate;
                break;
            }
        }

        if (resolvedInterface is null)
        {
            var baseType = BaseType;
            if (baseType is not null)
            {
                try
                {
                    return baseType.GetInterfaceMap(interfaceType);
                }
                catch (ArgumentException)
                {
                }
            }

            resolvedInterface = interfaceType;
        }

        interfaceType = resolvedInterface;

        var interfaceMethods = interfaceType.GetMethods(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance);
        var targetMethods = new MethodInfo[interfaceMethods.Length];

        var implementations = BuildInterfaceImplementationMap(interfaceType);

        for (var i = 0; i < interfaceMethods.Length; i++)
        {
            var interfaceMethod = interfaceMethods[i];
            if (!implementations.TryGetValue(interfaceMethod, out var target))
            {
                target = FindImplicitInterfaceImplementation(interfaceType, interfaceMethod);
            }

            if (target is null)
            {
                if (!interfaceMethod.IsAbstract)
                {
                    target = interfaceMethod;
                }
                else
                {
                    throw new TypeLoadException($"Type '{FullName}' does not implement interface method '{interfaceMethod.Name}' from '{interfaceType}'.");
                }
            }

            targetMethods[i] = target;
        }

        return new InterfaceMapping
        {
            InterfaceType = interfaceType,
            TargetType = this,
            InterfaceMethods = interfaceMethods,
            TargetMethods = targetMethods,
        };
    }

    private static bool InterfaceMatches(Type candidate, Type target)
    {
        if (ReferenceEquals(candidate, target))
        {
            return true;
        }

        if (candidate.MetadataToken == target.MetadataToken && Equals(candidate.Module, target.Module))
        {
            return true;
        }

        var candidateName = candidate.FullName;
        var targetName = target.FullName;
        if (candidateName is not null && targetName is not null && string.Equals(candidateName, targetName, StringComparison.Ordinal))
        {
            return true;
        }

        return false;
    }

    public override Type? GetElementType() => null;

    public override Type[] GetGenericArguments() => _genericArguments.Value.ToArray();

    public override Type GetGenericTypeDefinition()
    {
        if (!IsGenericType)
        {
            throw new InvalidOperationException("Type is not generic.");
        }

        return _module.GetOrAddType(_handle);
    }

    public override Type MakeGenericType(params Type[] typeArguments)
    {
        if (typeArguments is null)
        {
            throw new ArgumentNullException(nameof(typeArguments));
        }

        if (!IsGenericTypeDefinition)
        {
            throw new InvalidOperationException($"Type '{FullName}' is not a generic type definition.");
        }

        if (_definition.GetGenericParameters().Count != typeArguments.Length)
        {
            throw new ArgumentException($"Type '{FullName}' expects {_definition.GetGenericParameters().Count} arguments.", nameof(typeArguments));
        }

        return _module.ConstructGenericType(this, typeArguments);
    }

    public override Type MakeArrayType()
        => _module.GetArrayType(this, 1);

    public override Type MakeArrayType(int rank)
        => _module.GetArrayType(this, rank);

    public override Type MakeByRefType()
        => _module.GetByRefType(this);

    public override Type MakePointerType()
        => _module.GetPointerType(this);

    public override int GetArrayRank()
        => throw new ArgumentException("Type is not an array.");

    protected override ConstructorInfo? GetConstructorImpl(BindingFlags bindingAttr, Binder? binder, CallingConventions callConvention, Type[]? types, ParameterModifier[]? modifiers)
    {
        var constructors = FilterConstructors(bindingAttr).ToArray();
        if (types is null || types.Length == 0)
        {
            return constructors.FirstOrDefault();
        }

        foreach (var ctor in constructors)
        {
            var parameters = ctor.GetParameters();
            if (ParametersMatch(parameters, types))
            {
                return ctor;
            }
        }

        return null;
    }

    protected override MethodInfo? GetMethodImpl(string name, BindingFlags bindingAttr, Binder? binder, CallingConventions callConvention, Type[]? types, ParameterModifier[]? modifiers)
    {
        var candidates = FilterMembers(MethodsAndConstructors.Methods, name, bindingAttr).ToArray();
        if (types is null || types.Length == 0)
        {
            return candidates.FirstOrDefault();
        }

        foreach (var method in candidates)
        {
            var parameters = method.GetParameters();
            if (ParametersMatch(parameters, types))
            {
                return method;
            }
        }

        return null;
    }

    protected override PropertyInfo? GetPropertyImpl(string name, BindingFlags bindingAttr, Binder? binder, Type? returnType, Type[]? types, ParameterModifier[]? modifiers)
    {
        var candidates = FilterMembers(_properties.Value, name, bindingAttr).ToArray();
        foreach (var property in candidates)
        {
            if (returnType is not null && !Equals(property.PropertyType, returnType))
            {
                continue;
            }

            if (types is not null && types.Length > 0)
            {
                var parameters = property.GetIndexParameters();
                if (!ParametersMatch(parameters, types))
                {
                    continue;
                }
            }

            return property;
        }

        return null;
    }

    protected override TypeAttributes GetAttributeFlagsImpl() => _definition.Attributes;

    protected override bool HasElementTypeImpl() => false;

    protected override bool IsArrayImpl() => false;

    protected override bool IsByRefImpl() => false;

    protected override bool IsPointerImpl() => false;

    protected override bool IsPrimitiveImpl() => false;

    protected override bool IsCOMObjectImpl() => false;

    protected override bool IsValueTypeImpl()
    {
        var baseType = BaseType;
        if (baseType is null)
        {
            return false;
        }

        return baseType.FullName is "System.ValueType" or "System.Enum";
    }

    private string? CreateFullName()
    {
        var declaringType = DeclaringType;
        if (declaringType is not null)
        {
            var declaringName = declaringType.FullName;
            if (declaringName is null)
            {
                return null;
            }

            return declaringName + "+" + Name;
        }

        var ns = Namespace;
        return string.IsNullOrEmpty(ns) ? Name : ns + "." + Name;
    }

    private Type? ResolveBaseType()
    {
        if (_definition.BaseType.IsNil)
        {
            return null;
        }

        return _module.ResolveType(_definition.BaseType, this);
    }

    private IReadOnlyList<Type> ResolveInterfaces()
    {
        var list = new List<Type>();
        foreach (var implementation in _definition.GetInterfaceImplementations())
        {
            var interfaceType = _module.ResolveType(_reader.GetInterfaceImplementation(implementation).Interface, this);
            list.Add(interfaceType);
        }

        return list;
    }

    private IReadOnlyList<MetadataFieldInfo> ResolveFields()
    {
        var list = new List<MetadataFieldInfo>();
        foreach (var handle in _definition.GetFields())
        {
            list.Add(new MetadataFieldInfo(this, handle));
        }

        return list;
    }

    private MethodCollection ResolveMethodCollection()
    {
        var methods = new List<MetadataMethodInfo>();
        var constructors = new List<ConstructorInfo>();
        var methodMap = new Dictionary<MethodDefinitionHandle, MetadataMethodInfo>();
        foreach (var handle in _definition.GetMethods())
        {
            var definition = _reader.GetMethodDefinition(handle);
            var name = _reader.GetString(definition.Name);
            if (name is ".ctor" or ".cctor")
            {
                constructors.Add(new MetadataConstructorInfo(this, handle));
            }
            else
            {
                var method = new MetadataMethodInfo(this, handle);
                methods.Add(method);
                methodMap[handle] = method;
            }
        }

        return new MethodCollection(methods, constructors, methodMap);
    }

    private IReadOnlyList<MetadataPropertyInfo> ResolveProperties()
    {
        var list = new List<MetadataPropertyInfo>();
        foreach (var handle in _definition.GetProperties())
        {
            list.Add(new MetadataPropertyInfo(this, handle));
        }

        return list;
    }

    private IReadOnlyList<MetadataEventInfo> ResolveEvents()
    {
        var list = new List<MetadataEventInfo>();
        foreach (var handle in _definition.GetEvents())
        {
            list.Add(new MetadataEventInfo(this, handle));
        }

        return list;
    }

    private IReadOnlyList<Type> ResolveGenericArguments()
    {
        if (_instantiatedTypeArguments is not null)
        {
            return _instantiatedTypeArguments;
        }

        var handles = _definition.GetGenericParameters().ToArray();
        if (handles.Length == 0)
        {
            return Array.Empty<Type>();
        }

        var parameters = new Type[handles.Length];
        for (var i = 0; i < handles.Length; i++)
        {
            parameters[i] = new MetadataGenericParameterType(_module, this, null, handles[i]);
        }

        return parameters;
    }

    private IEnumerable<ConstructorInfo> FilterConstructors(BindingFlags bindingAttr)
    {
        foreach (var ctor in MethodsAndConstructors.Constructors)
        {
            if (BindingFlagsMatch(bindingAttr, ctor))
            {
                yield return ctor;
            }
        }
    }

    private IEnumerable<MemberInfo> FilterMembers(BindingFlags bindingAttr)
    {
        foreach (var method in MethodsAndConstructors.Methods)
        {
            if (BindingFlagsMatch(bindingAttr, method))
            {
                yield return method;
            }
        }

        foreach (var ctor in MethodsAndConstructors.Constructors)
        {
            if (BindingFlagsMatch(bindingAttr, ctor))
            {
                yield return ctor;
            }
        }

        foreach (var field in _fields.Value)
        {
            if (BindingFlagsMatch(bindingAttr, field))
            {
                yield return field;
            }
        }

        foreach (var property in _properties.Value)
        {
            if (BindingFlagsMatch(bindingAttr, property))
            {
                yield return property;
            }
        }

        foreach (var @event in _events.Value)
        {
            if (BindingFlagsMatch(bindingAttr, @event))
            {
                yield return @event;
            }
        }
    }

    private static IEnumerable<TMember> FilterMembers<TMember>(IEnumerable<TMember> source, BindingFlags bindingAttr)
        where TMember : MemberInfo
        => source.Where(member => BindingFlagsMatch(bindingAttr, member));

    private static IEnumerable<TMember> FilterMembers<TMember>(IEnumerable<TMember> source, string name, BindingFlags bindingAttr)
        where TMember : MemberInfo
    {
        var comparison = bindingAttr.HasFlag(BindingFlags.IgnoreCase) ? StringComparison.OrdinalIgnoreCase : StringComparison.Ordinal;
        foreach (var member in source)
        {
            if (!BindingFlagsMatch(bindingAttr, member))
            {
                continue;
            }

            if (string.Equals(member.Name, name, comparison))
            {
                yield return member;
            }
        }
    }

    internal static bool BindingFlagsMatch(BindingFlags bindingAttr, MemberInfo member)
    {
        bool isStatic;
        bool hasPublicVisibility;
        bool hasNonPublicVisibility;

        switch (member)
        {
            case MethodBase method:
                isStatic = method.IsStatic;
                hasPublicVisibility = method.IsPublic;
                hasNonPublicVisibility = !method.IsPublic;
                break;
            case FieldInfo field:
                isStatic = field.IsStatic;
                hasPublicVisibility = field.IsPublic;
                hasNonPublicVisibility = !field.IsPublic;
                break;
            case PropertyInfo property:
                var getter = property.GetMethod;
                var setter = property.SetMethod;
                var accessor = getter ?? setter;
                if (accessor is null)
                {
                    return false;
                }

                isStatic = accessor.IsStatic;
                hasPublicVisibility = (getter?.IsPublic ?? false) || (setter?.IsPublic ?? false);
                hasNonPublicVisibility = (getter is not null && !getter.IsPublic) || (setter is not null && !setter.IsPublic);
                break;
            case EventInfo @event:
                var addMethod = @event.AddMethod;
                var removeMethod = @event.RemoveMethod;
                var raiseMethod = @event.RaiseMethod;
                var eventAccessor = addMethod ?? removeMethod ?? raiseMethod;
                if (eventAccessor is null)
                {
                    return false;
                }

                isStatic = eventAccessor.IsStatic;
                hasPublicVisibility = (addMethod?.IsPublic ?? false) || (removeMethod?.IsPublic ?? false) || (raiseMethod?.IsPublic ?? false);
                hasNonPublicVisibility = (addMethod is not null && !addMethod.IsPublic)
                    || (removeMethod is not null && !removeMethod.IsPublic)
                    || (raiseMethod is not null && !raiseMethod.IsPublic);
                break;
            default:
                return false;
        }

        if (isStatic && !bindingAttr.HasFlag(BindingFlags.Static))
        {
            return false;
        }

        if (!isStatic && !bindingAttr.HasFlag(BindingFlags.Instance))
        {
            return false;
        }

        var visibilityMatch = (hasPublicVisibility && bindingAttr.HasFlag(BindingFlags.Public))
            || (hasNonPublicVisibility && bindingAttr.HasFlag(BindingFlags.NonPublic));

        return visibilityMatch;
    }

    private static bool BindingFlagsMatch(BindingFlags bindingAttr, Type type)
    {
        var isNested = type.IsNested;
        var isPublic = isNested ? type.IsNestedPublic : type.IsPublic;
        if (isPublic && !bindingAttr.HasFlag(BindingFlags.Public))
        {
            return false;
        }

        if (!isPublic && !bindingAttr.HasFlag(BindingFlags.NonPublic))
        {
            return false;
        }

        return true;
    }

    private static bool ParametersMatch(ParameterInfo[] parameters, Type[] types)
    {
        if (parameters.Length != types.Length)
        {
            return false;
        }

        for (var i = 0; i < parameters.Length; i++)
        {
            if (!Equals(parameters[i].ParameterType, types[i]))
            {
                return false;
            }
        }

        return true;
    }

    internal MethodInfo? ResolveMethod(MethodDefinitionHandle handle)
        => MethodsAndConstructors.TryGetMethod(handle);

    internal FieldInfo? ResolveField(FieldDefinitionHandle handle)
    {
        foreach (var field in _fields.Value)
        {
            if (field.Handle == handle)
            {
                return field;
            }
        }

        return null;
    }

    private Dictionary<MethodInfo, MethodInfo> BuildInterfaceImplementationMap(Type interfaceType)
    {
        var map = new Dictionary<MethodInfo, MethodInfo>();
        foreach (var implementationHandle in _definition.GetMethodImplementations())
        {
            var implementation = _reader.GetMethodImplementation(implementationHandle);
            if (_module.ResolveMethod(implementation.MethodDeclaration, this) is not MethodInfo declaration)
            {
                continue;
            }

            if (!Equals(declaration.DeclaringType, interfaceType))
            {
                continue;
            }

            if (_module.ResolveMethod(implementation.MethodBody, this) is MethodInfo body)
            {
                map[declaration] = body;
            }
        }

        return map;
    }

    private MethodInfo? FindImplicitInterfaceImplementation(Type interfaceType, MethodInfo interfaceMethod)
    {
        const BindingFlags candidateFlags = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic;
        foreach (var candidate in GetMethods(candidateFlags))
        {
            if (candidate.IsStatic)
            {
                continue;
            }

            if (!candidate.IsPublic)
            {
                continue;
            }

            if (!SignatureMatches(interfaceMethod, candidate))
            {
                continue;
            }

            return candidate;
        }

        var baseType = BaseType;
        while (baseType is not null)
        {
            InterfaceMapping baseMap;
            try
            {
                baseMap = baseType.GetInterfaceMap(interfaceType);
            }
            catch (ArgumentException)
            {
                baseType = baseType.BaseType;
                continue;
            }
            catch (TypeLoadException)
            {
                baseType = baseType.BaseType;
                continue;
            }

            for (var i = 0; i < baseMap.InterfaceMethods.Length; i++)
            {
                if (Equals(baseMap.InterfaceMethods[i], interfaceMethod))
                {
                    return baseMap.TargetMethods[i];
                }
            }

            baseType = baseType.BaseType;
        }

        return null;
    }

    private static bool SignatureMatches(MethodInfo interfaceMethod, MethodInfo candidate)
    {
        if (!string.Equals(interfaceMethod.Name, candidate.Name, StringComparison.Ordinal))
        {
            return false;
        }

        if (!Equals(interfaceMethod.ReturnType, candidate.ReturnType))
        {
            return false;
        }

        if (interfaceMethod.IsGenericMethod != candidate.IsGenericMethod)
        {
            return false;
        }

        if (interfaceMethod.IsGenericMethod)
        {
            var interfaceArgs = interfaceMethod.GetGenericArguments();
            var candidateArgs = candidate.GetGenericArguments();
            if (interfaceArgs.Length != candidateArgs.Length)
            {
                return false;
            }
        }

        var interfaceParameters = interfaceMethod.GetParameters();
        var candidateParameters = candidate.GetParameters();
        if (interfaceParameters.Length != candidateParameters.Length)
        {
            return false;
        }

        for (var i = 0; i < interfaceParameters.Length; i++)
        {
            if (!Equals(interfaceParameters[i].ParameterType, candidateParameters[i].ParameterType))
            {
                return false;
            }
        }

        return true;
    }

    private sealed class MethodCollection
    {
        private readonly IReadOnlyList<MetadataMethodInfo> _methods;
        private readonly IReadOnlyList<ConstructorInfo> _constructors;
        private readonly Dictionary<MethodDefinitionHandle, MetadataMethodInfo> _methodMap;

        public MethodCollection(IReadOnlyList<MetadataMethodInfo> methods, IReadOnlyList<ConstructorInfo> constructors, Dictionary<MethodDefinitionHandle, MetadataMethodInfo> methodMap)
        {
            _methods = methods;
            _constructors = constructors;
            _methodMap = methodMap;
        }

        public IReadOnlyList<MetadataMethodInfo> Methods => _methods;

        public IReadOnlyList<ConstructorInfo> Constructors => _constructors;

        public MetadataMethodInfo? TryGetMethod(MethodDefinitionHandle handle)
            => _methodMap.TryGetValue(handle, out var method) ? method : null;
    }
}
