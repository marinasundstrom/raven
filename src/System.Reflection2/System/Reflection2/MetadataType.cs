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
    private readonly Lazy<MethodCollection> _methodCollection;

    internal MetadataType(MetadataModule module, TypeDefinitionHandle handle)
    {
        _module = module;
        _reader = module.Reader;
        _handle = handle;
        _definition = _reader.GetTypeDefinition(handle);

        _namespace = new Lazy<string?>(() => _definition.Namespace.IsNil ? null : _reader.GetString(_definition.Namespace));
        _name = new Lazy<string>(() => _reader.GetString(_definition.Name));
        _fullName = new Lazy<string?>(CreateFullName);
        _baseType = new Lazy<Type?>(ResolveBaseType);
        _interfaces = new Lazy<IReadOnlyList<Type>>(ResolveInterfaces);
        _fields = new Lazy<IReadOnlyList<MetadataFieldInfo>>(ResolveFields);
        _methodCollection = new Lazy<MethodCollection>(ResolveMethodCollection);
    }

    public MetadataReader Reader => _reader;

    internal MetadataModule MetadataModule => _module;

    private MethodCollection MethodsAndConstructors => _methodCollection.Value;

    public override Assembly Assembly => _module.Assembly;

    public override string? Namespace => _namespace.Value;

    public override string Name => _name.Value;

    public override string? FullName => _fullName.Value;

    public override string? AssemblyQualifiedName => FullName is null ? null : $"{FullName}, {Assembly.FullName}";

    public override Module Module => _module;

    public override Type? BaseType => _baseType.Value;

    public override Guid GUID => Guid.Empty;

    public override Type? DeclaringType
    {
        get
        {
            var declaring = _definition.GetDeclaringType();
            return declaring.IsNil ? null : _module.ResolveType(declaring);
        }
    }

    public override Type UnderlyingSystemType => this;

    public override int MetadataToken => MetadataTokens.GetToken(_handle);

    public override bool ContainsGenericParameters => _definition.GetGenericParameters().Any();

    public override IEnumerable<CustomAttributeData> CustomAttributes => GetCustomAttributesData();

    public override IList<CustomAttributeData> GetCustomAttributesData()
        => Array.Empty<CustomAttributeData>();

    public override object[] GetCustomAttributes(bool inherit) => Array.Empty<object>();

    public override object[] GetCustomAttributes(Type attributeType, bool inherit) => Array.Empty<object>();

    public override bool IsDefined(Type attributeType, bool inherit) => false;

    public override ConstructorInfo[] GetConstructors(BindingFlags bindingAttr)
        => FilterConstructors(bindingAttr).ToArray();

    public override EventInfo? GetEvent(string name, BindingFlags bindingAttr) => null;

    public override EventInfo[] GetEvents(BindingFlags bindingAttr) => Array.Empty<EventInfo>();

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
            var nested = _module.ResolveType(handle);
            if (BindingFlagsMatch(bindingAttr, nested))
            {
                list.Add(nested);
            }
        }

        return list.ToArray();
    }

    public override PropertyInfo[] GetProperties(BindingFlags bindingAttr) => Array.Empty<PropertyInfo>();

    public override object? InvokeMember(string name, BindingFlags invokeAttr, Binder? binder, object? target, object?[]? args, ParameterModifier[]? modifiers, CultureInfo? culture, string[]? namedParameters)
        => throw new NotSupportedException("Invocation is not supported in metadata-only context.");

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
        => throw new NotSupportedException("Interface mapping is not supported in metadata-only context.");

    public override Type GetElementType() => throw new NotSupportedException();

    public override Type[] GetGenericArguments() => Array.Empty<Type>();

    public override Type GetGenericTypeDefinition() => throw new NotSupportedException();

    public override Type MakeGenericType(params Type[] typeArguments) => throw new NotSupportedException();

    public override int GetArrayRank() => throw new NotSupportedException();

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
        => null;

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
        return baseType is not null && baseType.FullName == "System.ValueType";
    }

    private string? CreateFullName()
    {
        var ns = Namespace;
        return string.IsNullOrEmpty(ns) ? Name : ns + "." + Name;
    }

    private Type? ResolveBaseType()
    {
        if (_definition.BaseType.IsNil)
        {
            return null;
        }

        return _module.ResolveType(_definition.BaseType);
    }

    private IReadOnlyList<Type> ResolveInterfaces()
    {
        var list = new List<Type>();
        foreach (var implementation in _definition.GetInterfaceImplementations())
        {
            var interfaceType = _module.ResolveType(_reader.GetInterfaceImplementation(implementation).Interface);
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
        var methods = new List<MethodInfo>();
        var constructors = new List<ConstructorInfo>();
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
                methods.Add(new MetadataMethodInfo(this, handle));
            }
        }

        return new MethodCollection(methods, constructors);
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
        var isStatic = member switch
        {
            MethodBase method => method.IsStatic,
            FieldInfo field => field.IsStatic,
            _ => false,
        };

        var isPublic = member switch
        {
            MethodBase method => method.IsPublic,
            FieldInfo field => field.IsPublic,
            _ => false,
        };

        if (isStatic && !bindingAttr.HasFlag(BindingFlags.Static))
        {
            return false;
        }

        if (!isStatic && !bindingAttr.HasFlag(BindingFlags.Instance))
        {
            return false;
        }

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

    private sealed record MethodCollection(IReadOnlyList<MethodInfo> Methods, IReadOnlyList<ConstructorInfo> Constructors);
}
