namespace System.Reflection2;

using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Reflection;

/// <summary>
/// An <see cref="IMetadataRuntimeBridge"/> that delegates invocation to real runtime reflection types.
/// </summary>
public sealed class RuntimeReflectionBridge : IMetadataRuntimeBridge
{
    private static readonly BindingFlags AllMembers = BindingFlags.Instance | BindingFlags.Static | BindingFlags.Public | BindingFlags.NonPublic;

    private readonly Func<MetadataType, Type?> _runtimeTypeResolver;
    private readonly Dictionary<string, Type> _cache = new(StringComparer.Ordinal);

    public RuntimeReflectionBridge(Func<MetadataType, Type?> runtimeTypeResolver)
    {
        _runtimeTypeResolver = runtimeTypeResolver ?? throw new ArgumentNullException(nameof(runtimeTypeResolver));
    }

    public object? Invoke(MethodInfo method, object? target, BindingFlags invokeAttr, Binder? binder, object?[]? parameters, CultureInfo? culture)
    {
        var runtimeMethod = ResolveRuntimeMethod(method);
        return runtimeMethod.Invoke(target, invokeAttr, binder, parameters, culture);
    }

    public object? Invoke(ConstructorInfo constructor, BindingFlags invokeAttr, Binder? binder, object?[]? parameters, CultureInfo? culture)
    {
        var runtimeConstructor = ResolveRuntimeConstructor(constructor);
        return runtimeConstructor.Invoke(invokeAttr, binder, parameters, culture);
    }

    public object? GetValue(PropertyInfo property, object? obj, BindingFlags invokeAttr, Binder? binder, object?[]? index, CultureInfo? culture)
    {
        var runtimeProperty = ResolveRuntimeProperty(property);
        return runtimeProperty.GetValue(obj, invokeAttr, binder, index, culture);
    }

    public void SetValue(PropertyInfo property, object? obj, object? value, BindingFlags invokeAttr, Binder? binder, object?[]? index, CultureInfo? culture)
    {
        var runtimeProperty = ResolveRuntimeProperty(property);
        runtimeProperty.SetValue(obj, value, invokeAttr, binder, index, culture);
    }

    public object? GetValue(FieldInfo field, object? obj)
    {
        var runtimeField = ResolveRuntimeField(field);
        return runtimeField.GetValue(obj);
    }

    public void SetValue(FieldInfo field, object? obj, object? value, BindingFlags invokeAttr, Binder? binder, CultureInfo? culture)
    {
        var runtimeField = ResolveRuntimeField(field);
        runtimeField.SetValue(obj, value, invokeAttr, binder, culture);
    }

    public void AddEventHandler(EventInfo eventInfo, object? target, Delegate? handler)
    {
        var runtimeEvent = ResolveRuntimeEvent(eventInfo);
        runtimeEvent.AddEventHandler(target, handler);
    }

    public void RemoveEventHandler(EventInfo eventInfo, object? target, Delegate? handler)
    {
        var runtimeEvent = ResolveRuntimeEvent(eventInfo);
        runtimeEvent.RemoveEventHandler(target, handler);
    }

    public object? InvokeMember(Type type, string name, BindingFlags invokeAttr, Binder? binder, object? target, object?[]? args, ParameterModifier[]? modifiers, CultureInfo? culture, string[]? namedParameters)
    {
        var runtimeType = ResolveRuntimeType(type);
        return runtimeType.InvokeMember(name, invokeAttr, binder, target, args, modifiers, culture, namedParameters);
    }

    public object[] GetCustomAttributes(MemberInfo member, Type? attributeType, bool inherit)
    {
        var runtimeMember = ResolveRuntimeMember(member);
        return attributeType is null
            ? runtimeMember.GetCustomAttributes(inherit)
            : runtimeMember.GetCustomAttributes(attributeType, inherit);
    }

    public bool IsDefined(MemberInfo member, Type attributeType, bool inherit)
    {
        var runtimeMember = ResolveRuntimeMember(member);
        return runtimeMember.IsDefined(attributeType, inherit);
    }

    public object[] GetCustomAttributes(ParameterInfo parameter, Type? attributeType, bool inherit)
    {
        var runtimeParameter = ResolveRuntimeParameter(parameter);
        return attributeType is null
            ? runtimeParameter.GetCustomAttributes(inherit)
            : runtimeParameter.GetCustomAttributes(attributeType, inherit);
    }

    public bool IsDefined(ParameterInfo parameter, Type attributeType, bool inherit)
    {
        var runtimeParameter = ResolveRuntimeParameter(parameter);
        return runtimeParameter.IsDefined(attributeType, inherit);
    }

    private MethodInfo ResolveRuntimeMethod(MethodInfo method)
    {
        if (method.DeclaringType is not MetadataType metadataType)
        {
            return method;
        }

        var runtimeType = ResolveRuntimeType(metadataType);
        var runtimeParameters = method.GetParameters().Select(p => ToRuntimeType(p.ParameterType)).ToArray();
        var runtimeMethods = runtimeType.GetMethods(AllMembers).Where(m => m.Name == method.Name);
        foreach (var candidate in runtimeMethods)
        {
            if (!ParametersMatch(candidate.GetParameters(), runtimeParameters))
            {
                continue;
            }

            if (method.IsGenericMethodDefinition)
            {
                return candidate.IsGenericMethodDefinition ? candidate : candidate.GetGenericMethodDefinition();
            }

            if (method.IsGenericMethod)
            {
                var genericArguments = method.GetGenericArguments().Select(ToRuntimeType).ToArray();
                return candidate.IsGenericMethodDefinition ? candidate.MakeGenericMethod(genericArguments) : candidate;
            }

            return candidate;
        }

        throw new MissingMethodException(runtimeType.FullName, method.Name);
    }

    private ConstructorInfo ResolveRuntimeConstructor(ConstructorInfo constructor)
    {
        if (constructor.DeclaringType is not MetadataType metadataType)
        {
            return constructor;
        }

        var runtimeType = ResolveRuntimeType(metadataType);
        var runtimeParameters = constructor.GetParameters().Select(p => ToRuntimeType(p.ParameterType)).ToArray();
        var candidates = runtimeType.GetConstructors(AllMembers);
        foreach (var candidate in candidates)
        {
            if (ParametersMatch(candidate.GetParameters(), runtimeParameters))
            {
                return candidate;
            }
        }

        throw new MissingMethodException(runtimeType.FullName, constructor.Name);
    }

    private PropertyInfo ResolveRuntimeProperty(PropertyInfo property)
    {
        if (property.DeclaringType is not MetadataType metadataType)
        {
            return property;
        }

        var runtimeType = ResolveRuntimeType(metadataType);
        var runtimeIndexParameters = property.GetIndexParameters().Select(p => ToRuntimeType(p.ParameterType)).ToArray();
        var runtimePropertyType = ToRuntimeType(property.PropertyType);
        var properties = runtimeType.GetProperties(AllMembers).Where(p => p.Name == property.Name);
        foreach (var candidate in properties)
        {
            if (candidate.PropertyType != runtimePropertyType)
            {
                continue;
            }

            if (ParametersMatch(candidate.GetIndexParameters(), runtimeIndexParameters))
            {
                return candidate;
            }
        }

        throw new MissingMemberException(runtimeType.FullName, property.Name);
    }

    private FieldInfo ResolveRuntimeField(FieldInfo field)
    {
        if (field.DeclaringType is not MetadataType metadataType)
        {
            return field;
        }

        var runtimeType = ResolveRuntimeType(metadataType);
        var fields = runtimeType.GetFields(AllMembers);
        return fields.FirstOrDefault(f => f.Name == field.Name) ?? throw new MissingFieldException(runtimeType.FullName, field.Name);
    }

    private EventInfo ResolveRuntimeEvent(EventInfo eventInfo)
    {
        if (eventInfo.DeclaringType is not MetadataType metadataType)
        {
            return eventInfo;
        }

        var runtimeType = ResolveRuntimeType(metadataType);
        var events = runtimeType.GetEvents(AllMembers);
        return events.FirstOrDefault(e => e.Name == eventInfo.Name) ?? throw new MissingMemberException(runtimeType.FullName, eventInfo.Name);
    }

    private MemberInfo ResolveRuntimeMember(MemberInfo member)
    {
        return member switch
        {
            MetadataType metadataType => ResolveRuntimeType(metadataType),
            MetadataMethodInfo metadataMethod => ResolveRuntimeMethod(metadataMethod),
            MetadataConstructorInfo metadataConstructor => ResolveRuntimeConstructor(metadataConstructor),
            MetadataPropertyInfo metadataProperty => ResolveRuntimeProperty(metadataProperty),
            MetadataFieldInfo metadataField => ResolveRuntimeField(metadataField),
            MetadataEventInfo metadataEvent => ResolveRuntimeEvent(metadataEvent),
            _ => member,
        };
    }

    private ParameterInfo ResolveRuntimeParameter(ParameterInfo parameter)
    {
        if (parameter is not MetadataParameterInfo metadataParameter)
        {
            return parameter;
        }

        if (metadataParameter.DeclaringMember is MethodInfo method)
        {
            var runtimeMethod = ResolveRuntimeMethod(method);
            if (metadataParameter.IsReturnParameter)
            {
                return runtimeMethod.ReturnParameter;
            }

            var parameters = runtimeMethod.GetParameters();
            if ((uint)metadataParameter.Position >= (uint)parameters.Length)
            {
                throw new InvalidOperationException($"Unable to resolve runtime parameter at position {metadataParameter.Position} for method '{method.Name}'.");
            }

            return parameters[metadataParameter.Position];
        }

        if (metadataParameter.DeclaringMember is ConstructorInfo constructor)
        {
            var runtimeConstructor = ResolveRuntimeConstructor(constructor);
            var parameters = runtimeConstructor.GetParameters();
            if ((uint)metadataParameter.Position >= (uint)parameters.Length)
            {
                throw new InvalidOperationException($"Unable to resolve runtime parameter at position {metadataParameter.Position} for constructor '{constructor.Name}'.");
            }

            return parameters[metadataParameter.Position];
        }

        throw new InvalidOperationException("Metadata parameter does not have a declaring member to resolve against.");
    }

    private Type ResolveRuntimeType(Type type)
    {
        if (type is not MetadataType metadataType)
        {
            return type;
        }

        var key = metadataType.AssemblyQualifiedName ?? metadataType.FullName ?? metadataType.Name;
        if (key is null)
        {
            throw new InvalidOperationException($"Unable to resolve runtime type for metadata type '{metadataType}'.");
        }

        if (_cache.TryGetValue(key, out var runtimeType))
        {
            return runtimeType;
        }

        runtimeType = _runtimeTypeResolver(metadataType);
        runtimeType ??= TryResolveFromRuntime(metadataType);
        if (runtimeType is null)
        {
            throw new TypeLoadException($"Unable to resolve runtime type for '{key}'.");
        }

        if (metadataType.IsGenericType && !metadataType.IsGenericTypeDefinition)
        {
            var definition = (MetadataType)metadataType.GetGenericTypeDefinition();
            var runtimeDefinition = ResolveRuntimeType(definition);
            var runtimeArguments = metadataType.GetGenericArguments().Select(ToRuntimeType).ToArray();
            runtimeType = runtimeDefinition.MakeGenericType(runtimeArguments);
        }

        _cache[key] = runtimeType;
        return runtimeType;
    }

    private static Type? TryResolveFromRuntime(MetadataType metadataType)
    {
        if (metadataType.AssemblyQualifiedName is { } qualified)
        {
            return Type.GetType(qualified, throwOnError: false);
        }

        if (metadataType.FullName is { } fullName)
        {
            return Type.GetType(fullName, throwOnError: false);
        }

        return null;
    }

    private Type ToRuntimeType(Type type)
    {
        if (type is MetadataType metadataType)
        {
            return ResolveRuntimeType(metadataType);
        }

        if (type.HasElementType)
        {
            var elementType = type.GetElementType();
            if (elementType is null)
            {
                throw new NotSupportedException($"Unable to resolve element type for '{type}'.");
            }

            var runtimeElement = ToRuntimeType(elementType);
            if (type.IsArray)
            {
                return type.GetArrayRank() == 1 ? runtimeElement.MakeArrayType() : runtimeElement.MakeArrayType(type.GetArrayRank());
            }

            if (type.IsByRef)
            {
                return runtimeElement.MakeByRefType();
            }

            if (type.IsPointer)
            {
                return runtimeElement.MakePointerType();
            }
        }

        return type;
    }

    private static bool ParametersMatch(IReadOnlyList<ParameterInfo> candidates, IReadOnlyList<Type> expected)
    {
        if (candidates.Count != expected.Count)
        {
            return false;
        }

        for (var i = 0; i < candidates.Count; i++)
        {
            if (candidates[i].ParameterType != expected[i])
            {
                return false;
            }
        }

        return true;
    }
}
