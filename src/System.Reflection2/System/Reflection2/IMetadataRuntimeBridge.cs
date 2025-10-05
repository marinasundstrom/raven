namespace System.Reflection2;

using System;
using System.Globalization;
using System.Reflection;

/// <summary>
/// Provides runtime services for metadata-backed reflection objects that need to execute code.
/// </summary>
public interface IMetadataRuntimeBridge
{
    object? Invoke(MethodInfo method, object? target, BindingFlags invokeAttr, Binder? binder, object?[]? parameters, CultureInfo? culture);

    object? Invoke(ConstructorInfo constructor, BindingFlags invokeAttr, Binder? binder, object?[]? parameters, CultureInfo? culture);

    object? GetValue(PropertyInfo property, object? obj, BindingFlags invokeAttr, Binder? binder, object?[]? index, CultureInfo? culture);

    void SetValue(PropertyInfo property, object? obj, object? value, BindingFlags invokeAttr, Binder? binder, object?[]? index, CultureInfo? culture);

    object? GetValue(FieldInfo field, object? obj);

    void SetValue(FieldInfo field, object? obj, object? value, BindingFlags invokeAttr, Binder? binder, CultureInfo? culture);

    void AddEventHandler(EventInfo eventInfo, object? target, Delegate? handler);

    void RemoveEventHandler(EventInfo eventInfo, object? target, Delegate? handler);

    object? InvokeMember(Type type, string name, BindingFlags invokeAttr, Binder? binder, object? target, object?[]? args, ParameterModifier[]? modifiers, CultureInfo? culture, string[]? namedParameters);

    object[] GetCustomAttributes(MemberInfo member, Type? attributeType, bool inherit);

    bool IsDefined(MemberInfo member, Type attributeType, bool inherit);

    object[] GetCustomAttributes(ParameterInfo parameter, Type? attributeType, bool inherit);

    bool IsDefined(ParameterInfo parameter, Type attributeType, bool inherit);
}
