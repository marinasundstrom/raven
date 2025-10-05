namespace System.Reflection2;

using System;
using System.Collections.Generic;
using System.Globalization;
using System.Reflection;

/// <summary>
/// Placeholder generic parameter used when signature decoding lacks explicit context.
/// </summary>
internal sealed class MetadataSignatureGenericParameterType : TypeInfo
{
    private readonly MetadataModule _module;
    private readonly MetadataType? _declaringType;
    private readonly bool _isMethodParameter;
    private readonly int _position;

    public MetadataSignatureGenericParameterType(MetadataModule module, MetadataType? declaringType, bool isMethodParameter, int position)
    {
        _module = module ?? throw new ArgumentNullException(nameof(module));
        _declaringType = declaringType;
        _isMethodParameter = isMethodParameter;
        _position = position;
    }

    public override Assembly Assembly => _declaringType?.Assembly ?? _module.Assembly;

    public override string? Namespace => _declaringType?.Namespace;

    public override string Name => _isMethodParameter ? $"!!{_position}" : $"!{_position}";

    public override string? FullName => null;

    public override string? AssemblyQualifiedName => null;

    public override Module Module => _declaringType?.Module ?? _module;

    public override Type? BaseType => typeof(object);

    public override Guid GUID => Guid.Empty;

    public override Type? DeclaringType => _declaringType;

    public override MethodBase? DeclaringMethod => null;

    public override int MetadataToken => 0;

    public override bool ContainsGenericParameters => true;

    public override bool IsGenericType => false;

    public override bool IsGenericTypeDefinition => false;

    public override bool IsGenericParameter => true;

    public override int GenericParameterPosition => _position;

    public override GenericParameterAttributes GenericParameterAttributes => GenericParameterAttributes.None;

    public override Type UnderlyingSystemType => this;

    public override Type[] GetGenericParameterConstraints() => Array.Empty<Type>();

    public override Type[] GetGenericArguments() => Array.Empty<Type>();

    public override Type GetGenericTypeDefinition() => throw new InvalidOperationException("Generic parameter does not have a definition.");

    public override Type MakeArrayType() => throw new NotSupportedException();

    public override Type MakeArrayType(int rank) => throw new NotSupportedException();

    public override Type MakeByRefType() => throw new NotSupportedException();

    public override Type MakeGenericType(params Type[] typeArguments) => throw new InvalidOperationException("Generic parameter cannot be constructed.");

    public override Type MakePointerType() => throw new NotSupportedException();

    public override ConstructorInfo[] GetConstructors(BindingFlags bindingAttr) => Array.Empty<ConstructorInfo>();

    public override object[] GetCustomAttributes(bool inherit) => Array.Empty<object>();

    public override object[] GetCustomAttributes(Type attributeType, bool inherit) => Array.Empty<object>();

    public override IList<CustomAttributeData> GetCustomAttributesData() => Array.Empty<CustomAttributeData>();

    public override Type? GetElementType() => null;

    public override EventInfo? GetEvent(string name, BindingFlags bindingAttr) => null;

    public override EventInfo[] GetEvents(BindingFlags bindingAttr) => Array.Empty<EventInfo>();

    public override FieldInfo? GetField(string name, BindingFlags bindingAttr) => null;

    public override FieldInfo[] GetFields(BindingFlags bindingAttr) => Array.Empty<FieldInfo>();

    public override Type? GetInterface(string name, bool ignoreCase) => null;

    public override Type[] GetInterfaces() => Array.Empty<Type>();

    public override MemberInfo[] GetMembers(BindingFlags bindingAttr) => Array.Empty<MemberInfo>();

    public override MethodInfo[] GetMethods(BindingFlags bindingAttr) => Array.Empty<MethodInfo>();

    public override Type GetNestedType(string name, BindingFlags bindingAttr) => throw new NotSupportedException();

    public override Type[] GetNestedTypes(BindingFlags bindingAttr) => Array.Empty<Type>();

    public override PropertyInfo[] GetProperties(BindingFlags bindingAttr) => Array.Empty<PropertyInfo>();

    public override object? InvokeMember(string name, BindingFlags invokeAttr, Binder? binder, object? target, object?[]? args, ParameterModifier[]? modifiers, CultureInfo? culture, string[]? namedParameters)
        => throw new NotSupportedException();

    public override bool IsAssignableFrom(Type? c) => ReferenceEquals(this, c);

    public override bool IsDefined(Type attributeType, bool inherit) => false;

    protected override TypeAttributes GetAttributeFlagsImpl() => TypeAttributes.Public;

    protected override ConstructorInfo? GetConstructorImpl(BindingFlags bindingAttr, Binder? binder, CallingConventions callConvention, Type[]? types, ParameterModifier[]? modifiers) => null;

    protected override MethodInfo? GetMethodImpl(string name, BindingFlags bindingAttr, Binder? binder, CallingConventions callConvention, Type[]? types, ParameterModifier[]? modifiers) => null;

    protected override PropertyInfo? GetPropertyImpl(string name, BindingFlags bindingAttr, Binder? binder, Type? returnType, Type[]? types, ParameterModifier[]? modifiers) => null;

    protected override bool HasElementTypeImpl() => false;

    protected override bool IsArrayImpl() => false;

    protected override bool IsByRefImpl() => false;

    protected override bool IsPointerImpl() => false;

    protected override bool IsPrimitiveImpl() => false;

    protected override bool IsCOMObjectImpl() => false;

    protected override bool IsValueTypeImpl() => false;

    public override int GetArrayRank() => throw new NotSupportedException();
}
