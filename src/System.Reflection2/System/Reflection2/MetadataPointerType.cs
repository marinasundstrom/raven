namespace System.Reflection2;

using System;
using System.Collections.Generic;
using System.Globalization;
using System.Reflection;

/// <summary>
/// Represents a pointer type within the metadata-only reflection universe.
/// </summary>
internal sealed class MetadataPointerType : TypeInfo
{
    private readonly Type _elementType;

    public MetadataPointerType(Type elementType)
    {
        _elementType = elementType ?? throw new ArgumentNullException(nameof(elementType));
    }

    public override Assembly Assembly => _elementType.Assembly;

    public override string? Namespace => _elementType.Namespace;

    public override string Name => _elementType.Name + "*";

    public override string? FullName => _elementType.FullName is null ? null : _elementType.FullName + "*";

    public override string? AssemblyQualifiedName => FullName is null ? null : $"{FullName}, {Assembly.FullName}";

    public override Module Module => _elementType.Module;

    public override Type? BaseType => typeof(void);

    public override Guid GUID => Guid.Empty;

    public override Type? DeclaringType => null;

    public override int MetadataToken => 0;

    public override bool ContainsGenericParameters => _elementType.ContainsGenericParameters;

    public override bool IsGenericType => false;

    public override bool IsGenericTypeDefinition => false;

    public override Type UnderlyingSystemType => this;

    public override Type GetElementType() => _elementType;

    public override ConstructorInfo[] GetConstructors(BindingFlags bindingAttr) => Array.Empty<ConstructorInfo>();

    public override object[] GetCustomAttributes(bool inherit) => Array.Empty<object>();

    public override object[] GetCustomAttributes(Type attributeType, bool inherit) => Array.Empty<object>();

    public override IList<CustomAttributeData> GetCustomAttributesData() => Array.Empty<CustomAttributeData>();

    public override Type[] GetGenericArguments() => Array.Empty<Type>();

    public override Type GetGenericTypeDefinition() => throw new InvalidOperationException("Pointer types are not generic.");

    public override Type MakeArrayType() => new MetadataArrayType(this, 1);

    public override Type MakeArrayType(int rank) => new MetadataArrayType(this, rank);

    public override Type MakeByRefType() => new MetadataByRefType(this);

    public override Type MakeGenericType(params Type[] typeArguments) => throw new InvalidOperationException("Pointer types are not generic.");

    public override Type MakePointerType() => new MetadataPointerType(this);

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

    protected override TypeAttributes GetAttributeFlagsImpl() => TypeAttributes.Public | TypeAttributes.Sealed;

    protected override ConstructorInfo? GetConstructorImpl(BindingFlags bindingAttr, Binder? binder, CallingConventions callConvention, Type[]? types, ParameterModifier[]? modifiers) => null;

    protected override MethodInfo? GetMethodImpl(string name, BindingFlags bindingAttr, Binder? binder, CallingConventions callConvention, Type[]? types, ParameterModifier[]? modifiers) => null;

    protected override PropertyInfo? GetPropertyImpl(string name, BindingFlags bindingAttr, Binder? binder, Type? returnType, Type[]? types, ParameterModifier[]? modifiers) => null;

    protected override bool HasElementTypeImpl() => true;

    protected override bool IsArrayImpl() => false;

    protected override bool IsByRefImpl() => false;

    protected override bool IsPointerImpl() => true;

    protected override bool IsPrimitiveImpl() => false;

    protected override bool IsCOMObjectImpl() => false;

    protected override bool IsValueTypeImpl() => false;

    public override int GetArrayRank() => throw new NotSupportedException();
}
