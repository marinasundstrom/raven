namespace System.Reflection2;

using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Reflection;
using System.Reflection.Metadata;
using System.Text;

/// <summary>
/// Represents a metadata-only function pointer type.
/// </summary>
internal sealed class MetadataFunctionPointerType : TypeInfo
{
    private readonly MetadataModule _module;
    private readonly SignatureHeader _header;
    private readonly Type _returnType;
    private readonly Type[] _parameterTypes;
    private readonly int _requiredParameterCount;
    private readonly string _displayName;

    internal MetadataFunctionPointerType(MetadataModule module, MethodSignature<Type> signature)
    {
        _module = module ?? throw new ArgumentNullException(nameof(module));
        _header = signature.Header;
        _returnType = signature.ReturnType ?? throw new ArgumentNullException(nameof(signature.ReturnType));
        _parameterTypes = signature.ParameterTypes.ToArray();
        _requiredParameterCount = signature.RequiredParameterCount;
        GenericParameterCount = signature.GenericParameterCount;
        _displayName = BuildDisplayName();
    }

    internal int GenericParameterCount { get; }

    internal Type ReturnType => _returnType;

    internal IReadOnlyList<Type> ParameterTypes => _parameterTypes;

    internal int RequiredParameterCount => _requiredParameterCount;

    internal SignatureHeader Header => _header;

    public SignatureCallingConvention CallingConvention => _header.CallingConvention;

    public bool HasVarArgs => _requiredParameterCount < _parameterTypes.Length;

    public override Assembly Assembly => _module.Assembly;

    public override string? Namespace => null;

    public override string Name => _displayName;

    public override string? FullName => _displayName;

    public override string? AssemblyQualifiedName => _displayName is null ? null : $"{_displayName}, {_module.Assembly.FullName}";

    public override Module Module => _module;

    public override Type? BaseType => null;

    public override Guid GUID => Guid.Empty;

    public override Type? DeclaringType => null;

    public override int MetadataToken => 0;

    public override bool ContainsGenericParameters => _returnType.ContainsGenericParameters || _parameterTypes.Any(p => p.ContainsGenericParameters);

    public override bool IsGenericType => false;

    public override bool IsGenericTypeDefinition => false;

    public override bool IsGenericParameter => false;

    public override Type UnderlyingSystemType => this;

    public override Type[] GetGenericArguments() => Array.Empty<Type>();

    public override Type GetGenericTypeDefinition() => throw new InvalidOperationException("Function pointers are not generic.");

    public override Type MakeArrayType() => new MetadataArrayType(this, 1);

    public override Type MakeArrayType(int rank) => new MetadataArrayType(this, rank);

    public override Type MakeByRefType() => _module.GetByRefType(this);

    public override Type MakePointerType() => _module.GetPointerType(this);

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

    public override int GetArrayRank() => throw new NotSupportedException();

    protected override TypeAttributes GetAttributeFlagsImpl() => TypeAttributes.Public | TypeAttributes.Sealed;

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

    public override bool IsFunctionPointer => true;

    public override string ToString() => _displayName;

    private string BuildDisplayName()
    {
        var builder = new StringBuilder("delegate*");
        var conventionSuffix = CallingConvention switch
        {
            SignatureCallingConvention.CDecl => " cdecl",
            SignatureCallingConvention.StdCall => " stdcall",
            SignatureCallingConvention.ThisCall => " thiscall",
            SignatureCallingConvention.FastCall => " fastcall",
            SignatureCallingConvention.VarArgs => " unmanaged[VarArgs]",
            SignatureCallingConvention.Unmanaged => " unmanaged",
            _ => string.Empty,
        };

        if (!string.IsNullOrEmpty(conventionSuffix))
        {
            builder.Append(conventionSuffix);
        }

        var components = new List<string>(_parameterTypes.Length + 1);
        for (var i = 0; i < _parameterTypes.Length; i++)
        {
            if (HasVarArgs && i == _requiredParameterCount)
            {
                components.Add("...");
            }

            components.Add(FormatTypeName(_parameterTypes[i]));
        }

        components.Add(FormatTypeName(_returnType));

        if (components.Count > 0)
        {
            builder.Append('<');
            builder.Append(string.Join(", ", components));
            builder.Append('>');
        }

        return builder.ToString();
    }

    private static string FormatTypeName(Type type)
        => type.FullName ?? type.Name ?? type.ToString();
}
