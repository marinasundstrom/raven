using System.Globalization;
using System.Reflection;

namespace Raven.CodeAnalysis.CodeGen;

// Persisted Reflection.Emit does not recognize a builder parameter nested inside
// an array when recovering the definition of a projected constructor. Retain the
// definition explicitly instead of asking RuntimeType to match its emit wrapper.
// Delegate constructor parameters are always object and IntPtr, so no parameter
// substitution is required. The declaring type still encodes the actual arguments.
internal sealed class ProjectedDelegateConstructorInfo(Type declaringType, ConstructorInfo definition) : ConstructorInfo
{
    private readonly Type _declaringType = new DeclaringTypeProjection(declaringType, definition);

    public override Type DeclaringType => _declaringType;
    public override Type ReflectedType => _declaringType;
    public override string Name => definition.Name;
    public override Module Module => definition.Module;
    public override int MetadataToken => definition.MetadataToken;
    public override MethodAttributes Attributes => definition.Attributes;
    public override CallingConventions CallingConvention => definition.CallingConvention;
    public override RuntimeMethodHandle MethodHandle => definition.MethodHandle;
    public override MethodImplAttributes GetMethodImplementationFlags() => definition.GetMethodImplementationFlags();
    public override ParameterInfo[] GetParameters() => definition.GetParameters();
    public override object[] GetCustomAttributes(bool inherit) => definition.GetCustomAttributes(inherit);
    public override object[] GetCustomAttributes(Type attributeType, bool inherit) => definition.GetCustomAttributes(attributeType, inherit);
    public override bool IsDefined(Type attributeType, bool inherit) => definition.IsDefined(attributeType, inherit);
    public override object Invoke(BindingFlags invokeAttr, System.Reflection.Binder? binder, object?[]? parameters, CultureInfo? culture) => throw new NotSupportedException();
    public override object? Invoke(object? obj, BindingFlags invokeAttr, System.Reflection.Binder? binder, object?[]? parameters, CultureInfo? culture) => throw new NotSupportedException();

    internal static MethodInfo NormalizeInvoke(MethodInfo method)
    {
        if (method.Name != "Invoke" || method.DeclaringType is not { IsConstructedGenericType: true } owner ||
            owner.GetType().FullName != "System.Reflection.Emit.TypeBuilderInstantiation" ||
            !owner.GetGenericArguments().Any(type => type.HasElementType) ||
            owner.GetGenericTypeDefinition().BaseType?.FullName != "System.MulticastDelegate")
            return method;

        var definition = owner.GetGenericTypeDefinition().GetMethod("Invoke")!;
        return new ProjectedInvokeMethodInfo(owner, definition);
    }

    private sealed class ProjectedInvokeMethodInfo(Type owner, MethodInfo definition) : MethodInfo
    {
        public override Type DeclaringType { get; } = new DeclaringTypeProjection(owner, definition);
        public override Type ReflectedType => DeclaringType;
        public override string Name => definition.Name;
        public override Module Module => definition.Module;
        public override int MetadataToken => definition.MetadataToken;
        public override MethodAttributes Attributes => definition.Attributes;
        public override CallingConventions CallingConvention => definition.CallingConvention;
        public override RuntimeMethodHandle MethodHandle => definition.MethodHandle;
        public override Type ReturnType => definition.ReturnType;
        public override ParameterInfo ReturnParameter => definition.ReturnParameter;
        public override ICustomAttributeProvider ReturnTypeCustomAttributes => definition.ReturnTypeCustomAttributes;
        public override MethodInfo GetBaseDefinition() => definition.GetBaseDefinition();
        public override Type[] GetGenericArguments() => definition.GetGenericArguments();
        public override MethodImplAttributes GetMethodImplementationFlags() => definition.GetMethodImplementationFlags();
        public override ParameterInfo[] GetParameters() => definition.GetParameters();
        public override object[] GetCustomAttributes(bool inherit) => definition.GetCustomAttributes(inherit);
        public override object[] GetCustomAttributes(Type attributeType, bool inherit) => definition.GetCustomAttributes(attributeType, inherit);
        public override bool IsDefined(Type attributeType, bool inherit) => definition.IsDefined(attributeType, inherit);
        public override object? Invoke(object? obj, BindingFlags invokeAttr, System.Reflection.Binder? binder, object?[]? parameters, CultureInfo? culture) => throw new NotSupportedException();
    }

    private sealed class DeclaringTypeProjection(Type type, MemberInfo constructor) : TypeDelegator(type)
    {
        public override bool IsGenericType => true;
        public override bool IsGenericTypeDefinition => false;
        public override bool IsConstructedGenericType => true;
        public override bool ContainsGenericParameters => type.ContainsGenericParameters;
        public override Type[] GetGenericArguments() => type.GetGenericArguments();
        public override Type GetGenericTypeDefinition() => new DefinitionProjection(type.GetGenericTypeDefinition(), constructor);
    }

    private sealed class DefinitionProjection(Type type, MemberInfo constructor) : TypeDelegator(type)
    {
        public override bool IsGenericType => true;
        public override bool IsGenericTypeDefinition => true;
        public override Type[] GetGenericArguments() => type.GetGenericArguments();
        public override Type GetGenericTypeDefinition() => type;
        public override MemberInfo GetMemberWithSameMetadataDefinitionAs(MemberInfo member) =>
            member is ProjectedDelegateConstructorInfo or ProjectedInvokeMethodInfo ? constructor : type.GetMemberWithSameMetadataDefinitionAs(member);
    }
}
