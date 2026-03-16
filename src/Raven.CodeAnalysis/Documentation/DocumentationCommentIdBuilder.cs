using System.Collections.Immutable;
using System.Reflection;
using System.Text;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Documentation;

internal static class DocumentationCommentIdBuilder
{
    public static bool TryGetMemberId(ISymbol symbol, out string memberId)
    {
        memberId = symbol switch
        {
            INamedTypeSymbol type => GetTypeMemberId(type),
            IMethodSymbol method => GetMethodMemberId(method),
            IPropertySymbol property => GetPropertyMemberId(property),
            IFieldSymbol field => GetFieldMemberId(field),
            IEventSymbol @event => GetEventMemberId(@event),
            _ => string.Empty
        };

        return memberId.Length > 0;
    }

    public static string GetTypeMemberId(INamedTypeSymbol type)
        => "T:" + GetTypeName(type);

    public static string GetMethodMemberId(IMethodSymbol method)
    {
        var declaringType = method.ContainingType;
        if (declaringType is null)
            return string.Empty;

        var builder = new StringBuilder();
        builder.Append("M:");
        builder.Append(GetTypeName(declaringType));
        builder.Append('.');
        builder.Append(GetMethodName(method));
        AppendParameterList(builder, method.Parameters);

        if (method.Name is "op_Implicit" or "op_Explicit")
        {
            builder.Append('~');
            builder.Append(GetParameterTypeName(method.ReturnType));
        }

        return builder.ToString();
    }

    public static string GetPropertyMemberId(IPropertySymbol property)
    {
        var declaringType = property.ContainingType;
        if (declaringType is null)
            return string.Empty;

        var builder = new StringBuilder();
        builder.Append("P:");
        builder.Append(GetTypeName(declaringType));
        builder.Append('.');
        builder.Append(property.MetadataName.Replace('.', '#'));
        AppendParameterList(builder, property.Parameters);
        return builder.ToString();
    }

    public static string GetFieldMemberId(IFieldSymbol field)
    {
        var declaringType = field.ContainingType;
        if (declaringType is null)
            return string.Empty;

        return $"F:{GetTypeName(declaringType)}.{field.MetadataName.Replace('.', '#')}";
    }

    public static string GetEventMemberId(IEventSymbol @event)
    {
        var declaringType = @event.ContainingType;
        if (declaringType is null)
            return string.Empty;

        return $"E:{GetTypeName(declaringType)}.{@event.MetadataName.Replace('.', '#')}";
    }

    public static string GetTypeMemberId(Type type)
        => "T:" + GetTypeName(type);

    public static string GetMethodMemberId(MethodBase method)
    {
        var declaringType = method.DeclaringType;
        if (declaringType is null)
            return string.Empty;

        var builder = new StringBuilder();
        builder.Append("M:");
        builder.Append(GetTypeName(declaringType));
        builder.Append('.');
        builder.Append(GetMethodName(method));
        AppendParameterList(builder, method.GetParameters().Select(static p => (Type: p.ParameterType, IsByRef: p.ParameterType.IsByRef)));

        if (method.Name is "op_Implicit" or "op_Explicit" && method is MethodInfo methodInfo)
        {
            builder.Append('~');
            builder.Append(GetParameterTypeName(methodInfo.ReturnType));
        }

        return builder.ToString();
    }

    public static string GetPropertyMemberId(PropertyInfo property)
    {
        var declaringType = property.DeclaringType;
        if (declaringType is null)
            return string.Empty;

        var builder = new StringBuilder();
        builder.Append("P:");
        builder.Append(GetTypeName(declaringType));
        builder.Append('.');
        builder.Append(property.Name.Replace('.', '#'));
        AppendParameterList(builder, property.GetIndexParameters().Select(static p => (Type: p.ParameterType, IsByRef: p.ParameterType.IsByRef)));
        return builder.ToString();
    }

    public static string GetFieldMemberId(FieldInfo field)
    {
        var declaringType = field.DeclaringType;
        if (declaringType is null)
            return string.Empty;

        return $"F:{GetTypeName(declaringType)}.{field.Name.Replace('.', '#')}";
    }

    public static string GetEventMemberId(EventInfo @event)
    {
        var declaringType = @event.DeclaringType;
        if (declaringType is null)
            return string.Empty;

        return $"E:{GetTypeName(declaringType)}.{@event.Name.Replace('.', '#')}";
    }

    internal static string GetMarkdownPathHash(string memberId)
        => Convert.ToHexString(System.Security.Cryptography.SHA256.HashData(Encoding.UTF8.GetBytes(memberId[2..]))).ToLowerInvariant();

    private static string GetMethodName(IMethodSymbol method)
    {
        if (method.IsConstructor)
            return method.IsStatic ? "#cctor" : "#ctor";

        return method.MetadataName.Replace('.', '#');
    }

    private static string GetMethodName(MethodBase method)
    {
        if (method.IsConstructor)
            return method.IsStatic ? "#cctor" : "#ctor";

        var name = method.Name.Replace('.', '#');
        if (method.IsGenericMethod)
            name += "``" + method.GetGenericArguments().Length;

        return name;
    }

    private static void AppendParameterList(StringBuilder builder, ImmutableArray<IParameterSymbol> parameters)
    {
        if (parameters.IsDefaultOrEmpty)
            return;

        builder.Append('(');
        for (var i = 0; i < parameters.Length; i++)
        {
            if (i > 0)
                builder.Append(',');

            builder.Append(GetParameterTypeName(parameters[i].Type));
            if (parameters[i].RefKind is RefKind.Ref or RefKind.Out or RefKind.In or RefKind.RefReadOnly or RefKind.RefReadOnlyParameter)
                builder.Append('@');
        }

        builder.Append(')');
    }

    private static void AppendParameterList(StringBuilder builder, IEnumerable<(Type Type, bool IsByRef)> parameters)
    {
        var parameterArray = parameters.ToArray();
        if (parameterArray.Length == 0)
            return;

        builder.Append('(');
        for (var i = 0; i < parameterArray.Length; i++)
        {
            if (i > 0)
                builder.Append(',');

            var parameterType = parameterArray[i].Type;
            if (parameterArray[i].IsByRef)
            {
                builder.Append(GetParameterTypeName(parameterType.GetElementType()!));
                builder.Append('@');
            }
            else
            {
                builder.Append(GetParameterTypeName(parameterType));
            }
        }

        builder.Append(')');
    }

    private static string GetTypeName(INamedTypeSymbol type)
    {
        if (type.ContainingType is INamedTypeSymbol containingType)
            return GetTypeName(containingType) + "." + GetTypeNameSegment(type, includeGenericArity: true);

        if (type.ContainingNamespace is { IsGlobalNamespace: false } containingNamespace)
            return containingNamespace.ToMetadataName() + "." + GetTypeNameSegment(type, includeGenericArity: true);

        return GetTypeNameSegment(type, includeGenericArity: true);
    }

    private static string GetParameterTypeName(ITypeSymbol type)
    {
        if (type is IArrayTypeSymbol arrayType)
        {
            var elementType = GetParameterTypeName(arrayType.ElementType);
            if (arrayType.Rank == 1)
                return arrayType.FixedSize is int fixedSize
                    ? elementType + $"[{fixedSize}]"
                    : elementType + "[]";

            return elementType + "[" + string.Join(",", Enumerable.Repeat("0:", arrayType.Rank)) + "]";
        }

        if (type is IPointerTypeSymbol pointerType)
            return GetParameterTypeName(pointerType.PointedAtType) + "*";

        if (type is ITypeParameterSymbol typeParameter)
            return typeParameter.OwnerKind == TypeParameterOwnerKind.Type
                ? "`" + typeParameter.Ordinal
                : "``" + typeParameter.Ordinal;

        if (type is not INamedTypeSymbol namedType)
            return type.MetadataName;

        if (!namedType.IsGenericType)
            return GetTypeName(namedType);

        var definitionName = GetParameterTypeDefinitionName(namedType.ConstructedFrom as INamedTypeSymbol ?? namedType);
        var formattedArguments = string.Join(",", namedType.TypeArguments.Select(GetParameterTypeName));
        return $"{definitionName}{{{formattedArguments}}}";
    }

    private static string GetTypeName(Type type)
    {
        if (type.IsGenericParameter)
            return type.DeclaringMethod is null
                ? "`" + type.GenericParameterPosition
                : "``" + type.GenericParameterPosition;

        if (type.IsNested && type.DeclaringType is not null)
            return GetTypeName(type.DeclaringType) + "." + GetTypeNameSegment(type, includeGenericArity: true);

        return string.IsNullOrEmpty(type.Namespace)
            ? GetTypeNameSegment(type, includeGenericArity: true)
            : type.Namespace + "." + GetTypeNameSegment(type, includeGenericArity: true);
    }

    private static string GetParameterTypeName(Type type)
    {
        if (type.IsByRef)
            return GetParameterTypeName(type.GetElementType()!) + "@";

        if (type.IsPointer)
            return GetParameterTypeName(type.GetElementType()!) + "*";

        if (type.IsArray)
        {
            var elementType = GetParameterTypeName(type.GetElementType()!);
            if (type.GetArrayRank() == 1)
                return elementType + "[]";

            return elementType + "[" + string.Join(",", Enumerable.Repeat("0:", type.GetArrayRank())) + "]";
        }

        if (type.IsGenericParameter)
            return type.DeclaringMethod is null
                ? "`" + type.GenericParameterPosition
                : "``" + type.GenericParameterPosition;

        if (!type.IsGenericType)
            return GetTypeName(type);

        var genericTypeDefinition = type.GetGenericTypeDefinition();
        var typeArguments = type.GetGenericArguments();
        var formattedArguments = string.Join(",", typeArguments.Select(GetParameterTypeName));
        return $"{GetParameterTypeDefinitionName(genericTypeDefinition)}{{{formattedArguments}}}";
    }

    private static string GetParameterTypeDefinitionName(INamedTypeSymbol type)
    {
        if (type.ContainingType is INamedTypeSymbol containingType)
            return GetParameterTypeDefinitionName(containingType) + "." + GetTypeNameSegment(type, includeGenericArity: false);

        if (type.ContainingNamespace is { IsGlobalNamespace: false } containingNamespace)
            return containingNamespace.ToMetadataName() + "." + GetTypeNameSegment(type, includeGenericArity: false);

        return GetTypeNameSegment(type, includeGenericArity: false);
    }

    private static string GetParameterTypeDefinitionName(Type type)
    {
        if (type.IsNested && type.DeclaringType is not null)
            return GetParameterTypeDefinitionName(type.DeclaringType) + "." + GetTypeNameSegment(type, includeGenericArity: false);

        return string.IsNullOrEmpty(type.Namespace)
            ? GetTypeNameSegment(type, includeGenericArity: false)
            : type.Namespace + "." + GetTypeNameSegment(type, includeGenericArity: false);
    }

    private static string GetTypeNameSegment(INamedTypeSymbol type, bool includeGenericArity)
    {
        if (!includeGenericArity || !type.IsGenericType)
            return type.Name;

        return $"{type.Name}`{type.Arity}";
    }

    private static string GetTypeNameSegment(Type type, bool includeGenericArity)
    {
        var name = type.Name;
        var tickIndex = name.IndexOf('`');
        if (tickIndex >= 0 && !includeGenericArity)
            return name[..tickIndex];

        return name;
    }
}
