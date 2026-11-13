using System;

namespace Raven.CodeAnalysis.Macros;

internal static partial class MacroExpressionTypeFacts
{
    public static TypeSymbol? ResolveConstraint(Compilation compilation, Type runtimeType)
    {
        if (runtimeType.IsGenericParameter)
            return null;

        if (runtimeType.IsArray)
        {
            var element = ResolveConstraint(compilation, runtimeType.GetElementType()!);
            return element is null ? null : compilation.CreateArrayTypeSymbol(element, runtimeType.GetArrayRank());
        }

        if (runtimeType.IsGenericType)
        {
            var definition = runtimeType.GetGenericTypeDefinition();
            if (compilation.GetTypeByMetadataName(GetMetadataName(definition)) is not NamedTypeSymbol namedDefinition)
                return null;

            var arguments = runtimeType.GetGenericArguments();
            var resolved = new TypeSymbol[arguments.Length];
            for (var index = 0; index < arguments.Length; index++)
            {
                if (ResolveConstraint(compilation, arguments[index]) is not { } argument)
                    return null;
                resolved[index] = argument;
            }

            return namedDefinition.Construct(resolved);
        }

        return compilation.GetTypeByMetadataName(GetMetadataName(runtimeType));
    }

    private static string GetMetadataName(Type type)
    {
        if (type.DeclaringType is null)
            return type.FullName ?? type.Name;

        return $"{GetMetadataName(type.DeclaringType)}+{type.Name}";
    }
}
