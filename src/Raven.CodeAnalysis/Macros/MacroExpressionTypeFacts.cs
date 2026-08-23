using System;
using System.Reflection;

namespace Raven.CodeAnalysis.Macros;

internal static class MacroExpressionTypeFacts
{
    public static bool TryGetConstraint(Type runtimeType, out Type constraint)
    {
        if (runtimeType.IsGenericType &&
            runtimeType.GetGenericTypeDefinition() == typeof(ExpressionSyntax<>))
        {
            constraint = runtimeType.GetGenericArguments()[0];
            return true;
        }

        constraint = null!;
        return false;
    }

    public static ITypeSymbol? ResolveConstraint(Compilation compilation, Type runtimeType)
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
            if (compilation.GetTypeByMetadataName(GetMetadataName(definition)) is not INamedTypeSymbol namedDefinition)
                return null;

            var arguments = runtimeType.GetGenericArguments();
            var resolved = new ITypeSymbol[arguments.Length];
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

    public static object CreateFacade(Type facadeType, MacroArgument argument)
    {
        var constructor = facadeType.GetConstructor(
            BindingFlags.Instance | BindingFlags.NonPublic,
            binder: null,
            [typeof(Syntax.ExpressionSyntax), typeof(ITypeSymbol)],
            modifiers: null) ?? throw new InvalidOperationException(
                $"Macro expression facade '{facadeType}' does not expose its compiler constructor.");
        return constructor.Invoke([argument.Expression, argument.SemanticType!]);
    }

    private static string GetMetadataName(Type type)
    {
        if (type.DeclaringType is null)
            return type.FullName ?? type.Name;

        return $"{GetMetadataName(type.DeclaringType)}+{type.Name}";
    }
}
