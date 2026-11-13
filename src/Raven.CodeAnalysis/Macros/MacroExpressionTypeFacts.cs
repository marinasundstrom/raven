using System;
using System.Reflection;
using System.Diagnostics.CodeAnalysis;

namespace Raven.CodeAnalysis.Macros;

internal static partial class MacroExpressionTypeFacts
{
    public static bool TryGetConstraint(Type runtimeType, [NotNullWhen(true)] out Type? constraint)
    {
        if (runtimeType.IsGenericType &&
            runtimeType.GetGenericTypeDefinition() == typeof(ExpressionSyntax<>))
        {
            constraint = runtimeType.GetGenericArguments()[0];
            return true;
        }

        constraint = null;
        return false;
    }

    public static object CreateFacade(Type facadeType, MacroArgument argument)
    {
        var constructor = facadeType.GetConstructor(
            BindingFlags.Instance | BindingFlags.NonPublic,
            binder: null,
            [typeof(Syntax.ExpressionSyntax), typeof(TypeSymbol)],
            modifiers: null) ?? throw new InvalidOperationException(
                $"Macro expression facade '{facadeType}' does not expose its compiler constructor.");
        return constructor.Invoke([argument.Expression, argument.SemanticType!]);
    }

}
