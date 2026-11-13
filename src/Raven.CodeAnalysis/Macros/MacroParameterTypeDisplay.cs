using System;

namespace Raven.CodeAnalysis.Macros;

internal static class MacroParameterTypeDisplay
{
    public static string Format(Type type)
    {
        if (MacroExpressionTypeFacts.TryGetConstraint(type, out var expressionType) && expressionType is { } constraint)
            return $"ExpressionSyntax<{Format(constraint)}>";

        var nullableType = Nullable.GetUnderlyingType(type);
        if (nullableType is not null)
            return Format(nullableType) + "?";

        return type == typeof(bool) ? "bool"
            : type == typeof(byte) ? "byte"
            : type == typeof(sbyte) ? "sbyte"
            : type == typeof(short) ? "short"
            : type == typeof(ushort) ? "ushort"
            : type == typeof(int) ? "int"
            : type == typeof(uint) ? "uint"
            : type == typeof(long) ? "long"
            : type == typeof(ulong) ? "ulong"
            : type == typeof(float) ? "float"
            : type == typeof(double) ? "double"
            : type == typeof(decimal) ? "decimal"
            : type == typeof(char) ? "char"
            : type == typeof(string) ? "string"
            : type == typeof(object) ? "object"
            : type.Name;
    }
}
