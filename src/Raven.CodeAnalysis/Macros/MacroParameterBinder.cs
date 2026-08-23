using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Globalization;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

internal static class MacroParameterBinder
{
    private static readonly DiagnosticDescriptor s_unknownNamedArgument = DiagnosticDescriptor.Create(
        "RAVM032",
        "Unknown macro argument",
        "",
        "",
        "Macro '{0}' does not define a named argument '{1}'.",
        "compiler",
        DiagnosticSeverity.Error,
        true);

    private static readonly DiagnosticDescriptor s_duplicateNamedArgument = DiagnosticDescriptor.Create(
        "RAVM033",
        "Duplicate macro argument",
        "",
        "",
        "Macro '{0}' argument '{1}' is specified more than once.",
        "compiler",
        DiagnosticSeverity.Error,
        true);

    private static readonly DiagnosticDescriptor s_missingRequiredArgument = DiagnosticDescriptor.Create(
        "RAVM034",
        "Missing macro argument",
        "",
        "",
        "Macro '{0}' requires argument '{1}'.",
        "compiler",
        DiagnosticSeverity.Error,
        true);

    private static readonly DiagnosticDescriptor s_invalidArgumentConversion = DiagnosticDescriptor.Create(
        "RAVM035",
        "Invalid macro argument value",
        "",
        "",
        "Macro '{0}' argument '{1}' cannot be converted to '{2}'.",
        "compiler",
        DiagnosticSeverity.Error,
        true);

    private static readonly DiagnosticDescriptor s_tooManyPositionalArguments = DiagnosticDescriptor.Create(
        "RAVM036",
        "Too many macro arguments",
        "",
        "",
        "Macro '{0}' does not accept positional argument #{1}.",
        "compiler",
        DiagnosticSeverity.Error,
        true);

    private static readonly DiagnosticDescriptor s_expressionTypeMismatch = DiagnosticDescriptor.Create(
        "RAVM037",
        "Macro expression argument type mismatch",
        "",
        "",
        "Macro '{0}' argument '{1}' requires an expression compatible with '{2}', but this expression has type '{3}'.",
        "compiler",
        DiagnosticSeverity.Error,
        true);

    public static bool ValidateArguments(
        string macroName,
        Location macroNameLocation,
        ImmutableArray<MacroParameterDescriptor> parameters,
        IReadOnlyList<MacroArgument> arguments,
        DiagnosticBag diagnostics)
    {
        var positionalArguments = arguments.Where(static argument => !argument.IsNamed).ToArray();
        if (positionalArguments.Length > parameters.Length)
        {
            diagnostics.Report(Diagnostic.Create(
                s_tooManyPositionalArguments,
                positionalArguments[parameters.Length].Syntax.GetLocation(),
                macroName,
                parameters.Length + 1));
            return false;
        }

        var assigned = new HashSet<int>();
        for (var index = 0; index < positionalArguments.Length; index++)
        {
            var parameter = parameters[index];
            if (!ValidateConversion(macroName, parameter, positionalArguments[index], diagnostics))
                return false;
            assigned.Add(index);
        }

        foreach (var argument in arguments.Where(static argument => argument.IsNamed))
        {
            var name = argument.Name!;
            var index = FindParameterIndex(parameters, name);
            if (index < 0)
            {
                diagnostics.Report(Diagnostic.Create(
                    s_unknownNamedArgument,
                    argument.Syntax.GetLocation(),
                    macroName,
                    name));
                return false;
            }

            if (!assigned.Add(index))
            {
                diagnostics.Report(Diagnostic.Create(
                    s_duplicateNamedArgument,
                    argument.Syntax.GetLocation(),
                    macroName,
                    name));
                return false;
            }

            if (!ValidateConversion(macroName, parameters[index], argument, diagnostics))
                return false;
        }

        for (var index = 0; index < parameters.Length; index++)
        {
            if (!assigned.Contains(index) && parameters[index].IsRequired)
            {
                diagnostics.Report(Diagnostic.Create(
                    s_missingRequiredArgument,
                    macroNameLocation,
                    macroName,
                    parameters[index].Name));
                return false;
            }
        }

        return true;
    }

    private static int FindParameterIndex(
        ImmutableArray<MacroParameterDescriptor> parameters,
        string name)
    {
        for (var index = 0; index < parameters.Length; index++)
        {
            if (string.Equals(parameters[index].Name, name, StringComparison.Ordinal))
                return index;
        }

        return -1;
    }

    private static bool ValidateConversion(
        string macroName,
        MacroParameterDescriptor parameter,
        MacroArgument argument,
        DiagnosticBag diagnostics)
    {
        if (MacroExpressionTypeFacts.TryGetConstraint(parameter.ParameterType, out var runtimeConstraint))
        {
            var actualType = argument.SemanticType;
            var expectedType = MacroExpressionTypeFacts.ResolveConstraint(
                argument.SemanticModel.Compilation,
                runtimeConstraint);
            var conversion = actualType is null || expectedType is null
                ? default
                : argument.SemanticModel.Compilation.ClassifyConversion(actualType, expectedType);
            if (actualType is null || expectedType is null || !conversion.Exists || !conversion.IsImplicit)
            {
                diagnostics.Report(Diagnostic.Create(
                    s_expressionTypeMismatch,
                    argument.Expression.GetLocation(),
                    macroName,
                    parameter.Name,
                    expectedType?.ToDisplayStringForDiagnostics(SymbolDisplayFormat.MinimallyQualifiedFormat) ?? runtimeConstraint.Name,
                    actualType?.ToDisplayStringForDiagnostics(SymbolDisplayFormat.MinimallyQualifiedFormat) ?? "<unknown>"));
                return false;
            }

            return true;
        }

        if (TryConvertValue(argument, parameter.ParameterType, out _))
            return true;

        diagnostics.Report(Diagnostic.Create(
            s_invalidArgumentConversion,
            argument.Syntax.GetLocation(),
            macroName,
            parameter.Name,
            GetTypeDisplay(parameter.ParameterType)));
        return false;
    }

    internal static bool TryConvertValue(MacroArgument argument, Type targetType, out object? converted)
    {
        if (MacroExpressionTypeFacts.TryGetConstraint(targetType, out _))
        {
            if (argument.SemanticType is null)
            {
                converted = null;
                return false;
            }

            converted = MacroExpressionTypeFacts.CreateFacade(targetType, argument);
            return true;
        }

        if (targetType.IsInstanceOfType(argument.Expression))
        {
            converted = argument.Expression;
            return true;
        }

        return TryConvertValue(argument.Constant, targetType, out converted);
    }

    private static bool TryConvertValue(TypedConstant constant, Type targetType, out object? converted)
    {
        if (constant.Kind == TypedConstantKind.Error)
        {
            converted = null;
            return false;
        }

        return TryConvertValue(constant.Value, targetType, out converted);
    }

    private static bool TryConvertValue(object? value, Type targetType, out object? converted)
    {
        var underlyingType = Nullable.GetUnderlyingType(targetType);
        if (underlyingType is not null)
        {
            if (value is null)
            {
                converted = null;
                return true;
            }

            targetType = underlyingType;
        }

        if (value is null)
        {
            converted = null;
            return !targetType.IsValueType;
        }

        if (targetType.IsInstanceOfType(value))
        {
            converted = value;
            return true;
        }

        if (targetType.IsEnum)
        {
            try
            {
                var enumUnderlyingType = Enum.GetUnderlyingType(targetType);
                if (!TryConvertValue(value, enumUnderlyingType, out var enumValue) || enumValue is null)
                {
                    converted = null;
                    return false;
                }

                converted = Enum.ToObject(targetType, enumValue);
                return true;
            }
            catch
            {
                converted = null;
                return false;
            }
        }

        try
        {
            converted = targetType switch
            {
                _ when targetType == typeof(string) => value.ToString(),
                _ when targetType == typeof(bool) => Convert.ToBoolean(value, CultureInfo.InvariantCulture),
                _ when targetType == typeof(char) => Convert.ToChar(value, CultureInfo.InvariantCulture),
                _ when targetType == typeof(sbyte) => Convert.ToSByte(value, CultureInfo.InvariantCulture),
                _ when targetType == typeof(byte) => Convert.ToByte(value, CultureInfo.InvariantCulture),
                _ when targetType == typeof(short) => Convert.ToInt16(value, CultureInfo.InvariantCulture),
                _ when targetType == typeof(ushort) => Convert.ToUInt16(value, CultureInfo.InvariantCulture),
                _ when targetType == typeof(int) => Convert.ToInt32(value, CultureInfo.InvariantCulture),
                _ when targetType == typeof(uint) => Convert.ToUInt32(value, CultureInfo.InvariantCulture),
                _ when targetType == typeof(long) => Convert.ToInt64(value, CultureInfo.InvariantCulture),
                _ when targetType == typeof(ulong) => Convert.ToUInt64(value, CultureInfo.InvariantCulture),
                _ when targetType == typeof(float) => Convert.ToSingle(value, CultureInfo.InvariantCulture),
                _ when targetType == typeof(double) => Convert.ToDouble(value, CultureInfo.InvariantCulture),
                _ when targetType == typeof(decimal) => Convert.ToDecimal(value, CultureInfo.InvariantCulture),
                _ when targetType == typeof(DateTime) => Convert.ToDateTime(value, CultureInfo.InvariantCulture),
                _ when targetType == typeof(object) => value,
                _ => null
            };

            return converted is not null || targetType == typeof(object);
        }
        catch
        {
            converted = null;
            return false;
        }
    }

    private static string GetTypeDisplay(Type type)
    {
        if (Nullable.GetUnderlyingType(type) is { } nullableUnderlying)
            return $"{GetTypeDisplay(nullableUnderlying)}?";

        return type switch
        {
            _ when type == typeof(bool) => "bool",
            _ when type == typeof(char) => "char",
            _ when type == typeof(sbyte) => "sbyte",
            _ when type == typeof(byte) => "byte",
            _ when type == typeof(short) => "short",
            _ when type == typeof(ushort) => "ushort",
            _ when type == typeof(int) => "int",
            _ when type == typeof(uint) => "uint",
            _ when type == typeof(long) => "long",
            _ when type == typeof(ulong) => "ulong",
            _ when type == typeof(float) => "float",
            _ when type == typeof(double) => "double",
            _ when type == typeof(decimal) => "decimal",
            _ when type == typeof(string) => "string",
            _ when type == typeof(object) => "object",
            _ => type.Name
        };
    }
}
