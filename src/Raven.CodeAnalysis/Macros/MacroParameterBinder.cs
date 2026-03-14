using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

internal static class MacroParameterBinder
{
    private static readonly DiagnosticDescriptor s_invalidParameterObjectType = DiagnosticDescriptor.Create(
        "RAVM030",
        "Invalid macro parameter object",
        "",
        "",
        "Macro '{0}' parameter type '{1}' is not supported. Use a reference type with a single public constructor and writable properties.",
        "compiler",
        DiagnosticSeverity.Error,
        true);

    private static readonly DiagnosticDescriptor s_noSuitableConstructor = DiagnosticDescriptor.Create(
        "RAVM031",
        "Invalid macro constructor shape",
        "",
        "",
        "Macro '{0}' parameter type '{1}' must declare at most one public instance constructor.",
        "compiler",
        DiagnosticSeverity.Error,
        true);

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

    public static bool TryBind(
        string macroName,
        Type parametersType,
        AttachedMacroContext context,
        DiagnosticBag diagnostics,
        out object? parameters)
        => TryBindCore(
            macroName,
            parametersType,
            context.Syntax.Name.GetLocation(),
            context.Arguments,
            diagnostics,
            out parameters);

    public static bool TryBind(
        string macroName,
        Type parametersType,
        FreestandingMacroContext context,
        DiagnosticBag diagnostics,
        out object? parameters)
        => TryBindCore(
            macroName,
            parametersType,
            context.Syntax.Name.GetLocation(),
            context.Arguments,
            diagnostics,
            out parameters);

    private static bool TryBindCore(
        string macroName,
        Type parametersType,
        Location macroNameLocation,
        IReadOnlyList<MacroArgument> arguments,
        DiagnosticBag diagnostics,
        out object? parameters)
    {
        parameters = null;

        if (!parametersType.IsClass || parametersType.IsAbstract)
        {
            diagnostics.Report(Diagnostic.Create(
                s_invalidParameterObjectType,
                macroNameLocation,
                macroName,
                parametersType.FullName ?? parametersType.Name));
            return false;
        }

        var constructors = parametersType
            .GetConstructors(BindingFlags.Public | BindingFlags.Instance)
            .OrderByDescending(static ctor => ctor.GetParameters().Length)
            .ToArray();

        if (constructors.Length > 1)
        {
            diagnostics.Report(Diagnostic.Create(
                s_noSuitableConstructor,
                macroNameLocation,
                macroName,
                parametersType.FullName ?? parametersType.Name));
            return false;
        }

        var constructor = constructors.SingleOrDefault();
        var positionalArguments = arguments.Where(static argument => !argument.IsNamed).ToArray();
        var namedArguments = arguments.Where(static argument => argument.IsNamed).ToArray();

        if (constructor is null)
        {
            if (positionalArguments.Length > 0)
            {
                diagnostics.Report(Diagnostic.Create(
                    s_tooManyPositionalArguments,
                    positionalArguments[0].Syntax.GetLocation(),
                    macroName,
                    1));
                return false;
            }

            try
            {
                parameters = Activator.CreateInstance(parametersType);
            }
            catch
            {
                diagnostics.Report(Diagnostic.Create(
                    s_invalidParameterObjectType,
                    macroNameLocation,
                    macroName,
                    parametersType.FullName ?? parametersType.Name));
                return false;
            }
        }
        else
        {
            var ctorParameters = constructor.GetParameters();
            var ctorArguments = new object?[ctorParameters.Length];

            if (positionalArguments.Length > ctorParameters.Length)
            {
                diagnostics.Report(Diagnostic.Create(
                    s_tooManyPositionalArguments,
                    positionalArguments[ctorParameters.Length].Syntax.GetLocation(),
                    macroName,
                    ctorParameters.Length + 1));
                return false;
            }

            for (var index = 0; index < ctorParameters.Length; index++)
            {
                var ctorParameter = ctorParameters[index];
                if (index < positionalArguments.Length)
                {
                    if (!TryConvertValue(positionalArguments[index], ctorParameter.ParameterType, out var converted))
                    {
                        diagnostics.Report(Diagnostic.Create(
                            s_invalidArgumentConversion,
                            positionalArguments[index].Syntax.GetLocation(),
                            macroName,
                            ctorParameter.Name ?? $"arg{index}",
                            GetTypeDisplay(ctorParameter.ParameterType)));
                        return false;
                    }

                    ctorArguments[index] = converted;
                    continue;
                }

                if (ctorParameter.HasDefaultValue)
                {
                    ctorArguments[index] = ctorParameter.DefaultValue;
                    continue;
                }

                diagnostics.Report(Diagnostic.Create(
                    s_missingRequiredArgument,
                    macroNameLocation,
                    macroName,
                    ctorParameter.Name ?? $"arg{index}"));
                return false;
            }

            parameters = constructor.Invoke(ctorArguments);
        }

        var seenNamedArguments = new HashSet<string>(StringComparer.Ordinal);
        foreach (var argument in namedArguments)
        {
            var name = argument.Name!;
            if (!seenNamedArguments.Add(name))
            {
                diagnostics.Report(Diagnostic.Create(
                    s_duplicateNamedArgument,
                    argument.Syntax.GetLocation(),
                    macroName,
                    name));
                return false;
            }

            var property = parametersType.GetProperty(name, BindingFlags.Public | BindingFlags.Instance);
            if (property is null || property.SetMethod is null)
            {
                diagnostics.Report(Diagnostic.Create(
                    s_unknownNamedArgument,
                    argument.Syntax.GetLocation(),
                    macroName,
                    name));
                return false;
            }

            if (!TryConvertValue(argument, property.PropertyType, out var converted))
            {
                diagnostics.Report(Diagnostic.Create(
                    s_invalidArgumentConversion,
                    argument.Syntax.GetLocation(),
                    macroName,
                    name,
                    GetTypeDisplay(property.PropertyType)));
                return false;
            }

            property.SetValue(parameters, converted);
        }

        return true;
    }

    private static bool TryConvertValue(MacroArgument argument, Type targetType, out object? converted)
        => TryConvertValue(argument.Constant, targetType, out converted);

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
