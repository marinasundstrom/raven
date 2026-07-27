using System;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Provides compiler-owned classification for macro definitions.
/// </summary>
public static class MacroFacts
{
    /// <summary>
    /// Gets the macro category implied by the definition's single
    /// category-specific interface.
    /// </summary>
    /// <exception cref="ArgumentException">
    /// <paramref name="macro"/> does not implement exactly one supported macro
    /// category interface.
    /// </exception>
    public static MacroKind GetKind(IMacroDefinition macro)
    {
        ArgumentNullException.ThrowIfNull(macro);
        if (TryGetKind(macro, out var kind))
            return kind;

        throw new ArgumentException(
            "A macro definition must implement exactly one supported macro category interface.",
            nameof(macro));
    }

    /// <summary>
    /// Tries to get the macro category implied by the definition's
    /// category-specific interface.
    /// </summary>
    /// <returns>
    /// <see langword="true"/> when the definition implements exactly one
    /// supported macro category interface; otherwise, <see langword="false"/>.
    /// </returns>
    public static bool TryGetKind(IMacroDefinition macro, out MacroKind kind)
    {
        ArgumentNullException.ThrowIfNull(macro);

        var isAttached = macro is IAttachedDeclarationMacro;
        var isFreestanding = macro is IFreestandingExpressionMacro;
        var isTokenTree = macro is ITokenTreeExpressionMacro;
        if ((isAttached ? 1 : 0) + (isFreestanding ? 1 : 0) + (isTokenTree ? 1 : 0) != 1)
        {
            kind = default;
            return false;
        }

        kind = isAttached
            ? MacroKind.AttachedDeclaration
            : MacroKind.FreestandingExpression;
        return true;
    }

    /// <summary>
    /// Gets the declaration targets supported by an attached macro, or
    /// <see cref="MacroTarget.None"/> for a freestanding macro.
    /// </summary>
    public static MacroTarget GetTargets(IMacroDefinition macro)
    {
        ArgumentNullException.ThrowIfNull(macro);
        return macro is IAttachedDeclarationMacro attached
            ? attached.Targets
            : MacroTarget.None;
    }

    /// <summary>
    /// Gets the parameter-object type declared by a typed macro definition, or
    /// <see langword="null"/> for an untyped definition.
    /// </summary>
    public static Type? GetParametersType(IMacroDefinition macro)
    {
        ArgumentNullException.ThrowIfNull(macro);

        var parameterTypes = macro.GetType()
            .GetInterfaces()
            .Where(static candidate =>
                candidate.IsGenericType &&
                candidate.GetGenericTypeDefinition() == typeof(IMacroDefinition<>))
            .Select(static candidate => candidate.GetGenericArguments()[0])
            .Distinct()
            .Take(2)
            .ToArray();

        return parameterTypes.Length == 1 ? parameterTypes[0] : null;
    }

    /// <summary>
    /// Gets the compiler-normalized positional and named parameter descriptors
    /// for a typed macro definition.
    /// </summary>
    public static ImmutableArray<MacroParameterDescriptor> GetParameters(IMacroDefinition macro)
    {
        ArgumentNullException.ThrowIfNull(macro);

        var parametersType = GetParametersType(macro);
        if (parametersType is null || !parametersType.IsClass || parametersType.IsAbstract)
            return ImmutableArray<MacroParameterDescriptor>.Empty;

        var constructors = parametersType
            .GetConstructors(BindingFlags.Public | BindingFlags.Instance)
            .OrderByDescending(static constructor => constructor.GetParameters().Length)
            .ToArray();
        if (constructors.Length != 1)
            return ImmutableArray<MacroParameterDescriptor>.Empty;

        var builder = ImmutableArray.CreateBuilder<MacroParameterDescriptor>();
        var constructorParameters = constructors[0].GetParameters();
        for (var ordinal = 0; ordinal < constructorParameters.Length; ordinal++)
        {
            var parameter = constructorParameters[ordinal];
            builder.Add(new MacroParameterDescriptor(
                parameter.Name ?? $"arg{ordinal}",
                parameter.ParameterType,
                MacroParameterKind.Positional,
                ordinal,
                isRequired: !parameter.HasDefaultValue,
                defaultValue: parameter.HasDefaultValue ? parameter.DefaultValue : null));
        }

        foreach (var property in parametersType
                     .GetProperties(BindingFlags.Public | BindingFlags.Instance)
                     .Where(static property => property.SetMethod is not null)
                     .OrderBy(static property => property.Name, StringComparer.Ordinal))
        {
            builder.Add(new MacroParameterDescriptor(
                property.Name,
                property.PropertyType,
                MacroParameterKind.Named,
                ordinal: -1,
                isRequired: false,
                defaultValue: null));
        }

        return builder.ToImmutable();
    }

    internal static string GetParameterTypeDisplay(Type type)
    {
        var nullableType = Nullable.GetUnderlyingType(type);
        if (nullableType is not null)
            return GetParameterTypeDisplay(nullableType) + "?";

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
