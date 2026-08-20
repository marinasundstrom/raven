using System;
using System.Collections.Immutable;
using System.Linq;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Provides compiler-owned classification for macro definitions.
/// </summary>
public static class MacroFacts
{
    /// <summary>
    /// Creates the compiler-normalized descriptor consumed by registration,
    /// binding, and language services.
    /// </summary>
    public static MacroDefinitionDescriptor GetDescriptor(IMacroDefinition macro)
    {
        ArgumentNullException.ThrowIfNull(macro);

        var parameters = GetParameters(macro);
        var acceptsDeclaredArguments = parameters.Any(static parameter =>
            parameter.Role is MacroParameterRole.Value or MacroParameterRole.SyntaxInput);
        var hasMethodExpand = MethodMacroFacts.TryGetExpandMethod(macro.GetType(), out var methodExpand);
        return new MacroDefinitionDescriptor(
            macro,
            GetApplicationKind(macro),
            GetInvocationTargets(macro),
            GetTargets(macro),
            parameters,
            acceptsDeclaredArguments || macro.AcceptsArguments,
            macro is IMacroExecutor { HasTokenBody: true } ||
            hasMethodExpand && MethodMacroFacts.GetParameters(methodExpand)
                .Any(static parameter =>
                    parameter.Source == MacroParameterSource.TokenBody ||
                    typeof(TokenTreeMacroContext).IsAssignableFrom(parameter.RuntimeType)));
    }

    public static bool AcceptsArguments(IMacroDefinition macro)
        => GetDescriptor(macro).AcceptsArguments;

    /// <summary>
    /// Gets how the macro is applied to authored Raven syntax.
    /// </summary>
    public static MacroApplicationKind GetApplicationKind(IMacroDefinition macro)
    {
        ArgumentNullException.ThrowIfNull(macro);
        if (TryGetApplicationKind(macro, out var applicationKind))
            return applicationKind;

        throw new ArgumentException(
            "A macro definition must expose exactly one supported Expand contract.",
            nameof(macro));
    }

    /// <summary>
    /// Tries to get how the macro is applied to authored Raven syntax.
    /// </summary>
    public static bool TryGetApplicationKind(
        IMacroDefinition macro,
        out MacroApplicationKind applicationKind)
    {
        ArgumentNullException.ThrowIfNull(macro);

        if (macro is IMacroExecutor executor)
        {
            applicationKind = executor.ApplicationKind;
            return true;
        }

        if (MethodMacroFacts.TryGetExpandMethod(macro.GetType(), out var expandMethod))
        {
            applicationKind = MethodMacroFacts.GetApplicationKind(expandMethod);
            return true;
        }

        if (!TryGetKind(macro, out var kind))
        {
            applicationKind = default;
            return false;
        }

        applicationKind = kind == MacroKind.AttachedDeclaration
            ? MacroApplicationKind.Attached
            : MacroApplicationKind.Freestanding;
        return true;
    }

    /// <summary>
    /// Gets the grammar positions supported by an invocable macro.
    /// </summary>
    public static MacroInvocationTargets GetInvocationTargets(IMacroDefinition macro)
    {
        ArgumentNullException.ThrowIfNull(macro);

        return GetApplicationKind(macro) == MacroApplicationKind.Freestanding
            ? macro.InvocationTargets
            : MacroInvocationTargets.None;
    }

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
            "A macro definition must expose exactly one supported Expand contract.",
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

        if (macro is IMacroExecutor executor)
        {
            kind = executor.ApplicationKind == MacroApplicationKind.Attached
                ? MacroKind.AttachedDeclaration
                : MacroKind.Invocable;
            return true;
        }

        if (MethodMacroFacts.TryGetExpandMethod(macro.GetType(), out var expandMethod))
        {
            kind = MethodMacroFacts.GetApplicationKind(expandMethod) == MacroApplicationKind.Attached
                ? MacroKind.AttachedDeclaration
                : MacroKind.Invocable;
            return true;
        }

        kind = default;
        return false;
    }

    /// <summary>
    /// Gets the declaration targets supported by an attached macro, or
    /// <see cref="MacroTarget.None"/> for an invocable macro.
    /// </summary>
    public static MacroTarget GetTargets(IMacroDefinition macro)
    {
        ArgumentNullException.ThrowIfNull(macro);
        if (MethodMacroFacts.TryGetExpandMethod(macro.GetType(), out var expandMethod) &&
            MethodMacroFacts.GetApplicationKind(expandMethod) == MacroApplicationKind.Attached)
        {
            return MethodMacroFacts.GetTargets(expandMethod);
        }

        return macro switch
        {
            IMacroExecutor executor when executor.ApplicationKind == MacroApplicationKind.Attached =>
                executor.Targets,
            _ => MacroTarget.None,
        };
    }

    /// <summary>
    /// Gets the compiler-normalized positional and named parameter descriptors
    /// for a typed macro definition.
    /// </summary>
    public static ImmutableArray<MacroParameterDescriptor> GetParameters(IMacroDefinition macro)
    {
        ArgumentNullException.ThrowIfNull(macro);

        if (macro is IMacroExecutor executor)
        {
            return executor.Parameters
                .Where(static parameter => parameter.InvocationArgumentOrdinal is not null)
                .OrderBy(static parameter => parameter.InvocationArgumentOrdinal)
                .Select(static parameter => new MacroParameterDescriptor(
                    parameter.Name,
                    parameter.RuntimeType,
                    GetRole(parameter.Source),
                    parameter.InvocationArgumentOrdinal!.Value,
                    parameter.IsRequired,
                    null,
                    parameter.TypeDisplayName,
                    parameter.DefaultValueDisplay))
                .ToImmutableArray();
        }

        if (MethodMacroFacts.TryGetExpandMethod(macro.GetType(), out var expandMethod))
        {
            var reflectedParameters = expandMethod.GetParameters();
            return MethodMacroFacts.GetParameters(expandMethod)
                .Where(static parameter => parameter.InvocationArgumentOrdinal is not null)
                .Select(parameter =>
                {
                    var reflectedParameter = reflectedParameters[parameter.DeclarationOrdinal];
                    return new MacroParameterDescriptor(
                        parameter.Name,
                        parameter.RuntimeType,
                        GetRole(parameter.Source),
                        parameter.InvocationArgumentOrdinal!.Value,
                        parameter.IsRequired,
                        reflectedParameter.HasDefaultValue ? reflectedParameter.DefaultValue : null,
                        parameter.TypeDisplayName,
                        parameter.DefaultValueDisplay);
                })
                .ToImmutableArray();
        }

        return ImmutableArray<MacroParameterDescriptor>.Empty;
    }

    private static MacroParameterRole GetRole(MacroParameterSource source)
        => source switch
        {
            MacroParameterSource.SyntaxInput => MacroParameterRole.SyntaxInput,
            MacroParameterSource.Context => MacroParameterRole.Context,
            MacroParameterSource.TokenBody => MacroParameterRole.TokenBody,
            MacroParameterSource.AttachedTarget => MacroParameterRole.AttachedTarget,
            _ => MacroParameterRole.Value,
        };

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
