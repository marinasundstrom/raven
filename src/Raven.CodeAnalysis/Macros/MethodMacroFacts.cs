using System.Collections.Immutable;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

internal static class MethodMacroFacts
{
    public const MacroTarget AllTargets =
        MacroTarget.Type |
        MacroTarget.Method |
        MacroTarget.Property |
        MacroTarget.Field |
        MacroTarget.Event |
        MacroTarget.Parameter |
        MacroTarget.Accessor |
        MacroTarget.Constructor;

    public static bool TryGetExpandMethod(Type type, out MethodInfo method)
    {
        if (typeof(IMacroExecutor).IsAssignableFrom(type))
        {
            method = null!;
            return false;
        }

        var candidates = type
            .GetMethods(BindingFlags.Public | BindingFlags.Instance | BindingFlags.DeclaredOnly)
            .Where(static candidate => candidate.Name == "Expand" && !candidate.IsSpecialName)
            .ToArray();

        if (candidates.Length == 1 && !candidates[0].ContainsGenericParameters)
        {
            method = candidates[0];
            return true;
        }

        method = null!;
        return false;
    }

    public static MacroApplicationKind GetApplicationKind(MethodInfo method)
        => typeof(MacroExpansionResult).IsAssignableFrom(method.ReturnType) ||
            method.GetParameters().Any(static parameter =>
                typeof(AttachedMacroContext).IsAssignableFrom(parameter.ParameterType))
            ? MacroApplicationKind.Attached
            : MacroApplicationKind.Freestanding;

    public static MacroTarget GetTargets(MethodInfo method)
    {
        if (GetApplicationKind(method) != MacroApplicationKind.Attached)
            return MacroTarget.None;

        var targetParameter = method.GetParameters()
            .FirstOrDefault(parameter => GetSource(parameter.ParameterType, MacroApplicationKind.Attached) ==
                MacroParameterSource.AttachedTarget);
        return targetParameter is null ? AllTargets : GetTarget(targetParameter.ParameterType);
    }

    internal static MacroTarget GetTarget(Type type)
    {
        if (typeof(BaseTypeDeclarationSyntax).IsAssignableFrom(type) ||
            typeof(CaseDeclarationSyntax).IsAssignableFrom(type))
            return MacroTarget.Type;
        if (typeof(MethodDeclarationSyntax).IsAssignableFrom(type) ||
            typeof(FunctionStatementSyntax).IsAssignableFrom(type))
            return MacroTarget.Method;
        if (typeof(PropertyDeclarationSyntax).IsAssignableFrom(type) ||
            typeof(IndexerDeclarationSyntax).IsAssignableFrom(type))
            return MacroTarget.Property;
        if (typeof(FieldDeclarationSyntax).IsAssignableFrom(type) ||
            typeof(ConstDeclarationSyntax).IsAssignableFrom(type))
            return MacroTarget.Field;
        if (typeof(EventDeclarationSyntax).IsAssignableFrom(type))
            return MacroTarget.Event;
        if (typeof(ParameterSyntax).IsAssignableFrom(type))
            return MacroTarget.Parameter;
        if (typeof(AccessorDeclarationSyntax).IsAssignableFrom(type))
            return MacroTarget.Accessor;
        if (typeof(ConstructorDeclarationSyntax).IsAssignableFrom(type) ||
            typeof(ParameterlessConstructorDeclarationSyntax).IsAssignableFrom(type))
            return MacroTarget.Constructor;
        return AllTargets;
    }

    public static ImmutableArray<MacroExecutorParameter> GetParameters(MethodInfo method)
    {
        var invocationOrdinal = 0;
        var parameters = method.GetParameters();
        var applicationKind = GetApplicationKind(method);
        var builder = ImmutableArray.CreateBuilder<MacroExecutorParameter>(parameters.Length);

        for (var declarationOrdinal = 0; declarationOrdinal < parameters.Length; declarationOrdinal++)
        {
            var parameter = parameters[declarationOrdinal];
            var source = GetSource(parameter.ParameterType, applicationKind);
            var currentInvocationOrdinal = source is MacroParameterSource.Value or MacroParameterSource.SyntaxInput
                ? invocationOrdinal++
                : -1;
            builder.Add(new MacroExecutorParameter(
                parameter.Name ?? $"arg{declarationOrdinal}",
                parameter.ParameterType,
                MacroFacts.GetParameterTypeDisplay(parameter.ParameterType),
                source,
                declarationOrdinal,
                currentInvocationOrdinal,
                isRequired: currentInvocationOrdinal >= 0 && !parameter.HasDefaultValue,
                parameter.HasDefaultValue ? parameter.DefaultValue?.ToString() ?? string.Empty : string.Empty));
        }

        return builder.MoveToImmutable();
    }

    private static MacroParameterSource GetSource(
        Type parameterType,
        MacroApplicationKind applicationKind)
    {
        if (typeof(MacroContext).IsAssignableFrom(parameterType))
            return MacroParameterSource.Context;
        if (typeof(IMacroTokenStream).IsAssignableFrom(parameterType))
            return MacroParameterSource.TokenBody;
        if (typeof(FreestandingMacroDeclarationSyntax).IsAssignableFrom(parameterType))
            return MacroParameterSource.DeclarationInput;
        if (applicationKind == MacroApplicationKind.Attached &&
            typeof(SyntaxNode).IsAssignableFrom(parameterType))
        {
            return MacroParameterSource.AttachedTarget;
        }
        if (typeof(ExpressionSyntax).IsAssignableFrom(parameterType))
            return MacroParameterSource.SyntaxInput;
        return MacroParameterSource.Value;
    }
}
