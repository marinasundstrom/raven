using System.Collections.Immutable;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

internal sealed class MethodMacroExecutorAdapter : IMacroExecutor
{
    private readonly IMacroDefinition _definition;
    private readonly MethodInfo _expandMethod;

    public MethodMacroExecutorAdapter(IMacroDefinition definition, MethodInfo expandMethod)
    {
        _definition = definition;
        _expandMethod = expandMethod;
        Parameters = MethodMacroFacts.GetParameters(expandMethod);
        ApplicationKind = MethodMacroFacts.GetApplicationKind(expandMethod);
    }

    public string Namespace => _definition.Namespace;
    public string Name => _definition.Name;
    public string? Alias => _definition.Alias;
    public string? Documentation => _definition.Documentation;
    public DocumentationFormat DocumentationFormat => _definition.DocumentationFormat;
    public MacroInvocationTargets InvocationTargets => _definition.InvocationTargets;
    public bool AcceptsArguments => Parameters.Any(static parameter => parameter.InvocationArgumentOrdinal is not null);
    public ImmutableArray<string> TypeParameters => [];
    public ImmutableArray<MacroExecutorParameter> Parameters { get; }
    public MacroApplicationKind ApplicationKind { get; }
    public bool HasTokenBody => Parameters.Any(static parameter => parameter.Source == MacroParameterSource.TokenBody);
    public MacroTarget Targets => ApplicationKind == MacroApplicationKind.Attached
        ? MethodMacroFacts.AllTargets
        : MacroTarget.None;

    public MacroExecutionResult Expand(MacroExecutionContext context)
    {
        var reflectedParameters = _expandMethod.GetParameters();
        var arguments = new object?[reflectedParameters.Length];
        foreach (var parameter in Parameters)
        {
            arguments[parameter.DeclarationOrdinal] = parameter.Source switch
            {
                MacroParameterSource.Context => GetContext(context.Context, parameter.RuntimeType),
                MacroParameterSource.TokenBody => GetTokenBody(context.Context),
                MacroParameterSource.AttachedTarget => GetAttachedTarget(context.Context, parameter.RuntimeType),
                _ => GetArgument(context, parameter),
            };
        }

        object? result;
        try
        {
            result = _expandMethod.Invoke(_definition, arguments);
        }
        catch (TargetInvocationException exception) when (exception.InnerException is not null)
        {
            throw exception.InnerException;
        }

        return NormalizeResult(result);
    }

    private object? GetArgument(MacroExecutionContext context, MacroExecutorParameter parameter)
    {
        var ordinal = parameter.InvocationArgumentOrdinal!.Value;
        var argument = context.Arguments.FirstOrDefault(candidate =>
            string.Equals(candidate.Name, parameter.Name, StringComparison.Ordinal)) ??
            context.Arguments.Where(static candidate => candidate.Name is null).ElementAtOrDefault(ordinal);
        if (argument is null)
        {
            var reflectedParameter = _expandMethod.GetParameters()[parameter.DeclarationOrdinal];
            if (reflectedParameter.HasDefaultValue)
                return reflectedParameter.DefaultValue;
            throw new InvalidOperationException($"Macro '{Name}' requires argument '{parameter.Name}'.");
        }

        if (MacroParameterBinder.TryConvertValue(argument.Argument, parameter.RuntimeType, out var converted))
            return converted;
        throw new InvalidOperationException(
            $"Macro '{Name}' argument '{parameter.Name}' cannot be converted to '{parameter.RuntimeType.Name}'.");
    }

    private static object GetContext(MacroContext context, Type parameterType)
        => parameterType.IsInstanceOfType(context)
            ? context
            : throw new InvalidOperationException(
                $"A {context.GetType().Name} cannot supply the requested {parameterType.Name} context.");

    private static object GetTokenBody(MacroContext context)
        => context is TokenTreeMacroContext tokenTreeContext
            ? tokenTreeContext.CreateTokenStream()
            : throw new InvalidOperationException("A token-body parameter requires a token-tree macro invocation.");

    private static object GetAttachedTarget(MacroContext context, Type parameterType)
        => context is AttachedMacroContext attachedContext &&
            parameterType.IsInstanceOfType(attachedContext.TargetDeclaration)
            ? attachedContext.TargetDeclaration
            : throw new InvalidOperationException(
                $"The attached declaration cannot be supplied as {parameterType.Name}.");

    private MacroExecutionResult NormalizeResult(object? result)
    {
        if (result is MacroExecutionResult executionResult)
            return executionResult;

        if (ApplicationKind == MacroApplicationKind.Attached)
        {
            return result switch
            {
                null => MacroExecutionResult.Attached(MacroExpansionResult.Empty),
                MacroExpansionResult expansion => MacroExecutionResult.Attached(expansion),
                SyntaxNode replacement => MacroExecutionResult.Attached(MacroExpansionResult.FromReplacement(replacement)),
                _ => throw UnsupportedResult(result),
            };
        }

        return result switch
        {
            null => MacroExecutionResult.Invocable(InvocableMacroExpansionResult.Empty),
            InvocableMacroExpansionResult expansion => MacroExecutionResult.Invocable(expansion),
            SyntaxNode syntax => MacroExecutionResult.Invocable(InvocableMacroExpansionResult.FromNode(syntax)),
            _ => throw UnsupportedResult(result),
        };
    }

    private InvalidOperationException UnsupportedResult(object result)
        => new($"Macro '{Name}' returned unsupported result type '{result.GetType().FullName}'.");
}
