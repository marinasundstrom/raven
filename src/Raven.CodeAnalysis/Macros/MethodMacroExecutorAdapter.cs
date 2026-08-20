using System.Collections.Immutable;
using System.Linq.Expressions;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

internal sealed class MethodMacroExecutorAdapter : IMacroExecutor
{
    private readonly IMacroDefinition _definition;
    private readonly MethodInfo _expandMethod;
    private readonly Func<object?[], object?> _invoke;

    public MethodMacroExecutorAdapter(IMacroDefinition definition, MethodInfo expandMethod)
    {
        _definition = definition;
        _expandMethod = expandMethod;
        _invoke = CreateInvoker(definition, expandMethod);
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

        return NormalizeResult(_invoke(arguments));
    }

    private static Func<object?[], object?> CreateInvoker(
        IMacroDefinition definition,
        MethodInfo expandMethod)
    {
        var arguments = Expression.Parameter(typeof(object[]), "arguments");
        var parameters = expandMethod.GetParameters();
        var callArguments = parameters
            .Select(parameter => Expression.Convert(
                Expression.ArrayIndex(arguments, Expression.Constant(parameter.Position)),
                parameter.ParameterType))
            .ToArray();
        var instance = expandMethod.IsStatic
            ? null
            : Expression.Convert(Expression.Constant(definition), expandMethod.DeclaringType!);
        var call = Expression.Call(instance, expandMethod, callArguments);
        Expression body = expandMethod.ReturnType == typeof(void)
            ? Expression.Block(call, Expression.Constant(null, typeof(object)))
            : Expression.Convert(call, typeof(object));
        return Expression.Lambda<Func<object?[], object?>>(body, arguments).Compile();
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
            null => MacroExecutionResult.Invocable(FreestandingMacroExpansionResult.Empty),
            FreestandingMacroExpansionResult expansion => MacroExecutionResult.Invocable(expansion),
            SyntaxNode syntax => MacroExecutionResult.Invocable(FreestandingMacroExpansionResult.FromNode(syntax)),
            _ => throw UnsupportedResult(result),
        };
    }

    private InvalidOperationException UnsupportedResult(object result)
        => new($"Macro '{Name}' returned unsupported result type '{result.GetType().FullName}'.");
}
