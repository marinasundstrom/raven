using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Immutable snapshot supplied to an erased macro executor.
/// </summary>
public sealed class MacroExecutionContext
{
    internal MacroExecutionContext(
        IMacroExecutor executor,
        MacroContext context,
        ImmutableArray<ITypeSymbol> typeArguments,
        ImmutableArray<MacroArgument> arguments)
    {
        Executor = executor;
        Context = context;
        TypeArguments = typeArguments.IsDefault ? [] : typeArguments;
        Arguments = arguments
            .Select(static (argument, ordinal) => new MacroExecutionArgument(ordinal, argument))
            .ToImmutableArray();
    }

    public IMacroExecutor Executor { get; }

    public MacroContext Context { get; }

    public ImmutableArray<ITypeSymbol> TypeArguments { get; }

    public ImmutableArray<MacroExecutionArgument> Arguments { get; }

    public TContext GetContext<TContext>() where TContext : MacroContext
        => Context as TContext ?? throw new InvalidOperationException(
            $"Macro '{Executor.Name}' does not have a {typeof(TContext).Name} context.");

    public T GetArgument<T>(int ordinal, string name)
    {
        var argument = FindArgument(ordinal, name) ?? throw new InvalidOperationException(
            $"Macro '{Executor.Name}' requires argument '{name}'.");
        return ConvertArgument<T>(argument, name);
    }

    public T GetArgumentOrDefault<T>(int ordinal, string name, T defaultValue)
    {
        var argument = FindArgument(ordinal, name);
        return argument is null ? defaultValue : ConvertArgument<T>(argument, name);
    }

    private MacroArgument? FindArgument(int ordinal, string name)
    {
        var named = Arguments.FirstOrDefault(argument =>
            string.Equals(argument.Name, name, StringComparison.Ordinal));
        if (named is not null)
            return named.Argument;

        return Arguments
            .Where(static argument => argument.Name is null)
            .ElementAtOrDefault(ordinal)
            ?.Argument;
    }

    private T ConvertArgument<T>(MacroArgument argument, string name)
    {
        if (MacroParameterBinder.TryConvertValue(argument, typeof(T), out var converted))
            return (T)converted!;

        throw new InvalidOperationException(
            $"Macro '{Executor.Name}' argument '{name}' cannot be converted to '{typeof(T).Name}'.");
    }
}
