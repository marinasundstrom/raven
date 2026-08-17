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
}
