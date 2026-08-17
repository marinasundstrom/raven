namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Associates one authored argument with its invocation ordinal.
/// </summary>
public sealed class MacroExecutionArgument
{
    internal MacroExecutionArgument(int ordinal, MacroArgument argument)
    {
        Ordinal = ordinal;
        Argument = argument;
    }

    public int Ordinal { get; }

    public string? Name => Argument.Name;

    public MacroArgument Argument { get; }
}
