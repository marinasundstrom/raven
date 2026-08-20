namespace Raven.CodeAnalysis.Symbols;

/// <summary>
/// Raven metadata representation for an <c>Option&lt;T&gt;</c> parameter whose
/// declaration-time default is <c>.None</c>.
/// </summary>
internal sealed class OptionNoneParameterDefaultValue
{
    public static OptionNoneParameterDefaultValue Instance { get; } = new();

    private OptionNoneParameterDefaultValue()
    {
    }

    public override string ToString() => ".None";
}
