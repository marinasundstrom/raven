namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Describes one invocation-facing parameter in macro signature help.
/// </summary>
public sealed class MacroSignatureParameter
{
    internal MacroSignatureParameter(
        string name,
        string typeDisplayName,
        MacroParameterRole role,
        MacroParameterSource source,
        int ordinal,
        bool isRequired,
        object? defaultValue,
        string? defaultValueDisplay = null)
    {
        Name = name;
        TypeDisplayName = typeDisplayName;
        Role = role;
        Source = source;
        Ordinal = ordinal;
        IsRequired = isRequired;
        DefaultValue = defaultValue;
        DefaultValueDisplay = defaultValueDisplay;
    }

    public string Name { get; }

    public string TypeDisplayName { get; }

    public MacroParameterRole Role { get; }

    public MacroParameterSource Source { get; }

    public int Ordinal { get; }

    public bool IsRequired { get; }

    public object? DefaultValue { get; }

    public string? DefaultValueDisplay { get; }
}
