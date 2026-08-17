namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Describes one invocation-facing parameter in macro signature help.
/// </summary>
public sealed class MacroSignatureParameter
{
    internal MacroSignatureParameter(
        string name,
        string typeDisplayName,
        MacroParameterKind kind,
        MacroParameterRole role,
        MacroParameterSource source,
        int ordinal,
        bool isRequired,
        object? defaultValue)
    {
        Name = name;
        TypeDisplayName = typeDisplayName;
        Kind = kind;
        Role = role;
        Source = source;
        Ordinal = ordinal;
        IsRequired = isRequired;
        DefaultValue = defaultValue;
    }

    public string Name { get; }

    public string TypeDisplayName { get; }

    public MacroParameterKind Kind { get; }

    public MacroParameterRole Role { get; }

    public MacroParameterSource Source { get; }

    public int Ordinal { get; }

    public bool IsRequired { get; }

    public object? DefaultValue { get; }
}
