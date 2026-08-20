using System;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Describes one compiler-normalized invocation-facing macro parameter.
/// </summary>
public sealed class MacroParameterDescriptor
{
    internal MacroParameterDescriptor(
        string name,
        Type parameterType,
        MacroParameterRole role,
        int ordinal,
        bool isRequired,
        object? defaultValue,
        string? typeDisplayName = null,
        string? defaultValueDisplay = null)
    {
        Name = name;
        ParameterType = parameterType;
        Role = role;
        Ordinal = ordinal;
        IsRequired = isRequired;
        DefaultValue = defaultValue;
        TypeDisplayName = typeDisplayName ?? MacroFacts.GetParameterTypeDisplay(parameterType);
        DefaultValueDisplay = defaultValueDisplay;
    }

    /// <summary>
    /// Gets the invocation-facing parameter name.
    /// </summary>
    public string Name { get; }

    /// <summary>
    /// Gets the CLR type accepted by the parameter.
    /// </summary>
    public Type ParameterType { get; }

    /// <summary>
    /// Gets the Raven-facing display name for <see cref="ParameterType"/>.
    /// </summary>
    public string TypeDisplayName { get; }

    /// <summary>
    /// Gets how the macro invocation supplies the parameter.
    /// </summary>
    public MacroParameterRole Role { get; }

    /// <summary>
    /// Gets the zero-based invocation argument position.
    /// </summary>
    public int Ordinal { get; }

    /// <summary>
    /// Gets whether the invocation must supply the parameter.
    /// </summary>
    public bool IsRequired { get; }

    /// <summary>
    /// Gets the optional method parameter default value when one is declared.
    /// </summary>
    public object? DefaultValue { get; }

    public string? DefaultValueDisplay { get; }
}
