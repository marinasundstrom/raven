using System;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Identifies how a typed macro parameter appears at its invocation site.
/// </summary>
public enum MacroParameterKind
{
    /// <summary>
    /// The parameter is supplied by position and originates from the parameter
    /// object's public constructor.
    /// </summary>
    Positional,

    /// <summary>
    /// The parameter is supplied by name and originates from a public writable
    /// property on the parameter object.
    /// </summary>
    Named
}

/// <summary>
/// Describes one compiler-normalized input in a typed macro parameter object.
/// </summary>
public sealed class MacroParameterDescriptor
{
    internal MacroParameterDescriptor(
        string name,
        Type parameterType,
        MacroParameterKind kind,
        MacroParameterRole role,
        int ordinal,
        bool isRequired,
        object? defaultValue,
        string? typeDisplayName = null,
        string? defaultValueDisplay = null)
    {
        Name = name;
        ParameterType = parameterType;
        Kind = kind;
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
    /// Gets whether the parameter is positional or named.
    /// </summary>
    public MacroParameterKind Kind { get; }

    /// <summary>
    /// Gets how the macro invocation supplies the parameter.
    /// </summary>
    public MacroParameterRole Role { get; }

    /// <summary>
    /// Gets the zero-based constructor position, or <c>-1</c> for a named
    /// property parameter.
    /// </summary>
    public int Ordinal { get; }

    /// <summary>
    /// Gets whether the invocation must supply the parameter.
    /// </summary>
    public bool IsRequired { get; }

    /// <summary>
    /// Gets the optional constructor default value when one is declared.
    /// </summary>
    public object? DefaultValue { get; }

    public string? DefaultValueDisplay { get; }
}
