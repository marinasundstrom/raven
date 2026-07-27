using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Describes the signature of a macro invocation at a source position.
/// </summary>
public sealed class MacroSignatureHelp
{
    internal MacroSignatureHelp(
        string name,
        MacroKind kind,
        ImmutableArray<MacroParameterDescriptor> parameters,
        int activeParameter,
        bool hasTokenTreeBody)
    {
        Name = name;
        Kind = kind;
        Parameters = parameters;
        ActiveParameter = activeParameter;
        HasTokenTreeBody = hasTokenTreeBody;
    }

    /// <summary>
    /// Gets the invocation-facing macro name.
    /// </summary>
    public string Name { get; }

    /// <summary>
    /// Gets the macro category.
    /// </summary>
    public MacroKind Kind { get; }

    /// <summary>
    /// Gets the compiler-normalized macro parameters.
    /// </summary>
    public ImmutableArray<MacroParameterDescriptor> Parameters { get; }

    /// <summary>
    /// Gets the zero-based active parameter index.
    /// </summary>
    public int ActiveParameter { get; }

    /// <summary>
    /// Gets whether the invocation accepts a token-tree body after its arguments.
    /// </summary>
    public bool HasTokenTreeBody { get; }
}
