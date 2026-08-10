using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Describes the compiler-normalized contract of a class-authored macro.
/// </summary>
public sealed class MacroDefinitionDescriptor
{
    internal MacroDefinitionDescriptor(
        IMacroDefinition definition,
        MacroApplicationKind applicationKind,
        MacroInvocationTargets invocationTargets,
        MacroTarget attachmentTargets,
        ImmutableArray<MacroParameterDescriptor> parameters,
        bool acceptsArguments,
        bool hasTokenBody)
    {
        Definition = definition;
        ApplicationKind = applicationKind;
        InvocationTargets = invocationTargets;
        AttachmentTargets = attachmentTargets;
        Parameters = parameters;
        AcceptsArguments = acceptsArguments;
        HasTokenBody = hasTokenBody;
    }

    public IMacroDefinition Definition { get; }
    public MacroApplicationKind ApplicationKind { get; }
    public MacroInvocationTargets InvocationTargets { get; }

    /// <summary>
    /// Gets the legacy attached-target projection while attached macros migrate
    /// to typed target parameters.
    /// </summary>
    public MacroTarget AttachmentTargets { get; }

    public ImmutableArray<MacroParameterDescriptor> Parameters { get; }
    public bool AcceptsArguments { get; }
    public bool HasTokenBody { get; }
}
