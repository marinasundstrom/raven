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
        System.Type? expressionResultType,
        MacroCarrierKinds carrierKinds,
        MacroBodyRequirement bodyRequirement,
        MacroTarget attachmentTargets,
        ImmutableArray<MacroParameterDescriptor> parameters,
        bool acceptsArguments,
        bool hasDeclarationInput)
    {
        Definition = definition;
        ApplicationKind = applicationKind;
        InvocationTargets = invocationTargets;
        ExpressionResultType = expressionResultType;
        CarrierKinds = carrierKinds;
        BodyRequirement = bodyRequirement;
        AttachmentTargets = attachmentTargets;
        Parameters = parameters;
        AcceptsArguments = acceptsArguments;
        HasTokenBody = bodyRequirement != MacroBodyRequirement.None;
        HasDeclarationInput = hasDeclarationInput;
    }

    public IMacroDefinition Definition { get; }
    public MacroApplicationKind ApplicationKind { get; }
    public MacroInvocationTargets InvocationTargets { get; }
    public System.Type? ExpressionResultType { get; }
    public MacroCarrierKinds CarrierKinds { get; }
    public MacroBodyRequirement BodyRequirement { get; }

    /// <summary>
    /// Gets the legacy attached-target projection while attached macros migrate
    /// to typed target parameters.
    /// </summary>
    public MacroTarget AttachmentTargets { get; }

    public ImmutableArray<MacroParameterDescriptor> Parameters { get; }
    public bool AcceptsArguments { get; }
    public bool HasTokenBody { get; }
    public bool HasDeclarationInput { get; }
}
