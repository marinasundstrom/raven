using System;

namespace Raven.CodeAnalysis.Macros;

/// <summary>
/// Declares the source carrier shapes and token-body requirement for a macro.
/// </summary>
/// <remarks>
/// This attribute lets compact Raven macro declarations opt into invocation
/// shapes that cannot be inferred from their typed parameters alone.
/// </remarks>
[AttributeUsage(AttributeTargets.Class | AttributeTargets.Method, AllowMultiple = false, Inherited = false)]
public sealed class MacroCarrierAttribute : Attribute
{
    public MacroCarrierAttribute(
        MacroCarrierKinds carrierKinds,
        MacroBodyRequirement bodyRequirement = MacroBodyRequirement.Default)
    {
        CarrierKinds = carrierKinds;
        BodyRequirement = bodyRequirement;
    }

    public MacroCarrierKinds CarrierKinds { get; }

    public MacroBodyRequirement BodyRequirement { get; }
}
