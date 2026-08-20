using System;

namespace Raven.CodeAnalysis.Macros;

public interface IInvocableMacro : IMacroDefinition
{
    FreestandingMacroExpansionResult Expand(FreestandingMacroContext context);
}

public interface IInvocableMacro<TParameters> : IInvocableMacro, IMacroDefinition<TParameters>
    where TParameters : class
{
    FreestandingMacroExpansionResult Expand(FreestandingMacroContext<TParameters> context);

    FreestandingMacroExpansionResult IInvocableMacro.Expand(FreestandingMacroContext context)
        => throw new NotSupportedException(
            $"Typed invocable macro '{GetType().Name}' must be invoked through {nameof(FreestandingMacroContext<TParameters>)}.");
}
