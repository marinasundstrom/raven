using System;

namespace Raven.CodeAnalysis.Macros;

public interface IInvocableMacro : IMacroDefinition
{
    InvocableMacroExpansionResult Expand(InvocableMacroContext context);
}

public interface IInvocableMacro<TParameters> : IInvocableMacro, IMacroDefinition<TParameters>
    where TParameters : class
{
    InvocableMacroExpansionResult Expand(InvocableMacroContext<TParameters> context);

    InvocableMacroExpansionResult IInvocableMacro.Expand(InvocableMacroContext context)
        => throw new NotSupportedException(
            $"Typed invocable macro '{GetType().Name}' must be invoked through {nameof(InvocableMacroContext<TParameters>)}.");
}
