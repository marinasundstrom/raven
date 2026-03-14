using System;

namespace Raven.CodeAnalysis.Macros;

public interface IFreestandingExpressionMacro : IMacroDefinition
{
    FreestandingMacroExpansionResult Expand(FreestandingMacroContext context);
}

public interface IFreestandingExpressionMacro<TParameters> : IFreestandingExpressionMacro, IMacroDefinition<TParameters>
    where TParameters : class
{
    FreestandingMacroExpansionResult Expand(FreestandingMacroContext<TParameters> context);

    FreestandingMacroExpansionResult IFreestandingExpressionMacro.Expand(FreestandingMacroContext context)
        => throw new NotSupportedException(
            $"Typed freestanding macro '{GetType().Name}' must be invoked through {nameof(FreestandingMacroContext<TParameters>)}.");
}
