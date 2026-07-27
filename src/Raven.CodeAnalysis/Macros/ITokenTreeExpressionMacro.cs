using System;

namespace Raven.CodeAnalysis.Macros;

public interface ITokenTreeExpressionMacro : IMacroDefinition
{
    FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context);
}

public interface ITokenTreeExpressionMacro<TParameters> : ITokenTreeExpressionMacro, IMacroDefinition<TParameters>
    where TParameters : class
{
    FreestandingMacroExpansionResult Expand(TokenTreeMacroContext<TParameters> context);

    FreestandingMacroExpansionResult ITokenTreeExpressionMacro.Expand(TokenTreeMacroContext context)
        => throw new NotSupportedException(
            $"Typed token-tree macro '{GetType().Name}' must be invoked through {nameof(TokenTreeMacroContext<TParameters>)}.");
}
