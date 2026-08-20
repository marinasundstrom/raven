using System;

namespace Raven.CodeAnalysis.Macros;

public interface ITokenTreeMacro : IMacroDefinition
{
    FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context);
}

public interface ITokenTreeMacro<TParameters> : ITokenTreeMacro, IMacroDefinition<TParameters>
    where TParameters : class
{
    FreestandingMacroExpansionResult Expand(TokenTreeMacroContext<TParameters> context);

    FreestandingMacroExpansionResult ITokenTreeMacro.Expand(TokenTreeMacroContext context)
        => throw new NotSupportedException(
            $"Typed token-tree macro '{GetType().Name}' must be invoked through {nameof(TokenTreeMacroContext<TParameters>)}.");
}
