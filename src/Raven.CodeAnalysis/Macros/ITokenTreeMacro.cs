using System;

namespace Raven.CodeAnalysis.Macros;

public interface ITokenTreeMacro : IMacroDefinition
{
    InvocableMacroExpansionResult Expand(TokenTreeMacroContext context);
}

public interface ITokenTreeMacro<TParameters> : ITokenTreeMacro, IMacroDefinition<TParameters>
    where TParameters : class
{
    InvocableMacroExpansionResult Expand(TokenTreeMacroContext<TParameters> context);

    InvocableMacroExpansionResult ITokenTreeMacro.Expand(TokenTreeMacroContext context)
        => throw new NotSupportedException(
            $"Typed token-tree macro '{GetType().Name}' must be invoked through {nameof(TokenTreeMacroContext<TParameters>)}.");
}
