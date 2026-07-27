using System;

namespace Raven.CodeAnalysis.Macros;

public interface IAttachedDeclarationMacro : IMacroDefinition
{
    MacroExpansionResult Expand(AttachedMacroContext context);
}

public interface IAttachedDeclarationMacro<TParameters> : IAttachedDeclarationMacro, IMacroDefinition<TParameters>
    where TParameters : class
{
    MacroExpansionResult Expand(AttachedMacroContext<TParameters> context);

    MacroExpansionResult IAttachedDeclarationMacro.Expand(AttachedMacroContext context)
        => throw new NotSupportedException(
            $"Typed attached macro '{GetType().Name}' must be invoked through {nameof(AttachedMacroContext<TParameters>)}.");
}
