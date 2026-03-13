namespace Raven.CodeAnalysis.Macros;

public interface IAttachedDeclarationMacro : IMacroDefinition
{
    MacroExpansionResult Expand(AttachedMacroContext context);
}
