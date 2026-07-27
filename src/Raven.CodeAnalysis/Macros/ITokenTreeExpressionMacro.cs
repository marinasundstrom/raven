namespace Raven.CodeAnalysis.Macros;

public interface ITokenTreeExpressionMacro : IMacroDefinition
{
    FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context);
}
