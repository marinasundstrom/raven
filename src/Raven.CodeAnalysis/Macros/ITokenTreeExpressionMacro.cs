namespace Raven.CodeAnalysis.Macros;

public interface ITokenTreeExpressionMacro : IMacroDefinition
{
    MacroKind IMacroDefinition.Kind => MacroKind.FreestandingExpression;

    FreestandingMacroExpansionResult Expand(TokenTreeMacroContext context);
}
