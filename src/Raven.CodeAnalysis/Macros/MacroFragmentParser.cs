using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

// Owns fragment parsing and advancement of the supplied cursor.
internal interface MacroFragmentParser
{
    MacroSyntaxParseResult<ExpressionSyntax> ParseExpression(IMacroTokenStream stream);
    MacroSyntaxParseResult<StatementSyntax> ParseStatement(IMacroTokenStream stream);
    MacroSyntaxParseResult<TypeSyntax> ParseType(IMacroTokenStream stream);
    MacroSyntaxParseResult<PatternSyntax> ParsePattern(IMacroTokenStream stream);
    MacroSyntaxParseResult<MemberDeclarationSyntax> ParseMemberDeclaration(IMacroTokenStream stream);
}
