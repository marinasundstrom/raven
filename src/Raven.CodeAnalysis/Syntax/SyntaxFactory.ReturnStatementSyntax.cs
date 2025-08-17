using System.Runtime.CompilerServices;

namespace Raven.CodeAnalysis.Syntax;

public static partial class SyntaxFactory
{
    public static ReturnStatementSyntax ReturnStatement(SyntaxToken returnKeyword, SyntaxToken terminatorToken) => new ReturnStatementSyntax(returnKeyword, null, terminatorToken);
}