using System.Diagnostics.CodeAnalysis;

namespace Raven.CodeAnalysis.Syntax;

public sealed class SyntaxNormalizer : SyntaxRewriter
{
    public override SyntaxToken VisitToken(SyntaxToken token)
    {
        if (token.Kind == SyntaxKind.None || token is { IsMissing: true, FullWidth: 0 })
        {
            return token;
        }
        
        return base.VisitToken(token);
    }

    public override SyntaxNode? VisitIfStatement(IfStatementSyntax node)
    {
        return node;
    }
}