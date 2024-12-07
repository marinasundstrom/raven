namespace Raven.CodeAnalysis.Syntax.InternalSyntax;


public class BlockSyntax : StatementSyntax
{
    public BlockSyntax(
        SyntaxToken openBraceToken,
        SyntaxList statements,
        SyntaxToken closeBraceToken,
        int startPosition = 0,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.Block,
              [
                      openBraceToken,
                      statements,
                      closeBraceToken
              ],
              openBraceToken.FullWidth + (statements?.FullWidth ?? 0) + closeBraceToken.FullWidth,
              diagnostics,
              startPosition)
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent)
    {
        return new Syntax.BlockSyntax(this, parent);
    }
}


public abstract class ExpressionSyntax : SyntaxNode
{
    public ExpressionSyntax(SyntaxKind kind, GreenNode[] slots, int fullWidth, IEnumerable<DiagnosticInfo> diagnostics = null, int startPosition = 0) : base(kind, slots, fullWidth, diagnostics, startPosition)
    {
    }
}


public class BinaryExpressionSyntax : ExpressionSyntax
{
    public BinaryExpressionSyntax(
        ExpressionSyntax leftHandSide,
        SyntaxToken operatorToken,
        ExpressionSyntax rightHandSide,
        int startPosition = 0,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.Block,
              [
                      leftHandSide,
                      operatorToken,
                      rightHandSide
              ],
              leftHandSide.FullWidth + (operatorToken?.FullWidth ?? 0) + rightHandSide.FullWidth,
              diagnostics,
              startPosition)
    {

    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent)
    {
        return new Syntax.BinaryExpressionSyntax(this, parent);
    }
}


public class IdentifierSyntax : ExpressionSyntax
{
    public IdentifierSyntax(
        SyntaxToken identifierToken,
        int startPosition = 0,
        IEnumerable<DiagnosticInfo> diagnostics = null)
        : base(
              SyntaxKind.Identifier,
              [
                      identifierToken,
              ],
              identifierToken.FullWidth,
              diagnostics,
              startPosition)
    {
    }

    public override Syntax.SyntaxNode CreateRed(Syntax.SyntaxNode? parent)
    {
        return new Syntax.IdentifierSyntax(this, parent);
    }
}
